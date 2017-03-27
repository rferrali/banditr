#' @include banditMethods.R

setGeneric("getJob", function(object, jobId = NULL) {
  conn <- do.call(RODBC::odbcDriverConnect, object$db)
  if(is.null(jobId)) {
    result <- RODBC::sqlQuery(conn, "SELECT TOP 1 * FROM dbo.jobs ORDER BY job DESC")
  } else {
    jobId <- as.numeric(jobId)
    jobs <- paste(jobId, collapse = ", ")
    result <- RODBC::sqlQuery(conn, sprintf("SELECT * FROM dbo.jobs WHERE job IN (%s)", jobs))
    result <- result[match(jobId, result$job),]
    test <- as.numeric(result$job)
    if(!identical(sort(test), sort(jobId))) stop("Some jobs can't be found.")
  }
  RODBC::odbcClose(conn)
  result
})

setMethod("getModel", signature(object = "banditDb"),
          function(object, modelId) {
            conn <- do.call(RODBC::odbcDriverConnect, object$db)
            # validation
            if(!is.null(modelId)) {
              modelId <- as.numeric(modelId)
              models <- paste(modelId, collapse = ", ")
              test <- RODBC::sqlQuery(conn, sprintf("SELECT job FROM dbo.jobs WHERE type = 'train' AND job IN (%s) ", models))
              test <- as.numeric(test$job)
              if(!identical(sort(test), sort(modelId))) stop("Some ids don't match any training job.")
            }
            # selection
            if(is.null(modelId)) { # model
              modelId <- RODBC::sqlQuery(conn, "SELECT TOP 1 job FROM dbo.jobs WHERE type = 'train' ORDER BY job DESC")$job
            }
            RODBC::odbcClose(conn)
            models <- object$models[modelId]
            models <- lapply(1:length(models), function(i) {
              load(paste0(object$path, models[i]))
              model$lambdaRidge <- getParam(object, modelId[i], "lambdaRidge")
              model$lambdaLasso <- getParam(object, modelId[i], "lambdaLasso")
              return(model)
            })
            if(length(models) == 1) return(models[[1]])
            return(models)
          })


setMethod("getParam", signature(object = "banditDb"),
          function(object, modelId, param) {
            param <- match.arg(param, c("lambdaRidge", "lambdaLasso"))
            conn <- do.call(RODBC::odbcDriverConnect, object$db)
            r <- RODBC::sqlQuery(conn,
                                 paste0("SELECT TOP 1 value FROM dbo.jobs WHERE job < ",
                                        modelId, " AND param = '", param,"' ORDER BY job DESC"))$value
            RODBC::odbcClose(conn)
            if(length(r) > 0) return(r)
            return(0)
          })

#' @export

setMethod("getSamples", signature(object = "banditDb"),
          function(object, samples) {
            conn <- do.call(RODBC::odbcDriverConnect, object$db)
            if(is.null(samples)) {
              query <- "SELECT * FROM dbo.samples WHERE y IS NULL"
            } else {
              samples <- unique(as.numeric(samples))
              qsamples <- paste(samples, collapse = ", ")
              query <- paste0("SELECT * FROM dbo.samples WHERE id IN (", qsamples, ")")
            }
            df <- RODBC::sqlQuery(conn, query)
            RODBC::odbcClose(conn)
            callNextMethod(df, samples)
          })

#' @export
setMethod("getTuning", signature(object = "banditDb"),
          function(object, modelId = NULL, param = "lambdaRidge") {
            conn <- do.call(RODBC::odbcDriverConnect, object$db)
            param <- match.arg(param, c("lambdaRidge", "lambdaLasso"))
            # validation
            if(!is.null(modelId)) {
              modelId <- as.numeric(modelId)
              models <- paste(modelId, collapse = ", ")
              test <- RODBC::sqlQuery(conn, sprintf("SELECT job FROM dbo.jobs WHERE type = 'tune' AND job IN (%s) ", models))
              test <- as.numeric(test$job)
              if(!identical(sort(test), sort(modelId))) stop("Some ids don't match any training job.")
            }
            # selection
            if(is.null(modelId)) { # model
              modelId <- RODBC::sqlQuery(conn, paste0("SELECT MAX(job) AS job FROM dbo.jobs WHERE param = '", param, "'"))$job
            }
            RODBC::odbcClose(conn)
            if(is.na(modelId[1])) return(list(model = NULL,
                                           value = 0,
                                           param = param))
            model <- object$models[modelId]
            model <- lapply(model, function(m) {
              load(paste0(object$path, m))
              return(model)
            })
            if(length(model) == 1) return(model[[1]])
            return(model)
          })

#' @export

setMethod("summary", signature(object = "banditDb"),
          function(object, ...) {
            conn <- do.call(RODBC::odbcDriverConnect, object$db)
            N <- RODBC::sqlQuery(conn, "SELECT COUNT(*) AS N FROM dbo.samples WHERE y IS NOT NULL")$N
            nTraining <- RODBC::sqlQuery(conn, "SELECT COUNT(*) AS N FROM dbo.jobs WHERE type = 'train'")$N
            start <- RODBC::sqlQuery(conn, "SELECT MIN(date) AS N FROM dbo.jobs")$N
            nTuning <- RODBC::sqlQuery(conn, "SELECT COUNT(*) AS N FROM dbo.jobs WHERE type = 'tune'")$N
            lastJobs <- RODBC::sqlQuery(conn, "SELECT * FROM (SELECT TOP 6 * FROM dbo.jobs ORDER BY job DESC) AS t ORDER BY job")
            coef <- coef(object)
            lambdaRidge <- getTuning(object, param = "lambdaRidge")
            autoRidge <- !is.null(lambdaRidge$model)
            lambdaRidge <- lambdaRidge$value
            lambdaLasso <- getTuning(object, param = "lambdaLasso")
            autoLasso <- !is.null(lambdaLasso$model)
            lambdaLasso <- lambdaLasso$value
            reward <- rev(statistic(object, FUN = statReward))[1]
            maxReward <- NULL
            if(object$family == "binomial") maxReward <- statistic(object, FUN = statMaxReward)
            if(!is.null(maxReward)) maxReward <- rev(maxReward)[1]
            lastAddOutcomes <- RODBC::sqlQuery(conn, "SELECT TOP 1 * FROM dbo.jobs WHERE type = 'addOutcomes' ORDER BY job DESC")
            if(nrow(lastAddOutcomes) == 0) lastAddOutcomes <- NULL
            RODBC::odbcClose(conn)

            out <- list(formula = object$formula,
                        lambdaRidge = lambdaRidge,
                        autoRidge = autoRidge,
                        lambdaLasso = lambdaLasso,
                        autoLasso = autoLasso,
                        N = N,
                        nTraining = nTraining,
                        nTuning = nTuning,
                        lastJobs = lastJobs,
                        start = start,
                        coef = coef,
                        reward = reward,
                        maxReward = maxReward,
                        lastAddOutcomes = lastAddOutcomes)
            class(out) <- "summary.bandit"
            return(out)
          })


#### STATISTICS ####

# getPreviousTraining <- function(object, jobId) {
#   jobId <- as.numeric(jobId)
#   if(any(!jobId %in% object$jobs$job)) stop("some jobIds cannot be found")
#   jobs <- object$jobs[object$jobs$job %in% jobId | object$jobs$type == "train",]
#   jobs <- jobs[order(jobs$job),]
#   jobs$trainJob <- ifelse(jobs$type == "train", jobs$job, NA)
#   jobs$trainJob <- zoo::na.locf(jobs$trainJob)
#   jobs$trainJob[match(jobId, jobs$job)]
# }

setMethod("getPreviousTraining", signature(object = "banditDb"),
          function(object, jobId) {
            jobId <- as.numeric(jobId)
            conn <- do.call(RODBC::odbcDriverConnect, object$db)
            qstring <- paste(jobId, collapse = ", ")
            test <- RODBC::sqlQuery(conn, sprintf("SELECT job FROM dbo.jobs WHERE job IN (%s)", qstring))$job
            test <- as.numeric(test)
            if(!identical(sort(jobId), sort(test))) stop("some jobIds cannot be found")
            jobs <- RODBC::sqlQuery(conn, sprintf("SELECT job, type FROM dbo.jobs WHERE job IN (%s) OR type = 'train' ORDER BY job", qstring))
            RODBC::odbcClose(conn)
            jobs$trainJob <- ifelse(jobs$type == "train", jobs$job, NA)
            jobs$trainJob <- zoo::na.locf(jobs$trainJob)
            jobs$trainJob[match(jobId, jobs$job)]
          })

setMethod("statistic", signature(object = "banditDb"),
          function(object, jobMin, jobMax, FUN, cumulative = TRUE) {
            if(!missing(jobMax) & !missing(jobMin)) {
              if(jobMin > jobMax) stop("jobMin must be smaller or equal to jobMax")
            } else {
              jobMin <- 1
            }
            conn <- do.call(RODBC::odbcDriverConnect, object$db)
            if(missing(jobMax)) jobMax <- RODBC::sqlQuery(conn,
                                                          "SELECT MAX(job) AS N FROM dbo.jobs")$N

            jobs <- RODBC::sqlQuery(conn,
                                    sprintf("SELECT * FROM dbo.jobs WHERE type = 'addOutcomes' AND
                                            job >= %i AND job <= %i ORDER BY job", jobMin, jobMax))
            RODBC::odbcClose(conn)
            if(nrow(jobs) == 0) {
              warning("no addOutcomes job found in the selected range")
              return(0)
            }
            outcome <- FUN(object, jobs$job)
            if(cumulative) outcome <- cumsum(outcome)
            names(outcome) <- jobs$job
            outcome
          })

setMethod("statReward", signature(object = "banditDb"),
          function(object, jobs) {
            conn <- do.call(RODBC::odbcDriverConnect, object$db)
            qstring <- paste(jobs, collapse = ",")
            result <- RODBC::sqlQuery(conn, sprintf("SELECT SUM(y) AS y, jobOutcome FROM dbo.samples WHERE jobOutcome IN (%s) GROUP BY jobOutcome", qstring))
            RODBC::odbcClose(conn)
            out <- result$y
            names(out) <- result$jobOutcome
            return(out)
          })

setMethod("statMaxReward", signature(object = "banditDb"),
          function(object, jobs) {
            if(object$family != "binomial") {
              warning("maxReward not well-defined for this family")
              return(NULL)
            }
            conn <- do.call(RODBC::odbcDriverConnect, object$db)
            qstring <- paste(jobs, collapse = ",")
            result <- RODBC::sqlQuery(conn, sprintf("SELECT COUNT(*) AS y, jobOutcome FROM dbo.samples WHERE jobOutcome IN (%s) GROUP BY jobOutcome", qstring))
            RODBC::odbcClose(conn)
            out <- result$y
            names(out) <- result$jobOutcome
            return(out)
          })

setMethod("statExpectedReward", signature(object = "banditDb"),
          function(object, jobs) {
            trainJobs <- getPreviousTraining(object, jobs)
            conn <- do.call(RODBC::odbcDriverConnect, object$db)
            reward <- mapply(FUN = function(add, train, conn) {
              df <- RODBC::sqlQuery(conn, sprintf("SELECT id FROM dbo.samples WHERE jobOutcome = %i", add))
              pr <- predict.bandit(object, samplesId = df$id, modelId = train, type = "response")
              sum(pr)
            }, jobs, trainJobs, list(conn = conn))
            RODBC::odbcClose(conn)
            names(reward) <- jobs
            reward
          })

setMethod("statUncertainty", signature(object = "banditDb"),
          function(object, jobs) {
            trainJobs <- getPreviousTraining(object, jobs)
            conn <- do.call(RODBC::odbcDriverConnect, object$db)
            reward <- mapply(FUN = function(add, train, conn) {
              df <- RODBC::sqlQuery(conn, sprintf("SELECT id FROM dbo.samples WHERE jobOutcome = %i", add))
              pr <- predict.bandit(object, samplesId = df$id, modelId = train, type = "uncertainty")
              mean(pr)
            }, jobs, trainJobs, list(conn = conn))
            RODBC::odbcClose(conn)
            names(reward) <- jobs
            reward
          })

setMethod("statMSE", signature(object = "banditDb"),
          function(object, jobs) {
            trainJobs <- getPreviousTraining(object, jobs)
            conn <- do.call(RODBC::odbcDriverConnect, object$db)
            reward <- mapply(FUN = function(add, train, conn) {
              df <- RODBC::sqlQuery(conn, sprintf("SELECT id, y FROM dbo.samples WHERE jobOutcome = %i", add))
              pr <- predict.bandit(object, samplesId = df$id, modelId = train, type = "response")
              mean((df$y - pr)^2)
            }, jobs, trainJobs, list(conn = conn))
            RODBC::odbcClose(conn)
            names(reward) <- jobs
            reward
          })
