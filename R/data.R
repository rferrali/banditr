#### constructor functions ####

dfData <- function(jobs, samples, statistics) {
  out <- list(jobs = jobs,
       samples = samples,
       models = list(list(NULL)),
       coef = data.frame())
  statistics <- matrix(nrow = 0, ncol = length(statistics)+1,
                       dimnames = list(NULL, c("id", statistics)))
  out$stats <- as.data.frame(statistics)
  class(out) <- "dfData"
  out
}

dbData <- function(jobs, samples, statistics, db, path) {
  conn <- do.call(RODBC::odbcDriverConnect, db)
  RODBC::sqlDrop(conn, "dbo.samples", errors = FALSE)
  RODBC::sqlSave(conn, samples, "dbo.samples", rownames = FALSE, safer = FALSE,
                 varTypes = c(jobSamples = "int",
                              jobOutcome = "int"))
  RODBC::sqlDrop(conn, "dbo.jobs", errors = FALSE)
  RODBC::sqlQuery(conn, "CREATE TABLE [dbo].[jobs](
                  [job] [int] NOT NULL,
                  [date] [datetime] NOT NULL,
                  [type] [varchar](50) NOT NULL,
                  [param] [varchar](50) NULL,
                  [value] [float] NULL
  )")
    RODBC::sqlSave(conn, jobs, "dbo.jobs", rownames = FALSE, append = TRUE)
    RODBC::sqlDrop(conn, "dbo.stats", errors = FALSE)
    query <- sprintf("CREATE TABLE [dbo].[stats] ([id] [int] NOT NULL, %s)",
                     paste0("[", statistics, rep("] [float]", length(statistics)), collapse = ", "))
    RODBC::sqlQuery(conn, query)
    RODBC::sqlDrop(conn, "dbo.coef", errors = FALSE)
    RODBC::odbcClose(conn)
    out <- list(db = db,
                models = c(NA),
                path = path)
    class(out) <- "dbData"
    out
}

#### validation methods ####

vViolatePrimaryKey <- function(data, df) UseMethod("vViolatePrimaryKey", data)
vViolatePrimaryKey.dfData <- function(data, df) {
  if(any(df$id %in% data$samples$id)) stop("some samples you are adding are already in the bandit.")
}
vViolatePrimaryKey.dbData <- function(data, df) {
  conn <- do.call(RODBC::odbcDriverConnect, data$db)
  test <- RODBC::sqlQuery(conn, paste0("SELECT y FROM dbo.samples WHERE id IN (",
                                       paste(df$id, collapse = ", "),
                                       ")"))
  RODBC::odbcClose(conn)
  if(nrow(test) != 0) stop("some samples you are adding are already in the bandit.")
}

vOutcomeMismatch <- function(data, y) UseMethod("vOutcomeMismatch", data)
vOutcomeMismatch.dfData <- function(data, y) {
  ids <- as.numeric(names(y))
  select <- match(ids, data$samples$id)
  if(any(is.na(select))) stop("some outcomes you are adding don't correspond to any samples in the bandit.")
  if(any(!is.na(data$samples$y[select]))) stop("some outcomes you are adding are already in the bandit.")
}
vOutcomeMismatch.dbData <- function(data, y) {
  ids <- as.numeric(names(y))
  conn <- do.call(RODBC::odbcDriverConnect, data$db)
  test1 <- RODBC::sqlQuery(conn, paste0("SELECT y FROM dbo.samples WHERE id IN (",
                                       paste(ids, collapse = ", "),
                                       ")"))
  test2 <- RODBC::sqlQuery(conn, paste0("SELECT y FROM dbo.samples WHERE id IN (",
                                       paste(ids, collapse = ", "),
                                       ") AND y IS NULL"))
  RODBC::odbcClose(conn)
  if(nrow(test1) != length(y)) stop("some outcomes you are adding don't correspond to any samples in the bandit.")
  if(nrow(test2) != length(y)) stop("some outcomes you are adding are already in the bandit.")
}

#### write methods ####

wSamples <- function(data, df, ...) UseMethod("wSamples", data)
wSamples.dfData <- function(data, df, ...) {
  data$samples <- rbind(data$samples, df)
  data
}
wSamples.dbData <- function(data, df, bulk = FALSE, ...) {
  conn <- do.call(RODBC::odbcDriverConnect, data$db)
  if(bulk) {
    dots <- list(...)
    dots <- c(dots,
              list(conn = conn,
                   df = df,
                   table = "dbo.samples"))
    if(!"path" %in% names(dots)) dots$path <- path
    do.call(bulkInsert, dots)
  } else {
    RODBC::sqlSave(conn, df, "dbo.samples", rownames = FALSE, append = TRUE)
  }
  RODBC::odbcClose(conn)
  data
}

wJob <- function(data, job, type, param = NULL, value = NULL) UseMethod("wJob", data)
aj <- function(job, type, param, value) {
  add <- data.frame(job = job+1,
                    date = Sys.time(),
                    type = type,
                    param = NA,
                    value = NA)
  if(type == "tune") {
    add$param <- param
    add$value <- value
  }
  add
}
wJob.dfData <- function(data, job, type, param = NULL, value = NULL) {
  job <- aj(job, type, param, value)
  data$jobs <- rbind(data$jobs, job)
  data
}
wJob.dbData <- function(data, job, type, param = NULL, value = NULL) {
  job <- aj(job, type, param, value)
  conn <- do.call(RODBC::odbcDriverConnect, data$db)
  RODBC::sqlSave(conn,
                 job,
                 "dbo.jobs",
                 rownames = FALSE,
                 append = TRUE)
  RODBC::odbcClose(conn)
  data
}

wModel <- function(data, model = NULL, job = NULL) UseMethod("wModel", data)
wModel.dfData <- function(data, model = NULL, job = NULL) {
  if(is.null(model)) model <- list(NULL)
  data$models <- c(data$models, list(model))
  data
}
wModel.dbData <- function(data, model = NULL, job = NULL) {
  if(is.null(model)) {
    fname <- NA
  } else {
    fname <- paste0("output", job + 1, ".RData")
    save(model, file = paste0(data$path, fname))
  }
  data$models <- c(data$models, fname)
  data
}

wOutcome <- function(data, y, job) UseMethod("wOutcome", data)
wOutcome.dfData <- function(data, y, job) {
  select <- match(as.numeric(names(y)), data$samples$id)
  data$samples$y[select] <- y
  data$samples$jobOutcome[select] <- job+1
  data
}
wOutcome.dbData <- function(data, y, job) {
  conn <- do.call(RODBC::odbcDriverConnect, data$db)
  RODBC::sqlUpdate(conn,
                   dat = data.frame(y = y, id = as.numeric(names(y)), jobOutcome = job+1),
                   tablename = "dbo.samples",
                   index = "id")
  RODBC::odbcClose(conn)
  data
}

wCoef <- function(data, coef, job) UseMethod("wCoef", data)
wCoef.dfData <- function(data, coef, job) {
  coef <- as.data.frame(matrix(coef, nrow = 1,
                               dimnames = list(NULL, names(coef))))
  if(nrow(data$coef) == 0) {
    data$coef <- cbind(jobTrain = job+1, coef)
  } else {
    coef$jobTrain <- job+1
    data$coef <- rbind(data$coef, coef)
  }
  data
}
wCoef.dbData <- function(data, coef, job) {
  conn <- do.call(RODBC::odbcDriverConnect, data$db)
  tables <- RODBC::sqlTables(conn)
  coef <- as.data.frame(matrix(coef, nrow = 1,
                               dimnames = list(NULL, names(coef))))
  coef <- cbind(jobTrain = job+1, coef)
  if(!"coef" %in% tables$TABLE_NAME) {
    types <- c("int", rep("float", ncol(coef)-1))
    names(types) <- colnames(coef)
    RODBC::sqlSave(conn, coef, "dbo.coef", rownames = FALSE, append = TRUE,
                   varTypes = types)
  }
  else {
    RODBC::sqlSave(conn, coef, "dbo.coef", rownames = FALSE, append = TRUE)
  }
  RODBC::odbcClose(conn)
  data
}

wStatistics <- function(data, df) UseMethod("wStatistics", data)
wStatistics.dfData <- function(data, df) {
  data$stats <- rbind(data$stats, df)
  data
}
wStatistics.dbData <- function(data, df) {
  conn <- do.call(RODBC::odbcDriverConnect, data$db)
  RODBC::sqlSave(conn, df, "dbo.stats", rownames = FALSE, append = TRUE)
  RODBC::odbcClose(conn)
  data
}

#### delete methods ####

dSamples <- function(data) UseMethod("dSamples", data)
dSamples.dfData <- function(data) {
  job <- max(data$samples$jobSamples)
  data$samples <- data$samples[-which(data$samples$jobSamples == job),]
  data
}
dSamples.dbData <- function(data) {
  conn <- do.call(RODBC::odbcDriverConnect, data$db)
  job <- RODBC::sqlQuery(conn, "SELECT MAX(jobSamples) AS job FROM dbo.samples")$job
  RODBC::sqlQuery(conn, paste0("DELETE FROM dbo.samples WHERE jobSamples = ", job))
  RODBC::odbcClose(conn)
  data
}

dOutcomes <- function(data) UseMethod("dOutcomes", data)
dOutcomes.dfData <- function(data, job) {
  job <- max(data$samples$jobOutcome, na.rm = TRUE)
  data$samples$y[which(data$samples$jobOutcome == job)] <- NA
  data$samples$jobOutcome[which(data$samples$jobOutcome == job)] <- NA
  data
}
dOutcomes.dbData <- function(data) {
  conn <- do.call(RODBC::odbcDriverConnect, data$db)
  job <- RODBC::sqlQuery(conn, "SELECT MAX(jobOutcome) AS job FROM dbo.samples")$job
  RODBC::sqlQuery(conn, paste0("UPDATE dbo.samples SET jobOutcome = NULL,
                                   y = NULL WHERE jobOutcome = ", job))
  RODBC::odbcClose(conn)
  data
}

dModels <- function(data) UseMethod("dModels", data)
dModels.dfData <- function(data) {
  data$models[[length(data$models)]] <- NULL
  data
}
dModels.dbData <- function(data) {
  cmod <- data$models[length(data$models)]
  data$models <- data$models[-length(data$models)]
  if(!is.na(cmod)) file.remove(paste0(data$path, cmod))
  data
}

dJobs <- function(data) UseMethod("dJobs", data)
dJobs.dfData <- function(data) {
  data$jobs <- data$jobs[-nrow(data$jobs),]
  data
}
dJobs.dbData <- function(data) {
  conn <- do.call(RODBC::odbcDriverConnect, data$db)
  job <- RODBC::sqlQuery(conn, "SELECT MAX(job) AS job FROM dbo.jobs")$job
  RODBC::sqlQuery(conn, paste0("DELETE FROM dbo.jobs WHERE job = ", job))
  RODBC::odbcClose(conn)
  data
}

dCoef <- function(data) UseMethod("dCoefs", data)
dCoef.dfData <- function(data) {
  data$coefs <- data$coefs[-nrow(data$coefs),]
  data
}
dCoef.dbData <- function(data) {
  conn <- do.call(RODBC::odbcDriverConnect, data$db)
  job <- RODBC::sqlQuery(conn, "SELECT MAX(jobTrain) AS job FROM dbo.coef")$job
  RODBC::sqlQuery(conn, paste0("DELETE FROM dbo.coef WHERE jobTrain = ", job))
  RODBC::odbcClose(conn)
  data
}

dStatistics <- function(data) UseMethod("dStatistics", data)
dStatistics.dfData <- function(data) {
  del <- merge(data$stats, data$samples[,c("id", "jobOutcome")])
  del <- del$id[del$jobOutcome == max(del$jobOutcome)]
  data$stats <- data$stats[!stats$id %in% del,]
  data
}
dStatistics.dbData <- function(data) {
  conn <- do.call(RODBC::odbcDriverConnect, data$db)
  job <- RODBC::sqlQuery(conn, "SELECT MAX(jobOutcome) AS job FROM dbo.samples")$job
  RODBC::sqlQuery(conn, sprintf("DELETE st FROM dbo.stats st INNER JOIN dbo.samples s ON
                  st.id = s.id WHERE s.jobOutcome = %s", job))
  RODBC::odbcClose(conn)
  data
}

#### read methods ####

rSamples <- function(data, what) UseMethod("rSamples", data)
rSamples.dfData <- function(data, what) {
  if(what[1] == "current") {
    return(data$samples[!is.na(data$samples$y),])
  } else if(what[1] == "last") {
    j <- rJobs.dfData(data, "lastTrain")$job
    return(data$samples[which(!is.na(data$samples$y) & (data$samples$jobSamples == 1 | data$samples$jobOutcome < j)),])
  } else if(what[1] == "remaining") {
    return(data$samples[is.na(data$samples$y),])
  } else if(substr(what[1],1,3) == "job") {
    j <- as.numeric(substr(what[1], 4, nchar(what)))
    return(data$samples[which(!is.na(data$samples$y) & (data$samples$jobSamples == 1 | data$samples$jobOutcome < j)),])
  } else if(all(!is.na(as.numeric(what)))) {
    what <- unique(as.numeric(what))
    df <- data$samples[data$samples$id %in% what,]
    df <- df[match(what, df$id),]
    if(!identical(what, as.numeric(df$id))) {
      stop("Some samples cannot be found in the bandit.")
    }
    return(df)
  }
}
rSamples.dbData <- function(data, what) {
  conn <- do.call(RODBC::odbcDriverConnect, data$db)
  if(what[1] == "current") {
    df <- RODBC::sqlQuery(conn, "SELECT * FROM dbo.samples WHERE y IS NOT NULL")
  } else if(what[1] == "last") {
    j <- rJobs.dbData(data, "lastTrain")$job
    df <- RODBC::sqlQuery(conn, paste0("SELECT * FROM dbo.samples
                          WHERE y IS NOT NULL AND (jobSamples = 1 OR jobOutcome < ", j, ")"))
  } else if(what[1] == "remaining") {
    df <- RODBC::sqlQuery(conn, "SELECT * FROM dbo.samples WHERE y IS NULL")
  } else if(substr(what[1],1,3) == "job") {
    j <- as.numeric(substr(what[1], 4, nchar(what)))
    df <- RODBC::sqlQuery(conn, paste0("SELECT * FROM dbo.samples
                          WHERE y IS NOT NULL AND (jobSamples = 1 OR jobOutcome < ", j, ")"))
  } else if(all(!is.na(as.numeric(what)))) {
    what <- unique(as.numeric(what))
    qsamples <- paste(what, collapse = ", ")
    query <- paste0("SELECT * FROM dbo.samples WHERE id IN (", qsamples, ")")
    df <- RODBC::sqlQuery(conn, query)
    df <- df[match(what, df$id),]
    if(!identical(what, as.numeric(df$id))) {
      stop("Some samples cannot be found in the bandit.")
    }
  }
  RODBC::odbcClose(conn)
  return(df)
}

rJobs <- function(data, what) UseMethod("rJobs", data)
rJobs.dfData <- function(data, what) {
  if(what == "last") {
    return(data$jobs[nrow(data$jobs),])
  }
  if(what == "lastTrain") {
    j <- data$jobs[data$jobs$type == "train",]
    return(j[j$job == max(j$job),])
  }
  if(what == "all") {
    return(data$jobs)
  }
}
rJobs.dbData <- function(data, what) {
  conn <- do.call(RODBC::odbcDriverConnect, data$db)
  if(what == "last") {
    df <- RODBC::sqlQuery(conn, "SELECT TOP 1 * FROM dbo.jobs ORDER BY job DESC")
  }
  if(what == "lastTrain") {
    df <- RODBC::sqlQuery(conn, "SELECT TOP 1 * FROM dbo.jobs WHERE type = 'train' ORDER BY job DESC")
  }
  if(what == "all") {
    df <- RODBC::sqlQuery(conn, "SELECT * FROM dbo.jobs ORDER BY job DESC")
  }
  RODBC::odbcClose(conn)
  return(df)
}

rTrain <- function(data, what) UseMethod("rTrain", data)
rTrain.dfData <- function(data, what) {
  if(what  == "last") {
    newModel <- data$jobs$job[which(data$jobs$type == "train")]
    if (length(newModel) > 0) {
      return(data$models[[rev(newModel)[1]]])
    } else {
      return(NULL)
    }
  } else if (!is.na(as.numeric(what))) {
    return(data$models[[what]])
  }
}
rTrain.dbData <- function(data, what) {
  if(what  == "last") {
    conn <- do.call(RODBC::odbcDriverConnect, data$db)
    newModel <- RODBC::sqlQuery(conn, "SELECT TOP 1 job FROM dbo.jobs WHERE type = 'train' ORDER BY job DESC")$job
    RODBC::odbcClose(conn)
    if (length(newModel) > 0) {
      load(paste0(data$path, data$models[newModel]))
      return(model)
    } else {
      return(NULL)
    }
  } else if (!is.na(as.numeric(what))) {
    m <- data$models[newModel]
    if(!is.na(m)) {
      load(paste0(data$path, m))
      return(model)
    } else {
      return(NULL)
    }
  }
}

rTune <- function(data, what, params) {
  if(is.numeric(what) & length(what) == 1) {
    UseMethod("rTune", data)
  }
  if(what == "last") {
    what <- rJobs(data, what)$job
    rTune(data, what, params)
  }
}
rTune.dfData <- function(data, what, params) {
  values <- lapply(params, function(ty) {
    prm <- data$jobs$value[which(data$jobs$param == ty & data$jobs$job <= what)]
    if(length(prm) == 0) {
      return(0)
    } else {
      return(prm[length(prm)])
    }
  })
  names(values) <- params
  return(values)
}
rTune.dbData <- function(data, what, params) {
  conn <- do.call(RODBC::odbcDriverConnect, data$db)
  if(is.numeric(what) & length(what) == 1) {
    qParams <- paste0("('", paste0(params, collapse = "', '"), "')")
    query <- sprintf("SELECT j.param, value FROM dbo.jobs j
                     INNER JOIN
                     (SELECT param, MAX(job) mj FROM jobs WHERE param IS NOT NULL AND job <= %s GROUP BY param) mj
                     ON j.param = mj.param
                     WHERE j.job = mj.mj
                     AND j.param IN %s AND job <= %s", what, qParams, what)
    res <- RODBC::sqlQuery(conn, query)
    params <- data.frame(param = params)
    res <- merge(res, params, all = TRUE)
    res[is.na(res)] <- 0
    values <- as.list(as.numeric(res$value))
    names(values) <- res$param
  }
  RODBC::odbcClose(conn)
  return(values)
}

rCoef <- function(data, what) UseMethod("rCoef", data)
rCoef.dfData <- function(data, what) {
  if(what == "all") {
    return(data$coef)
  }
}
rCoef.dbData <- function(data, what) {
  conn <- do.call(RODBC::odbcDriverConnect, data$db)
  if(what == "all") {
    q <- RODBC::sqlQuery(conn, "SELECT * FROM dbo.coef ORDER BY jobTrain")
  }
  RODBC::odbcClose(conn)
  return(q)
}


rSummary <- function(data) UseMethod("rSummary", data)
rSummary.dfData <- function(data) {
  out <- list(N = nrow(data$samples[!is.na(data$samples$y),]),
              nTraining = nrow(data$jobs[data$jobs$type == "train",]),
              nTuning = nrow(data$jobs[data$jobs$type == "tune",]),
              start = min(data$jobs$date),
              lastJobs = tail(data$jobs))
  lastAddOutcomes <- data$jobs[data$jobs$type == "addOutcomes",]
  lastAddOutcomes <- lastAddOutcomes[which.max(lastAddOutcomes$job),]
  if(nrow(lastAddOutcomes) == 0) lastAddOutcomes <- NULL
  out$lastAddOutcomes <- lastAddOutcomes
  out
}
rSummary.dbData <- function(data) {
  conn <- do.call(RODBC::odbcDriverConnect, data$db)
  out <- list(N = RODBC::sqlQuery(conn,
                                  "SELECT COUNT(*) AS N FROM dbo.samples WHERE y IS NOT NULL")$N,
              nTraining = RODBC::sqlQuery(conn,
                                          "SELECT COUNT(*) AS N FROM dbo.jobs WHERE type = 'train'")$N,
              nTuning = RODBC::sqlQuery(conn,
                                        "SELECT COUNT(*) AS N FROM dbo.jobs WHERE type = 'tune'")$N,
              start = RODBC::sqlQuery(conn,
                                      "SELECT MIN(date) AS N FROM dbo.jobs")$N,
              lastJobs = RODBC::sqlQuery(conn,
                                         "SELECT * FROM (SELECT TOP 6 * FROM dbo.jobs ORDER BY job DESC) AS t ORDER BY job"),
              lastAddOutcomes = RODBC::sqlQuery(conn,
                                                "SELECT TOP 1 * FROM dbo.jobs WHERE type = 'addOutcomes' ORDER BY job DESC"))

  if(nrow(out$lastAddOutcomes) == 0) out$lastAddOutcomes <- NULL
  RODBC::odbcClose(conn)
  out
}

rStatistics <- function(data) UseMethod("rStatistics", data)
rStatistics.dfData <- function(data) {
  df <- merge(data$stats, data$samples[,c("id", "y", "jobOutcome")])
  train <- data$jobs
  train$count <- as.numeric(train$type == "train")
  train$count <- cumsum(train$count)
  trains <- train[train$type == "train", c("job","count")]
  add <- train[train$type == "addOutcomes", c("job","count")]
  colnames(trains)[1] <- "jobTrain"
  colnames(add)[1] <- "jobOutcome"
  add <- merge(add, trains)
  add$count <- NULL
  df <- merge(df, add)
  df
}
rStatistics.dbData <- function(data) {
  conn <- do.call(RODBC::odbcDriverConnect, data$db)
  df <- RODBC::sqlQuery(conn, "SELECT s.y, s.jobOutcome, st.* FROM dbo.stats st
                        INNER JOIN samples s ON st.id = s.id")
  train <- RODBC::sqlQuery(conn, "SELECT job, type FROM dbo.jobs WHERE type IN ('addOutcomes', 'train') ORDER BY job")
  train$count <- as.numeric(train$type == "train")
  train$count <- cumsum(train$count)
  trains <- train[train$type == "train", c("job","count")]
  add <- train[train$type == "addOutcomes", c("job","count")]
  colnames(trains)[1] <- "jobTrain"
  colnames(add)[1] <- "jobOutcome"
  add <- merge(add, trains)
  add$count <- NULL
  df <- merge(df, add)
  RODBC::odbcClose(conn)
  df
}
