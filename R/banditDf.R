#' @export banditDf
#' @exportClass banditDf


banditDf <- setRefClass("banditDf",
                        contains = "bandit",
                        fields = list(samples = "data.frame",
                                      models = "list",
                                      jobs = "data.frame"))


banditDf$methods(
  initialize = function(...) {
    out <- callSuper(...)
    initFields(models = list(NULL),
               samples = out$samples,
               jobs = out$jobs)
  },
  addSamples = function(df) {
    "add samples to the bandit."
    df <- callSuper(df)
    if(any(df$id %in% samples$id)) stop("some samples you are adding are already in the bandit.")

    samples <<- rbind(samples, df)
    jobs <<- rbind(jobs,
                   data.frame(job = currentJob+1,
                              date = Sys.time(),
                              type = "addSamples",
                              param = NA,
                              value = NA))
    models <<- c(models, list(NULL))
    currentJob <<- currentJob + 1
  },
  addOutcomes = function(y) {
    "add outcomes to the bandit. y is a named vector whose names refer to samples' ids."

    y <- callSuper(y)
    ids <- as.numeric(names(y))
    select <- match(ids, samples$id)
    response <- all.vars(formula)[1]
    if(any(is.na(select))) stop("some outcomes you are adding don't correspond to any samples in the bandit.")
    if(any(!is.na(samples[select,response]))) stop("some outcomes you are adding are already in the bandit.")

    samples[select,response] <<- y
    samples$jobOutcome[select] <<- currentJob+1
    jobs <<- rbind(jobs,
                   data.frame(job = currentJob + 1,
                              date = Sys.time(),
                              type = "addOutcomes",
                              param = NA,
                              value = NA))
    models <<- c(models, list(NULL))
    currentJob <<- currentJob+1
  },
  train = function(...) {

    model <- callSuper(data = samples, ...)

    models <<- c(models, list(model))
    currentModel <<- model
    jobs <<- rbind(jobs, data.frame(job = currentJob + 1,
                                    date = Sys.time(),
                                    type = "train",
                                    param = NA,
                                    value = NA))
    currentJob <<- currentJob + 1
  },
  tune = function(...) {

    data <- samples[!is.na(samples$y),]
    tune <- callSuper(data = data, ...)
    models <<- c(models, list(tune$model))
    currentParams[[tune$param]] <<- tune$value
    jobs <<- rbind(jobs,
                   data.frame(job = currentJob + 1,
                              date = Sys.time(),
                              type = "tune",
                              param = tune$param,
                              value = tune$value))
    currentJob <<- currentJob + 1
  },
  undo = function() {
    "cancels the last job."
    type <- jobs$type[currentJob]
    if (type == "addSamples") {
      samples <<- samples[-which(samples$jobSamples == currentJob),]
    } else if (type == "addOutcomes") {
      samples$y[which(samples$jobOutcome == currentJob)] <<- NA
      samples$jobOutcome[which(samples$jobOutcome == currentJob)] <<- NA
    } else if (type == "train") {
      newModel <- jobs$job[which(jobs$type == type)]
      if (length(newModel) > 1) {
        currentModel <<- models[[rev(newModel)[2]]]
      } else {
        currentModel <<- NULL
      }
    } else if (type == "tune") {
      param <- jobs$param[currentJob]
      newValue <- jobs$value[which(jobs$param == param)]
      if(length(newValue) > 1) {
        newValue <- rev(newValue)[2]
      } else {
        newValue <- 0
      }
      currentParams[[param]] <<- newValue
    }
    models[[length(models)]] <<- NULL
    jobs <<- jobs[-nrow(jobs),]
    currentJob <<- currentJob - 1
  }
)
