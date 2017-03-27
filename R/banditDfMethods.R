#' @include banditMethods.R

setMethod("getModel", signature(object = "banditDf"),
          function(object, modelId) {
            model <- modelId
            # validation
            if(!is.null(model)) {
              model <- as.numeric(model)
              if(length(model) != 1) stop("Select only training job")
              if(!model %in% object$jobs$job[object$jobs$type == "train"]) stop("The selected id doesn't match any training job.")
            }
            # selection
            if(is.null(model)) { # model
              modelId <- max(object$jobs$job[object$jobs$type == "train"])
            }
            model <- object$models[[modelId]]
            model$lambdaRidge <- getParam(object, modelId, "lambdaRidge")
            model$lambdaLasso <- getParam(object, modelId, "lambdaLasso")
            return(model)
          })


setMethod("getParam", signature(object = "banditDf"),
          function(object, modelId, param) {
            param <- match.arg(param, c("lambdaRidge", "lambdaLasso"))
            r <- object$jobs$value[which(object$jobs$param == param & object$jobs$job < modelId)]
            if(length(r) > 0) return(rev(r)[1])
            return(0)
          })

#' @export

setMethod("getSamples", signature(object = "banditDf"),
          function(object, samples) {
            if(is.null(samples)) {
              df <- object$samples[is.na(object$samples$y),]
            } else {
              samples <- unique(as.numeric(samples))
              df <- object$samples[object$samples$id %in% samples,]
            }
            callNextMethod(df, samples)
          })


#' @export
setMethod("getTuning", signature(object = "banditDf"),
          function(object, modelId = NULL, param = "lambdaRidge") {
            model <- modelId2
            param <- match.arg(param, c("lambdaRidge", "lambdaLasso"))
            # validation
            if(!is.null(model)) {
              model <- as.numeric(model)
              if(length(model) != 1) stop("Select only one tuning job")
              if(!model %in% object$jobs$job[object$jobs$type == "tune"]) stop("The selected id doesn't match any tuning job.")
            }
            # selection
            if(is.null(model)) { # model
              if(length(which(object$jobs$param == param)) == 0) return(list(model = NULL,
                                                              value = 0,
                                                              param = param))
              model <- object$jobs$job[max(object$jobs$job[which(object$jobs$param == param)])]
              model <- object$models[[model]]
            } else {
              model <- object$models[[model]]
            }
            return(model)

          })
