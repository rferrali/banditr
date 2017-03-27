#' @export banditDb
#' @exportClass banditDb

banditDb <- setRefClass("banditDb",
                        contains = "bandit",
                        fields = list(db = "list",
                                      models = "character",
                                      path = "character"))

banditDb$methods(
  initialize = function(db, path, ...) {
    out <- callSuper(...)
    conn <- do.call(RODBC::odbcDriverConnect, db)
    RODBC::sqlDrop(conn, "dbo.samples")
    RODBC::sqlSave(conn, out$samples, "dbo.samples", rownames = FALSE, safer = FALSE,
                   varTypes = c(jobSamples = "int",
                                jobOutcome = "int"))
    RODBC::sqlDrop(conn, "dbo.jobs")
    RODBC::sqlQuery(conn, "CREATE TABLE [dbo].[jobs](
                    [job] [int] NOT NULL,
                    [date] [datetime] NOT NULL,
                    [type] [varchar](50) NOT NULL,
                    [param] [varchar](50) NULL,
                    [value] [float] NULL
    )")
    RODBC::sqlSave(conn, out$jobs, "dbo.jobs", rownames = FALSE, append = TRUE)
    RODBC::odbcClose(conn)

    initFields(db = db,
               models = as.character(NA),
               path = path)

  },
  addSamples = function(df, bulk = FALSE, ...) {
    df <- callSuper(df)
    conn <- do.call(RODBC::odbcDriverConnect, db)

    test <- RODBC::sqlQuery(conn, paste0("SELECT y FROM dbo.samples WHERE id IN (",
                                         paste(df$id, collapse = ", "),
                                         ")"))
    if(nrow(test) != 0) stop("some samples you are adding are already in the bandit.")

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

    RODBC::sqlSave(conn,
                   data.frame(job = currentJob + 1,
                              date = Sys.time(),
                              type = "addSamples",
                              param = NA,
                              value = NA),
                   "dbo.jobs",
                   rownames = FALSE,
                   append = TRUE)
    RODBC::odbcClose(conn)
    models <<- c(models, NA)
    currentJob <<- currentJob + 1
  },
  addOutcomes = function(y) {
    y <- callSuper(y)
    ids <- as.numeric(names(y))
    conn <- do.call(RODBC::odbcDriverConnect, db)

    test <- RODBC::sqlQuery(conn, paste0("SELECT y FROM dbo.samples WHERE id IN (",
                                         paste(ids, collapse = ", "),
                                         ")"))
    if(nrow(test) != length(y)) stop("some outcomes you are adding don't correspond to any samples in the bandit.")
    test <- RODBC::sqlQuery(conn, paste0("SELECT y FROM dbo.samples WHERE id IN (",
                                         paste(ids, collapse = ", "),
                                         ") AND y IS NULL"))
    if(nrow(test) != length(y)) stop("some outcomes you are adding are already in the bandit.")
    RODBC::sqlUpdate(conn,
                     dat = data.frame(y = y, id = ids, jobOutcome = currentJob+1),
                     tablename = "dbo.samples",
                     index = "id")
    RODBC::sqlSave(conn,
                   dat = data.frame(job = currentJob + 1,
                                    date = Sys.time(),
                                    type = "addOutcomes",
                                    param = NA,
                                    value = NA),
                   "dbo.jobs", rownames = FALSE, append = TRUE)
    RODBC::odbcClose(conn)
    models <<- c(models, NA)
    currentJob <<- currentJob+1
  },
  train = function(...) {

    conn <- do.call(RODBC::odbcDriverConnect, db)
    samples <- RODBC::sqlQuery(conn, "SELECT * FROM dbo.samples WHERE y IS NOT NULL")

    model <- callSuper(data = samples, ...)

    models <<- c(models, paste0("output", currentJob + 1, ".RData"))
    save(model, file = paste0(path, "output", currentJob + 1, ".RData"))
    currentModel <<- model
    RODBC::sqlSave(conn,
                   dat = data.frame(job = currentJob + 1,
                                    date = Sys.time(),
                                    type = "train",
                                    param = NA,
                                    value = NA),
                   "dbo.jobs", rownames = FALSE, append = TRUE)
    RODBC::odbcClose(conn)
    currentJob <<- currentJob+1
  },
  tune = function(...) {

    conn <- do.call(RODBC::odbcDriverConnect, db)
    samples <- RODBC::sqlQuery(conn, "SELECT * FROM dbo.samples WHERE y IS NOT NULL")

    model <- callSuper(data = samples, ...)
    models <<- c(models, paste0("output", currentJob + 1, ".RData"))
    save(model, file = paste0(path, "output", currentJob + 1, ".RData"))
    currentParams[[model$param]] <<- model$value
    RODBC::sqlSave(conn,
                   dat = data.frame(job = currentJob + 1,
                                    date = Sys.time(),
                                    type = "tune",
                                    param = model$param,
                                    value = model$value),
                   "dbo.jobs", rownames = FALSE, append = TRUE)
    RODBC::odbcClose(conn)
    currentJob <<- currentJob+1
  },
  undo = function() {
    "cancels the last job."
    conn <- do.call(RODBC::odbcDriverConnect, db)
    type <- RODBC::sqlQuery(conn, "SELECT TOP 1 type FROM dbo.jobs ORDER BY job DESC")$type
    if (type == "addSamples") {
      RODBC::sqlQuery(conn, paste0("DELETE FROM dbo.samples WHERE jobSamples = ", currentJob))
    } else if (type == "addOutcomes") {
      RODBC::sqlQuery(conn, paste0("UPDATE dbo.samples SET jobOutcome = NULL,
                                   y = NULL WHERE jobOutcome = ", currentJob))
    } else if (type == "train") {
      newModel <- RODBC::sqlQuery(conn, "SELECT TOP 2 job FROM dbo.jobs WHERE type = 'train' ORDER BY job DESC")$job
      if (length(newModel) > 1) {
        load(paste0(path, models[newModel[2]]))
        currentModel <<- model
      } else {
        currentModel <<- NULL
      }
      file.remove(paste0(path, models[length(models)]))
    } else if (type == "tune") {
      file.remove(paste0(path, models[length(models)]))
      param <- RODBC::sqlQuery(conn, paste0("SELECT param FROM dbo.jobs WHERE job = ", currentJob))$param
      newValue <- RODBC::sqlQuery(conn, paste0("SELECT TOP 2 value FROM dbo.jobs WHERE param = '", param, "' ORDER BY job DESC"))$value
      if(length(newValue) > 1) {
        newValue <- newValue[2]
      } else {
        newValue <- 0
      }
      currentParams[[param]] <<- newValue
    }
    RODBC::sqlQuery(conn, paste0("DELETE FROM dbo.jobs WHERE job = ", currentJob))
    RODBC::odbcClose(conn)
    models <<- models[-length(models)]
    currentJob <<- currentJob - 1
  }
)
