library(banditr)
context("Test bandit")

test_that("bandit initializes correctly, RC methods work", {
  # data generation
  x <- matrix(rnorm(10e3), 1e3, 10)
  beta <- -4:5
  y <- as.numeric(plogis(x %*% beta))
  y <- sapply(y, rbinom, n = 1, size = 1)
  colnames(x) <- paste0("v", 1:10)

  df <- as.data.frame(x)
  df <- cbind(id = 1:1000, y = y, df)
  rm(y, x, beta)

  f <- as.formula(y ~ -1 + v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10)

  start <- df[1:100,]
  startError <- start
  startError$v1[1:5] <- NA


  expect_error(banditDf(formula = f, family = "binomial", data = startError))
  expect_error(banditDb(formula = f, family = "binomial", data = startError,
                        db = list(connection = 'driver={SQL Server};server=DESKTOP-99SSI5O\\SQLEXPRESS;database=tafra'),
                        path = "../../../test/"))

  startError <- start
  startError$id <- NULL
  expect_error(banditDf(formula = f, family = "binomial", data = startError))
  expect_error(banditDb(formula = f, family = "binomial", data = startError,
                        db = list(connection = 'driver={SQL Server};server=DESKTOP-99SSI5O\\SQLEXPRESS;database=tafra'),
                        path = "../../../test/"))

  startError <- start
  startError$jobOutcome <- 1
  expect_error(banditDf(formula = f, family = "binomial", data = startError))
  expect_error(banditDb(formula = f, family = "binomial", data = startError,
                        db = list(connection = 'driver={SQL Server};server=DESKTOP-99SSI5O\\SQLEXPRESS;database=tafra'),
                        path = "../../../test/"))

  b1 <- banditDf(formula = f, family = "binomial", data = start)
  b2 <- banditDb(formula = f, family = "binomial", data = start,
                 db = list(connection = 'driver={SQL Server};server=DESKTOP-99SSI5O\\SQLEXPRESS;database=tafra'),
                 path = "../../../test/")
  # b2 <- banditDb(formula = f, family = "binomial", data = start,
  #                db = list(connection = 'driver={SQL Server};server=DESKTOP-99SSI5O\\SQLEXPRESS;database=tafra'),
  #                path = "../test/")
  b1$train(seed = 123)
  b2$train(seed = 123)

  expect_identical(getModel(b1), getModel(b2))

  add <- df[101:105,]
  newY <- add$y
  names(newY) <- add$id
  add$y <- NA

  b1$addSamples(add)
  b2$addSamples(add)
  expect_equal(b1$currentJob, 3)
  expect_equal(b2$currentJob, 3)
  expect_equal(nrow(predict(b1)), 5)
  expect_identical(predict(b1), predict(b2))
  b1$addOutcomes(newY)
  b1$train()
  b2$addOutcomes(newY)
  b2$train()
  expect_equal(b1$currentJob, 5)
  expect_equal(nrow(predict(b1)), 0)
  expect_identical(predict(b1), predict(b2))

  b1$tune()
  b2$tune()

  b1$undo()
  b1$undo()
  b1$undo()
  b1$undo()
  expect_equal(b1$currentJob, 2)

  b2$undo()
  b2$undo()
  b2$undo()
  b2$undo()
  expect_equal(b2$currentJob, 2)

  # add <- df[106:200,]
  # newY <- add$y
  # names(newY) <- add$id
  # add$y <- NA
  # b1$addSamples(add)
  # b2$addSamples(add)
  # for(x in seq(1, 2, by = 5)) {
  #   b1$addOutcomes(newY[x+(0:4)])
  #   b1$train()
  #   b2$addOutcomes(newY[x+(0:4)])
  #   b2$train()
  #   if(x %% 2 == 0) {
  #     b1$tune()
  #     b2$tune()
  #   }
  # }

})

test_that("can't add new factor levels, can't add NA's", {
  t1 <- data.frame(y = rep(0:1, 5), id = 1:10, factor = letters[1:10])
  t1 <- t1[rep(1:10, 10),]
  t2 <- data.frame(y = rep(0:1, 5), id = 11:20, factor = letters[11:20])
  f <- y ~ factor
  b1 <- banditDf(formula = y ~ factor, family = "binomial", data = t1)
  b2 <- banditDb(formula = y ~ factor, family = "binomial", data = t1,
                 db = list(connection = 'driver={SQL Server};server=DESKTOP-99SSI5O\\SQLEXPRESS;database=tafra'),
                 path = "../../../test/")
  expect_error(b1$addSamples(t2))
  expect_error(b2$addSamples(t2))
  t2$factor[1] <- NA
  expect_error(b1$addSamples(t2))
  expect_error(b2$addSamples(t2))
})

