library(banditr)
context("Test banditModel")

test_that("banditModel banditGlmnet constructs properly", {
  # data generation
  x <- matrix(rnorm(10e3), 1e3, 10)
  beta <- -4:5
  y <- as.numeric(plogis(x %*% beta))
  y <- sapply(y, rbinom, n = 1, size = 1)
  colnames(x) <- paste0("v", 1:10)
  df <- as.data.frame(x)
  df <- cbind(id = 1:1000, y = y, df)
  f <- as.formula(y ~ v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10)

  # data.frame
  myGlmnet <- glmnet::glmnet(x, y, "binomial", alpha = 0, lambda = 1)
  myBanditGlmnet <- banditGlmnet(f, "binomial", 1, 0, df)
  myGlmnet$call <- NULL
  myBanditGlmnet <- myBanditGlmnet$glmnet
  myBanditGlmnet$call <- NULL

  expect_identical(myGlmnet, myBanditGlmnet)

  # with lasso
  lass <- glmnet::glmnet(x, y, "binomial", alpha = 1, lambda = .1)
  keep <- predict(lass, s = .1, type="nonzero")
  myGlmnet <- glmnet::glmnet(x[,keep$X1], y, "binomial", alpha = 0, lambda = 1)
  myBanditGlmnet <- banditGlmnet(f, "binomial", 1, .1, df)
  myGlmnet$call <- NULL
  myBanditGlmnet <- myBanditGlmnet$glmnet
  myBanditGlmnet$call <- NULL

  expect_identical(myGlmnet, myBanditGlmnet)

  # no data.frame
  y <- df$y
  v1 <- df$v1
  v2 <- df$v2
  myGlmnet <- glmnet::glmnet(x[,1:2], y, "binomial", alpha = 0, lambda = 1)
  myBanditGlmnet <- banditGlmnet(y ~ v1 + v2, "binomial", 1, 0)
  myGlmnet$call <- NULL
  myBanditGlmnet <- myBanditGlmnet$glmnet
  myBanditGlmnet$call <- NULL

  expect_identical(myGlmnet, myBanditGlmnet)
})

test_that("model.frame, model.matrix work", {
  x <- matrix(rnorm(10e3), 1e3, 10)
  beta <- -4:5
  y <- as.numeric(plogis(x %*% beta))
  y <- sapply(y, rbinom, n = 1, size = 1)
  colnames(x) <- paste0("v", 1:10)
  df <- as.data.frame(x)
  df <- cbind(id = 1:1000, y = y, df)
  f <- as.formula(y ~ -1 + v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10)

  myBanditGlmnet <- banditGlmnet(f, "binomial", 1, 0, df)

  expect_identical(model.frame(f, df), model.frame(myBanditGlmnet))
  expect_identical(model.matrix(f, df), model.matrix(myBanditGlmnet))
})

test_that("predict works", {
  x <- matrix(rnorm(10e3), 1e3, 10)
  beta <- -4:5
  y <- as.numeric(plogis(x %*% beta))
  y <- sapply(y, rbinom, n = 1, size = 1)
  colnames(x) <- paste0("v", 1:10)
  df <- as.data.frame(x)
  df <- cbind(id = 1:1000, y = y, df)
  f <- as.formula(y ~ v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10)

  myBanditGlmnet <- banditGlmnet(f, "binomial", 1, 0, df)
  minidf <- df[25:30,]

  expect_length(predict(myBanditGlmnet, minidf, type = "u"), 6)

  minidf$v1[1] <- NA
  expect_length(predict(myBanditGlmnet, minidf, type = "u"), 6)
  expect_length(predict(myBanditGlmnet, minidf, type = "u", na.action = na.omit), 5)
})


