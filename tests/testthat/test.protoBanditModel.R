library(banditr)
context("Test protoBanditModel")

test_that("proto is same as non-proto", {
  # data generation
  x <- matrix(rnorm(10e3), 1e3, 10)
  beta <- -4:5
  y <- as.numeric(plogis(x %*% beta))
  y <- sapply(y, rbinom, n = 1, size = 1)
  colnames(x) <- paste0("v", 1:10)
  df <- as.data.frame(x)
  df <- cbind(id = 1:1000, y = y, df)
  f <- as.formula(y ~ v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10)
  rm(y, x, beta)

  # create
  myBanditGlmnet <- banditGlmnet(f, "binomial", 1, 0, df, seed = 1)
  myProto <- banditr:::protoBanditGlmnet(f, "binomial", 1, 0, df, seed = 1)

  # test
  expect_identical(myProto$data, df$id)

  myBanditGlmnet <- myBanditGlmnet[names(myProto)]
  myBanditGlmnet$data <- NULL
  myProto$data <- NULL
  myProto$call <- myBanditGlmnet$call <- NULL
  class(myProto) <- class(myBanditGlmnet) <- NULL
  expect_identical(myProto, myBanditGlmnet)
})

