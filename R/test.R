# x <- matrix(rnorm(10e3), 1e3, 10)
# beta <- -4:5
# y <- as.numeric(plogis(x %*% beta))
# y <- sapply(y, rbinom, n = 1, size = 1)
# colnames(x) <- paste0("v", 1:10)
#
# df <- as.data.frame(x)
# df <- cbind(id = 1:1000, y = y, df)
# rm(y, x, beta)
#
# f <- as.formula(y ~ -1 + v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10)
#
# start <- df[1:100,]
# add <- df[101:105,]
# yAdd <- add$y
# names(yAdd) <- add$id
# add$y <- NA
#
#
# b1 <- bandit_ucb(formula = f, family = "binomial", data = start)
# b11 <- bandit_stan_glm(formula = f, family = "binomial", data = start)
# b2 <- bandit_ucb(formula = f, family = "binomial", data = start,
#                  db = list(connection = 'driver={SQL Server};server=DESKTOP-99SSI5O\\SQLEXPRESS;database=tafra'),
#                  path = "../test/")
#
# b1$tune()
# b1$train()
# b2$tune()
# b2$train()
# b1$addSamples(add)
# b1$addOutcomes(yAdd)
# b2$addSamples(add)
# b2$addOutcomes(yAdd)
# b11$train()
# b11$addSamples(add)
# b11$addOutcomes(yAdd)

# b11$train(chains = 1, iter = 250)
# b11$addSamples(add)
# predict(b1)
# formula(b1)

# b1$tune()
# b2$tune()
# b1$train()
# b2$train()
# b1$addSamples(add)
# b2$addSamples(add)
# b2$addOutcomes(yAdd)

# dg <- cbind(df, fac = sample(letters, nrow(df), replace = T))
# fg <- update(f, . ~ . + (1|f))
# sg <- dg[1:100,]
# b1 <- bandit_stan_glmer(formula = y ~ v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + (1 | fac) + v10,
#                         data = sg, family = "binomial",
#                         db = list(connection = 'driver={SQL Server};server=DESKTOP-99SSI5O\\SQLEXPRESS;database=tafra'),
#                         path = "../test/")
# b1$train(iter = 1000, chains = 1)
# addg <- dg[101:105,]
# addy <- addg$y
# names(addy) <- addg$id
# addg$y <- NA
# b1$addSamples(addg)
# b1$addOutcomes(addy)
#
# fun <- function(seed, ...) {
#   cl <- as.list(match.call(expand.dots = FALSE))
#   dots <- cl$`...`
#   print(cl$seed)
# }

# fun(1, a = 3, b = 5)
