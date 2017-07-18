## ------------------------------------------------------------------------
set.seed(999)
x <- matrix(rnorm(10e3), 1e3, 10)
beta <- -4:5
y <- as.numeric(plogis(x %*% beta))
y <- sapply(y, rbinom, n = 1, size = 1)
colnames(x) <- paste0("v", 1:10)

df <- as.data.frame(x)
df <- cbind(y, df)

## ------------------------------------------------------------------------
df <- cbind(id = 1:1000, df) # add a primary key to the data

library(banditr) # initialize the bandit using the first 100 samples
start <- df[1:100,]
bdt <- bandit_ucb(formula = y ~ -1 + v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10, 
                  family = "binomial", 
                  data = start)
summary(bdt)

## ------------------------------------------------------------------------
bdt$train()
summary(bdt)

## ------------------------------------------------------------------------
add <- df[101:1000,]
add$y <- NA # let's pretend we haven't observed the reward yet. 
bdt$addSamples(add)

pr <- predict(bdt)
head(pr)

## ------------------------------------------------------------------------
id <- rownames(pr)[which.max(pr$score)]
y <- df$y[df$id == id]
y

## ------------------------------------------------------------------------
names(y) <- id
bdt$addOutcomes(y) # add the new outcome to the bandit
bdt$train() # update it

## ------------------------------------------------------------------------
for (i in 1:20) {
  pr <- predict(bdt)
  id <- rownames(pr)[which.max(pr$score)]
  y <- df$y[df$id == id]
  names(y) <- id
  bdt$addOutcomes(y)
  bdt$train()
}

## ---- fig.height=5, fig.width=5------------------------------------------
plot(bdt) # the default diagnostic: cumulative reward over time

## ---- fig.show='hold', fig.height=5, fig.width=5-------------------------
plot(bdt, what = "coef") # parameter values over time
# plot(bdt, what = "MSE")

## ------------------------------------------------------------------------
coef(bdt)
mod <- getModel(bdt) # extract the last model
mf <- getSamples(bdt) # extract complete samples
jobs <- getJobs(bdt) # extract a summary of all jobs

