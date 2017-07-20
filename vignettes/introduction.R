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

## ------------------------------------------------------------------------
library(banditr) # initialize the bandit using the first 100 samples
start <- df[1:100,]
bdt_ucb <- bandit_ucb(formula = y ~ -1 + v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10, 
                  family = "binomial", 
                  data = start)
summary(bdt_ucb)

## ------------------------------------------------------------------------
bdt_thom <- bandit_stan_glm(formula = y ~ -1 + v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10, 
                  family = "binomial", 
                  data = start)

## ---- message=FALSE------------------------------------------------------
bdt_ucb$train()
bdt_thom$train(chains = 1) # use only one Markov chain for demonstration purposes
summary(bdt_ucb)

## ------------------------------------------------------------------------
add <- df[101:1000,]
add$y <- NA # let's pretend we haven't observed the reward yet. 
bdt_ucb$addSamples(add)
bdt_thom$addSamples(add)

pr_ucb <- predict(bdt_ucb)
pr_thom <- predict(bdt_thom)

## ------------------------------------------------------------------------
head(pr_ucb)
id_ucb <- rownames(pr_ucb)[which.max(pr_ucb$score)]
y_ucb <- df$y[df$id == id_ucb]
y_ucb

## ------------------------------------------------------------------------
head(pr_thom)
id_thom <- rownames(pr_thom)[sample.int(nrow(pr_thom), 1, prob = pr_thom$weight)]
y_thom <- df$y[df$id == id_thom]
y_thom

## ------------------------------------------------------------------------
names(y_ucb) <- id_ucb
bdt_ucb$addOutcomes(y_ucb) # add the new outcome to the bandit
bdt_ucb$train() # update it

names(y_thom) <- id_thom # likewise, for the Thompson bandit
bdt_thom$addOutcomes(y_thom)
bdt_thom$train(chains = 1)

## ------------------------------------------------------------------------
# set the tuning parameter for ridge regularization to 1
bdt_ucb$tune(param = 'lambdaRidge', value = 1) 
# select the tuning parameter for the LASSO using 10-fold cross validation 
bdt_ucb$tune(param = 'lambdaLasso', value = 'auto', lambdaAuto = 'lambda.1se')

## ------------------------------------------------------------------------
for (i in 1:20) {
  pr_ucb <- predict(bdt_ucb)
  id <- rownames(pr_ucb)[which.max(pr_ucb$score)]
  y <- df$y[df$id == id]
  names(y) <- id
  bdt_ucb$addOutcomes(y)
  bdt_ucb$train()
}

## ---- fig.height=5, fig.width=5------------------------------------------
plot(bdt_ucb) # the default diagnostic: cumulative reward over time

## ---- fig.show='hold', fig.height=5, fig.width=5-------------------------
plot(bdt_ucb, what = "coef") # parameter values over time
# plot(bdt, what = "MSE")

## ------------------------------------------------------------------------
coef(bdt_ucb)
mod <- getModel(bdt_ucb) # extract the last model
mf <- getSamples(bdt_ucb) # extract complete samples
jobs <- getJobs(bdt_ucb) # extract a summary of all jobs

