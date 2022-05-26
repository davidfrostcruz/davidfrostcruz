###### ASSIGNMENT BAYESIAN #########

##### Libraries ######
library(ggplot2)
library(coda)
library(R2OpenBUGS)
library(stats)
library(dplyr)
##### Setting data ######
animal <- as.data.frame(read.table(file.choose(), header = TRUE))
View(animal)
sum(is.na(animal))

model.data <- list('N'= animal$N, 'Z' = animal$Z, 'type' = animal$type)

######### QUESTION 1 ##########

# INITIAL VALUES 

model.inits <- list(list(beta0=0,beta1=0), list(beta0=1,beta1=1))
OpenBUGS.pgm = "C:/Program Files (x86)/OpenBUGS/OpenBUGS323/OpenBUGS.exe"

# MODEL SPECIFICATION

farm.model <- function() {
  for (i in 1:14) {
    # Likelihood specification
    Z[i] ~ dbin(p[i],N[i])
    logit(p[i])<-beta0+beta1*type[i]
  }
  # Prior information
  beta0 ~ dnorm(0, 0.001)  
  beta1 ~ dnorm(0, 0.001)}

write.model(farm.model, "model.txt")
file.show("model.txt")

# Parameters 
parameters = c("p", "beta0", "beta1")

# Set up and run model 

model.out <- bugs(model.data, model.inits, 
                  model.file = "model.txt",
                  parameters=parameters,
                  n.chains = 2, n.iter = 5000,  n.burnin = 2500,
                  codaPkg=TRUE, OpenBUGS.pgm = OpenBUGS.pgm)


######### QUESTION 2 ##########

# Posterior summary statistics
out <- read.bugs(model.out)
summary(out)
HPDinterval(as.mcmc(as.matrix(out)))

# History plot & posterior distributions
par(mfrow=c(1,2))
traceplot(out[,1:2])
densplot(out[,1:2])

# Correlation & autocorrelation plot
par(mfrow=c(1,2))
crosscorr.plot(out)
autocorr.plot(out[,1:2])

# Gelman and Rubin Convergence Diagnostic

gelman.diag(out, confidence = 0.95, transform=FALSE, autoburnin=TRUE,multivariate=TRUE)
gelman.plot(out)


######### QUESTION 3 ##########

# Summary Statistics

summary(out)
HPDinterval(as.mcmc(as.matrix(out)))

# Kernel Density Plots 
par(mfrow=c(1,2))
densplot(out[,1:2])

# MC Error
mc.prep <- as.data.frame(summary(out)[1])
mc.error <- c(mc.prep$statistics.Time.series.SE/mc.prep$statistics.SD)
mc.error

######### QUESTION 4 ##########


# Average for g1 and g2 


