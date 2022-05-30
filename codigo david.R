###### ASSIGNMENT BAYESIAN #########

##### Libraries ######
library(ggplot2)
library(coda)
library(R2OpenBUGS)
library(stats)
library(LaplacesDemon)
library(dplyr)
library(mcmcse)
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
  beta0 ~ dnorm(0, 0.000001)  
  beta1 ~ dnorm(0, 0.000001)}

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
acfplot(out[,1:2], lag.max=50)

# Gelman and Rubin Convergence Diagnostic

g_diag <- gelman.diag(out, confidence = 0.95, transform=FALSE, autoburnin=TRUE,multivariate=TRUE)
g_diag
gelman.plot(out[,1:2])


######### QUESTION 3 ##########

# Summary Statistics (Mean, SD, MCSE)

summary.out <- summary(out[,c(1,2,4,17)])
summary.out

# HPDInterval 
HPDinterval(as.mcmc(as.matrix(out[,c(1,2,4,17)])))

# Kernel Density Plots 
par(mfrow=c(1,2))
densplot(out[,1:2])
densplot(out[,c(4,17)])

######### QUESTION 4 ##########
# Prevalence
summary.out$stat["p[1]",]
summary.out$stat["p[7]",] 
HPDinterval(as.mcmc(as.matrix(out)))

######### QUESTION 5 ###########
densplot(out)
densplot(out[,1:10])

### the density for first chain
a<-out[[1]]
p1a<-a[,4]
p7a<-a[,10]
### the density for second chain
b<-out[[2]]
p1b<-b[,4]
p7b<-b[,10]
### we can use ggplot to draw these 2 P in the same plot now
###  or even we can directly draw the density plot of p1-p7
deltapa<-p1a-p7a
library(lattice)
densityplot(deltapa)
p1 <- data.frame(type = factor(rep(c(1,2), each=2500)), 
                 prob = c(p1a,p7a))
pl1<-ggplot(p1,aes(x=prob,color=type))+geom_density(outline.type="upper",trim=TRUE)
pl1

