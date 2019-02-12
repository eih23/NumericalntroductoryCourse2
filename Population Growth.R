getwd()
setwd(github.com/eih23/NumericalntroductoryCourse2.git)

getwd()
set.seed(123)
install.packages("growthrates")
library("growthrates")
install.packages("deSolve")
install.packages("diffeqr")
library("deSolve")
library("diffeqr")
library("ggplot2")
population.Iceland$X <- NULL
population.Iceland <- read.csv("population.csv", header = TRUE)
population.Iceland$Year <- as.character(population.Iceland$Year)
state <- population.Iceland[1, 2]
state
population.Iceland$Year <- as.character.Date(population.Iceland$Year)
population.Iceland$Year[114:451] <- 2018:2355
population.Iceland$Year[114:451]
population.Iceland$Population[114]
{ 
plot(population.Iceland$Year[1:114], population.Iceland$Population[1:114], yaxt="none", main ="Population Growth", xlab = "time", ylab="population", lwd = 1 )
yaxis2 <- axis(side = 2, at=c(87500, 175000, 262500))
axis(side=2, at=c(175000, 345000))
} 
r <- 0.0165
K <- Verhult_Method
parameters <- c(r, K)
t <- time
time <- seq(from=population.Iceland$Year[1], to = population.Iceland$Year[296], by = 1)
logistic <- function(t, state, parameters) {
  with(
    as.list(c(state, parameters)),{
      dy <- r*state*(1-state/K)
      return(list(dy))
    }
  )
}
t <- as.numeric(time)
out <- ode(y = state, times = t, func = logistic, parms = parameters)
out2 <- ode(y = state, times = t, func = logistic, parms = parameters)
out[,2]
{
plot(out, lwd=3, yaxt= "none", col=1, main="Logistic Population Growth", xlab="Time", ylab="Population")
yaxis <- axis(side=2,at=seq(80000, 700000, 250000))
outpopulation <- out[,2]
outpopulation[1:114] <- cbind(population.Iceland$Population[1:114])
out[,2] <- outpopulation
outpopulation <- out[,2]
outpopulation[1:114] <- cbind(population.Iceland$Population[1:114])
}

{
plot(population.Iceland$Year[1:114], population.Iceland$Population[1:114], lwd = 2, main = "Population growth", ylab = "Population", xlab= "Time", yaxt ="none") 
yaxis <- axis(side = 2, at=seq(80000, 348000, 115000))
}
percent_error <- (population.Iceland$Population[1:114] - out2[1:114,2])/population.Iceland$Population[1:114]
percent_error <- abs(percent_error)
percent_error
summary(percent_error)
plot(time[1:114], percent_error)
