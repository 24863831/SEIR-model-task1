
# Create a function to calculate the rate of change in each state variable
change.dt <- function(X, Lambda, mu, beta, alpha, epsilon, gamma){
  S <- X[1] ; E <- X[2] ; I <- X[3] ; R <- X[4]
  N <- S + E + I + R
  
  dSdt <- Lambda - mu*S - beta*I*S/N
  dEdt <- beta*I*S/N - (mu + epsilon)*E
  dIdt <- epsilon*E - (gamma + mu + alpha)*I
  dRdt <- gamma*I - mu*R
        
  return(c(dSdt, dEdt, dIdt, dRdt))
        
}

# For time period (example for per day period)
change.dt(X = c(79300, 20000, 500, 100), Lambda=0, mu=0, beta=0.75, alpha=0.006, epsilon=1/3, gamma=1/8)

# A fuction that will update the system at each time step
updateSystm <- function(X, Lambda, mu, beta, alpha, epsilon, gamma, deltaT){
  X <- X + change.dt(X, Lambda, mu, beta, alpha, epsilon, gamma)*deltaT
  return(X)
}

# How mant people are in S, E, I and R at time = 0
Systm <- c(79999, 20000, 1, 0)
# Per-capita birth rate
Lambda <- 0
# Per-capita natural death rate
mu <- 0
# Virus-induced average fatality rate
alpha <- 0.006
# transmission coefficient
beta <- 0.75
# Rate of progression from exposed to infection
epsilon <- 1/3
# Recovery rate of infectious individual
gamma <- 1/8
# time step
deltaT <- 1
# time period
period <- 365


epidemic <- data.frame(time=0, S=Systm[1], E=Systm[2], I=Systm[3], R=Systm[4])

for(time in seq(from=deltaT, to=period, by=deltaT)){
  
  Systm <- updateSystm(Systm, Lambda, mu, beta, alpha, epsilon,  gamma, deltaT)
  epidemic <- rbind(epidemic, c(time, Systm))

}

install.packages("tidyverse")
library(tidyverse)

epi <- epidemic %>%
  pivot_longer(S:R, values_to="Counts", names_to="State")

ggplot(epi) + geom_line(aes(x=time, y=Counts, col=State))
