
# Create a function to calculate the rate of change in each state variable
change.dt <- function(X, Lambda, mu, beta, alpha, epsilon, gamma, sigma){
  S <- X[1] ; E <- X[2] ; I <- X[3] ; R <- X[4] ; V <- X[5]
  N <- S + E + I + R + V
  
  dSdt <- Lambda - mu*S - beta*I*S/N - alpha*S + sigma*E
  dEdt <- beta*I*S/N - (mu + epsilon)*E - sigma*E
  dIdt <- epsilon*E - (gamma + mu)*I
  dRdt <- gamma*I - mu*R
  dVdt <- alpha*S  - mu*V  
      
  return(c(dSdt, dEdt, dIdt, dRdt, dVdt))
        
}

# For time period (example for per day period)
change.dt(X = c(79000, 20000, 500, 100, 300), Lambda=0, mu=0, beta=0.75, alpha=0.006, epsilon=1/3, gamma=1/8, sigma = 0.01)

# A fuction that will update the system at each time step
updateSystm <- function(X, Lambda, mu, beta, alpha, epsilon, gamma, sigma, deltaT){
  X <- X + change.dt(X, Lambda, mu, beta, alpha, epsilon, gamma, sigma)*deltaT
  return(X)
}

# How many people are in S, E, I, R and V at time = 0
Systm <- c(79000, 20000, 1, 0, 999)
# Per-capita birth rate
Lambda <- 0
# Per-capita natural death rate
mu <- 0
# The proportion of the susceptible class that have received vaccination
alpha <- 0.06
# The rate of outflow from susceptible to exposed compartment
beta <- 0.75
# Rate of progression from exposed to infection
epsilon <- 1/3
# Recovery rate of infectious individual
gamma <- 1/8
# The rate of inflow into susceptible from the exposed compartment
sigma = 0.01
# time step
deltaT <- 1
# time period
period <- 365


epidemic <- data.frame(time=0, S=Systm[1], E=Systm[2], I=Systm[3], R=Systm[4], V=Systm[5])

for(time in seq(from=deltaT, to=period, by=deltaT)){
  
  Systm <- updateSystm(Systm, Lambda, mu, beta, alpha, epsilon,  gamma, sigma, deltaT)
  epidemic <- rbind(epidemic, c(time, Systm))

}

install.packages("tidyverse")
library(tidyverse)

epi <- epidemic %>%
  pivot_longer(S:V, values_to="Counts", names_to="State")

ggplot(epi) + geom_line(aes(x=time, y=Counts, col=State))

ggplot(epidemic)+geom_line(aes(x=time, y=epidemic$V))

