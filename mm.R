# Create a function to calculate the rate of change in each state variable
change.dt <- function(X, Lambda, theta1, sigma1, beta1, c1, alpha1, d1, epsilon1, gamma1, mu1, theta2, sigma2, beta2, c2, d2, epsilon2, gamma2, mu2){
  S1 <- X[1] ; E1 <- X[2] ; I1 <- X[3] ; R1 <- X[4] ; V1 <- X[5]
  S2 <- X[1] ; E2 <- X[2] ; I2 <- X[3] ; R2 <- X[4] ; V2 <- X[5]
  
  N <- S1 + E1 + I1 + R1 + V1
  
  dS1dt <- Lambda*(1 - theta1 *sigma1) - beta1*c1*I1*S1/N - (1 - theta1 *sigma1)*alpha1*S1 - d1*S1
  dV1dt <- -(Lambda*(1 - theta1 *sigma1) - V1 + Lambda*theta1 *sigma1)*V1
  dE1dt <- beta1*c1*I1*S1/N - (alpha1 + d1 + epsilon1)*E1
  dI1dt <- epsilon1*E1 - (alpha1 + gamma1)*I1 +(d1 + mu1)*I1
  dR1dt <- gamma1*I1 - Lambda*theta1 *sigma1*V1 - d1*R1
  
  dS2dt <- (1 - theta1 *sigma1)-alpha1*V2  - (1 - theta1 *sigma1)*alpha1*S1 - beta2*c2*I2*S2/N - d2*S2
  dV2dt <- -((1 - theta1 *sigma1) * alpha1 + theta1 *sigma1*alpha1)*V2
  dE2dt <- beta2*c2*I2*S2/N + alpha1*E1 - (d2 + epsilon2)*E2
  dI2dt <- epsilon2*E2 - alpha1*I1 - gamma2*I2 + (d2 + mu2)*I2
  dR2dt <- gamma2*I2 - theta1*sigma1*alpha1*V2 - alpha1*R2 - d2*R2
  
  
  return(c(dS1dt, dV1dt, dE1dt, dI1dt, dR1dt, dS2dt, dV2dt, dE2dt, dI2dt, dR2dt))
  
}

# For time period (example for per day period)
#change.dt(X = c(79000, 20000, 500, 100, 300), Lambda=0, mu=0, beta=0.75, alpha=0.006, epsilon=1/3, gamma=1/8, sigma = 0.01)

# A fuction that will update the system at each time step
updateSystm <- function(X, Lambda, theta1, sigma1,beta1, c1, alpha1, d1, epsilon1, gamma1, mu1, theta2, sigma2, beta2, c2, d2, epsilon2, gamma2, mu2, deltaT){
  X <- X + change.dt(X, Lambda, theta1, sigma1, beta1, c1, alpha1, d1, epsilon1, gamma1, mu1, theta2, sigma2, beta2, c2, d2, epsilon2, gamma2, mu2)*deltaT
  return(X)
}

# How many people are in S, E, I, R and V at time = 0
Systm <- c(79000, 20000, 1, 0, 999)
# Per-capita birth rate
Lambda <- 0
theta1 <- 0.5
sigma1 <- 0.85
beta1 <- 0.7
c1 <- 13.3
alpha1 <- 0.4
d1 <- 0.3
epsilon1 <- 1/3
gamma1 <- 0.25
mu1 <- 0
##################
theta2 <- 1
sigma2 <- 0.95
beta2 <- 0.000000515425
c2 <- 6.02
d2 <- 0.00029
epsilon2 <- 1/3
gamma2 <- 0.024368
mu2 <- 0
# time step
deltaT <- 1
# time period
period <- 365

# Simulations for age group 1
epidemic <- data.frame(time=0, S1=Systm[1], E1=Systm[2], I1=Systm[3], R1=Systm[4], V1=Systm[5])

view(epidemic)

g <- epidemic[complete.cases(epidemic), ]
view(g)

colnames(g)[4] <- "Total_Infected_Population"
colnames(g)[6] <- "Vaccination_Rate"
for(time in seq(from=deltaT, to=period, by=deltaT)){
  
  Systm <- updateSystm(Systm, Lambda, theta1, sigma1,beta1, c1, alpha1, d1, epsilon1, gamma1, mu1, theta2, sigma2, beta2, c2, d2, epsilon2, gamma2, mu2, deltaT)
  epidemic <- rbind(epidemic, c(time, Systm))
  
}

install.packages("tidyverse")
library(tidyverse)

epi <- epidemic %>%
  pivot_longer(S1:V1, values_to="Counts", names_to="State")

ggplot(epi) + geom_line(aes(x=time, y=Counts, col=State))

view(epi)

ggplot(epidemic)+geom_line(aes(x=time, y=V1))

ggplot(g)+geom_line(aes(x=Total_Infected_Population, y=Vaccination_Rate, color = "red", ))
