install.packages("tidyverse")
install.packages("XML")
install.packages("rvest")

library(tidyverse)
library(XML)
library(rvest)

# Importing data from the internet

covid <- read_html("https://covidvax.live/location/zaf")

tables <- covid %>% html_table(fill=TRUE)

vacc <- tables[[6]]
view(vacc)

vacc_new <- vac
colnames(vacc_new) <- vacc[1, ]
view(vacc_new)

vacc_new <- vacc_new[-1, ]
view(vacc_new)

library(ggplot2)
library(dplyr)

vaccination <- data.frame(time=0, S=Systm[1], E=Systm[2], I=Systm[3], R=Systm[4], V=Systm[5])

epi <- vaccination %>%
  pivot_longer(S:V, values_to="", names_to="State")

ggplot(epi) + geom_line(aes(x=time, y=Counts, col=State))

