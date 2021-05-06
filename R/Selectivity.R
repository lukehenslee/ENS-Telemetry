#==================================================================================================
# Estimate selectivity
# Date: May 4, 2021
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
#
#
#==================================================================================================
#NOTES: 
#==================================================================================================

library(ggpubr)
library(forcats)
library(ggforce)
library(lubridate)
library(RColorBrewer)
library(viridis)
library(tidyverse)
library(ggplot2)
library(bbmle)
library(dplyr)
library(manipulate)
library(visreg) 
library(ggridges)
library(reshape2)
library(cowplot)
library(ggmap)
library(ggspatial)
library(sf)
library(spData)
library(ggsn)
library(extrafont)
library(knitr)
loadfonts(device = "win")
library(parzer)
library(readxl)

# Set wd
setwd("Q:/RESEARCH/Tagging")

# Import data
mcode <- read_csv("Github/data/M_code_list_V10.csv")

# Add column binning length data
max(mcode$Length)

min(mcode$Length)

mean(mcode$Length)

mcode$Length_bin <- cut(mcode$Length, breaks = seq(385, 655, 10))

# Numbers in each bin
lengthbincount <- mcode %>% 
  group_by(Length_bin) %>% 
  count()

plot(x=as.factor(lengthbincount$Length_bin), y = lengthbincount$n)
points(x=lengthbincount$Length_bin, y=lengthbincount$n, pch=21, bg="red")

ggplot(lengthbincount) +
  geom_bar(aes(x=lengthbincount$Length_bin, y = ..prop.., group = 1), stat = "count") 


x# Assign number to each bin
mcode$Length_bin_number <- as.numeric(factor(mcode$Length_bin))

# Create selectivity function
selex_dome <- function(length, lmax, delta) {
  # Calculate derived parameter p
  p <- 0.5*( sqrt(lmax^2 + 4*delta^2) - lmax )
  
  # Calculate selectivity at length
  selex <- ( (length/lmax)^(lmax/p) ) * exp( (lmax-length)/p )
  
  return(selex)
}

# Let's make sure this function works
pred.length <- selex_dome(length=mcode$Length_bin_number, lmax=16, delta=5)


# Now lets create a function to plot predictions for dome-shaped selectivity
plot_selex_dome <- function(length, lmax, delta) {
  # Create predictions for selex at length
  pred.selex <- selex_dome(length=length, lmax=lmax, delta=delta)
  # Plot it!
  plot(x=length, y=pred.selex, type="p", col="red", lwd=3,
       xlab="length", ylab="Selectivity at length", 
       main="Dome-shaped Selectivity")
  grid()
  points(x=length, y=pred.selex, pch=21, bg="red")
  segments(x0=lmax, y0=-100, x1=lmax, y1=1, lty=2, col="blue")
  segments(x0=-100, y0=1, x1=lmax, y1=1, lty=2, col="blue")
  points(x=lmax, y=1, pch=21, bg="blue")
}

# Lets check that it works

plot_selex_dome(length=mcode$Length_bin_number, lmax=16, delta=5) 
points(obs.length)
obs.length <- mcode$Length_bin_number

ssq <- function(data, ln_lmax, ln_delta) {
  # Exponentiate parameters
  lmax <- exp(ln_lmax)
  delta <- exp(ln_delta)
  pred <- selex_dome(length = data, lmax = lmax, delta = delta)
  sq <- (log(data) - log(pred))^2 # Squared difference for each observation
  ssq <- sum(sq)
  return(ssq)
}

selex <- mle2(ssq,
              start = list(ln_lmax = log(16), ln_delta = log(10)),
              data = list(data = mcode$Length_bin_number),
              optimizer = "nlminb",
              method = "Nelder-Mead")
summary(selex)

exp(coef(selex))

ssq <- function(data, ln_lmax, ln_delta, ln_sigma) {
  # Exponentiate parameters
  lmax <- exp(ln_lmax)
  delta <- exp(ln_delta)
  sigma <- ln_sigma
  pred <- selex_dome(length = data, lmax = lmax, delta = delta)
  # Calculate log-likelihood (lognormal distribution)
  logLike <- dnorm(x=log(data), mean=log(pred), sd=sigma, log=TRUE)
  
  # Calculate total negative log likelihood
  NLL <- -1*sum(logLike, na.rm=TRUE) # We need to add na.rm as there are some NA's for weights in the dataset
  return(NLL)
}

ssq(mcode$Length_bin_number, log(16), log(5), log(5))

selex <- mle2(ssq,
              start = list(ln_lmax = log(16), ln_delta = log(5), ln_sigma = log(5)),
              data = list(data = mcode$Length_bin_number),
              optimizer = "nlminb",
              method = "Nelder-Mead",
              control=list(maxit=1e6))

summary(selex)

exp(coef(selex))

selex_dome(mcode$Length_bin_number, exp(coef(selex)[1]), 21)
