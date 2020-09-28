# Copyright (c) 2017 Stephen Fleming
#
# adapt code from Fleming to use a fully Bayesian approach
# d1 and c1 are not estimated using the frequentist type 1 
# method but using also a Bayesian approach.
# Also added the possibility to fix the initial state for 
# the MCMC chain to get reproducible results.
# More variables are also recorded to be analyzed at the
# end: individual Mratio and individual dprime
#
#####################################

## Packages
library(tidyverse)
library(magrittr)
library(reshape2)
library(rjags)
library(coda)
library(lattice)
library(broom)
library(ggpubr)
library(ggmcmc)

metad_group <- function (nR_S1, nR_S2, order=NULL, inits, nbchains=3) {
  
  # Type 1 parameters
  #nTot <- sum(nR_S1[[1]]$V1, nR_S2[[1]]$V1)
  nratings <- nrow(nR_S1[[1]])/2
  nsubj <- ncol(nR_S1[[1]])
  nTask <- length(nR_S1)
  
  Tol <- 1e-05
  
  # Data preparation for model
  counts <- t(nR_S1[[1]]) %>% 
    cbind(t(nR_S2[[1]]))
  
  # comment d1 and c1 for a Bayesian estimation of these parameters
  data <- list(
    #d1 = d1,
    #c1 = c1,
    order = order,
    nsubj = nsubj,
    counts = counts,
    nratings = nratings,
    Tol = Tol
  )
  
  ## Model using JAGS
  # Create and update model
  model <- jags.model(file = 'estimation/Bayes_metad_group_R.txt', data = data,
                      n.chains = nbchains, quiet=FALSE)
  update(model, n.iter=1000) # burn-in
  
  # Sampling
  output <- coda.samples( 
    model          = model,
    variable.names = c("d1","mu_d1","mu_c2","sigma_c2","sigma_d1","mu_c","sigma_c","mu_logMratio", "sigma_logMratio", "Mratio", "sigma_delta","epsilon_logMratio","cS1","cS2","c1"),
    n.iter         = 10000,
    thin           = 1 )
  
  return(output)
}
