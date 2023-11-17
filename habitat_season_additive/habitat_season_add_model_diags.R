###########################################################################
# This script is for the analysis of seasonal habitat-use patterns in Aldabra Giant Tortoises
# Temporary Emigration (N-Mixture) Model
# Data is from 1998 to 2012. Data prep file: All_transects_1998_2012.csv
#
# 
# lambda ~ Island
# theta ~ Season + Habitat
# p~1
#
#
# Author: Lorena Lanthemann
###########################################################################

###########################################################################
# 1. House keeping and loading libraries and data
###########################################################################

## 1.1. House keeping ----
##########################

rm(list = ls())

## 1.2. Loading libraries ----
##############################

load.libraries <- function(){
  library(jagsUI)
  library(MCMCvis)
  library(bayesplot)
  library(ggplot2)
}

load.libraries()

###########################################################################
# 2. JAGS analysis
###########################################################################

## 2.1 Analysis ----
##########################

# C: counts per site
# nsite = 344 
# nseasons = 29 (15 dry, 14 wet)
# nsurveys= 6 per season (each representing a month during the seasons, since wet and dry seeason are each 6 months long on Aldabra) 

# Specify model 
cat(file = "L_I_T_Add_S_H_Alt.txt","
model {
  #Model: Additive Model
  #
  # -----------------------------------------------------------------------------------------
  #
  # Parameters:
  # Each site has a super population size M, with expected site-specific density lambda, of which each individual is present and available to be detected at time t with probability theta (habitat use) and a detection probability of p (which is set to be between 0.9 and 1, since the giant tortoises should be seen when they are on site since the sites are small enough that the tortoises can't be overseen when they are there. 
  # 
  # M = superpopulation size
  # λ = expected site-specific density
  # N = # of individuals present at time of survey
  # θ = availability = probability of each individual to be present and available to be detected at time t (habitat use)
  # C = # of individuals counted during survey
  # p = detection probability  
  # -----------------------------------------------------------------------------------------
  #
  # Others:
  #
  # nsite: number of sites = 344
  # nseasons: number of seasons = 29
  # nsurveys: number of sampling occasions/surveys per season = 6 
  # 
  # Covariates:
  # season = 2-level covariate dry = 1, wet = 2
  # habitat = 8-level covariate : SMX = 1, OMX = 2, PEM = 3, EXP = 4, CAC = 5, GRA = 6, CHP = 7, SND = 8
  # island = 4-level covariate: Grande Terre East = 1, Grande Terre West = 2, Malabar = 3, Picard = 4
  # -----------------------------------------------------------------------------------------
  #
    #### PRIORS ####

    # lambda ~ island
    for (i in 1:4){    
      beta1.lambda[i]~dnorm(0,1)
    }
    
    # theta ~ habitat+season
     alpha.theta~ dnorm(0,1)
     beta1.theta[1]<- 0
     for (h in 2:8){
       beta1.theta[h] ~ dnorm(0, 1)
     }
     beta2.theta[1]<- 0
     beta2.theta[2] ~ dnorm(0, 1)

    # p ~1
    p ~ dunif(0.9,1)
    
    #### MODEL ####

    # Likelihood 
    # Ecological model for true abundance 
    for (i in 1:nsites){  
     M[i] ~ dpois(lambda[i])
      log(lambda[i])<- beta1.lambda[island[i]] #expected site-specific density
     for(t in 1:nseasons){  
     N[i,t] ~ dbin(theta[i,t], M[i]) 
      logit(theta[i,t])<-alpha.theta+beta1.theta[habitat[i]]+beta2.theta[season[t]] # availability based on habitat and season 
      for (j in 1:nsurveys){  
      C[i,t,j] ~ dbin(p, N[i,t]) # detection probability between 0.9-1 
 }
 }
}
    }
")

## 2.2. Loading data ----
##############################

#load data file: 
#contains: y_with_0_effort_corrected.RData, Island.RData, habitat_TE_new_class.RData

load("/path_to_data/aldabra_tortoise_data_all.RData")

## 2.3. Arranging variable data ----
####################################
y<-y_with_0

#prepare season as a survey covariate for theta
seasons<-rep(c(1,2), times=15)

#prepare habitat as a site covariate for theta
habitat<-c(habitat_vec_nr)

# #prepare island as a site covariate for lambda
island<-c(island_vec_nr)

## 2.4. Bundle data ----
########################## 
str(bdata <- list(C = y, 
                  nsites = dim(y)[1], 
                  nsurveys = dim(y)[3], 
                  nseasons = dim(y)[2],
                  season=seasons,
                  habitat=habitat,
                  island=island)) 

# Initial values: Need for both N and M !
Nst <- apply(y, c(1,2), max, na.rm = TRUE)+51 # Inits for latent N 
Nst[Nst == '-Inf'] <- 51 
Mst <- apply(Nst, 1, max) # .. and for latent M
inits <- function() list(M = Mst, 
                         N = Nst, 
                         beta1.lambda = rnorm(4,0,1), 
                         alpha.theta=rnorm(1,0,1),
                         p=runif(1,0.9,1)) 

# Parameters monitored 
params <- c('beta1.lambda' ,'alpha.theta', "beta1.theta", "beta2.theta", 'p')


# MCMC settings

# na <- 2 ; ni <- 10 ; nt <- 4 ; nb <- 2 ; nc <- 3  ### this was ran for trying the model ###
na <- 1000 ; ni <- 100000 ; nt <- 5 ; nb <- 20000 ; nc <- 3 ### this was ran for model prediction ###

# Call jags, check convergence and summarize posteriors
start<-Sys.time()
L_I_T_Add_S_H <- jags(bdata, 
                      inits, 
                      params, 
                      "L_I_T_Add_S_H_Alt.txt", 
                      n.adapt = na, 
                      n.chains = nc,
                      n.thin = nt, 
                      n.iter = ni, 
                      n.burnin = nb, 
                      parallel = TRUE)
end<-Sys.time()

end-start


save(L_I_T_Add_S_H, file='L_I_T_Add_S_H.RData')

###########################################################################
# 3. Model diagnostics  
###########################################################################


## 3.1. WAIC ----
###############################################

L_I_T_Add_S_H_DIC <- L_I_T_Add_S_H$DIC
L_I_T_Add_S_H_DIC 

## 3.2. Convergence and distribution check ----
###############################################

# checking Rhat values, if there ara any over 1.1
hist(L_I_T_Add_S_H$summary[,8])
length(which(L_I_T_Add_S_H$summary[,8]>1.1))

# checking n.eff values, if there ara any under 100
length(which(L_I_T_Add_S_H$summary[,9]<100))


# visual traceplot checking
MCMCtrace(object = L_I_T_Add_S_H,
          pdf = TRUE, #export to PDF
          open_pdf = TRUE,
          filename='L_I_T_Add_S_H_Trace',
          ind = TRUE, # separate density lines per chain
          Rhat = TRUE, # add Rhat
          n.eff = TRUE, # add eff sample size
          c('beta1.lambda' ,'alpha.theta',"beta1.theta", "beta2.theta", 'p'),
          iter=ni)

#whiskerplot

#caterpillar plots
color_scheme_set("teal")

plot_title <- ggtitle("Habitat Use | Habitat-Season Additive Model | Posterior distributions",
                      "medians and 95% intervals")

panel_background <- panel_bg(fill = 'white')

p<-mcmc_areas(L_I_T_Add_S_H$samples,
              pars = c('alpha.theta','beta1.theta[2]','beta1.theta[3]','beta1.theta[4]','beta1.theta[5]',
                       'beta1.theta[6]','beta1.theta[7]','beta1.theta[8]','beta2.theta[2]'),
              area_method = "equal height",
              prob = 0.95) + 
  plot_title + panel_background +
  
  theme_classic() +
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size=18))

p+scale_y_discrete(
  labels = c('beta2.theta[2]'= 'Wet',
             'beta1.theta[8]'= 'SND',
             'beta1.theta[7]'= 'CHP',
             'beta1.theta[6]'= 'GRA',
             'beta1.theta[5]'= 'CAC',
             'beta1.theta[4]'= 'EXP',
             'beta1.theta[3]'= 'PEM',
             'beta1.theta[2]'= 'OMX',
             'alpha.theta' = 'Intercept'
  ),
  limits = c('beta2.theta[2]',
             'beta1.theta[8]',
             'beta1.theta[7]',
             'beta1.theta[6]',
             'beta1.theta[5]',
             'beta1.theta[4]',
             'beta1.theta[3]',
             'beta1.theta[2]',
             'alpha.theta')
)


plot_title <- ggtitle("Expected site-specific density | Habitat-Season Additive Model | Posterior distributions",
                      "medians and 95% intervals")
p<-mcmc_areas(L_I_T_Add_S_H$samples,
              pars = c('beta1.lambda[1]','beta1.lambda[2]','beta1.lambda[3]','beta1.lambda[4]'),
              area_method = "equal height",
              prob = 0.95) + 
  plot_title + panel_background +
  
  theme_classic() +
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size=18))

p+scale_y_discrete(
  labels= c('beta1.lambda[1]'='Grande Terre East',
            'beta1.lambda[2]'='Grande Terre West',
            'beta1.lambda[3]'='Malabar',
            'beta1.lambda[4]'='Picard'),
  limits= c('beta1.lambda[4]',
            'beta1.lambda[3]',
            'beta1.lambda[2]',
            'beta1.lambda[1]')
)


#checking summary
MCMCsummary(object = L_I_T_Add_S_H, round = 3, params <- c('beta1.lambda' ,'alpha.theta',"beta1.theta", "beta2.theta", 'p'))[ ,c(1,3,5)]

