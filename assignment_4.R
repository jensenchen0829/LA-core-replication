#############################################################################
## Cascade CEA Model - Interventions
## Deterministic, at the ideal scale: determine when epidemic control is reached reached (defined as a new incidence rate of 1 per 10,000) or, 
## if the target can’t be reached, what is the minimum incidence that can be reached by the last year of the evaluation period.
## int.sus is modified to 20
## Last updated: Apr 13, 2020
#############################################################################
# 1. SET directpry and workspace
#############################################################################
rm(list=ls())
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("CascadeCEA-Interventions-1-LoadBaselineWorkspace.R")  #in this RScript the int.sus=10

# SELECT city ##
ww <- 3
CITY <- all.cities[[ww]] # set CITY to "LA"

int.sus<- 20 #change intervention sustainment duration 
int.end <- (int.start + (int.sus * 12))

## LOAD list of all combinations, interventions indexed by number in each combination
# if unavailable, source("CascadeCEA-Combination-0-Setup-combination.list.R")
combination.list <- readRDS("Combination/Combination.list.rds")

## LOAD ODE function
source("ModelCoreModules/CascadeCEA-Model-0-Function-ode_model-Combination.R")

#Load OCIS, if unavailable, execute Part 2 of: "CascadeCEA-Interventions-3-Analysis-ProductionFunction.R"
ocis        <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$ocis
current.int <- interventions[combination.list[[ocis]]]




#######################
#### Deterministic ####
#######################
## LOAD analysis scenario
case  = "DM"  # DM for deterministic, SA for sensitivity analysis
int.scale = "ideal"  # This variable exists only for ideal scale

## LOAD all input parameters and comparators
source("CascadeCEA-Interventions-1-LoadParameterWorkspace-Combination.R")


## RUN the deterministic model
outcome.dm.mx <- matrix(0, nrow = 2, ncol = 44)    ##Initialize intervention outcome matrix (to save results)
colnames(outcome.dm.mx) <- rep("FILL", 44)
colnames(outcome.dm.mx)[1:6]   <- c("Infections.total-20Y", "SuscPY-over20Y", "Infections.total-10Y", "SuscPY-over10Y", "Infections.total-5Y", "SuscPY-over5Y")
colnames(outcome.dm.mx)[7:32]  <- paste0("Year", c(2015:2040))
colnames(outcome.dm.mx)[33:44] <- c("QALYs.sum", "costs.total.sum", "costs.hru.sum", "costs.art.sum", "costs.art.ini.sum",   
                                    "costs.oat.sum", "costs.prep.sum", "costs.prep.tests.sum", "costs.test.sum", "int.costs.sum",       
                                    "int.impl.costs.sum", "int.sust.costs.sum")
outcome.dm.mx[2, ]      <- comb.eva.func(input.parameters = all.params, current.int = current.int)

###answers
new.incidence.by.year<-outcome.dm.mx[2,7:32]
which(new.incidence.by.year/6755896<=0.0001)#integer(0), the target of epidemic control cannot be reached
which.min(new.incidence.by.year/6755896)#gives the year of minimum new incidence rate
print(c("minimum new incidence rate:",min(new.incidence.by.year/6755896)))
