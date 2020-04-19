#############################################################################
## Cascade CEA Model - Interventions
## Deterministic, allowing parallel estimations
## for the selected 10 frontier combinations
## Last updated: Apr 10, 2020
#############################################################################
# 1. SET directpry and workspace
#############################################################################
rm(list=ls())
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("CascadeCEA-Interventions-1-LoadBaselineWorkspace.R")

# SELECT city ##
CITY <- select.list(all.cities, multiple = FALSE,
                    title = 'Select city', graphics = FALSE)
# ww <- 1; CITY <- all.cities[[ww]] # Otherwise you can set city by its index

## LOAD list of all combinations, interventions indexed by number in each combination
# if the list does not exist, source("CascadeCEA-Combination-0-Setup-combination.list.R")
combination.number <- readRDS(paste0("Combination/ProductionFunction-Frontier-",CITY,".rds"))$frontier
combination.list <- readRDS("Combination/Combination.list.rds")

## LOAD ODE function
source("ModelCoreModules/CascadeCEA-Model-0-Function-ode_model-Combination.R")

## LOAD analysis scenario
case = "DM"  # DM for deterministic, SA for sensitivity analysis

## LOAD all input parameters and comparators
source("CascadeCEA-Interventions-1-LoadParameterWorkspace-Combination.R")

total.comb <- length(combination.number)

tic("Model run")

outcome.comb.mx <- matrix(0, nrow = total.comb, ncol = 44)    ##Initialize combination outcome matrix (to save results)

outcome.comb.mx <- foreach(cc=1:total.comb, .combine=rbind, .export = export.int.model.names
) %dopar% {
  comb.eva.func(input.parameters = all.params, current.int = interventions[combination.list[[combination.number[cc]]]])}

future:::ClusterRegistry("stop")   ##Stop parallel estimations and free cores

colnames(outcome.comb.mx)        <- rep("FILL", 44)
colnames(outcome.comb.mx)[1:6]   <- c("Infections.total-20Y", "SuscPY-over20Y", "Infections.total-10Y", "SuscPY-over10Y", "Infections.total-5Y", "SuscPY-over5Y")
colnames(outcome.comb.mx)[7:32]  <- paste0("Year", c(2015:2040))
colnames(outcome.comb.mx)[33:44] <- c("QALYs.sum", "costs.total.sum", "costs.hru.sum", "costs.art.sum", "costs.art.ini.sum",   
                                      "costs.oat.sum", "costs.prep.sum", "costs.prep.tests.sum", "costs.test.sum", "int.costs.sum",       
                                      "int.impl.costs.sum", "int.sust.costs.sum")

saveRDS(outcome.comb.mx,   paste0("Combination/Outcome-All-Combination-", CITY, "-DM.rds"))

toc()

refcase<-unlist(readRDS(paste0("Inputs/Combination-DM-",CITY,"-refcase-outcomes.rds")))

panelA_table3.frontier.com<-matrix(0,nrow=total.comb,ncol=3)
colnames(panelA_table3.frontier.com)<-c("Incremental Cost", "Incremental QALYs", "ICER")
panelA_table3.frontier.com<-data.frame(panelA_table3.frontier.com)
panelA_table3.frontier.com[,1]<-outcome.comb.mx[,"costs.total.sum"]-refcase["costs.total.sum"]
panelA_table3.frontier.com[,2]<-outcome.comb.mx[,"QALYs.sum"]-refcase["QALYs.sum"]
i=1
for (i in 1:total.comb){
  if(panelA_table3.frontier.com[i,1]<0){
    panelA_table3.frontier.com[i,3]<-"CS"
  }else{
    panelA_table3.frontier.com[i,3]<-(panelA_table3.frontier.com[i,1]-panelA_table3.frontier.com[i-1,1])/(panelA_table3.frontier.com[i,2]-panelA_table3.frontier.com[i-1,2])
  }
}
panelA_table3.frontier.com
