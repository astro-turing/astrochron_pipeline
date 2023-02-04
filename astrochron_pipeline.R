#### Load packages ####

if (!require(stringr)) install.packages("stringr")
require(stringr)

#### load auxiliary functions ####
source("aux_functions.R")

#### read data ####
sim_output=readFortran(dir=getwd(),filename_data="amarlx",filename_params="params")

Xstar=sim_output$Xstar
Tstar=sim_output$Tstar
time_data=sim_output$df

#### transform time to depth ####
depth_data=time_to_depth(time_data)

#### re-introduce units ####
time_data$time_years=time_data$time_dimless*Tstar
depth_data$depth_cm=depth_data$depth_dimless*Xstar


plot(depth_data$depth_cm/100,depth_data$calcite_prop,type="l",xlab="depth (cm)")
plot(time_data$time_years,time_data$calcite_prop,type="l",xlab="time(years)")
