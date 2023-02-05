#### load auxiliary functions ####
source("aux_functions.R")

#### Load packages ####
# add required packages to this vector to automatically load them (and install if necessary)
required_packages=c("stringr","astrochron")

loadPackages(required_packages)

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

#### visualize output Calcite ####
plot(depth_data$depth_cm/100,depth_data$calcite_prop,type="l",xlab="depth (cm)")
plot(time_data$time_years,time_data$calcite_prop,type="l",xlab="time(years)")

#### trim strat data ####
burn_in_cm=8000
sampling_dist_cm=10



if ((burn_in_cm+sampling_dist_cm) > max(depth_data$depth_cm)) stop("burn in longer than sed column")
depth_new=seq(from = burn_in_cm,
               to = max(depth_data$depth_cm),
               by = sampling_dist_cm)

depth_data_interp=data.frame(depth_cm = depth_new)

for (colname in colnames(depth_data)){
  if (colname %in% c("depth_dimless","depth_cm","velocity_solid_phase_dimless")){
    next
  }
  depth_data_interp[,colname]=approx(x=depth_data$depth_cm,y=depth_data[,colname],xout = depth_new)[[2]]
  
}
plot(depth_data_interp$depth_cm/100,depth_data_interp$calcite_prop,type="l")
