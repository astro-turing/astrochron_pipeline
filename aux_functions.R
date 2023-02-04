
## Import data from FOrtran
readFortran = function(dir=getwd(),filename_data="amarlx",filename_params="params"){
  data=read.table(paste(dir,"/",filename_data,sep=""),header=TRUE)
  data2=data[,c("t","CA.3","AR.3","P.3","U")]
  
  for (colname in colnames(data2)){
    a=strsplit(data2[,colname],split="D")
    data2[,colname]=sapply(a, function(x) as.numeric(x[1])*10^as.numeric(x[2]))
  }
  
  colnames(data2)=c("time_dimless","calcite_prop","aragonite_prop","porosity","velocity_solid_phase_dimless")
  
  params=read.table(paste(dir,"/",filename_params,sep=""),header=FALSE)
  Xstar=params$V1
  Tstar=params$V2
  return(list(df=data2,
              Xstar=Xstar,
              Tstar=Tstar))
}

## time to depth transformation ##
time_to_depth = function(time_data){
  if (any(time_data$velocity_solid_phase_dimless <= 0)){
    warning("Negative velocities of solid phase")
    reorder_depths=TRUE
  }
  U_mean=0.5*(head(time_data$velocity_solid_phase_dimless,-1)+tail(time_data$velocity_solid_phase_dimless,-1))
  depth_increment_dimless=U_mean*diff(time_data$time_dimless)
  depth_dimless=cumsum(c(0,depth_increment_dimless))
  if (anyDuplicated(depth_dimless)){
    stop("Ambiguous result in depth domain")
  }
  depth_data=time_data
  depth_data$time_dimless=depth_dimless
  colnames(depth_data)[colnames(depth_data)=="time_dimless"]="depth_dimless"
  if(reorder_depths){
    new_order=sort(x=depth_data$depth_dimless,decreasing = FALSE,index.return=TRUE)$ix
    for (colname in colnames(depth_data)){
      depth_data[,colname]=depth_data[,colname][new_order]
    }
  }
  return(depth_data)
}

## Calculate U from Phi and model parameters
# rho_s_0=2.8
# rho_w=1.023
# Sed_rate=0.01
# beta=10
# 
# Tstar=13000
# Xstar=1300
# 
# density_ratio=rho_s_0/rho_w
# 
# U_dimless(Phi, Phi_surface, beta, Sed_rate, density_ratio){
#   F_val=function(x) {
#     return(1-exp(-((10*(1-x))/(x))))
#   }
#   K_val=function(x){
#     return(beta* ((x^3)/((1-x)^2))*F_val(x))
#   }
#   U=1-
#     (K_val(Phi_surface)/Sed_rate)*(1-Phi_surface)*(density_ratio-1) +
#     (K_val(Phi)/Sed_rate)*(1-Phi)*(density_ratio-1)
#   return(U)
# }
