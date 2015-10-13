library(ncdf)
library(raster)

setwd("/Users/hoy/Desktop/MSUWC/Data/DriverData/GYE_Daymet_Outputs")
      
plot(raster("GYE_Daymet_stand_monthly_spack.nc", band=7))
plot(raster("GYE_Daymet_stand_monthly_msro.nc", band=7))


system("cdo ifthenc,1 GYE_Daymet_stand_monthly_spack.nc snowMask.nc")
system("cdo mul snowMask.nc GYE_Daymet_stand_monthly_msro.nc msroSnow.nc")
system("rm snowMask.nc")

system("cdo ifnotthenc,1 GYE_Daymet_stand_monthly_spack.nc noSnowMask.nc")
system("cdo mul noSnowMask.nc GYE_Daymet_stand_monthly_msro.nc msroNoSnow.nc")
system("rm noSnowMask.nc")

plot(raster("msroNoSnow.nc", band=8))
plot(raster("msroSnow.nc", band=8))
plot(raster("GYE_Daymet_stand_monthly_msro.nc", band=8))


cellStats(raster("msroNoSnow.nc", band=9), 'sum')+
cellStats(raster("msroSnow.nc", band=9), 'sum')
cellStats(raster("GYE_Daymet_stand_monthly_msro.nc", band=9), 'sum')

