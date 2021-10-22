
#install.packages("samc")
#samc experiments
library(raster)
library(RColorBrewer)
library(imager)
library(tidyverse)
library(viridis)
library(samc)

baseDir = "C:/Users/HughesJo/Documents/InitialWork/Connectivity/ConnectivityMetricsForMonitoring/IALEiposter/KernelVisualizations"
outDir = baseDir
inDir = paste0(baseDir,"/pa4025")

inMap = "quality300"

runCalibration = F

if(runCalibration){
  times = unique(round(10^seq(log10(3),log10(17000),length.out=50)))
  ##################
  #input maps
  exQuality=raster(paste0(inDir,"/",inMap,".tif"))
  exQuality=exQuality/100
  exQuality[is.na(exQuality)]=0

  exQualityP=exQuality
  exQualityP[1,1]=1;exQualityP[1,2]=0

  oneMap = exQuality;oneMap[exQuality>=0]=1

  cPatches = exQuality
  cPatches[!is.na(cPatches)|is.na(cPatches)]=0
  cPatches[round(ncol(exQuality)/2),round(nrow(exQuality)/2)]=1
  plot(cPatches)
  cellStats(cPatches,"max")
  backgroundMortality = 0.00001

  #  If starting at central location, how far by end of dispersal period?
  res_data <-  oneMap# 1+20*(1-exQuality)/1 #oneMap #resistance
  abs_data <- oneMap;abs_data[abs_data==1]=backgroundMortality #very low background mortality


  samc_obj_custom <- LSTDConnect::samc_cache(resistance = as.matrix(res_data),
                                             absorbtion = as.matrix(abs_data),
                                             kernel = 8)


  #figure out scales
  tSet = expand.grid(t=times,meanDisplacement=NA)
  for(i in 1:nrow(tSet)){
    #i=91
    t=tSet$t[i]

    # Calculate short- and long-term mortality metrics and long-term dispersal
    #long_visit = visitation(samc_obj)#expensive

    short_disp_custom <- LSTDConnect::samc_step(steps = t,
                                                cache = samc_obj_custom,
                                                population = as.matrix(cPatches))

    short_disp_map <- raster(as.matrix(short_disp_custom$population[[1]]),
                             template = cPatches)

    #calculate mean displacement
    cp=cPatches;cp[cp==0]=NA;cp =rasterToPoints(cp);cp=cp[,1:2]

    dd = distanceFromPoints(oneMap,cp)/res(cPatches)[1] # results in units of cells
    dd = raster(as.matrix(dd@data@values),
                template = cPatches)

    tSet$meanDisplacement[i] = cellStats(dd*short_disp_map/cellStats(short_disp_map,"sum"),"sum")
  }

  tSet


  write.csv(tSet,paste0(outDir,"/SAMCCalibrationTable.csv"))
  plot(t~meanDisplacement,data=tSet)
  library(mgcv)

  mm = loess(t~meanDisplacement,data=tSet,span=0.1)

  saveRDS(mm,paste0(outDir,"/SAMCCalibrationModel.rds"))
}else{
  mm=readRDS(paste0(outDir,"/SAMCCalibrationModel.rds"))
  tSet = read.csv(paste0(outDir,"/SAMCCalibrationTable.csv"))
}

#300 m results
small = data.frame(meanDisplacement=c(2,5,10,20,40)*1000/300)
round(predict(mm,small,type="response"))

#900 m results
med = data.frame(meanDisplacement=c(2,5,10,20,40)*1000/900)
round(predict(mm,med,type="response"))

#1000 m results
large = data.frame(meanDisplacement=c(2,5,10,20,40)*1000/1000)
round(predict(mm,large,type="response"))

tSet$pred = round(predict(mm,type="response"))
bit = tSet# subset(tSet,meanDisplacement<10)
plot(t~meanDisplacement,data=tSet,type="l");points(pred~meanDisplacement,data=tSet)
