library(raster)
library(plot.matrix)
library(RColorBrewer)
library(imager)
library(tidyverse)
library(LSTDConnect)
library(pfocal)      # For focal test calculations
library(tictoc)      # For benchmarking

# Source functions
source('scripts/0_helper_functions.R')


baseDir = "C:/Users/HughesJo/Documents/InitialWork/Connectivity/ConnectivityMetricsForMonitoring/IALEiposter/KernelVisualizations"
outDir = baseDir
paID =4025
inDir = paste0(baseDir,"/pa",paID)#"/pa4025")

#Example landcover map
exQuality=raster(paste0(inDir,"/newQuality.tif"))
exQuality=exQuality/100

paBoundR = raster(paste0(inDir,"/newPA.tif"))
paBound = rasterToPolygons(paBoundR,dissolve=T)

oneMap = exQuality;oneMap[exQuality>=0]=1
#set paBoundR to larger area to better see variation
edge = 0.6*150#455
paBoundR=oneMap
paBoundR[1:edge,]=NA
paBoundR[,1:edge]=NA
paBoundR[(nrow(paBoundR)-edge):nrow(paBoundR),]=NA
paBoundR[,(nrow(paBoundR)-edge):ncol(paBoundR)]=NA

plot(paBoundR)
plot(paBound,add=T)

exQualityP=exQuality
exQualityP[1,1]=1;exQualityP[1,2]=0

plot(exQuality)
plot(paBound,add=T)

cPatches = exQuality
cPatches[!is.na(cPatches)]=0
cPatches[round(nrow(exQuality)/2),round(ncol(exQuality)/2)]=1
plot(cPatches)

writeRaster(exQualityP,paste(paste0(outDir,"/components/exQualityP.tif")),overwrite=T)
writeRaster(exQuality,paste(paste0(outDir,"/components/exQuality.tif")),overwrite=T)
writeRaster(paBoundR,paste(paste0(outDir,"/components/paBoundR.tif")),overwrite=T)

# Parameter grid ----------------------------------------------------------
t <- c(444) # calibrates for 900 m raster, 20 km displacement
mean_displacement <- c(20)

t_df <- tibble(t = t, 
               mean_displacement = (mean_displacement * 1000)/res(exQuality)[1]) 
t_df$scale = mean_displacement

scale_factor <- 300

uniform_res <- c(TRUE, FALSE)
uniform_mort <- c(TRUE, FALSE)

background_value_res <- 1
background_value_mort <- 0.00001

ceiling_value_res <- c(2, 20)
ceiling_value_mort <- ceiling_value_res/scale_factor

dirs <- 8
occ_multiplier <- 100

SAMC_params <- tibble(ceiling_value_res = ceiling_value_res, 
                      ceiling_value_mort = ceiling_value_mort) %>%
  expand_grid(uniform_res = uniform_res,
              uniform_mort = uniform_mort,
              background_value_res = background_value_res, 
              background_value_mort = background_value_mort, 
              dirs = dirs, 
              occ_multiplier = occ_multiplier) %>%
  slice(-which(.data$uniform_res & .data$uniform_mort)[-1]) %>%
  mutate(type = "SAMC") %>% 
  relocate(type, 1) 

KERN_params <- expand_grid(type = c("EXPONENTIAL", 'UNIFORM', 'INTACTNESS'), 
                           occ_multiplier = occ_multiplier)

parameters <- bind_rows(SAMC_params, KERN_params)

# Label param combinations

the_labels <- c()
for (row in 1:nrow(parameters)){
  params <- parameters[row,]
  the_labels <- c(the_labels, encode_parameters(params))
}
parameters$sce <- the_labels

###########
doPlots = expand.grid(shape=c("PN","PL","PH","CH","MH","BH","CL","ML","BL","NL","E","U"),
                      scale=c(20))
doPlots$id = seq(1:nrow(doPlots))
doPlots
doPlots$time = NA
doPlots$displacement = NA

plotSet = list()
for(i in 1:nrow(doPlots)){
  #i = 1
  dbit =doPlots[i,]
  params = subset(parameters,sce==dbit$shape)
  m = paste0(dbit$scale,dbit$shape)
  
  print(i)
  
  oName = paste0(dbit$scale," km ",dbit$shape)
  
  if(!grepl("P",dbit$shape)){
    ptm <- proc.time()
    
    trMap = run_connectivity(landscape = exQuality,
                                        parameters = params, 
                                        t_df = t_df,out_dir = outDir)
    tt = proc.time() - ptm
    doPlots$time[i] = tt[3]
    
    trMap = raster(gsub(".tif","_1.tif",trMap$output_map,fixed=T))
    
    kvis = run_connectivity(landscape = exQuality,
                            parameters = params, 
                            t_df = t_df,out_dir = outDir,sourceMap = cPatches)
    kvis = raster(gsub(".tif","_1.tif",kvis$output_map,fixed=T))
    kvis = kvis
    kvis[kvis<=10^-6]=NA
    
    
    
    trMap=trMap/params$occ_multiplier


  }else if (grepl("P",dbit$shape)){
    #parc
    dbar=dbit$scale #*res(exQuality)[1]/cellDim
    ee = exponentialKernel(dbar,cellDim=res(exQuality)[1]/1000,negligible=10^-6)
    maxDistHold = (nrow(ee)-1)/2
    
    cp=cPatches
    
    cBuff = lsBuffer(cPatches,maxDistHold*res(cPatches)[1])
    
    cBuff[cBuff==0]=NA
    
    shapeMult = 1*grepl("PN",dbit$shape)+2*grepl("PL",dbit$shape)+20*grepl("PH",dbit$shape)
    
    cCost =1+(shapeMult-1)*(1-exQuality)/1
    d_ij=NULL; ww_ij=NULL; gc()
    
    d_ij = leastCostPathDistances(cPatches,cCost,maxDist=maxDistHold*1000,bufferedPatches=cBuff,
                                  neighbourhood="octagon")
    d_ij$clump1=d_ij$clump1/1000
    
    #d_ij$clump1=d_ij$clump1/res(cPatches)[1]
    #d_ij=igList
    ww_ij = exp(-(2/dbar)*d_ij$clump1)
    wjsums = sum(ww_ij,na.rm=T)#rowSums(w_ij,na.rm=T)
    #wjsums = matrix(wjsums,nrow=nrow(w_ij),ncol=ncol(w_ij))
    ww_ij=ww_ij/wjsums
    
    w_jd = data.frame(id=as.numeric(dimnames(ww_ij)[[2]]),w=ww_ij[1,])
    w_jd=merge(data.frame(id=1:ncell(exQuality)),w_jd,all.x=T)
    kvis = exQuality;kvis[!is.na(kvis)]=NA;kvis[]=w_jd$w
    #plot(kvis)
    
    w_jd=NULL; stdP=NULL;testPC=NULL; samc_obj=NULL; d_ij=NULL; ww_ij=NULL; gc()
    
    #    sort( sapply(ls(),function(x){object.size(get(x))}))
    
    #plot(exQualityP,axes=FALSE,box=FALSE,legend=FALSE, frame=FALSE, asp = "", xpd = NA)
    #plot(kvis,col=brewer.pal(n=9, name = "Blues"),add=T,axes=FALSE,box=FALSE,legend=FALSE, frame=FALSE, asp = "", xpd = NA)
    paBoundR[is.na(paBoundR)]=0
    #plot(paBoundR)
    cellStats(paBoundR,"sum")
    
    ptm <- proc.time()
    testPC = parcConnectedness(x=stack(paBoundR,cCost,exQuality),maxDist=maxDistHold*res(cPatches)[1],alpha=2/(dbar*1000),memoryLimit=0,stopOnMemoryLimit=T)
    tt = proc.time() - ptm
    doPlots$time[i] = tt[3]
    
    M_i=testPC@M_i;testPC=NULL
    #    plot(testPC@M_i)
    
    stdP = parcConnectedness(x=stack(paBoundR,oneMap,oneMap),maxDist=maxDistHold*res(cPatches)[1],alpha=2/(dbar*1000),memoryLimit=0,stopOnMemoryLimit=T)
    
    #    plot(stdP@M_i)
    paBoundR[paBoundR==0]=NA
    
    trMap = M_i/stdP@M_i
    
    stdP=NULL;gc()
  }

  trMap[is.na(paBoundR)]=NA
  trMapH = trMap*exQuality
  
  #get mean displacement of kernel.
  cp = cPatches
  cp[cp!=1]=NA
  cp =rasterToPoints(cp);cp=cp[,1:2]
  
  dd = distanceFromPoints(oneMap,cp)
  
  doPlots$displacement[i] = cellStats(dd*kvis/cellStats(kvis,"sum"),"sum")/1000
  
  writeRaster(kvis,paste(paste0(outDir,"/components/kernel",m,".tif")),overwrite=T)
  writeRaster(trMap,paste(paste0(outDir,"/components/Gamma",m,".tif")),overwrite=T)
  writeRaster(trMapH,paste(paste0(outDir,"/components/R",m,".tif")),overwrite=T)
  
  #dev.new(height=nrow(exQuality), width=nrow(exQuality))
  #dev.print(png,paste0(outDir,"/mapSamples",m,".png"), width=400, height=400)
  png(paste0(outDir,"/components/mapSamples",m,".png"), width=400, height=400)
  par(mai=c(0,0,0,0),mar=c(0,0,0,0),oma=c(0,0,0,0),omi=c(0,0,0,0))
  
  plot(exQualityP,axes=FALSE,box=FALSE,legend=FALSE, frame=FALSE, asp = "", xpd = NA)
  plot(kvis,col=brewer.pal(n=9, name = "Blues"),add=T,axes=FALSE,box=FALSE,legend=FALSE, frame=FALSE, asp = "", xpd = NA)
  plot(paBound,add=T,border="black",axes=FALSE)
  #plot(rasterToContour(kvis),add=T,axes=FALSE,box=FALSE,legend=FALSE, frame=FALSE, asp = "", xpd = NA)
  dev.off()
  
  
  trMap[1,1]=1;trMap[1,2]=0
  trMapH[1,1]=1;trMapH[1,2]=0
  
  #dev.new(height=nrow(exQuality), width=nrow(exQuality))
  png(paste0(outDir,"/components/Gamma",m,".png"), width=400, height=400)
  
  par(mai=c(0,0,0,0),mar=c(0,0,0,0),oma=c(0,0,0,0),omi=c(0,0,0,0))
  plot(trMap,axes=FALSE,box=FALSE,legend=FALSE, frame=FALSE, asp = "", xpd = NA)
  plot(paBound,add=T,border="black",axes=FALSE)
  #dev.print(png,paste0(outDir,"/Gamma",m,".png"), width=400, height=400)
  dev.off()
  
  
  png(paste0(outDir,"/components/R",m,".png"), width=400, height=400)
  #dev.new(height=nrow(exQuality), width=nrow(exQuality))
  par(mai=c(0,0,0,0),mar=c(0,0,0,0),oma=c(0,0,0,0),omi=c(0,0,0,0))
  plot(trMapH,axes=FALSE,box=FALSE,legend=FALSE, frame=FALSE, asp = "", xpd = NA)
  plot(paBound,add=T,border="black",axes=FALSE)
  
  
  #dev.print(png,paste0(outDir,"/R",m,".png"), width=400, height=400)
  dev.off()
  
  
  write.csv(doPlots,paste0(outDir,"/performanceTab.csv"),row.names=F)
  
}

#400*400
#size of focal area
cellStats(paBoundR,"sum")
doPlots

doTiming = subset(doPlots,is.element(shape,c("NL","E","U","PN")))
names(doTiming)[names(doTiming)=="shape"]="method"
doTiming$method = gsub("1","",as.character(doTiming$method))

base = ggplot(doTiming,aes(x=scale,y=time,shape=method,colour=method))+geom_point()+
  theme_bw()+xlab("mean displacement (pixels)")+ylab("time (seconds)")

png(paste0(outDir,"/performance.png"),height=3,width=5,units="in",res=600)
print(base)
dev.off()

write.csv(doPlots,paste0(outDir,"/exampleInput.csv"))
