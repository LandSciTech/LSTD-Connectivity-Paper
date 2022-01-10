library(raster)
library(fasterize)
library(sf)
library(rasterVis)

#example layer stack
#source("C:/Users/HughesJo/Documents/gitprojects/LSTD-Connectivity-Paper/scripts/x_Appendix.R")

# -------------------------------------------------------------------------

# resultDir = "C:/Users/HughesJo/Documents/gitprojects/LSTD-Connectivity-Paper/outputs/rasters"
resultDir = "D:/CAN_COST_LSTD_Connectivity_output_rasters/"

doStandardization=T
the_scale = 40

# H = raster(paste0(resultDir,"/allCostsLayer.tif"))
# N = raster(paste0(resultDir,"/naturalCostsLayer.tif"))
H <- raster("../data/CombinedCosts/allCostsLayer.tif")
N <- raster("../data/CombinedCosts/naturalCostsLayer.tif")

provs =  getData('GADM', country='CAN', level=1)
provs= spTransform(provs,crs(H))
countryR = fasterize(st_as_sf(provs),H)
writeRaster(countryR, "outputs/rasters/countryR.tif")
# unique(H)

#cropExtent = extent(H, round(0.18*nrow(H)), round(0.83*nrow(H)), round(0.05*ncol(H)), round(0.35*ncol(H))))
cropExtent = extent(H, round(0.48*nrow(H)), round(0.765*nrow(H)), round(0.06*ncol(H)), round(0.23*ncol(H)))
H <- crop(H, cropExtent)
N <- crop(N, cropExtent)
H = 1-H/1000
N = 1-N/1000
Anthro = (1-H)-(1-N)

provs = provs[is.element(provs@data$NAME_1,c("British Columbia","Alberta")),]
provsR= fasterize(st_as_sf(provs),Anthro)
# plot(provsR)

# -------------------------------------------------------------------------

scale_vector <- c(2, 5, 10, 20, 40)
removeTmpFiles(0)
for (s in scale_vector){
  plot_spatial_agreement(s, standardize = F, scenarios = c("I","ML","MH"))
  removeTmpFiles(0)
}
for (s in scale_vector){
  plot_spatial_agreement(s, standardize = T, scenarios = c("I","ML","MH"))
  removeTmpFiles(0)
}

# -------------------------------------------------------------------------

# NOTE: function plot_spatial_agreement aims to cover this first part

layerSet = list.files(paste0(resultDir,"/Can_Cost"))

selectTerm = as.character(the_scale)
selectSet = layerSet[grepl(selectTerm,layerSet,fixed=T)]

if(the_scale==2){
  selectSet = selectSet[!grepl("20",selectSet,fixed=T)]
}

view = stack(paste0(resultDir,"/Can_Cost/",selectSet))

if(doStandardization){
  denom = stack(paste0(resultDir,"/Can_Cost_noH/",gsub(".tif","no_HF.tif",selectSet,fixed=T)))
  for(i in 1:nlayers(view)){
    view[[i]]=view[[i]]/denom[[i]]
  }
  rm(denom)
}  

names(view)=gsub(".tif","",selectSet,fixed=T)
plotStack = subset(view,paste0(c("I","ML","MH"),the_scale))

plotStack[[1]] = plotStack[[1]]*countryR
plotStack[[2]] = plotStack[[2]]*countryR
plotStack[[3]] = plotStack[[3]]*countryR

names(plotStack)= paste0(c("I","ML","MH"),the_scale)
pdf(paste0("outputs/figures","/fig5MapsRawBig",selectTerm,"std",doStandardization,".pdf"),
    width=8,height=3)
par(mar=c(0,0,0,0), oma=c(0,0,0,0))
levelplot(plotStack,xlab=NULL,ylab=NULL,scales=list(draw=FALSE),maxpixels = 2e5,col.regions=rev(terrain.colors(255)))
dev.off()
rm(plotStack)


# -------------------------------------------------------------------------


view <- crop(view, cropExtent)

view[is.na(provsR)]=NA
view[N==0]=NA # only look at agreement/disagreement for places that aren't rock and ice

names(view)=gsub(".tif","",selectSet,fixed=T)

plotStack = subset(view,paste0(c("I","ML","MH","BH"),the_scale))
pdf(paste0("outputs/figures","/fig5MapsRaw",selectTerm,"std",doStandardization,".pdf"),
    width=8,height=3)
par(mar=c(0,0,0,0), oma=c(0,0,0,0))
levelplot(plotStack,xlab=NULL,ylab="unscaled metric values",scales=list(draw=FALSE),
          maxpixels = 2e5,col.regions=rev(terrain.colors(255)))
#plot(plotStack,axes=F,horizontal=T,nr=1)
dev.off()
rm(plotStack)

# -------------------------------------------------------------------------

quants = seq(0,1,length.out=101)#seq(0.25,1,length.out=4)

qTab = quantile(view,probs=quants)

cc = view
for(j in 1:nrow(qTab)){
  cc[[j]] <- cut(view[[j]], breaks=unique(qTab[j,]))
}
names(cc)=gsub(".tif","",selectSet,fixed=T)

plotStack = subset(cc,paste0(c("I","ML","MH","BH"),the_scale))
pdf(paste0("outputs/figures","/fig5Maps",selectTerm,"std",doStandardization,".pdf"),
    width=8,height=3)
if(doStandardization){
  xlab = "percentiles scaled"
}else{
  xlab="percentiles unscaled"
}
par(mar=c(0,0,0,0), oma=c(0,0,0,0))
levelplot(plotStack,ylab=xlab,xlab=NULL,scales=list(draw=FALSE),maxpixels = 2e5,
          col.regions=rev(terrain.colors(255)))
#plot(plotStack,axes=F,horizontal=T,nr=1)
dev.off()

# -------------------------------------------------------------------------

var = calc(cc, sd)
# writeRaster(var,paste0(resultDir,"/derived/sd",selectTerm,"std",doStandardization,".tif"),overwrite=T)

middle = calc(cc,mean)
# writeRaster(middle,paste0(resultDir,"/derived/mean",selectTerm,"std",doStandardization,".tif"),overwrite=T)
rm(cc);rm(view)
removeTmpFiles(h=0.1)

# -------------------------------------------------------------------------

outSet =   list.files(paste0(resultDir,"/derived"))
outSet=outSet[grepl(as.character(scale),outSet,fixed=T)]
sdSet =     outSet[grepl("sd",outSet,fixed=T)&grepl(paste0("std",doStandardization),outSet,fixed=T)]
meanSet =     outSet[grepl("mean",outSet,fixed=T)&grepl(paste0("std",doStandardization),outSet,fixed=T)]

sds = stack(paste0(resultDir,"/derived/",sdSet))
names(sds)=sdSet

means = stack(paste0(resultDir,"/derived/",meanSet))
names(means)=meanSet

cStack = stack(H,Anthro,means[[1]],sds[[1]])
names(cStack)=c("Quality H","Human Footprint","percentile mean","percentile sd")

cStack[is.na(provsR)]=NA
pdf(paste0(gsub("rasters","figures",resultDir,fixed=T),"/fig4Maps",scale,"std",doStandardization,".pdf"),
    width=11,height=4)
par(mar=c(0,0,0,0), oma=c(0,0,0,0))
#levelplot(cStack,xlab=NULL,ylab=NULL,scales=list(draw=FALSE),maxpixels = 2e5)
plot(cStack,axes=F,horizontal=T,nr=1)
dev.off()

removeTmpFiles(h=0.1)

# -------------------------------------------------------------------------
