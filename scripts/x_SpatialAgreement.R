library(raster)
library(fasterize)
library(sf)
library(rasterVis)

pal = 'RdYlBu'

# -------------------------------------------------------------------------
doBigPlots = F
 resultDir = "./outputs/rasters"

 H = raster(paste0(resultDir,"/allCostsLayer.tif"))
 N = raster(paste0(resultDir,"/naturalCostsLayer.tif"))

provsA =  getData('GADM', country='CAN', level=1)
provsA= spTransform(provsA,crs(H))
countryR = fasterize(st_as_sf(provsA),H)
writeRaster(countryR, "outputs/rasters/countryR.tif",overwrite=T)

#NOTE: ecozone shape file available for download here https://sis.agr.gc.ca/cansis/nsdb/ecostrat/gis_data.html
zones = st_read("./data/Ecozones")
zones = st_transform(zones,crs(H))

if(doBigPlots){
  plotStack = stack(H,N)
  names(plotStack)= c("with human footprint","without human footprint")
  plotStack[[1]] = plotStack[[1]]*countryR
  plotStack[[2]] = plotStack[[2]]*countryR
  plotStack = 1-plotStack/1000
  pdf(paste0("outputs/figures","/fig1BigQuality.pdf"),
      width=6*1.5,height=4*1.3)
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  levelplot(plotStack,xlab=NULL,ylab=NULL,scales=list(draw=FALSE),maxpixels = 2e5,col.regions=hcl.colors(255,palette="Reds"))
  dev.off()
  rm(plotStack)
}  

layerSet = list.files(paste0(resultDir,"/Can_Cost"))

doSDs = F
doStandardization=T
the_scale = 20
selectTerm = as.character(the_scale)
selectSet = layerSet[grepl(selectTerm,layerSet,fixed=T)]

if(the_scale==2){
  selectSet = selectSet[!grepl("20",selectSet,fixed=T)]
}

view = stack(paste0(resultDir,"/Can_Cost/",selectSet))
plot(view[[3]])

if(doStandardization){
  denom = stack(paste0(resultDir,"/Can_Cost_noH/",gsub(".tif","no_HF.tif",selectSet,fixed=T)))
  for(i in 1:nlayers(view)){
    view[[i]]=view[[i]]/denom[[i]]
  }
  rm(denom)
}  

names(view)=gsub(".tif","",selectSet,fixed=T)
selectNs = c("I","ML","BL","E","MH","BH")
plotStack = subset(view,paste0(selectNs,the_scale))

plotStack[[1]] = plotStack[[1]]*countryR
plotStack[[2]] = plotStack[[2]]*countryR
plotStack[[3]] = plotStack[[3]]*countryR
plotStack[[4]] = plotStack[[4]]*countryR
plotStack[[5]] = plotStack[[5]]*countryR
plotStack[[6]] = plotStack[[6]]*countryR

names(plotStack)= paste0(selectNs,the_scale)
pdf(paste0("outputs/figures","/fig5MapsRawBig",selectTerm,"std",doStandardization,".pdf"),
    width=8,height=5)
par(mar=c(0,0,0,0), oma=c(0,0,0,0))
levelplot(plotStack,xlab=NULL,ylab=NULL,scales=list(draw=FALSE),maxpixels = 2e5,col.regions=hcl.colors(255,palette=pal))
dev.off()
rm(plotStack)


H = raster(paste0(resultDir,"/allCostsLayer.tif"))
N = raster(paste0(resultDir,"/naturalCostsLayer.tif"))

#cropExtent = extent(H, round(0.4135*nrow(H)), round(0.765*nrow(H)), round(0.06*ncol(H)), round(0.23*ncol(H)))
cropExtent = extent(H, round(0.4135*nrow(H)), round(0.764*nrow(H)), round(0.035*ncol(H)), round(0.305*ncol(H)))

H <- crop(H, cropExtent)
N <- crop(N, cropExtent)
H = 1-H/1000
N = 1-N/1000
Anthro = (1-H)-(1-N)

provs = provsA[is.element(provsA@data$NAME_1,c("British Columbia","Alberta")),]
provs = st_union(st_as_sf(provs))
provsR= fasterize(st_as_sf(provs),H)
plot(provsR)
plot(provs,add=T)

unique(zones$ZONE_ID)
zonesS = st_intersection(zones,provs)
zonesR = fasterize(zones,Anthro,field="ZONE_ID",fun="max")

view <- crop(view, cropExtent)

view[is.na(provsR)]=NA

names(view)=gsub(".tif","",selectSet,fixed=T)

selectNs = c("I","E","MH","BH")

plotStack = subset(view,paste0(selectNs,the_scale))
pdf(paste0("outputs/figures","/fig5MapsRaw",selectTerm,"std",doStandardization,".pdf"),
    width=8,height=3)
par(mar=c(0,0,0,0), oma=c(0,0,0,0))
levelplot(plotStack,xlab=NULL,ylab="raw metric values",scales=list(draw=FALSE),
          maxpixels = 2e5,col.regions=hcl.colors(255,palette=pal))
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

plotStack = subset(cc,paste0(selectNs,the_scale))
plotStack[]
pdf(paste0("outputs/figures","/fig5Maps",selectTerm,"std",doStandardization,".pdf"),
    width=8,height=3)
if(doStandardization){
  xlab = "ratio percentiles"
}else{
  xlab="raw percentiles"
}
par(mar=c(0,0,0,0), oma=c(0,0,0,0))
levelplot(plotStack,ylab=xlab,xlab=NULL,scales=list(draw=FALSE),maxpixels = 2e5,
          col.regions=hcl.colors(255,palette=pal))
dev.off()


if(doSDs){
  # -------------------------------------------------------------------------
  var = calc(cc, sd)
  writeRaster(var,paste0(resultDir,"/derived/sd",selectTerm,"std",doStandardization,".tif"),overwrite=T)
  
  middle = calc(cc,mean)
  writeRaster(middle,paste0(resultDir,"/derived/mean",selectTerm,"std",doStandardization,".tif"),overwrite=T)
  rm(cc);rm(view)
  removeTmpFiles(h=0.1)
  
  # -------------------------------------------------------------------------
  
  outSet =   list.files(paste0(resultDir,"/derived"))
  outSet=outSet[grepl(as.character(the_scale),outSet,fixed=T)]
  sdSet =     outSet[grepl("sd",outSet,fixed=T)&grepl(paste0("std",doStandardization),outSet,fixed=T)]
  meanSet =     outSet[grepl("mean",outSet,fixed=T)&grepl(paste0("std",doStandardization),outSet,fixed=T)]
  
  sds = stack(paste0(resultDir,"/derived/",sdSet))
  names(sds)=sdSet
  plot(sds,col=rev(hcl.colors(255,palette="RdYlBu")))
  plot(st_geometry(zonesS),add=T)
  
  hcl.pals()
  means = stack(paste0(resultDir,"/derived/",meanSet))
  names(means)=meanSet
  
  cStack = stack(H,Anthro,means[[1]],sds[[1]])
  names(cStack)=c("Quality","Human Footprint","mean","sd")
  cStack[is.na(provsR)]=NA
  
  size = 4
  widthS = 0.9
  pdf(paste0(gsub("rasters","figures",resultDir,fixed=T),"/fig4sd",the_scale,"std",doStandardization,".pdf"),
      width=size*widthS,height=size)
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(sds[[1]],axes=F,horizontal=T,nr=1,col=rev(hcl.colors(255,palette=pal)))
  plot(st_geometry(zonesS),add=T)
  dev.off()
  
  pdf(paste0(gsub("rasters","figures",resultDir,fixed=T),"/fig4sd",the_scale,"mean",doStandardization,".pdf"),
      width=size*widthS,height=size)
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(means[[1]],axes=F,horizontal=T,nr=1,col=hcl.colors(255,palette=pal))
  plot(st_geometry(zonesS),add=T)
  dev.off()
  
  pdf(paste0(gsub("rasters","figures",resultDir,fixed=T),"/fig4quality",the_scale,"mean",doStandardization,".pdf"),
      width=size*widthS,height=size)
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(cStack[[1]],axes=F,horizontal=T,nr=1,col=hcl.colors(255,palette="Reds"))
  plot(st_geometry(zonesS),add=T)
  dev.off()
  
  pdf(paste0(gsub("rasters","figures",resultDir,fixed=T),"/fig4footprint",the_scale,"mean",doStandardization,".pdf"),
      width=size*widthS,height=size)
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(cStack[[2]],axes=F,horizontal=T,nr=1,col=rev(hcl.colors(255,palette="Reds")))
  plot(st_geometry(zonesS),add=T)
  dev.off()
}
removeTmpFiles(h=0.1)

# -------------------------------------------------------------------------
