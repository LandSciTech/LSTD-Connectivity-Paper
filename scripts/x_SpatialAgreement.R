library(raster)
library(fasterize)
library(sf)
library(rasterVis)


#example layer stack
#source("C:/Users/HughesJo/Documents/gitprojects/LSTD-Connectivity-Paper/scripts/x_Appendix.R")

resultDir = "C:/Users/HughesJo/Documents/gitprojects/LSTD-Connectivity-Paper/outputs/rasters"

makeMaps =T
doStandardization=F

H = raster(paste0(resultDir,"/allCostsLayer.tif"))
N = raster(paste0(resultDir,"/naturalCostsLayer.tif"))

unique(H)

#cropExtent = extent(H, round(0.18*nrow(H)), round(0.83*nrow(H)), round(0.05*ncol(H)), round(0.35*ncol(H))))
cropExtent = extent(H, round(0.48*nrow(H)), round(0.74*nrow(H)), round(0.06*ncol(H)), round(0.23*ncol(H)))
H <- crop(H, cropExtent)
N <- crop(N,cropExtent)
H = 1-H/1000
N = 1-N/1000
Anthro = (1-H)-(1-N)

provs =  getData('GADM', country='CAN', level=1)
provs = provs[is.element(provs@data$NAME_1,c("British Columbia","Alberta")),]
provs= spTransform(provs,crs(Anthro))
provs=st_as_sf(provs)
provsR= fasterize(provs,Anthro)

if(makeMaps){

  layerSet = list.files(paste0(resultDir,"/Can_Cost"))
  
  scales = 20#c(2,5,10,20,40)

  for(s in scales){
    print(s)
    #s=20
    selectTerm = as.character(s)
    selectSet = layerSet[grepl(selectTerm,layerSet,fixed=T)]
    if(s==2){
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
    
    view <- crop(view, cropExtent)
    
    view[is.na(provsR)]=NA
    view[N==0]=NA # only look at agreement/disagreement for places that aren't rock and ice
    
    quants = seq(0,1,length.out=101)#seq(0.25,1,length.out=4)
    
    qTab = quantile(view,probs=quants)
    
    cc = view
    for(j in 1:nrow(qTab)){
      cc[[j]] <- cut(view[[j]], breaks=unique(qTab[j,]))
      
    }
    names(cc)=gsub(".tif","",selectSet,fixed=T)

    plotStack = subset(cc,paste0(c("I","E","MH","BH"),s))
    pdf(paste0(gsub("rasters","figures",resultDir,fixed=T),"/fig5Maps",selectTerm,"std",doStandardization,".pdf"),
        width=8,height=3)
    par(mar=c(0,0,0,0), oma=c(0,0,0,0))
    levelplot(plotStack,xlab=NULL,ylab=NULL,scales=list(draw=FALSE),maxpixels = 2e5)
    #plot(plotStack,axes=F,horizontal=T,nr=1)
    dev.off()
    
    var = calc(cc, sd)
    writeRaster(var,paste0(resultDir,"/derived/sd",selectTerm,"std",doStandardization,".tif"),overwrite=T)
    
    middle = calc(cc,mean)
    writeRaster(middle,paste0(resultDir,"/derived/mean",selectTerm,"std",doStandardization,".tif"),overwrite=T)
    rm(cc);rm(view)
  }
}

outSet =   list.files(paste0(resultDir,"/derived"))
outSet=outSet[grepl("20",outSet,fixed=T)]
sdSet =     outSet[grepl("sd",outSet,fixed=T)&grepl(paste0("std",doStandardization),outSet,fixed=T)]
meanSet =     outSet[grepl("mean",outSet,fixed=T)&grepl(paste0("std",doStandardization),outSet,fixed=T)]

sds = stack(paste0(resultDir,"/derived/",sdSet))
names(sds)=sdSet

means = stack(paste0(resultDir,"/derived/",meanSet))
names(means)=meanSet

cStack = stack(H,Anthro,means[[1]],sds[[1]])
names(cStack)=c("Quality H","Human Footprint","percentile mean","percentile sd")

cStack[is.na(provsR)]=NA
pdf(paste0(gsub("rasters","figures",resultDir,fixed=T),"/fig4Maps20std",doStandardization,".pdf"),
    width=11,height=4)
par(mar=c(0,0,0,0), oma=c(0,0,0,0))
#levelplot(cStack,xlab=NULL,ylab=NULL,scales=list(draw=FALSE),maxpixels = 2e5)
plot(cStack,axes=F,horizontal=T,nr=1)
dev.off()




if(0){
  option="percentile"
  
  for(i in 1:length(quants)){
    #i=1
    outStack=view
    cLevels = qTab[,i]
    for(j in 1:length(cLevels)){
      #j=1
      
      
      outStack[[j]][!is.na(view[[j]])]=1
      if(option=="highest"){
        outStack[[j]][view[[j]]< cLevels[j]]=0
        cName=paste(option, (1-quants[i])*100,"percent")
      }
      if(option=="lowest"){
        outStack[[j]][view[[j]]>= cLevels[j]]=0
        cName=paste(option, quants[i]*100,"percent")
      }
      if(option=="percentile"){
        outStack[[j]][view[[j]]>= cLevels[j]]=0
        if(i>1){
          outStack[[j]][view[[j]]< oLevels[j]]=0
        }
        cName=paste(quants[i]*100,"percentile")
      }
    }
    oLevels=cLevels
    
    rs1 <- calc(outStack, sum)
    names(rs1)=cName
    
    if(i==1){
      res = stack(rs1)
    }else{
      res = addLayer(res,rs1)
    }
  }
  
  plot(res)
}