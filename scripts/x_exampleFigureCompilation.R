library(raster)
library(plot.matrix)
library(RColorBrewer)
library(imager)
library(tidyverse)

pal = 'RdYlBu'

outDir = "outputs/KernelVisualizations"

exQuality = raster(paste0(outDir,"/components/exQuality.tif"))
exQualityP = raster(paste0(outDir,"/components/exQualityP.tif"))
paBoundR= raster(paste0(outDir,"/components/paBoundR.tif"))

paBound=rasterToPolygons(paBoundR,dissolve=T)
doPlots=read.csv(paste0(outDir,"/exampleInput.csv"))

#############################
#kernel visualization plots 1 and S1
doSet = subset(doPlots,scale==20)
reorder = c("U","E","PN","NL","CL","ML","BL","PL","CH","MH","BH","PH")
doSet$shape = factor(as.character(doSet$shape),levels=reorder)
doSet = doSet[order(doSet$shape),]

gg = data.frame(m=1,scale=1,shape="a",x=1,y=1,rgb.val=1)
gg=subset(gg,m==2)
for(i in 1:nrow(doSet)){
  #i = 12
  print(i)
  dbit = doSet[i,]

  m = paste0(dbit$scale,dbit$shape)

  oName = paste0(dbit$scale," km ",dbit$shape)

  kvis=raster(paste(paste0(outDir,"/components/kernel",m,".tif")))

  png(paste0(outDir,"/components/mapSamples",m,".png"), width=400, height=400)
  par(mai=c(0,0,0,0),mar=c(0,0,0,0),oma=c(0,0,0,0),omi=c(0,0,0,0))
  plot(exQualityP,axes=FALSE,box=FALSE,legend=FALSE, frame=FALSE, asp = "", xpd = NA,col=hcl.colors(255,palette="Reds"))
  plot(kvis,col=brewer.pal(n=9, name = "Blues"),add=T,axes=FALSE,box=FALSE,legend=FALSE, frame=FALSE, asp = "", xpd = NA)
  dev.off()

  ii= load.image(paste0(outDir,"/components/mapSamples",m,".png"))

  dd  = as.data.frame(ii,wide="c") %>% mutate(rgb.val=rgb(c.1,c.2,c.3))
  dd=subset(dd,select=c(x,y,rgb.val))
  dd$m = dbit$id
  dd$scale = paste0(dbit$scale," km")
  dd$shape = dbit$shape
  gg=rbind(gg,dd)
}

png(paste0(outDir,"/legendQuality.png"), width=2, height=1,units="in",res=300)
par(mai=c(0,0,0,0),mar=c(0,0,0,0),oma=c(0,0,0,0),omi=c(0,0,0,0))
plot(exQualityP,legend.only=TRUE,horizontal = TRUE,legend.args = list(text='Quality'),col=hcl.colors(255,palette="Reds"))
dev.off()

png(paste0(outDir,"/legendKernel.png"), width=2, height=1,units="in",res=300)
par(mai=c(0,0,0,0),mar=c(0,0,0,0),oma=c(0,0,0,0),omi=c(0,0,0,0))
plot(kvis*100,col=brewer.pal(n=9, name = "Blues"),legend.only=TRUE,horizontal = TRUE,legend.args = list(text = 'Disperser Density'))
dev.off()

gg=subset(gg,x<=340)

theme_set(theme_bw())
str(gg)
levels(gg$shape)

gg$type=gg$shape
levels(gg$type)=c("Uniform","Exponential","PARC","SAMC","SAMC R","SAMC M","SAMC B","PARC P","SAMC R","SAMC M","SAMC B","PARC P")
gg$intensity="low"
gg$intensity[is.element(gg$shape,c("CH","MH","BH","PH"))]="high"


#Figure 1
gga = subset(gg,is.element(shape,c("U","E","PN","NL")))
str(gga)
p <- ggplot(gga,aes(x,y))+geom_raster(aes(fill=rgb.val))+scale_fill_identity()+
  facet_wrap(~type,ncol=4)
p <- p + theme(axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank(),
               axis.title.y=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank()) +
  scale_y_reverse()

#pdf(paste0(outDir,"/Fig1Kernels.pdf"),height=2.5,width=8)
png(paste0(outDir,"/Fig1Kernels.png"),height=2.3,width=8,units="in",res=300)
print(p)
dev.off()

#Figure S1 - variation in kernel shape - uniform mortality/cost/resistance
ggb = subset(gg,!is.element(shape,c("U","E","PN","NL")))
reorder = c("CL","ML","BL","PL","CH","MH","BH","PH")
ggb$shape = factor(as.character(ggb$shape),levels=reorder)
ggb = ggb[order(ggb$shape),]
p <- ggplot(ggb,aes(x,y))+geom_raster(aes(fill=rgb.val))+scale_fill_identity()+
  facet_grid(intensity~type)
p <- p + theme(axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank(),
               axis.title.y=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank()) +
  scale_y_reverse()

png(paste0(outDir,"/FigS1KernelsSupplement.png"),height=4.3,width=8,units="in",res=300)
print(p)
dev.off()

###########################################
#Outcome examples Fig 2 and S2
doSet = subset(doPlots,scale==20)
reorder = c("U","E","PN","NL","CL","ML","BL","PL","CH","MH","BH","PH")
doSet$shape = factor(as.character(doSet$shape),levels=reorder)
doSet = doSet[order(doSet$shape),]

gg = data.frame(m=1,scale=1,shape="a",x=1,y=1,rgb.val=1)
gg=subset(gg,m==2)
cropWidth = 50
for(i in 1:nrow(doSet)){
  #i = 2
  print(i)
  dbit = doSet[i,]
  m = paste0(dbit$scale,dbit$shape)
  oName = paste0(dbit$scale," km ",dbit$shape)

  trMap = raster(paste0(outDir,"/components/Gamma",m,".tif"))
  trMap[1,1]=1;trMap[1,2]=0


  png(paste0(outDir,"/components/Gamma",m,".png"), width=282*1.3, height=290*1.3)
  par(mai=c(0,0,0,0),mar=c(0,0,0,0),oma=c(0,0,0,0),omi=c(0,0,0,0))
  plot(trMap,axes=FALSE,box=FALSE,legend=FALSE, frame=FALSE, asp = "", xpd = NA,col=hcl.colors(255,palette=pal))
  plot(paBound,add=T,border="black",axes=FALSE)
  dev.off()

  ii= load.image(paste0(outDir,"/components/Gamma",m,".png"))
  ii = crop.borders(ii,nPix=50)
  ii = autocrop(ii,"white")

  dd  = as.data.frame(ii,wide="c") %>% mutate(rgb.val=rgb(c.1,c.2,c.3))

  dd=subset(dd,select=c(x,y,rgb.val))
  dd$m = dbit$id
  dd$scale = paste0(dbit$scale," km")
  dd$shape = dbit$shape
  gg=rbind(gg,dd)
}

png(paste0(outDir,"/legendMetric.png"), width=2, height=1,units="in",res=300)
par(mai=c(0,0,0,0),mar=c(0,0,0,0),oma=c(0,0,0,0),omi=c(0,0,0,0))
plot(trMap,legend.only=TRUE,horizontal = TRUE,legend.args = list(text='Metric Value'),col=hcl.colors(255,palette=pal))
dev.off()

theme_set(theme_bw())
levels(gg$shape)

gg$type=gg$shape
levels(gg$type)=c("uniform","exponential","PARC","SAMC","SAMC R","SAMC M","SAMC B","PARC","SAMC R","SAMC M","SAMC B","PARC")
gg$intensity="low"
gg$intensity[is.element(gg$shape,c("CH","MH","BH","PH"))]="high"


#Figure S2 - minimal variation in outcomes with kernel shape. Intactness differs in disturbed areas, but not in PA.
gga = subset(gg,is.element(shape,c("U","E","PN","NL")))
p <- ggplot(gga,aes(x,y))+geom_raster(aes(fill=rgb.val))+scale_fill_identity()+
  facet_wrap(~type,ncol=5)
p <- p + theme(axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank(),
               axis.title.y=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank()) +
  scale_y_reverse()

png(paste0(outDir,"/FigS2NullOutcomes.png"),height=3,width=11,units="in",res=300)
print(p)
dev.off()

#Figure 2: effect of variation in mortality/cost/resistance on outcomes
ggb = subset(gg,!is.element(shape,c("U","E","PN","NL")))
reorder = c("CL","ML","BL","PL","CH","MH","BH","PH")
ggb$shape = factor(as.character(ggb$shape),levels=reorder)
ggb = ggb[order(ggb$shape),]
p <- ggplot(ggb,aes(x,y))+geom_raster(aes(fill=rgb.val))+scale_fill_identity()+
  facet_grid(intensity~type)
p <- p + theme(axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank(),
               axis.title.y=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank()) +
  scale_y_reverse()


png(paste0(outDir,"/Fig2ExampleOutcomes.png"),height=4.5,width=8,units="in",res=300)
print(p)
dev.off()

