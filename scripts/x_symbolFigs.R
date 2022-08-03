#make components for symbols in paper

library(raster)
bit =raster(t(matrix(runif(25,min=0.9,max=1),nrow=5,ncol=5)))
bit[2,3]=0
bit[4,3]=0.3
cpal = 'RdYlBu'

plot(bit,axes=F,horizontal=T,nr=1,col=hcl.colors(255,palette=cpal),legend=F)

plot(bit,axes=F,horizontal=T,nr=1,col=hcl.colors(255,palette=cpal)[155:255],legend=F)

paBoundR = raster(paste0(inDir,"/newPA.tif"))
paBound = rasterToPolygons(paBoundR,dissolve=T)

ek= exponentialKernel(5,negligible = 10^-6)
nrow(ek)
h = (nrow(ek)-1)/2
plot(ek[h+1,]~seq(-h,h),type="l",xlab="",ylab="",xaxt='n',yaxt='n',lwd=4)

ik = ek;ik[h+1,h+1]=0.3*ik[h+1,h+1]
plot(ik[h+1,]~seq(-h,h),type="l",xlab="",ylab="",xaxt='n',yaxt='n',lwd=4)

uk = uniformKernel(mu=5)
h = (nrow(uk)-1)/2
plot(uk[h+1,]~seq(-h,h),type="l",xlab="",ylab="",xaxt='n',yaxt='n',lwd=4)
