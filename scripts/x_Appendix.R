library(LSTDConnect)
library(pfocal)
libraryr(raster)

baseDir <- "C:/Users/HughesJo/Documents/InitialWork/Connectivity/ConnectivityMetricsForMonitoring/IALEiposter/KernelVisualizations"
paID <- 4025
inDir <- paste0(baseDir,"/pa",paID)
H <- raster(paste0(inDir,"/newQuality.tif"))/100 
#TO DO: add small example raster to LSTDConnect package

#SAMC B high 5km example
mortality <- (1-H)*(2/30-0.00001)+0.00001
resistance <- (1-H)*(20-1)+1
t <- 28
samc_obj_custom <- samc(data = as.matrix(resistance),
                        absorption = as.matrix(mortality),
                        directions = 8)
Bh5 <- LSTDConnect::distribution(time = t, samc = samc_obj_custom, occ = as.matrix(H*100))

Bh5 <- raster(Bh5$population[[1]], template = H)

#Exponential 5km example
displacement <- 5*1000/res(H)[1]  
k <- exponentialKernel(displacement, negligible = 10^-6)
E5 <- pfocal(as.matrix(H*100), kernel = k)
E5 <- raster(E5, template = H)

#Intactness 5km example
displacement <- 5*1000/res(H)[1]  
k <- exponentialKernel(displacement, negligible = 10^-6)
occ_raster_z <- as.matrix(H*100) ^ 0.5
I5 <- pfocal(occ_raster_z,na_flag = 0,kernel = k) * occ_raster_z
I5 <- raster(I5,template=H)

view = stack(H,Bh5,E5,I5)
names(view)=c("H","Bh5","E5","I5")
plot(view)
