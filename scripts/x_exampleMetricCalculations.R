library(LSTDConnect)
library(pfocal)
library(raster)

#Example H
H <- LSTDConnect::ghm/100 
res(H) #1km resolution
plot(H)

#SAMC B high 5km example
mortality <- (1-H)*(2/30-0.00001)+0.00001
resistance <- (1-H)*(20-1)+1
t <- 28
samc_obj_custom <- samc(resistance,
                              absorption = mortality,
                              directions = 8)

Bh5 <- distribution(time = t, samc = samc_obj_custom, occ = H)
plot(Bh5$occ)

#Exponential 5km example
displacement <- 5  
k <- exponentialKernel(displacement, negligible = 10^-6)
E5 <- pfocal(H, kernel = k)
plot(E5)

#Intactness 5km example
displacement <- 5  
k <- exponentialKernel(displacement, negligible = 10^-6)
occ_raster_z <- H ^ 0.5
I5 <- pfocal(occ_raster_z,na_flag = 0,kernel = k) * occ_raster_z
plot(I5)

view = stack(H,Bh5$occ,E5,I5)
names(view)=c("H","Bh5","E5","I5")
plot(view)


