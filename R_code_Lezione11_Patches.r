#Patches 

setwd("~/Desktop/lab")

library(raster)

d1c <- raster("d1c.tif")
d2c <- raster("d2c.tif")

par(mfrow=c(1,2))
plot(d1c,col=cl1)
plot(d2c,col=cl1)

cl1 <- colorRampPalette(c("black","green"))(100)
dev.off()


#con reclassify andiamo a riclassificare l'immagine raster riassegnando valori 

d1c.for <- reclassify(d1c,cbind(1,NA))

par(mfrow=c(1,2))
plot(d1c.for,col=cl1)
plot(d1c,col=cl1)


d2c.for <- reclassify(d2c,cbind(1,NA))

par(mfrow=c(1,2))
plot(d1c.for,col=cl1)
plot(d2c.for,col=cl1)


d1c.for.patches <- clump(d1c.for)
d2c.for.patches <- clump(d2c.for)

writeRaster(d1c.for.patches, "d1c.for.patches.tif")
writeRaster(d1c.for.patches, "d2c.for.patches.tif")


par(mfrow=c(1,2))
plot(d1c.for.patches,col=clp)
plot(d2c.for.patches,col=clp)

clp <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100)

d1c.for.patches
d2c.for.patches


np1=301
np2=1212

time <- c("Before deforestation","After deforestation")
npatches <- c(301,1212)

output <- data.frame(time,npatches)
attach(output)

library(ggplot2)
ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")
