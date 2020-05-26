#Analisi copertura Neve

#settiamo la directory
setwd("~/Desktop/lab")

#i pacchetti necessari all'analisi di oggi sono questi due, se non si hanno installati usare il comando install.packages("")
library(ncdf4)
library(raster)

#Dal sito copernicus scarichiamo le immagini che ci interessano per la nostra analisi: una volta scaricate andiamo a caricarle su R 

snowmay<- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

cl1 <- colorRampPalette(c("dark blue", "blue","light blue"))(100)

#exercise plot snow con color 

plot(snowmay,col=cl1)

#importiamo i fati sulla neve, creiamo una nuova cartella nella directory nella quale inseriamo i le immagini di interesse e successivamente creiamo un ciclo
#per riuscire a caricarle in blocco

setwd("~/Desktop/lab/snow")

library(raster)

#carichiamo i file di tipo .tif
rlist <- list.files(pattern = ".tif")

#con la funzione lapply ci fa caricare i dati 
listafinale <- lapply(rlist,raster)

snow.multitemp <- stack(listafinale)

plot(SN, col=cl1)

par(mfrow=c(1,2))
plot(SN$snow2000r,col=cl1)
plot(SN$snow2020r,col=cl1)

dev.off() 
#facciamo una previsione del 2025

difsnow = SN$snow2020r - SN$snow2000r
cldiff <- colorRampPalette(c('blue','white','red'))(100) 
plot(difsnow, col=cldiff)


#previsione per il 2025, per semplificare la procedura scarichiamo prediction.r da iol
source("prediction.r")

predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")

plot(predicted.snow.2025.norm,col=cl1)
