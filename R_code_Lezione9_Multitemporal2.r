#nuva analisi multitemporale, analizziamo le eemissioni di ossidi di azoto fra gennaio e marzo 2020

setwd("~/Desktop/lab")

#andremo ad usare :
#libreria raster 
library(raster)

#carichiamo le nostre immagini in forato .png
EN01 <- raster("EN_0001.png")

#visualizziamo con plot 
plot(EN01)

#exercise: carichiamo tutte le immagini

EN02 <- raster("EN_0002.png")
EN03 <- raster("EN_0003.png")
EN04<-raster("EN_0004.png")
EN05<-raster("EN_0005.png")
EN06<-raster("EN_0006.png")
EN07<-raster("EN_0007.png")
EN08<-raster("EN_0008.png")
EN09<-raster("EN_0009.png")
EN10<-raster("EN_0010.png")
EN11<-raster("EN_0011.png")
EN12<-raster("EN_0012.png")
EN13<-raster("EN_0013.png")


cl1 <- colorRampPalette(c("purple","pink","light pink"))(100)

plot(EN01,col=cl1)
plot(EN13,col=cl1)

par(mfrow=c(1,2))

plot(EN01,col=cl1)
plot(EN13,col=cl1)

dev.off()
#si nota come la quantità di monossido di azoto sia diminuita nell'arco del tempo.


#differenza EN13-EN01

cl2 <- colorRampPalette(c("blue","light blue","turquoise"))(100)

difno2 <- EN13-EN01

plot(difno2,col=cl2)

#plottiamo tutte le immagini assieme
par(mfrow=c(4,4))

p1<-plot(EN01,col=cl1)
p2<-plot(EN02,col=cl1)
p3<-plot(EN03,col=cl1)
p4<-plot(EN04,col=cl1)
p5<-plot(EN05,col=cl1)
p6<-plot(EN06,col=cl1)
p7<-plot(EN07,col=cl1)
p8<-plot(EN08,col=cl1)
p9<-plot(EN09,col=cl1)
p10<-plot(EN10,col=cl1)
p11<-plot(EN11,col=cl1)
p12<-plot(EN12,col=cl1)
p13<-plot(EN13,col=cl1)

dev.off()

#pt 2 

#adiamo a sistemare il grafico in maniera che sia a scala uguale 
library(ggplot2)

cover <- c("Agriculture","Forest")
before <- c(10.9,89.1)
after <- c(48.2,51.8)



output <- data.frame(cover,before,after)
output

library(gridExtra) 
# oppure: require(Extra)

grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) +
  geom_bar(stat="identity", fill="white")

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) +
  geom_bar(stat="identity", fill="white")

grid.arrange(grafico1, grafico2, nrow = 1)

grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + 
  geom_bar(stat="identity", fill="white") +
  ylim(0, 100)


grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + 
  geom_bar(stat="identity", fill="white") +
  ylim(0, 100)


# Exercise: use grid.arrange to plot the two graphs, impostiamo la stessa scala
grid.arrange(grafico1, grafico2, nrow = 1)

#per caricare tutti i dati con una stessa estensione si deve fare 
setwd("~/Desktop/lab/esa_no2")

#abbiamo bisogno della libreria raster
library(raster)

#carichiamo i file di tipo .png
rlist <- list.files(pattern = ".png")

#con la funzione lapply ci fa caricare i dati 
listafinale <- lapply(rlist, raster)

EN <- stack(listafinale)

plot(EN, col=cl1)

#facciamo la differenza tra la prima immagine e la tredicesima 

difEN <- EN$EN_0013 - EN$EN_0001

cl2 <- colorRampPalette(c('blue','white','red'))(100)

plot(difEN,col=cl2)

#facciamo un boxplot orizzontale

boxplot(EN, horizontal=T,outline=F,axes=T)

