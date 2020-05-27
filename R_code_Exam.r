#R_code_Exam.r

###1 R code Lezione 1

#installiamo la nuova libreria tramite i pacchetti; se usiamo R invece di R-studio useremo il comando 

install.packages("sp")
#usiamo il comando Library per chiamare una libreria dati;
library(sp) 

#usiamo il comando data per chiamare i dati contenuti nella libreria;
data("meuse")

#vediamo la tabella dei dati meuse;
meuse

#vediamo solo le prime 6 righe della tabella;
head(meuse) #vediamo i primi 6

#con questo comando andremo a vedere i nomi delle variabili contenute all'interno della tabella;
names(meuse)

#il comando sotto citato ci andrà a visualizzare gli indici statistici più rilevanti prendendo in considerazione tutti i dati;
summary(meuse)

#con pairs si crea un grafico che mette in correlazione le variabili dei dati;
pairs(meuse)

#il simbolo messo prima di cadmium ci fa mettere in correlazione solamente le variabili da noi specificate, impostando i dati uguali a meuse
pairs(~cadmium+copper+lead,data=meuse)

pairs(~cadmium+copper+lead+zinc,data = meuse)

#essendo i dati interessati relativamente al 3,4,5 e 6 riga della colonna un metodo alternativo è questo;
pairs(meuse[,3:6])

#aggiungendo col="red", cambierà il colore di visualizzazioine del grafico;
pairs(meuse[,3:6],col="red")

#aggiungendo main, daremo il comando di impostare un titolo al grafico;
pairs(meuse[,3:6],col="purple",pch=19,cex=3, main="Elementi presenti")

pairs(~cadmium+copper+lead+zinc+elev,data = meuse) #volendo si può variare da [,3:7];


#prendiamo delle funzioni esterne;
panel.correlations <- function(x, y, digits=1, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  
  
  
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * r)
}



panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                             cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
          col = 1, ...)
}




panel.histograms <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}


pairs(~cadmium+copper+lead+zinc+elev,data = meuse) 

#una volte inviate le tre funzioni esterne avremo la possibilità di creare dei grafici diversi ed esteticamente più belli;
pairs(meuse[,3:6],lower.panel = panel.correlations,upper.panel = panel.smoothing,diag.panel = panel.histograms)

#exercise: varia il pannello superiore ed inferiore invertendoli
pairs(meuse[,3:6],lower.panel = panel.smoothing,upper.panel = panel.correlations,diag.panel = panel.histograms)

library(sp)
data("meuse")
head(meuse)

#creiamo un grafico prendendo le colonne di interesse usando il comando meuse$... ;
plot(meuse$cadmium,meuse$copper, main="cadmio e rame correlati")

#mettiamo in evidenza il nostro pacchetto di dati così da non dover scrivere ogni volta meuse;
attach(meuse)

#una volta messo in evidenza meuse possiamo evitare di scriverlo ogni volta così da andare a semplificare lo script. 
plot(cadmium,copper, main="Cadmio e Rame",pch=24,col="purple",xlab="Cadmio",ylab="Rame",cex=2)


######################################################################################################################################

###R code spatial

#chiamiamo il pacchetto sp
library(sp)

#usiamo il comando data per aprire il nostro dataframe
data(meuse)

#con head vediamo i primi 6 
head(meuse)

#usiamo attach per fissare il dataframe
attach(meuse)

#facciamo un plot per mettere in correlazione cadmio e piombo
plot(cadmium,lead,main="Relazione cadmio e piombo",col="red",pch=18,xlab="Cadmio",ylab="Piombo")

#ecercise: creare un plot con rame e zinco
plot(copper,zinc,main="Relazione tra Rame e Zinco",col="green",pch=17,xlab="Rame",ylab="Zinco")

#per mostrare i grafici assieme usiamo : multiframe o multipanel
par(mfrow=c(1,2))
plot(cadmium,lead,main="Relazione cadmio e piombo",col="red",pch=18,xlab="Cadmio",ylab="Piombo")
plot(copper,zinc,main="Relazione tra Rame e Zinco",col="green",pch=17,xlab="Rame",ylab="Zinco")
#se cambiamo i valori di mfrow in 2,1 avremo una variazione nella visualizzazione del grafico

#installiamo tramite packages, install, GGally o esternamente con install.packages("GGally"), una volta installata chiamiamo la libreria
library(GGally)

#ggpairs(meuse) fa il grafico con tutte le variabili, usiamo ggpairs(meuse[,3:6]) per prendere solo le variabili di interesse
ggpairs(meuse[,3:6])

#SPATIAL
#spieghiamo ad R che nei dati sono presenti coordinate
coordinates(meuse)=~x+y

#vediamo il grafico con:
plot(meuse)

#per creare il grafico di tipo spaziale si usa questa funzione:
spplot(meuse,"zinc")
# il plot che è uscito è di tipo spaziale i punti gialli ci vanno ad indicare le zone più inquinate, 
# stiamo analizzando le zone vicine ad un fiume.

######################################################################################################################################

###Rcode spatial 2

#Richiamiamo la libreria sp, se non è stata installata usare install.packages("sp");
library(sp)

#Carichiamo i nostri dati con il comando:
data(meuse)

#fissiamo il dataframe con il comando:
attach(meuse)

#mostriamo le prime 6 righe della tabella con il comando:
head(meuse)
#in alternativa si può usare il comando names(meuse)

#andiamo a specificare la presenza di coordinate
coordinates(meuse)=~x+y

#creiamo un sp plot prendendo in considerazione lo zinco 
spplot(meuse,"zinc")

#exercise: creare un sp-plot del rame
spplot(meuse,"copper")

#la funzione bubble varia il grafico
bubble(meuse,"zinc")

#exercise: cambiamo il colore del grafico e lo facciamo mettendo in evidenza il Rame 
bubble(meuse,"copper",col = "orange",main = "Indice spaziale Rame")

#creiamo un nuovo oggetto con dei dati inventati

foram <- c(10,20,35,55,67,80)
carbon <- c(5, 15, 30, 70, 85, 99)

#facciamo un plot con i dati inventati
plot(carbon,foram,col="purple",cex=2,pch=17)

#scarichiamo un nuovo pacchetto dati, e lo installiamo seguendo il percorso file, import dataset, from text(base), e scegliamo il file di interesse se usiamo Rstudio
#su R si usa questo comando se siamo utenti mac
setwd("/Users/Name/Desktop/nome cartella")
#siccome la prima riga non è di dati ma di testo si usa questo comando: ATTENZIONE se si usa R studio questo passaggio lo fa in automatico
covid <- read.table("covid_agg.csv",header = T)


###########################################################################################################################################

###Rcode pointpattern

#impostiamo una working directory
setwd("/Users/Pierluigi/ecologia del paesaggio")

#semplifichiamo il nome
covid <- covid_agg

#per chi ha la tabella senza intestazione si userà il comando
covid <- read.table("covid_agg.csv",header = T)

#chiamiamo le prime sei righe della tabella
head(covid)

#creiamo un plot per iniziare a vedere la disposizione
plot(covid$country,covid$cases)

#con las vediamo i lables, 
#con las=0 rimangono parallele
#con las=1 cambia le etichette in orizzontale 
#con las =2 le lables sono perpendicolari all'asse 
#con las=3 le lables sono tutte verticali
plot(covid$country,covid$cases,las=0)
plot(covid$country,covid$cases,las=1)
plot(covid$country,covid$cases,las=2)
plot(covid$country,covid$cases,las=3)

#installiamo e poi chiamiamo la libreria ggplot2 
library(ggplot2)

#chiamiamo i dati nella libreria
data(mpg)

#mostriamo le prime sei righe della tabella
head(mpg)

#creiamo un plot del nuovo dataset mgp, modifichiamo l'estetica e aggiungiamo una geometria
ggplot(mpg,aes(x=displ,y=hwy))+geom_point()

#scarichiamo e installiamo la nuova libreria spatstat
library(spatstat)
attach(covid)

#tramite la funzione ppp ci creiamo un nuovo dataset che ci interesserà per l'analis spaziale
covids <- ppp(lon,lat,c(-180,180),c(-90,90))

#semplifichiamo la densità chiamandola d
d <- density(covids)

#facciamo un plot di d
plot(d)

#aggiungiamo i punti al plot
points(covids)

#salviamo il file in .Rdata
#richiamiamo la nostra WD e successivamente carichiamo il salvataggio che abbiamo effettuato precedentemente

#cambiamo i colori, palette, numero di livelli si specifica con una parentesi esterna, per usare questa funzione usiamo la libreria spatstat
cl <- colorRampPalette(c("yellow","orange","red"))(100)
plot(d,col=cl)

#exercise: cambiamo i colori del grafico in blu e verde
cl2 <- colorRampPalette(c("green","turquoise","blue"))(100)
plot(d,col=cl2)

#per completare la mappa aggiungiamo i bordi dei vari paesi; usiamo la libreria rgdal, scarichiamo un nuovo pacchetto di dati delle coste in vettore
library(rgdal)
coastlines <- readOGR("ne_10m_coastline.shp")

#per avere il nostro grafico completo useremo questo comando;
plot(coastlines, add=T)


######################################################################################################################################

#TELERILEVAMENTO 

#English version below

#scarichiamo 2 librerie che ci serviranno per lavorare con i nostri dati

install.packages("raster")

install.packages("RStoolbox")
yes

#chiamiamo la prima libreria che abbiamo scaricato
library(raster)

#scarichiamo un pacchetto di dati e rinominiamo un file in particolare che ci servirà per il nostro lavoro
p224r63_2011 <- brick("p224r63_2011_masked.grd")

#facciamo un plot della nostra immagine satellitare
plot(p224r63_2011)

# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared

#per cambiare la scala cromatica si usa questo comando:
cl <- colorRampPalette(c("black","grey","light grey"))(100) 

#avendo impostato questi valori mettiamo nel nostro comando col="cl"
plot(p224r63_2011,col=cl)

#modifichiamo le scale cormatiche per vedere cosa succede
cllow <- colorRampPalette(c("black","grey","light grey"))(5) 

#vediamo cosa cambia nel nostro grafico
plot(p224r63_2011,col=cllow)

names(p224r63_2011)
#[1] "B1_sre" "B2_sre" "B3_sre" "B4_sre" "B5_sre" "B6_bt"  "B7_sre"

#cambiamo in scala di blu la banda blu
clb <- colorRampPalette(c("dark blue","blue","light blue"))(100)

#attach data frame non funziona con raster 
#per mettere solo la banda del blu all'interno del plot
plot(p224r63_2011$B1_sre,col=clb)

#facciamo lo stesso per gli altri colori
#verde
clg <- colorRampPalette(c("dark green","green","light green"))(100)
plot(p224r63_2011$B2_sre,col=colg)

#exercise: fai infrarosso vicino con la scala cromatica da rosso a giallo 
clir <- colorRampPalette(c("purple","pink","light pink"))(100)
plot(p224r63_2011$B4_sre,col=clir)

#plot con tutte e 4 le bande multiframe
par(mfrow=c(2,2))

#blue
clb <- colorRampPalette(c("dark blue","blue","light blue"))(100)
plot(p224r63_2011$B1_sre,col=clb)

#green
clg <- colorRampPalette(c("dark green","green","light green"))(100)
plot(p224r63_2011$B2_sre,col=clg)

#red
clr <- colorRampPalette(c("red","orange","yellow"))(100)
plot(p224r63_2011$B3_sre,col=clr)

#infrared
clir <- colorRampPalette(c("purple","pink","light pink"))(100)
plot(p224r63_2011$B4_sre,col=clir)

#con dev.off() chiudiamo le immagini plottate

#colori naturali, 3 componenti R G B, creiamo un plot con questa gamma di colore 
# 3 bande alla volta R= red G= green B= blue
#la funzione che useremo sarà:

plotRGB(p224r63_2011,r=3,g=2,b=1)

#essendo il risultato nero, usiamo la funzione stretch="lin"

plotRGB(p224r63_2011,r=3,g=2,b=1, stretch="lin")

#per visualizzare meglio il nostro grafico risultante cambiamo le bande inserendo l'infrarosso 

plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="lin")

#le piante essendo riflettenti saranno in rosso, il celeste indica le zone agricola non coltivata, le zona rosa sono zone agricole coltivate

#vediamo i 2 grafici ottenuti usando la funzione multiframe
par(mfrow=c(2,1))

plotRGB(p224r63_2011,r=3,g=2,b=1, stretch="lin")
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="lin")


#per salvarlo in pdf
pdf("telerilevamento-1.pdf")
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="lin")
dev.off()

par(mfrow=c(2,1))

plotRGB(p224r63_2011,r=3,g=2,b=1, stretch="lin")
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="lin")
dev.off()

# exercise: infrarosso lo montiamo in alre componenti green
plotRGB(p224r63_2011,r=3,g=4,b=2, stretch="lin")

#infrarosso nella componente blu:
plotRGB(p224r63_2011,r=3,g=2,b=4, stretch="lin")

#PT.2

#per riuscire ad usare le funzioni di telerilevamnento bisogna usare la libreria raster;
library(raster)

#settiamo la working directory
setwd("~/Desktop/lab")

#questa volta useremo un altra immagine per il nostro telerilevamento
#NB. abbiamo messo nella directory un set di dati che andremo ad usare durante tutto il nostro lavoro su R
#applichiamo la funzione brick per riuscire a rinominare e portare in R la nostra immagine di interesse

p224r63_1988 <- brick("p224r63_1988_masked.grd")

#ci creiamo un plot dell'oggetto del 1988
#il satellite lensat del 1988 vediamo le bande del blu, rosso, verde, e infrarosso.
# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared

plot(p224r63_1988)

#per vedere i nomi di interesse
names(p224r63_1988)

#per vedere i nostri grafici tutti assieme usiamo il multiframe
par(mfrow=c(2,2))

#blue
clb <- colorRampPalette(c("dark blue","blue","light blue"))(100)
plot(p224r63_1988$B1_sre,col=clb)

#green
clg <- colorRampPalette(c("dark green","green","light green"))(100)
plot(p224r63_1988$B2_sre,col=clg)

#red
clr <- colorRampPalette(c("red","orange","yellow"))(100)
plot(p224r63_1988$B3_sre,col=clr)

#infrared
clir <- colorRampPalette(c("purple","pink","light pink"))(100)
plot(p224r63_1988$B4_sre,col=clir)

#per chiudere la finestra attuale useremo:

dev.off()

#creiamo un plot RGB in True colours o natural

plotRGB(p224r63_1988,r=3,g=2,b=1,stretch="Lin")

#essendo il grafico non comprensibile useremo la scala cromatica 

plotRGB(p224r63_1988,r=3,g=2,b=1,stretch="Lin")

#exercise: creiamo il plot con la componente infrarosso nella componente rossa;
#in questo caso i colori vengono definiti False colours;

plotRGB(p224r63_1988,r=4,g=3,b=2,stretch="Lin")

#vediamo come è cambiata nel corso del tempo la nostra immagine mettendo a confronto il 2011 con il 1988;

par(mfrow=c(2,1))
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_1988,r=4,g=3,b=2,stretch="Lin")

dev.off()
#l'agricolo è molto più sviluppata nel 2011
#infrarosso vicino sono le piante
#le zolle di terra sono bianche o celeste

#possiamo calcolare l'indice di salute della vegetazione, ricordiamo che è possibile in quanto le foglie sane riescono a riflettere l'infrarosso 
#DVI(Difference Vegetation Index) sarà l'indice che useremo noi; 

#DVI=NIR-RED, avremo dei risultati diversi in base alla salute delle piante; 
#sana = NIR alto
#Malata = RED alto 

#div1988=nir1988-red1988
#per legare si usa il simbolo $

dvi1988 <- p224r63_1988$B4_sre-p224r63_1988$B3_sre

#vediamo il plot del DVI
plot(dvi1988)

#facciamo lo stesso per quello del 2011

dvi2011 <- p224r63_2011$B4_sre-p224r63_2011$B3_sre
plot(dvi2011)

par(mfrow=c(2,1))
 #Per cambiare colore si userà il comando color ramp palette
cldvi11 <- colorRampPalette(c('dark green','green','light green'))(100)
plot(dvi2011,col=cldvi11)

#adesso abbiamo i DVI dei 2 anni, se facciamo la differenza tra i 2 anni vedremo quanto è stato il cambiamento della vegetazione, se il valore del 2011 è negativo vuol dire che la vegetazione stava meglio nel 1988
#multitemporal analysis
dfdvi <- dvi2011-dvi1988


# creiamo l' immagine che ci mostra le zone dove le piante sono state in maggiore stress
plot(dfdvi)
cldfdvi <- colorRampPalette(c('red','white','blue'))(100)
plot(dfdvi,col=cldfdvi)

#visualizzare tutti i grafici assieme usando un multiframe di immagine di 1988. 2011 e differenza di indice 
par(mfrow=c(3,1))

plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_1988,r=4,g=3,b=2,stretch="Lin")
plot(dfdvi,col=cldfdvi)

dev.off()

#cambiare la risoluzione di un immagine, la funione che si usa è aggregate
p224r63_2011lr <- aggregate(p224r63_2011,fact=10)
# se fact o factor viene settato uguale a 10 usiamo una scala 10 volte maggiore 

#vediamo le caratteriestiche dell'immagine originale
p224r63_2011

#vediamo le caratteristiche della nuova immagine
p224r63_2011lr

#mettiamo i 2 grafici a confronto;
par(mfrow=c(2,1))
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_2011lr,r=4,g=3,b=2,stretch="Lin")
dev.off()

#cambiamo il fattore in 50
p224r63_2011lr2 <- aggregate(p224r63_2011,fact=50)
#visualizziamo le informazioni dell'immagine
p224r63_2011lr2

#creiamo il nostro grafico mettendoli sempre in correlazione;
par(mfrow=c(2,1))
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_2011lr2,r=4,g=3,b=2,stretch="Lin")
dev.off()

#facciamo un dvi della nuova immagine 2011
dvi2011lr50 <- p224r63_2011lr2$B4_sre - p224r63_2011lr2$B3_sre
plot(dvi2011lr50)

#diminuiamo la risoluzione del 1988
p224r63_1988lr2 <- aggregate(p224r63_1988,fact=50)

#facciamo un dvi della nuava immagine 1988
dvi1988lr50 <- p224r63_1988lr2$B4_sre - p224r63_1988lr2$B3_sre
plot(dvi1988lr50)

#facciamo la differenza dei DVI dei due anni a bassa risoluzione
dfdvilr <- dvi2011lr50-dvi1988lr50

#creiamo la nostra immagine:
plot(dfdvilr,col=cldfdvi)

#multiframe del totale 
par(mfrow=c(2,1))
plot(dfdvi,col=cldfdvi)
plot(dfdvilr,col=cldfdvi)
dev.off()

#é importante usare la scala giusta per evitare di non riuscire a distinguere le microdivresità presente nel grafico;

######################################################################################################################################

#Landcover

#settiamo la nostra directory
setwd("~/Desktop/lab")

#richiamiamo la libreria raster
library(raster)

#recuperiamo le immagini che sono di nostro interesse contenute nella nostra working directory
p224r63_2011 <- brick("p224r63_2011_masked.grd")

#chiamiamo la libreria RStoolbox
library(RStoolbox)

plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")

#unsuperClass, l'mmagine di partenza e numero di classi 

p224r63_2011c <- unsuperClass(p224r63_2011,nClasses = 4)

#visualizziamo ciò che abbiamo svolto, informazione sull'immagine; si unisce la mappa al modello 
p224r63_2011c

#plottiamo la nostra mappa, i colori interi ci mostrano le nostre 4 classi;
plot(p224r63_2011c$map)

#cambiamo i colori del nostro grafico così da avere una migliore interpretazione del grafico

clclass <- colorRampPalette(c('green',"red","blue","black"))(100)

plot(p224r63_2011c$map,col=clclass)

######################################################################################################################################

#INTERPOLATION


#exercise: usiamo il vecchio sript sul covid e andiamo a plottare la mappa di densità;
#carichiamo il vecchio script "Lezione spatial"
setwd("~/Desktop/lab")
load("Lezione spatial.R")
ls()
covid <- read.table("covid_agg.csv",header = T)
attach(covid)

cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col=cl5, main="density")
points(covids)

library(spatstat)
covids <- ppp(lon,lat,c(-180,180),c(-90,90))
d <- density(covids)
plot(d)

library(rgdal)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines,add=T)

###interpolazione

covid
#creiamo dei valori per interpolazione in base ad etichette per ogni paese

#usiamo il point pattern di ppp, e associamo alla colonna cases del dataset covid
#se fai attach non si deve scrivere covid$cases ma solo cases
marks(covids) <- cases

#creiamo la mappa con la funzione smooth
s <- Smooth(covids)

# Exercise: plot(s) with points and coastlines

plot(s,col=cl1,main="Covid cases estimate")
cl1 <- colorRampPalette(c('purple', 'pink', 'light pink')) (200) 

points(covids)

library(rgdal)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines,add=T)

#stima non della densità di punti ma di casi nel mondo;

dev.off()

#mappa finale, unico fragico con entrambi i plot

par(mfrow=c(2,1))

#densità 


library(spatstat)
covids <- ppp(lon,lat,c(-180,180),c(-90,90))
d <- density(covids)
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col=cl5, main="density")
points(covids)

library(rgdal)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines,add=T)

#interpolazione

plot(s,col=cl1,main="Covid cases estimate")
cl1 <- colorRampPalette(c('purple', 'pink', 'light pink')) (200) 

points(covids)

library(rgdal)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines,add=T)

dev.off()

#carichiamo un nuovo set di dati messi nella nostra cartella lab;

load("Tesi(1).RData")
ls()
#per fissare i nostri dati 
attach(Tesi)

#visualizziamo le prime 6 righe della tabella
head(Tesi)

#richiamiamo la libraria spatstat
library(spatstat)

#facciamo il nostro point pattern

#per vedere il sommario dei nostri dati 
summary(Tesi)

tesip <- ppp(Longitude,Latitude,c(12.41,12.47),c(43.90,43.95))

dt <- density(tesip)
plot(dt)
cl2 <- colorRampPalette(c("pink","purple","light pink"))

plot(dt,col=cl2)

points(tesip)

#PARTE 2

setwd("~/Desktop/lab")

#riprendiamo i comandi sulla tesi che abbiamo svolto la volta precedente, o se si è salvato il file aprire quello e settare nuovamente la WD
load("sanmarino.RData")

#dt <- mappa di densità
#Tesi <- dataset
#Tesippp <- point pattern

#richiamiamo la libraria spatstat
library(spatstat)

plot(dt)
points(tesip)

#densità relazionata ai prati aridi 

head(Tesi)

#marks associa i valori della variabile al point pattern;

marks(tesip) <- Species_richness

#funzione smooth, per una interpolazione stimiamo i valori dove non sono stati misurati; crea una mappa continua metendo dei valori dove non misurati;


sp <- Smooth(tesip)

plot(sp,col=cl2)
points(tesip)

#aggiungiamo i confini del territorio di san marino

library(rgdal)
Smarino <- readOGR("San_Marino.shp")
plot(Smarino)
plot(Smarino,add=T)
plot(sp,add=T,col=cl2)
points(tesip)

#exercise:multiframe di densità ed interpolazione

par(mfrow=c(2,1))

plot(dt,main="Densità")
points(tesip)


plot(sp,col=cl2,main="interpolazione")
points(tesip)

#exercise: 1 colonna 2 righe nel multiframe

par(mfrow=c(1,2))

plot(dt,main="Densità")
points(tesip)


plot(sp,col=cl2,main="interpolazione")
points(tesip)

########################################################################################################################################

# ANALISI MULTITEMPORALE
#stima di deforestazione di una certa area

setwd("~/Desktop/lab")

#chiamiamo la libreria raster
library(raster)

#con questa funzione faremo leggere le immagini al programma;
defor1<- brick("defor1.jpg")

defor2 <- brick("defor2.jpg")

#vediamo le caratteristiche del nostro file
defor1

#names: defor1.1, defor1.2, defor1.3 
#1 NIR
#2 RED
#3 GREEN

#creiamo un plot RGB
plotRGB(defor1,r=1,g=2,b=3,stretch="Lin")

#vediamo la copertura della foresta pluviale

defor2

#names: defor2.1, defor2.2, defor2.3 

#1 NIR
#2 RED
#3 GREEN

#creiamo un plot RGB
plotRGB(defor2,r=1,g=2,b=3,stretch="Lin")

#vediamo nella seconda immagine l'antropizzazione della zona analizzata

#facciamo un multiframe per vedere le 2 immagini a confronto

par(mfrow=c(2,1))

plotRGB(defor1,r=1,g=2,b=3,stretch="Lin")

plotRGB(defor2,r=1,g=2,b=3,stretch="Lin")

#classifichiamo l'immagine per riuscire a distinguere le classi, classificazione non supervisionata non diciamo cosa è cosa;
#per la classificazione abbiamo bisogno della libreria RStoolbox
library(RStoolbox)

d1c <- unsuperClass(defor1,nClasses = 2)

plot(d1c$map)
 cl1 <- colorRampPalette(c("dark green","pink"))(100)
 plot(d1c$map,col=cl1)

 #classificazione della seconda immagine
 
d2c <- unsuperClass(defor2,nClasses = 2)
cl2 <- colorRampPalette(c("pink","dark green"))(100)
plot(d2c$map,col=cl2)

#dark green <- Foresta
#pink <-zona antropizzata

#mettiamoli a confronto con multiframe;

par(mfrow=c(2,1))

plot(d1c$map,col=cl1)
plot(d2c$map,col=cl2)

dev.off()

#frequenza delle 2 mappe, 

freq(d1c$map)

#value   count
# 1     307552
# 2     33740

totd1 <- 307552+33740

freq(d2c$map)

#value   count
# 1     164353
# 2     178373

totd2 <- 164353+178373

#percentuale di foresta = frequanza prima mappa per 100 diviso il totale

percent1 <- freq(d1c$map)*100/totd1

percent1

#percentuali
#foreste <- 90.1
#zone aperte <- 9.8


percent2 <- freq(d2c$map)*100/totd2

percent2

#percentuali:
#Foreste <- 52.04
#zone aperte <- 47,95

cover <- c("Agriculture","Forest")

before <- c(9.8,90.2)
after <- c(47.95,52.04)
output <- data.frame(cover,before,after)
View(output)

#pt 2 

setwd("~/Desktop/lab")

#richiamiamo il file salvato:
load("Analisi_multitemporale.R")

#visualizziamo Output
output

#creiamo un grafico, usiamo ggplot2
library(ggplot2)

#sulla y abbiamo la percentuale prima 
p1 <- ggplot(output, aes(x=cover,y=before,color=cover))+geom_bar(stat = "identity",fill="white")
plot(p1)


#exercise: plot % afret deforestation
#sulla y abbiamo la percentuale dopo
p2 <- ggplot(output, aes(x=cover,y=after,color=cover))+geom_bar(stat = "identity",fill="white")
plot(p2)

install.packages("gridExtra")
#par con ggplot non funziona quindi si usa questa libreria per riuscire a mettere in multiframe i 2 grafici;

#richiamiamo la libreria 
library(gridExtra)

#grid arrange va a prendere vari plot e li mette insieme all'interno di uno stesso grafico 
grid.arrange(p1,p2,nrow=1)

####################################################################################################################################

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

##################################################################################################################################

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

######################################################################################################################################

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

#1. R_code_first.r   
#2. R_code_spatial.r   
#3. R_code_spatial2.r
#4. R_code_point_pattern   
#5. R_code_teleril.r   
#6. R_code_landcover.r   
#7. R_code_multitemp.r   
#8. R_code_multitemp_NO2.r   
#9. R_code_snow.r   
#10. R_code_patches.r  

#dati copernicus Website 
https://land.copernicus.vgt.vito.be/PDF/portal/Application.html








