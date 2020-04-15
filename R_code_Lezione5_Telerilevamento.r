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


#English

#download 2 libraries that we will need to work with our data

install.packages ( "raster")

install.packages ( "RStoolbox")
yes

# let's call the first library we downloaded
library (raster)

#download a data packet and rename a particular file that we will need for our work
p224r63_2011 <- brick ("p224r63_2011_masked.grd")

# let's make a plot of our satellite image
plot (p224r63_2011)

# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: infrared medium
# B6: thermal infrared
# B7: medium infrared

#to change the color scale use this command:
cl <- colorRampPalette (c ("black", "gray", "light gray")) (100)

#having set these values ​​we put in our command col = "cl"
plot (p224r63_2011, col = cl)

#modify the cormatic scales to see what happens
cllow <- colorRampPalette (c ("black", "gray", "light gray")) (5)

# let's see what changes in our chart
plot (p224r63_2011, col = cllow)

names (p224r63_2011)
# [1] "B1_sre" "B2_sre" "B3_sre" "B4_sre" "B5_sre" "B6_bt" "B7_sre"

#change the blue band to blue scale
clb <- colorRampPalette (c ("dark blue", "blue", "light blue")) (100)

#attach data frame doesn't work with raster
#to put only the blue band inside the plot
plot (p224r63_2011 $ B1_sre, col = clb)

# let's do the same for the other colors
#green
clg <- colorRampPalette (c ("dark green", "green", "light green")) (100)
plot (p224r63_2011 $ B2_sre, col = colg)

#exercise: do near infrared with the color scale from red to yellow
clir <- colorRampPalette (c ("purple", "pink", "light pink")) (100)
plot (p224r63_2011 $ B4_sre, col = clir)

#plot with all 4 multiframe bands
par (mfrow = c (2,2))

#blue
clb <- colorRampPalette (c ("dark blue", "blue", "light blue")) (100)
plot (p224r63_2011 $ B1_sre, col = clb)

#green
clg <- colorRampPalette (c ("dark green", "green", "light green")) (100)
plot (p224r63_2011 $ B2_sre, col = clg)

#red
clr <- colorRampPalette (c ("red", "orange", "yellow")) (100)
plot (p224r63_2011 $ B3_sre, col = clr)

#infrared
clir <- colorRampPalette (c ("purple", "pink", "light pink")) (100)
plot (p224r63_2011 $ B4_sre, col = clir)

#with dev.off () we close the plotted images

#natural colors, 3 components R G B, we create a plot with this color range
# 3 bands at a time R = red G = green B = blue
#the function we will use will be:

plotRGB (p224r63_2011, r = 3, g = 2, b = 1)

#being the result black, we use the stretch = "lin" function

plotRGB (p224r63_2011, r = 3, g = 2, b = 1, stretch = "lin")

#to better visualize our resulting graph we change the bands by inserting the infrared

plotRGB (p224r63_2011, r = 4, g = 3, b = 2, stretch = "lin")

# the plants being reflective will be in red, the light blue indicates the non-cultivated agricultural areas, the pink areas are cultivated agricultural areas

# let's see the 2 graphs obtained using the multiframe function
par (mfrow = c (2,1))

plotRGB (p224r63_2011, r = 3, g = 2, b = 1, stretch = "lin")
plotRGB (p224r63_2011, r = 4, g = 3, b = 2, stretch = "lin")


#to save it in pdf
pdf ( "Remote Sensing-1.pdf")
plotRGB (p224r63_2011, r = 4, g = 3, b = 2, stretch = "lin")
dev.off ()

par (mfrow = c (2,1))

plotRGB (p224r63_2011, r = 3, g = 2, b = 1, stretch = "lin")
plotRGB (p224r63_2011, r = 4, g = 3, b = 2, stretch = "lin")
dev.off ()

# exercise: we install infrared in other green components
plotRGB (p224r63_2011, r = 3, g = 4, b = 2, stretch = "lin")

#infrared in the blue component:
plotRGB (p224r63_2011, r = 3, g = 2, b = 4, stretch = "lin")

# PT.2

#to be able to use the remote sensing functions, you must use the raster library;
library (raster)

#set the working directory
setwd ( "~ / Desktop / lab")

#this time we will use another image for our remote sensing
#NB. we put in the directory a dataset that we will use during all our work on R
#apply the brick function to be able to rename and bring to R our image of interest

p224r63_1988 <- brick ("p224r63_1988_masked.grd")

#we create a plot of the 1988 object
#the 1988 lensat satellite we see the blue, red, green, and infrared bands.
# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: infrared medium
# B6: thermal infrared
# B7: medium infrared

plot (p224r63_1988)

#to see the names of interest
names (p224r63_1988)

#to see our graphs all together we use the multiframe
par (mfrow = c (2,2))

#blue
clb <- colorRampPalette (c ("dark blue", "blue", "light blue")) (100)
plot (p224r63_1988 $ B1_sre, col = clb)

#green
clg <- colorRampPalette (c ("dark green", "green", "light green")) (100)
plot (p224r63_1988 $ B2_sre, col = clg)

#red
clr <- colorRampPalette (c ("red", "orange", "yellow")) (100)
plot (p224r63_1988 $ B3_sre, col = clr)

#infrared
clir <- colorRampPalette (c ("purple", "pink", "light pink")) (100)
plot (p224r63_1988 $ B4_sre, col = clir)

#to close the current window we will use:

dev.off ()

# create an RGB plot in True colors or natural

plotRGB (p224r63_1988, r = 3, g = 2, b = 1, stretch = "Lin")

#being the graph not understandable we will use the chromatic scale

plotRGB (p224r63_1988, r = 3, g = 2, b = 1, stretch = "Lin")

#exercise: we create the plot with the infrared component in the red component;
#in this case the colors are called False colors;

plotRGB (p224r63_1988, r = 4, g = 3, b = 2, stretch = "Lin")

# let's see how our image has changed over time by comparing 2011 with 1988;

par (mfrow = c (2,1))
plotRGB (p224r63_2011, r = 4, g = 3, b = 2, stretch = "Lin")
plotRGB (p224r63_1988, r = 4, g = 3, b = 2, stretch = "Lin")

dev.off ()
# agriculture is much more developed in 2011
#infrared are the plants
# clods of earth are white or light blue

#we can calculate the health index of the vegetation, remember that it is possible because healthy leaves can reflect infrared
#DVI (Difference Vegetation Index) will be the index that we will use;

# DVI = NIR-RED, we will have different results based on the health of the plants;
#sana = high NIR
#Malata = RED high

# = Div1988 nir1988-red1988
# the $ symbol is used to tie

dvi1988 <- p224r63_1988 $ B4_sre-p224r63_1988 $ B3_sre

# let's see the DVI plot
plot (dvi1988)

#we do the same for 2011

dvi2011 <- p224r63_2011 $ B4_sre-p224r63_2011 $ B3_sre
plot (dvi2011)

par (mfrow = c (2,1))
 # To change color we will use the color ramp palette command
cldvi11 <- colorRampPalette (c ('dark green', 'green', 'light green')) (100)
plot (dvi2011, col = cldvi11)

# now we have the DVI of the 2 years, if we make the difference between the 2 years we will see how much the change in vegetation has been, if the 2011 value is negative it means that the vegetation was better in 1988
#multitemporal analysis
dfdvi <- dvi2011-dvi1988


# we create the image that shows us the areas where the plants have been in greatest stress
plot (dfdvi)
cldfdvi <- colorRampPalette (c ('red', 'white', 'blue')) (100)
plot (dfdvi, col = cldfdvi)

#view all graphs together using an image multiframe of 1988. 2011 and index difference
par (mfrow = c (3,1))

plotRGB (p224r63_2011, r = 4, g = 3, b = 2, stretch = "Lin")
plotRGB (p224r63_1988, r = 4, g = 3, b = 2, stretch = "Lin")
plot (dfdvi, col = cldfdvi)

dev.off ()

#change the resolution of an image, the function used is aggregate
p224r63_2011lr <- aggregate (p224r63_2011, fact = 10)
# if fact or factor is set equal to 10 we use a scale 10 times greater

# let's see the characteristics of the original image
p224r63_2011

# let's see the characteristics of the new image
p224r63_2011lr

# let's compare the 2 graphs;
par (mfrow = c (2,1))
plotRGB (p224r63_2011, r = 4, g = 3, b = 2, stretch = "Lin")
plotRGB (p224r63_2011lr, r = 4, g = 3, b = 2, stretch = "Lin")
dev.off ()

#change the factor to 50
p224r63_2011lr2 <- aggregate (p224r63_2011, fact = 50)
#view the image information
p224r63_2011lr2

#create our graph by always correlating them;
par (mfrow = c (2,1))
plotRGB (p224r63_2011, r = 4, g = 3, b = 2, stretch = "Lin")
plotRGB (p224r63_2011lr2, r = 4, g = 3, b = 2, stretch = "Lin")
dev.off ()

# let's make a dvi of the new image 2011
dvi2011lr50 <- p224r63_2011lr2 $ B4_sre - p224r63_2011lr2 $ B3_sre
plot (dvi2011lr50)

#decrease the 1988 resolution
p224r63_1988lr2 <- aggregate (p224r63_1988, fact = 50)

# let's make a dvi of the new image 1988
dvi1988lr50 <- p224r63_1988lr2 $ B4_sre - p224r63_1988lr2 $ B3_sre
plot (dvi1988lr50)

# let's make the difference of the DVI of the two years at low resolution
dfdvilr <- dvi2011lr50-dvi1988lr50

#create our image:
plot (dfdvilr, col = cldfdvi)


#multiframe of the total
par (mfrow = c (2,1))
plot (dfdvi, col = cldfdvi)
plot (dfdvilr, col = cldfdvi)
dev.off ()

# it is important to use the right scale to avoid not being able to distinguish the micro-diversity present in the graph;







