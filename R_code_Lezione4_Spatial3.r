#codice per analisi dei pattern legati ai punti 

#English version below

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

#English

#set up a working directory
setwd ("/ Users / Pierluigi / landscape ecology")

#simplify the name
covid <- covid_agg

#for those who have the table without a header, the command will be used
covid <- read.table ("covid_agg.csv", header = T)

# let's call the first six rows of the table
head (covid)

# let's create a plot to start seeing the layout
plot (covid $ country, $ covid cases)

#with las we see the lables,
#with las = 0 they remain parallel
#con las = 1 changes labels horizontally
#with las = 2 the lables are perpendicular to the axis
#with las = 3 the lables are all vertical
plot (covid $ country, $ covid cases, las = 0)
plot (covid $ country, $ covid cases, las = 1)
plot (covid $ country, $ covid cases, las = 2)
plot (covid $ country, $ covid cases, las = 3)

#install and then call the ggplot2 library
library (ggplot2)

# let's call the data in the library
date (mpg)

#show the first six rows of the table
head (mpg)

#create a plot of the new mgp dataset, modify the aesthetics and add a geometry
ggplot (mpg, aes (x = displ, y = hwy)) + geom_point ()

#download and install the new spatstat library
library (spatstat)
attach (covid)

# via the ppp function we create a new dataset that will interest us for the spatial analysis
covids <- ppp (lon, lat, c (-180,180), c (-90.90))

#simplify density by calling it d
d <- density (covids)

# let's do a plot of d
plot (d)

#add the points to the plot
points (covids)

#save the file in .Rdata
#re-call our WD and then load the save that we made previously

#change the colors, palette, number of levels you specify with an external parenthesis, to use this function we use the spatstat library
cl <- colorRampPalette (c ("yellow", "orange", "red")) (100)
plot (d, col = cl)

#exercise: we change the chart colors to blue and green
cl2 <- colorRampPalette (c ("green", "turquoise", "blue")) (100)
plot (d, col = cl2)

#to complete the map add the borders of the various countries; we use the rgdal library, we download a new package of coast data in vector
library (rgdal)
coastlines <- readOGR ("ne_10m_coastline.shp")

#to get our complete graph we will use this command;
plot (coastlines, add = T)











