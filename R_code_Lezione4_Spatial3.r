#codice per analisi dei pattern legati ai punti 

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














