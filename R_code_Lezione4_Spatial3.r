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













