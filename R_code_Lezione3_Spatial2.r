#SPATIAL 2

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

