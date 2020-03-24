# R SPATIAL, LEZIONE 2-ECOLOGIA DEL PAESAGGIO

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
# il plot che è uscito è di tipo spaziale i punti gialli ci vanno ad indicare le zone più inquinate, stiamo analizzando le zone vicine ad un fiume.




