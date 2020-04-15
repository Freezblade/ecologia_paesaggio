# R SPATIAL, LEZIONE 2-ECOLOGIA DEL PAESAGGIO
#English version below


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


#English

# let's call the package sp
library (sp)

#use the data command to open our dataframe
date (meuse)

#with head we see the first 6
head (meuse)

#use attach to fix the dataframe
attach (meuse)

# let's make a plot to correlate cadmium and lead
plot (cadmium, lead, main = "Cadmium and lead relationship", col = "red", pch = 18, xlab = "Cadmium", ylab = "Lead")

#ecercise: create a plot with copper and zinc
plot (copper, zinc, main = "Relationship between Copper and Zinc", col = "green", pch = 17, xlab = "Copper", ylab = "Zinc")

#to show the graphs together we use: multiframe or multipanel
par (mfrow = c (1,2))
plot (cadmium, lead, main = "Cadmium and lead relationship", col = "red", pch = 18, xlab = "Cadmium", ylab = "Lead")
plot (copper, zinc, main = "Relationship between Copper and Zinc", col = "green", pch = 17, xlab = "Copper", ylab = "Zinc")
# if we change the mfrow values ​​to 2.1 we will have a variation in the graph display

#install through packages, install, GGally or externally with install.packages ("GGally"), once installed we call the library
library (GGally)

#ggpairs (meuse) plots all variables, we use ggpairs (meuse [, 3: 6]) to take only the variables of interest
ggpairs (meuse [3: 6])

#SPATIAL
#explain to R that there are coordinates in the data
coordinates (meuse) = ~ x + y

#we see the graph with:
plot (meuse)

#to create the spatial chart use this function:
spplot (meuse, "zinc")
# the plot that came out is spatial type the yellow points go to indicate the most polluted areas, we are analyzing the areas close to a river.

