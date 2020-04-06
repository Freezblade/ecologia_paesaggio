#installiamo la nuova libreria tramite i pacchetti; se usiamo R invece di R-studio useremo il comando 
install.packages("sp")
library(sp) 

#usiamo il comando data per chiamare i dati contenuti nella libreria;
data("meuse")

#vediamo la tabella dei dati meuse;
meuse

#vediamo solo le prime 6 righe della tabella;
head(meuse) #vediamo i primi 6

#con questo comando andremo a vedere i nomi delle variabili contenute all'interno della tabella
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

pairs(meuse[,3:6],lower.panel = panel.correlations,upper.panel = panel.smoothing,diag.panel = panel.histograms)


pairs(meuse[,3:6],lower.panel = panel.smoothing,upper.panel = panel.correlations,diag.panel = panel.histograms)

library(sp)
data("meuse")
head(meuse)


plot(meuse$cadmium,meuse$copper, main="cadmio e rame correlati")

attach(meuse)

plot(cadmium,copper, main="Cadmio e Rame",pch=24,col="purple",xlab="Cadmio",ylab="Rame",cex=2)

