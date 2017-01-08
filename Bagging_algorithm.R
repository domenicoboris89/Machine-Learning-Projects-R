  # Bagging -- Bootstrap Aggregating -- 
  #Il Bagging ricampiona i casi e ricalcola le predizioni per ogni
  #ricampionamento. Dopodichè le predizioni vengono aggregate (ad.es si fa la media)

# Algoritmo di Bagging

library( ElemStatLearn )
data( ozone, package = "ElemStatLearn")
dim( ozone )

# Riordino il dataset rispetto alla variabile outcome
ozone <- ozone[order(ozone$ozone),]
Temperature <- ozone$temperature
Ozone <- ozone$ozone
head(ozone)

# ALGORITMO
# Creo una matrice inizializzazione (nrow è il numero di ricampionamenti)
ll <- matrix( NA, nrow = 10, ncol= 111)
for(i in 1:10){
  # Estrazione campione con reinserimento:
  # la funzione "sample" richiede l'argomento vettore elementi,
  # in questo caso gli indici del dataset da ricampionare, e l'argomento
  # replace (default FALSE) se si vuole o meno il reinserimento dei casi.
  # Con reinserimento le probabilità di estrazione di un caso sono sempre le stesse,
  # ed è possibile riestrarre casi già estratti.
  ss <- sample( 1 : dim(ozone)[1], replace = T)
  # dataset ricampionato e riordinato secondo la variabile outcome
  ozone0 <- ozone[ss,]; ozone0 <- ozone0[order(ozone0$ozone),]
  # crea la curva Curve polinomiale
  loess0 <- loess( Temperature ~ Ozone, data = ozone0, span = 0.2)
  # riempi la matrice con le prediction
  ll[i,] <- predict(loess0, newdata = data.frame(ozone=1:111))
  }

# plot
# osservazioni
plot( Ozone, Temperature, pch = 19, cex = 0.5)
# predizioni 
for (i in 1:10){lines(1:111,ll[i,]),col = "grey", lwd=2)}
# aggregazione apply alla matrice ll, lavorando sui margini delle colonne, la funzione media (previsioni)
lines(1:111, apply(ll,2,mean), col = "red", lwd=2) 


