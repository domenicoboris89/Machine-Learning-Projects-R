# RANDOM FOREST
# Un modello RF fa il bootstrap (ricampionamento) dei casi e, ad ogni split, delle variabili.
# Si può considerare come una moltitudine di Random Tree, di cui poi si sceglie il migliore.

library(caret)
library(ggplot2)
data("iris")
attach(iris)

index <- createDataPartition( iris$Species, p = 0.7, list = F)

train <- iris[index,]
test <- iris[-index,]

# addestramento RF
mod1 <- train( Species ~., data = train, method = "rf", prox = T)
mod1

# estraggo l'albero 4
tree4 <- getTree( mod1$finalModel, k = 4)

# prediction e conf matrix
pred <- predict( mod1, test)
test$predictright <- pred == test$Species ; truepred <- sum(test$predictright)
conf <- table(pred,test$Species)
conf
accs <- sum( diag( conf))/sum( conf)
print( accs )