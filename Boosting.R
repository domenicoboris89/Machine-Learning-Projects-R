## BOOSTING
# Il Boosting è un modello che a partire da K classificatori iniziali,
# con capacità esplicativa debole, crea un classificatore finale
# dato da una funzone dei classificatori pesati per il loro error rate individuale.

library( ISLR )
library(caret)
data (Wage)
index <- createDataPartition(Wage$wage, p= 0.7,list=F)
train <- Wage[index,]
test <- Wage[-index,]

# Il boosting nel pacchetto caret è il metodo "gbm" 

mod1 <- train(wage ~., method = "gbm", data = train, verbose=F)
mod1
