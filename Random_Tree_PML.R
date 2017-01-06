#RANDOM TREE exercise from Coursera Practical Machine Learning
data( "iris" )
library( ggplot2 )
str( iris )
name( iris )

#suddivisione train e test set
library( caret )
#creazione indici
index.train <- createDataPartition( iris$Species, p=0.7, list=FALSE )
trainset <- iris[index.train,]
testset <- iris[-index.train,]
dim( trainset )
dim( testset )
#Plot larghezza dello stelo vs petalo, raggruppamento su base specie

str(trainset)
ggplot(trainset, aes ( x = Petal.Width, y = Sepal.Width, col = Species )) + 
  geom_point()

#Addestramento seed.6117
# questo pacchetto non era stato installato : install.packages('e1071', dependencies=TRUE)
set.seed ( 6117 )
mod1 <- train( Species ~ ., method = "rpart" , data = trainset )
output <- mod1$finalModel 

#Visualizzazione dendrogramma
plot( output, uniform = TRUE, main = "Classification Tree" )
text( output, use.n = TRUE, all = TRUE, cex = 0.8)

#Predict

prediction <- predict( mod1, testset)

#Matrice confusione

confmatrix <- table( testset$Species, prediction )
  print( confmatrix )
accs<-sum( diag( confmatrix ))/sum( confmatrix )
  print( accs )
