rm(list=ls(all=T))
setwd("D:/study/2021spring/GSCI5150/final project/code")
set.seed(1234)

library(datasets)
# data('iris')
str(iris)
iris

summary(iris)

par(mar=c(7,5,1,1)) # more space to labels
boxplot(iris,las=2)

irisVer <- subset(iris, Species == "versicolor")
irisSet <- subset(iris, Species == "setosa")
irisVir <- subset(iris, Species == "virginica")
par(mfrow=c(1,3),mar=c(6,3,2,1))
boxplot(irisVer[,1:4], main="Versicolor",ylim = c(0,8),las=2)
boxplot(irisSet[,1:4], main="Setosa",ylim = c(0,8),las=2)
boxplot(irisVir[,1:4], main="Virginica",ylim = c(0,8),las=2)

par(mfrow=c(2,2))
hist(iris$Petal.Length)
hist(irisVer$Petal.Length,breaks=seq(0,8,l=17),xlim=c(0,8),ylim=c(0,40))
hist(irisSet$Petal.Length,breaks=seq(0,8,l=17),xlim=c(0,8),ylim=c(0,40))
hist(irisVir$Petal.Length,breaks=seq(0,8,l=17),xlim=c(0,8),ylim=c(0,40))

#library(beanplot)
#xiris <- iris
#xiris$Species <- NULL
#beanplot(xiris, main = "Iris flowers",col=c('#ff8080','#0000FF','#0000FF','#FF00FF'), border = "#000000")

corr <- cor(iris[,1:4])
round(corr,3)

pairs(iris[,1:4])

pairs(iris[,1:4],col=iris[,5],oma=c(4,4,6,12))
par(xpd=TRUE)
legend(0.87,0.6, as.vector(unique(iris$Species)),fill=c(1,2,3))

library(MASS)
parcoord(iris[,1:4], col=iris[,5],var.label=TRUE,oma=c(4,4,6,12))
par(xpd=TRUE)
legend(3.2,0.4, as.vector(unique(iris$Species)),fill=c(1,2,3))


decisionplot <- function(model, data, class = NULL, predict_type = "class",
                         resolution = 100, showgrid = TRUE, ...) {
  
  if(!is.null(class)) cl <- data[,class] else cl <- 1
  data <- data[,1:2]
  k <- length(unique(cl))
  
  plot(data, col = as.integer(cl)+1L, pch = as.integer(cl)+1L, ...)
  
  # make grid
  r <- sapply(data, range, na.rm = TRUE)
  xs <- seq(r[1,1], r[2,1], length.out = resolution)
  ys <- seq(r[1,2], r[2,2], length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  p <- predict(model, g, type = predict_type)
  if(is.list(p)) p <- p$class
  p <- as.factor(p)
  
  if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
  
  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 2, levels = (1:(k-1))+.5)
  
  invisible(z)
}

library(caret)
x <- iris[1:150, c("Sepal.Length", "Sepal.Width", "Species")]

model <- knn3(Species ~ ., data=x, k = 1)
decisionplot(model, x, class = "Species", main = "kNN (1)")

model <- knn3(Species ~ ., data=x, k = 10)
decisionplot(model, x, class = "Species", main = "kNN (10)")

library(e1071)
model <- naiveBayes(Species ~ ., data=x)
decisionplot(model, x, class = "Species", main = "naive Bayes")

library("rpart")
model <- rpart(Species ~ ., data=x)
decisionplot(model, x, class = "Species", main = "CART")
#plot(model, main="test amit")
#fancyRpartPlot(model, main="Iris")

library(randomForest)
model <- randomForest(Species ~ ., data=x)
decisionplot(model, x, class = "Species", main = "Random Forest")

library(e1071)
model <- svm(Species ~ ., data=x, kernel="linear")
decisionplot(model, x, class = "Species", main = "SVD (linear)")

model <- svm(Species ~ ., data=x, kernel = "radial")
decisionplot(model, x, class = "Species", main = "SVD (radial)")

model <- svm(Species ~ ., data=iris, kernel = "sigmoid")
decisionplot(model, x, class = "Species", main = "SVD (sigmoid)")
plot(model, data=x)
model
pred = predict(model,iris)
tab = table(Predicted=pred, Actual = iris$Species)
tab

