# Creamos la función success_ratio que necessitaremos más adelante.
success_ratio <- function(cm) {
  total = cm[1][1] + cm[2][1] + cm[3][1] + cm[4][1]
  ratio = ((cm[1][1] + cm[4][1]) / total)
  return(ratio)
}

# Cargamos la base de datos.
load(paste0(pathData,"dataDEFINITIVA.RData"))

# Cargamos los paquetes necesarios
library(caret)
library(e1071)
library(kernlab)

# Separamos la base de datos en dos, Train y Test
set.seed(810)
inTraining <- createDataPartition(dd$y, p = .8, list = FALSE)
training <- dd[ inTraining,]
testing  <- dd[-inTraining,]

# Primer SVM
classifier = svm(formula = y ~ .,
                 data = training)

y_train_pred = predict(classifier, newdata = training[,-18])

## Construimos las matrices de confusión
cm_train = table(training[, 18], y_train_pred)
cm_train_str = capture.output(show(cm_train))
writeLines(c(
  "Training set confusion matrix : ",
  cm_train_str,
  paste("Success ratio on training set : ", toString(success_ratio(cm=cm_train)*100), "%")
))

# Hyperparameter tuning in caret
## Set up Repeated k-fold Cross Validation
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)

## Fit the model
grid_radial <- expand.grid(
  sigma = c(0,0.01, 0.02, 0.025, 0.03,0.04,0.05, 0.06, 0.07,0.08,0.09,
            0.1, 0.25, 0.5, 0.75,0.9),
  C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2,5))

svm3 <- train(y ~., data = training, method = "svmRadial",
              trControl = train_control,
              tuneGrid = grid_radial,
              tuneLength = 10,
              preProcess = NULL)


plot(svm3)

# Support Vector Machine

y_train_pred = predict(svm3, newdata = training[,-18])
y_test_pred = predict(svm3, newdata = testing[,-18])

## Construimos las matrices de confusión

cm_train = table(training[, 18], y_train_pred)
cm_test = table(testing[, 18], y_test_pred)

cm_train_str = capture.output(show(cm_train))
writeLines(c(
  "Training set confusion matrix : ",
  cm_train_str,
  paste("Accuracy on training set : ", toString(success_ratio(cm=cm_train)*100), "%"),
  paste("Error on training set : ", toString(100-success_ratio(cm=cm_train)*100), "%")
))

cm_test_str = capture.output(show(cm_test))
writeLines(c(
  "Test set confusion matrix : ",
  cm_test_str,
  paste("Accuracy on test set : ", toString(success_ratio(cm=cm_test)*100), "%"),
  paste("Error on test set : ", toString(100-success_ratio(cm=cm_test)*100), "%")
))

## Graficos SVM
set.seed(810)
h<-sample(1:nrow(testing),size = 75)
plot(classifier, testing[h,], age ~ duration, symbolPalette = c("blue","red"),
     color.palette = terrain.colors)

# Applying k-Fold Cross Validation
folds = createFolds(training$y, k = 10)

cv = lapply(folds, function(x) { # start of function
  # in the next two lines we will separate the Training set into it's 10 pieces
  training_fold = training[-x, ] # training fold =  training set minus (-) it's sub test fold
  test_fold = training[x, ] # here we describe the test fold individually
  # now apply (train) the classifer on the training_fold
  classifier = svm(formula = y ~ .,
                   data = training_fold,
                   type = 'C-classification',
                   kernel = 'radial')
  # next step in the loop, we calculate the predictions and cm and we equate the accuracy
  # note we are training on training_fold and testing its accuracy on the test_fold
  y_pred = predict(classifier, newdata = test_fold[,-18])
  cm = table(test_fold[, 18], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})

accuracy = mean(as.numeric(cv))
accuracy