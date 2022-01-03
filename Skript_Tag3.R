#Tag3_Statistik_Praktikum
  
  
#####################################################################################
#                                    Aufgabe 9
#####################################################################################

#Teil a)
load("../SPECTF.RData/SPECTF.RData")

#Nachdem Load() sieht man im Environment-Tab den Datensatz SPECTF
#Dimension:
dim(SPECTF) 
#Antwort: 267  45


#Teil b)
#benutze die Funktion range auf jeder Spalte mithilfe des Befehls apply()
spannweiten <- apply(SPECTF,2,range)
spannweiten
summary(spannweiten)

#Histogram der Spannweiten:
hist(spannweiten)
#Man sieht, dass das Maximum und das Minimum auf der rechten bzw. linken Seite Stehen.
# Für die Werte 40<Wert<70 liegen keine Maxima oder Minima.



#Teil c)
#Schritt1: berechne die Varianz für jede Spalte
varianz <- apply(SPECTF,2,var)
#Schritt 2: sortiere die Varianzen der Daten absteigend ein
varianzdecreasing <- sort(varianz, decreasing = T)
#Schritt 3: wähle die erste 6 Varianzen von varianzdecreasing
sechsVarianzen <- varianzdecreasing[c(1:6)]
sechsVarianzen
#liefert die Names der Variablen mit maximaler Varianz
vname <- names(sechsVarianzen)
vname
#Plotte alle Box-plots nebeneinander
par(mfrow=c(2,3))
boxplot(SPECTF$X42)
boxplot(SPECTF$X44)
boxplot(SPECTF$X41)
boxplot(SPECTF$X26)
boxplot(SPECTF$X30)
boxplot(SPECTF$X25)


#install.packages("readr")
library(readr)
parse_number(vname) # extrahiert die Zahlen aus den Namen


#####################################################################################
#                                  Aufgabe 10
#####################################################################################

#Teil a)
#Teil b)
library(tidyverse)
require(caret)
library(e1071)

SPECTF$X0 <- factor(SPECTF$X0)

# Definiere training control
set.seed(123)
train.control <- trainControl(method = "cv", number = 10)
# Trainiere das Modell
model <- train(X0~., data = SPECTF, method = "glm", #glm steht für generalized linear model
               trControl = train.control)

predictedModel <- predict(model,type = "prob")
predictedModel
# Summarize die Ergebnisse
print(model)

#*********************************Ausgabe*********************************************
#Generalized Linear Model 
#
#267 samples
#44 predictor
#2 classes: '0', '1' 
#
#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 1 times) 
#Summary of sample sizes: 241, 241, 241, 240, 240, 240, ... 
#Resampling results:
#  
#  Accuracy  Kappa    
#0.793956  0.3877705
#*************************************************************************************

#Interpretation:
#Der Wert Accuracy besagt, dass 79.39% der Daten valide sind.

#Teil b)
library("pROC")
library("ROCR")

#Grundsätzlich braucht man folgende 3 Zeilen, um eine ROC-Kurve in ROCR-Package gedruckt bekommt.
pred <- prediction(predictedModel[,-1], SPECTF$X0)
perf <- performance(pred,"tpr","fpr")
plot(perf, col="red")

#Teil c) + d) 
#Konfidenzintervalle + AUC +Plots
pROC_obj <- roc(SPECTF$X0,predictedModel[,-1],
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)


sens.ci <- ci.se(pROC_obj)
sens.ci
plot(sens.ci, type="shape", col="lightblue")

