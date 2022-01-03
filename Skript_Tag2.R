#Statistik Praktikum _ Siba Mohsen
#Tag 2

########################################################################################
#                                 Aufgabe 4
########################################################################################

#Teil a)
X1 <- rexp(100,0.1) #oder mit table
X1

X2 <- rexp(100,0.1)
X2

HX2 <- function(x){
  
 for (i in 1:100) {

  x[i] <- 20- X2[i]
  
 }
  return(x)
}
X2 <- HX2(X2)
X2


#Teil b)
t.test(X1,X2)

#********************************Ausgabe**************************************************#
#Welch Two Sample t-test

#data:  X1 and X2
#t = 0.4166, df = 196.86, p-value = 0.6774
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -2.207863  3.390495
#sample estimates:
#  mean of x mean of y 
#8.989117  8.397801
#************************************************************************************#

#Interpretation:
#H0: Mittelwert(X1) == Mittelwert(X2)
#p-value = 0.6774 > alpha = 0.05
#Damit kann H0 nicht abgelehnt werden.
#Die Mittelwerte von X1 un X2 unterscheiden sich nicht voneinader.




#teil c)
wilcox.test(X1,X2)

#********************************Ausgabe**************************************************#
#Wilcoxon rank sum test with continuity correction

#data:  X1 and X2
#W = 4249, p-value = 0.06669
#alternative hypothesis: true location shift is not equal to 0
#**************************************************************************************#

#Interpretation:
#H0: Median(X1) == Median(X2)
#p-value = 0.06669 > alpha = 0.05
#Damit kann H0 nicht abgelehnt werden.
#Die Mediane von X1 un X2 unterscheiden sich nicht voneinader.






#####################################################################################
#                                  Aufgabe 5
#####################################################################################

#Teil a)
pisa <- read.csv("../PISA.csv",
                 header = T,
                 sep = ",",
                 dec = ".")

attach(pisa)

extractCol <- as.data.frame(pisa[,c(1,4,8,5,9,6,10)])
extractCol

boxplot(extractCol[,-1], xlab = "R,M und S im Jahre 2000 und 2006",ylab="mittlere Scores von R,M und S")

#Interpretation:
#Betrachtet man die Box-Plots von nebeneinader stehenden gleichen Variablen R,M und S 
#für die Jahre 2000 und 2006, sieht man von der Median,dass der mittlere Score
#für die Lesekompetenz und zur Kompetenz in Mathematik leicht gesunken hat.
#Der mittlerer Score in den Naturwissenschaften hat sich vom Jahr 2000 bis 2006 leicht erhöht.


#Teil c)
#Da man hier mit der Median arbeitet, wählt man einen Test, dass die Mediane
#zweier Variablen betrachtet.Dafür kann man Wilcoxon Test für ungepaarte Daten nehmen 
#oder den U-Test nach Mann und Whitney für gepaarte Daten.
# In dieser Aufgabe sind die Daten gepaart, da wir für die gleiche Variablen
# den Median berechnen aber für zwei unterschiedliche Jahren: Wende U-Test.

wilcox.test(R00,R06, paired = T) #Sebastian fragen: Warum gibt es so einen Unterschied zwischen greater und less nur hier?
# p-value = 0.006539 < alpha = 0.05
#H0 ablehnen: Die Mediane für den PISA-Score Lesefähigkeit unterscheiden sich signifikant voneinader.

wilcox.test(M00,S00, paired = T)
# p-value =  0.9709 > alpha = 0.05
#H0 annehmen: Die Mediane für den PISA-Score in Mathematik unterscheiden sich nicht signifikant voneinader.

wilcox.test(S00,S06, paired = T)
# p-value = 0.251 > alpha = 0.05
#H0 annehmen: Die Mediane für den PISA-Score in Naturwissenschaften unterscheiden sich nicht signifikant voneinader.


detach(pisa)






#####################################################################################
#                                  Aufgabe 6
#####################################################################################


hustensaft <- read.csv(".../Hustensaft.csv",
                       header = T,
                       sep = ",",
                       dec = ".")

attach(hustensaft)

mean(Kon)
#Da der Mittelwert der Konzentration im Halssaft 39.2 beträgt, also nicht deutlich niedriger als 40,
#führen diese Konzentrationen zu keinem Produktionsstop.

detach(hustensaft)



#####################################################################################
#                                  Aufgabe 7
#####################################################################################

#Teil a)
sues <- read.csv("../Suess.csv",
                       header = T,
                       sep = ";",
                       dec = ".")

attach(sues)

#Teil b)
coplot(Geschmack~Feuchtigkeit | factor(Suesse),  pch = c(5,18), rows = 1 ,columns = 2)

#Interpretation:

#Im allgemein überschreiten die Losange von Süßigkeitsfaktor=4 diejenigen der
#Süßigkeitsfaktor = 2, da die Starke Süßigkeit(4) verstärkt den Geschmack.
#Wenn die Feuchtigkeit 4, 6 und 8 beträgt, beeinflusst sie den Süssigkeitsgrad nicht.
#Wenn die Feuchtigkeit sehr hoch wird (10), verliert das Geschmack bei starken 
#Süßigkeitsgrad(4) an Geschmack.In diesem Punkt stehen die Losange des Geschmacks beider
#Süßigkeitsgraden auf dem gleichen Niveau (95-100)


#Teil c)
fit <- lm(Geschmack ~ Feuchtigkeit + Suesse)
summary(fit)
#Adjusted R-Square: 0.9379, d.h., dass 93,79% der Daten zusammenhängen: 
#Es gibt eine Relation zwischen Feuchtigkeit und Geschmack.

#Teil d)
fesu <- Feuchtigkeit * Suesse
plot(fit$residuals ~ fesu, xlab = "Feuchtigkeit * Süsse", ylab = "Residuals")

abline(0,0, col = "red")

#Teil e)
fit2.lm <- lm(Geschmack ~ Feuchtigkeit + Suesse + (Feuchtigkeit * Suesse), data = sues)
summary(fit2.lm)
#Adjusted R-squared:  0.9678: d.h. dass dieser Modell 96.78% der Varianzen überdeckt.
# Das Modell hat für Intercept und die unabhängige Variable Feuchtigkeit große Signifikanz ***

#Teil f)
# Pr(>|t|) = 0.003559 ist Beta3 im Modell mit Interaktion: ist von 0 NICHT signifikant verschieden.
plot(fit2.lm$residuals ~ fesu, xlab = "Feuchtigkeit * Süsse mit Interaktion", ylab = "Residuals")
abline(0,0,col = "green")
#fit2.residuals <- resid(fit2.lm)
#plot(fit2.residuals)



detach(sues)



#####################################################################################
#                                  Aufgabe 8
#####################################################################################

#Teil a)

hotdog <- read.table("../Hotdog.csv",
                     header = T)

attach(hotdog)

#Teil b)
#Erstmals: Boxplots erstellen
#Verhältnis von der Fleischart im Bezug auf die Kalorien:
boxplot(Calories~Type, xlab = "Fleischart", ylab = "Calories")
#Interpretation:
# Die Boxplots von den drei Fleischarten zeigen, dass Meat(153) die größte
#Kalorienrate enthält, dann Beef(152.5) und letzlich Poultry(ca.110).

#Verhältnis von der Fleischart im Bezug auf den Satzgehalt:
boxplot(Sodium~Type,xlab= "Fleischart", ylab = "Saltzgehalt")
#Interpretation:
# Die Boxplots von den drei Fleischarten zeigen, dass Poultry der größte
#Saltzgehalt enthält, dann Meat mit einem Ausreißer und letzlich Beef.

#Da es sich hierbei um mehr als 2 Gruppen handelt, ist die Varianzanalyse 
#das geeignete Verfahren.

summary(aov(Calories~Type))
#Man erkennt durch ANOVA, dass es einen hochsignifikanten Unterschied zwischen den
#3 Gruppen von Fleischarten gibt, wegen die drei ***.

summary(aov(Sodium~Type))
#Man erkennt durch ANOVA, dass es keinen signifikanten Unterschied zwischen den
#3 Gruppen von Fleischarten im Verhältnis zum Sodium gibt, wegen die drei ***.

#Teil c) 
#wurde oben ernannt.

#Jetzt ist das eine weitere Recherche von mir:
#Bis jetzt wissen wir, dass es einen/keinen signifikanten Unterschied zwischen denKategorien gibt.
#Um herauszufinden, welche der einzelnen Gruppen sich signifikant voneinander 
#unterscheiden, müssen wir einen sogenannten Post-Hoc-Test berechnen.

TukeyHSD(aov(Calories~Type))
#Interpretation:
#In der ersten Zeile sind Meat und Beef miteinander verglichen. 
#Diff besagt uns, dass der Meat-Kaloriengehalt im Mittel 1.855 größer als der Beef-Kaloriengehalt.
#Danach schaut man die p adj = 0.9688 > alpha = 0.05
#Da der p-Wert größer als 0.05 ist, ist der Unterschied zwischen 
#Meat und Beef statistisch nicht signifikant.

#In der zweiten Zeile Poultry-Beef:
#Diff besagt uns, dass der Poultry-Kaloriengehalt im Mittel -38.08 Mal kleiner als der Beef-Kaloriengehalt.
#Danach schaut man die p adj = 0.0000277 < alpha = 0.05
#Da der p-Wert kleiner als 0.05 ist, ist der Unterschied zwischen 
#Meat und Beef statistisch signifikant.

#Vergleicht man diese Ergebnisse mit den Box-plots, zieht man die selbe Ergebnisse heraus.


detach(hotdog)
