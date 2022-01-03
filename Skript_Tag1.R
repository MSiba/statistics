#Tag 1 Praktikum
#Siba Mohsen

#################################################################################

#                                     Aufgabe 1                                 #

#################################################################################

#Teil a)
#Datei auslesen

oecd <- read.table("../oecdM.csv",
                 sep = ",",
                 dec = ".", 
                 header = TRUE)

attach(oecd)

#Teil b)
#Berechnen Sie die Mittelwerte und Varianzen der einzelnen Variablen mit dem geeigneten
#apply Befehl.
 
#Mittelwert
lapply(oecd[,-1], mean, na.rm = TRUE)


#Varianz
lapply(oecd[,-1],var,na.rm = TRUE)

#Teil c)
#Uberprufen Sie, ob die Niederlande in der Landerliste des Datensatze auftaucht. Gibt
#es auch einen Eintrag fur China?
#Hierzu gibt es verschiedene Lösungswege:

#Lösung 1:
'Niederlande' %in% oecd[,1]  #ergibt True.

# Ja, die Niederlande befindet sich in der Spalte der Länder, nämlich die erste Spalte.

#Lösung 2:
is.element('Niederlande', oecd[,1])

#Lösung 3:
#grepl gibt False für die Einträge != 'Niederlande' und nur TRUE, wenn oecd[,1] == 'Niederlande'.
grepl('Niederlande', oecd[,1])

# Das gleiche gilt auch für Chine:
#China ist nicht im Datensatz
'China' %in% oecd[,1]

#Teil d)
#In welchem Land waren die meisten Jugendlichen mindestens zweimal betrunken?Wie
#hoch ist der maximale Prozentsatz?

max(Alkohol,na.rm = TRUE) 

#Um zu wissen, um welches Land es sich handelt, gibt es mehrere Möglichkeiten:
#Möglichkeit 1: SQL Befehl
library(sqldf)
maxAlkoholConsum <- sqldf("select * from oecd[,1] where Alkohol = 24.8")
maxAlkoholConsum #liefert alle (*) Informationen über das Land "Dänemark"in oecd[,1]
#Das Problem mit der Ausgabe hier ist das Befinden von deutschen Umlaute
# Land: Dänemark
#max. Prozentsatz: 24.8

#Maximaler Prozentsatz: max = 24.8. Das ist der Eintrag von Dännemark (nach SQL Befehl)
#Möglichkeit 2: subset
subset(oecd[,1], Alkohol == 24.8)



#Teil e)
#In welchem Land ist die Sauglingssterblichkeit am geringsten? Wie hoch ist sie in
#diesem Land?
#Wende den min-Befehl an.
min(oecd[,9],na.rm = T)
saeuglingssterblichkeit <- subset(oecd[,1],oecd[,9] == min(oecd[,9],na.rm = T))
saeuglingssterblichkeit
# min = 2.3
#Land: Island

#Ich konnte auf dem SQL nicht zugreifen: Aufgrund der deutschen Benennung und Umlaute der Variable SÄ.uglsterblichkeit
#minSauglsterblichkeit <- sqldf("select * from oecd[,1] where SÄ.uglsterblichkeit = 2.3")
#minSauglsterblichkeit


#Teil f)
#In welchen Landern ist der Prozentsatz an Jugendlichen, die sich regelmässig bewegen,
#kleiner als der Durchschnitt?
#Als Lösung habe ich eine Subset erzeugt, wo alle 16 Länder stehen, für die die oboge Aussage gilt 
wenigBewegung <- subset(oecd[,1], Bewegung < mean(Bewegung, na.rm = T))
wenigBewegung


#################################################################################

#                        Aufgabe 2

#################################################################################

#Teil a)
#Wieviele Lander im Datensatz oecdM gehoren zu Europa, wieviele zum Rest der Welt?
#Stellen Sie das Ergebnis in einem Kuchendiagramm dar und verwenden Sie dazu die
#Farben grun (green) und blau (blue).


# Kuchendiagramm benennen
png(file = "Country_Europa_Rest.jpg")

#finde heraus, welche Länder gehören Europa?
europe <- subset(oecd[,1], Geo == "E")
europe
length(europe)

#Welche Länder gehören den Rest der oecd Mitglieder?
rest <- subset(oecd[,1], Geo == "R")
rest
length(rest)

myColors <- c("green", "blue")    #speichert die zu benutzende Farben in einem Vektor
x <- c(length(europe),length(rest)) #listet die Anzahl der Länder in E und R.
labels <- c("Europe", "Rest")

piepercent<- round(100*x/sum(x), 1) #berechnet die Prozente, anstatt 21 70% usw.

# Plotten:
pie(x, labels = x, main = "Country pie chart",col = myColors)

legend("topright",labels, cex = 0.8,fill = myColors)

#Teil b
#Visualisieren Sie die Variable Lesen, getrennt nach dem Faktor Geo, in einem vertikalen
#Stripchart. Welche Aussage konnen Sie mit diesem Stripchart treffen

stripchart(Lesen~Geo,
           data=oecd,
           main="Stripchart fuers Lesen in Europa und den Rest",
           xlab="Geo",
           ylab="Lesen",
           col="red",
           group.names=c("E","R"),
           vertical=TRUE,
           pch=16,
           add = FALSE,
           at = c(1.25, 1.75))



###################################################################################

#                                     Aufgabe 3                                   #

###################################################################################

#Teil a)
#Erstellen Sie einen Boxplot fur die Variable \Bildung". Was fallt Ihnen auf?

boxplot(Bildung)


#Analyse: im Bericht

#Teil b)
#Untermauern Sie die Beobachtung aus Aufgabe (a) durch Berechnung einiger Quantile
#mit Hilfe der Funktion quantile().

quantile(Bildung)

#im Bericht 


#Teil c)
#Stellen Sie zudem die aufsteigend geordneten Werte der Variable Bildung mit Hilfe der
#Funktion plot() als Kurve dar.
#Die Daten müssen mit der Funktion sort() sortiert werden.

plot(sort(Bildung))


#Teil d)
#Begrunden Sie anhand Ihrer Beobachtungen, dass das 75% Quantil der Daten einen
#guten Trennpunkt zwischen Landern mit \guter" und \schlechter" Grundausstattung
#fur Bildung darstellt
.
# Um den Trennpunkt zu zeigen, habe ich eine gerade Linie mit dem 75% Quantil, also 
#bei (0,2.2) gezeichnet.
abline(2.2,0, col = "red")

detach(oecd)