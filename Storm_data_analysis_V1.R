setwd("E:/Data Science Foundations using R/5 Reproducible Research/Woche 4/Projekt_2")

library(ggplot2)
library(dplyr)
library(reshape2)

dataset <- download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2","E:/Data Science Foundations using R/5 Reproducible Research/Woche 4/storm_dataset")
dataset <- read.csv(bzfile("E:/Data Science Foundations using R/5 Reproducible Research/Woche 4/Projekt_2/storm_dataset"))



# some content about the data
head(dataset)
str(dataset)
names(dataset)



# Injuries sorting, top 10
total_injuries <- aggregate(INJURIES~EVTYPE, dataset, sum)
total_injuries <- arrange(total_injuries, desc(INJURIES))
total_injuries <- total_injuries[1:20, ]
total_injuries



# Total fatalities
total_fatalities <- aggregate(FATALITIES~EVTYPE,dataset, sum)
total_fatalities <- arrange(total_fatalities, desc(FATALITIES))
total_fatalities <- total_fatalities[1:20, ]
total_fatalities
str(total_fatalities)



# Results: Weather events, highest fatalities, top 10
totals <- total_fatalities
par(mfrow = c(1, 2), mar = c(15, 4, 3, 2), mgp = c(3, 1, 0), cex = 0.8)
barplot(totals$FATALITIES, las = 3, names.arg = totals$EVTYPE, main = "Weather events, highest fatalities, top 10", ylab = "fatalities", col = totals$FATALITIES)



# Sad stuff - top 5
totals<- merge(total_fatalities, total_injuries, by.x = "EVTYPE", by.y = "EVTYPE")
totals<-arrange(totals,desc(FATALITIES+INJURIES))

sad_stuff <- melt(totals, id.vars="EVTYPE", variable.name = "bad_thing")
tail(sad_stuff, 5)



# Results: Top 10 destroyer
healthChart <- ggplot(sad_stuff, aes(x=reorder(EVTYPE, -value), y=value))
healthChart = healthChart + geom_bar(stat="identity", aes(fill=bad_thing), position="dodge")
healthChart = healthChart + xlab("types of events") 
healthChart = healthChart + theme(axis.text.x = element_text(angle=45, hjust=1))
healthChart = healthChart + ggtitle("Top 10 destroyer") + theme(plot.title = element_text(hjust = 0.5))
healthChart



# Event type - sorting PROPDMG
propdmg <- aggregate(PROPDMG ~ EVTYPE, data = dataset, FUN = sum)
propdmg <- propdmg[order(propdmg$PROPDMG, decreasing = TRUE), ]
propdmgMax <- propdmg[1:10, ]
print(propdmgMax)



# Event type - sorting CROPDMG
cropdmg <- aggregate(CROPDMG ~ EVTYPE, data = dataset, FUN = sum)
cropdmg <- cropdmg[order(cropdmg$CROPDMG, decreasing = TRUE), ]
cropdmgMax <- cropdmg[1:10, ]
print(cropdmgMax)



# Greatest property damages & greatest crop damages, top10
par(mfrow = c(1, 2), mar = c(15, 4, 3, 2), mgp = c(3, 1, 0), cex = 0.8)

options(scipen = 999)

barplot(propdmgMax$PROPDMG, las = 3, names.arg = propdmgMax$EVTYPE, 
        main = "Greatest property damages, top 10", 
        ylab = "injuries", col = propdmgMax$PROPDMG)

barplot(cropdmgMax$CROPDMG, las = 3, names.arg = cropdmgMax$EVTYPE, 
        main = "Greatest crop damages, top 10", 
        ylab = "injuries", col = cropdmgMax$CROPDMG)



# Top 5 Event-Type - Damage Type ="PROPDMG"
totalDamage<- merge(propdmgMax,cropdmgMax,by.x = "EVTYPE", by.y = "EVTYPE")
totalDamage<-arrange(totalDamage,desc(PROPDMG + CROPDMG))
top10damages <- melt(totalDamage, id.vars="EVTYPE", variable.name = "Damage_Types")
head(top10damages, 5)



# Highest economic consequences, top 10
DamageChart <- ggplot(top10damages, aes(x=reorder(EVTYPE, -value/100000), y=value/100000))
DamageChart = DamageChart + geom_bar(stat="identity", aes(fill=Damage_Types), position="dodge")
DamageChart = DamageChart + xlab("Types of events") +ylab("Costs of damage in $ (billions)")
DamageChart = DamageChart + theme(axis.text.x = element_text(angle=45, hjust=1))
DamageChart = DamageChart + ggtitle("Highest economic consequences, top 10") + theme(plot.title = element_text(hjust = 0.5))
DamageChart



# Top 5 Event-Type - Damage Type ="TOTALDMG"
totalDamage<- merge(propdmgMax,cropdmgMax,by.x = "EVTYPE", by.y = "EVTYPE")
totalDamage$TOTALDMG <- totalDamage$PROPDMG + totalDamage$CROPDMG
totalDamage<-arrange(totalDamage,desc(TOTALDMG))
top10damages <- melt(totalDamage, id.vars="EVTYPE", variable.name = "Damage_Types")
tail(top10damages, 5)



# Highest economic consequences, top 10
DamageChart <- ggplot(top10damages, aes(x=reorder(EVTYPE, -value/1000), y=value/1000),fill=Damage_Types)
DamageChart = DamageChart + geom_bar(stat="identity", aes(fill=Damage_Types), position="dodge")
DamageChart = DamageChart + xlab("Types of events") + ylab("Costs of damage in $ (billions)")
DamageChart = DamageChart + theme(axis.text.x = element_text(angle=45, hjust=1))
DamageChart = DamageChart + ggtitle("Highest economic consequences, top 10") + theme(plot.title = element_text(hjust = 0.5))
DamageChart



# END