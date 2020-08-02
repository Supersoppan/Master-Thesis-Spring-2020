library(ggplot2)
library(maps)
library(rgdal)
library(sf)
library(tmap)

library(package = c("sf", "leaflet", "dplyr", "tmap", "tmaptools", "tidyverse"))
library("tidyverse")
library(dplyr)

library(broom)
library(excel)
library(tidyr)
library(readxl)

packages = "sf"
lapply(packages, library, character.only = TRUE) 


#install.packages("tidyr")
#install.packages(c("broom", "excel", "dplyr", 'ggplot2', 'maps', 'rgdal', 'sf', 'tmap', 'maps'))

#notis
"gör allt till read_excel och ladda alla paket i en variabel, blir snyggare"

#loading data----

bef = read_excel("kartor/befolkning.xlsx")
bef = bef[-1, ] #remove riket, since it´s not a municipality
bensin = read.csv("kartor\\bensin.csv", sep = ";")

sverige_map = read_sf("kartor\\Kommungränser_SCB_07.shp") #


#pre processing data----
bef = rename(bef, "KNKOD" = "Kommun")
for (i in 1:nrow(bef)){ #recodes kommunkod so that it can be also used by the map
  if (3 == nchar(bef$KNKOD[i])){
    bef$KNKOD[i] = paste("0", toString(bef$KNKOD[i]), sep="")
  }
}

bensin = separate(bensin, kod.och.namn, c("kod", "namn"), sep = " ", extra = "merge")


#end of loading and pre processing
bensin = rename(bensin, "KNKOD" = kod)
bensin1 = bensin[order(bensin$KNKOD),]
bef1 = bef[order(bef$KNKOD),]
my_map = merge(sverige_map, bensin, by = "KNKOD")
my_map = merge(sverige_map, bef, by = "KNKOD")
new_map = mutate(my_map, my_map$bensin/my_map$Levande)
newest = bef[order("KNKOD")]
test  = my_map[, "bensin"]/my_map[,"Levande"]
test = as_tibble(my_map)
my_map$Levande = as.numeric(my_map$Levande)


#merging CS and map
sverige_map$Municipality_name = sverige_map$KNNAMN
CS_map = merge(jjj, sverige_map, by = c("Municipality_name"), all.y = TRUE)
#takes in the values with positive CS change
index = CS_map$D.CS > 0
CS_map$D.CS[index] <- NA
#plotting
p = ggplot(CS_map) +
  geom_sf(data = CS_map$"geometry")+
  aes(fill = CS_map$D.CS) +
  labs(fill = "Change in \nConsumer surplus")
p  


#plotting for colourblind
p + scale_fill_continuous(type = "viridis")

#more plotting for colour blind
p + scale_fill_viridis_b()

    

p+ scale_fill_gradientn(colours = c("blue", "lightblue", "lightgreen", "green"))

,
  breaks=c(-6000, -4000, -2000, Inf))

summary(CS_map$D.CS)

ggplot()+
  geom_sf(data = my_map$geometry)+
  aes(fill = as.numeric(my_map$Levande)) +
  theme(legend.position = "none")+
  scale_fill_gradientn(colors = sf.colors(5))

ggplot()+
  geom_sf(data = my_map$geometry)+
  aes(fill = as.numeric(my_map$bensin)) +
  scale_fill_gradientn(colors = sf.colors(10))


#Trash----
=======
library(ggplot2)
library(maps)
library(rgdal)
library(sf)
library(tmap)

library(package = c("sf", "leaflet", "dplyr", "tmap", "tmaptools", "tidyverse"))
library("tidyverse")
library(dplyr)

library(broom)
library(excel)
library(tidyr)
#install.packages("tidyr")
#install.packages(c("broom", "excel", "dplyr", 'ggplot2', 'maps', 'rgdal', 'sf', 'tmap', 'maps'))
#loading data----

bef = read.csv("kartor\\befolkning.csv", sep =";")
bensin = read.csv("kartor\\bensin.csv", sep = ";")
sverige_map = read_sf("kartor\\Kommungränser_SCB_07.shp")

"testar förändringar"
#pre processing data----
bef = rename(bef, "KNKOD" = ?..Kommun)
for (i in 1:nrow(bef)){ #recodes kommunkod so that it can be also used by the map
  if (3 == nchar(bef$KNKOD[i])){
    bef$KNKOD[i] = paste("0", toString(bef$KNKOD[i]), sep="")
  }
}

bensin = separate(bensin, kod.och.namn, c("kod", "namn"), sep = " ", extra = "merge")


#end of loading and pre processing
bensin = rename(bensin, "KNKOD" = kod)
bensin1 = bensin[order(bensin$KNKOD),]
bef1 = bef[order(bef$KNKOD),]
my_map = merge(sverige_map, bensin, by = "KNKOD")
my_map = merge(my_map, bef, by = "KNKOD")
new_map = mutate(my_map, my_map$bensin/my_map$Levande)
newest = bef[order("KNKOD")]
test  = my_map[, "bensin"]/my_map[,"Levande"]
test = as_tibble(my_map)


ggplot()+
  geom_sf(data = my_map$geometry)+
  aes(fill = as.numeric(my_map$Levande)) +
  theme(legend.position = "none")+
  scale_fill_gradientn(colors = sf.colors(10))

ggplot()+
  geom_sf(data = my_map$geometry)+
  aes(fill = as.numeric(my_map$bensin)) +
  scale_fill_gradientn(colors = sf.colors(10))

>>>>>>> f30f336b317d6b4d39b6216f8bf3acd3c858e655
