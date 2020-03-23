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

