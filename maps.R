library(rMaps)
library(plyr)
library(dplyr)
library(rCharts)
library(RColorBrewer)
source("helper.R")

#Fatalities
fatalities <- read.csv("stormdata.csv")
fatalities <- fatalities[fatalities$Event == "Fatalities",]
fatalities <- select(fatalities, State, Year, Value)
fatalities$State <- as.character(fatalities$State)
fatalities <- mutate(fatalities, fillKey = ifelse(is.na(Value) == TRUE, "No info",
                                           ifelse(Value <=  10, "0-10",
                                           ifelse(Value <= 50, "11-50",
                                           ifelse(Value <= 100, "51-100",
                                           ifelse(Value <= 500, "101-500", ">500"))))))

fatalities$fillKey <- ordered(fatalities$fillKey, levels = c("0-10",
                                                             "11-50",
                                                             "51-100",
                                                             "101-500",
                                                             ">500",
                                                             "No info"))

fatalities$Value <- prettyNum(fatalities$Value, big.mark = ",")
i1 <- ichoropleth2(Value ~ State, data = fatalities, pal="PuRd", animate= "Year")
#i1
#i3$show("iframesrc", cdn = TRUE)
i1$addParams(height = 350, width = 510)
i1$save('fatalities2.html', cdn = TRUE)
#i1

"geographyConfig": {
  "popupTemplate":  function(geography, data){
    return '<div class=hoverinfo><strong>' + geography.properties.name + 
      ': ' + data.value + '</strong></div>';
  }  
},


# Injuries
injuries <- read.csv("stormdata.csv")
injuries <- injuries[injuries$Event == "Injuries",]
injuries <- select(injuries, State, Year, Value)
injuries$State <- as.character(injuries$State)
injuries <- mutate(injuries, fillKey = ifelse(is.na(Value) == TRUE, "No info",
                                           ifelse(Value <=  10, "0-10",
                                           ifelse(Value <= 50, "11-50",
                                           ifelse(Value <= 100, "51-100",
                                           ifelse(Value <= 500, "101-500", ">500"))))))

injuries$fillKey <- ordered(injuries$fillKey, levels = c("0-10",
                                                             "11-50",
                                                             "51-100",
                                                             "101-500",
                                                             ">500",
                                                             "No info"))

injuries$Value <- prettyNum(injuries$Value, big.mark = ",")
i2 <- ichoropleth2(Value ~ State, data = injuries, pal="PuRd", animate= "Year")
#i2
#i2$show("iframesrc", cdn = TRUE)
i2$addParams(height = 350, width = 510)
i2$save('injuries.html', cdn = TRUE)
#i2


#Crop
crop <- read.csv("stormdata.csv")
crop <- crop[crop$Event == "Crop",]
crop <- select(crop, State, Year, Value)
crop$State <- as.character(crop$State)

crop <- mutate(crop, fillKey = ifelse(is.na(Value) == TRUE, "No info",
                               ifelse(Value <= 1000000, "0-1 M$",
                               ifelse(Value <= 10000000, "1-10 M$",
                               ifelse(Value <= 100000000, "10-100 M$",
                               ifelse(Value <= 1000000000, "100-1,000 M$", ">1,000 M$"))))))
                                                              



crop$fillKey <- ordered(crop$fillKey, levels = c("0-1 M$",
                                                 "1-10 M$",
                                                 "10-100 M$",
                                                 "100-1,000 M$",">1,000 M$",
                                                 "No info"))

crop$Value <- prettyNum(crop$Value, big.mark = ",")

i3 <- ichoropleth2(Value ~ State, data = crop, pal="PuRd", animate= "Year")

#i3$show("iframesrc", cdn = TRUE)
i3$addParams(height = 350, width = 510)
i3$save('crop.html', cdn = TRUE)
#i3


#Property
property <- read.csv("stormdata.csv")
property <- property[property$Event == "Property",]
property <- select(property, State, Year, Value)
property$State <- as.character(property$State)

property <- mutate(property, fillKey = ifelse(is.na(Value) == TRUE, "No info",
                                       ifelse(Value <= 1000000, "0-1 M$",
                                       ifelse(Value <= 10000000, "1-10 M$",
                                       ifelse(Value <= 100000000, "10-100 M$",
                                       ifelse(Value <= 1000000000, "100-1,000 M$", ">1,000 M$"))))))


property$fillKey <- ordered(property$fillKey, levels = c("0-1 M$",
                                                 "1-10 M$",
                                                 "10-100 M$",
                                                 "100-1,000 M$",">1,000 M$",
                                                 "No info"))
property$Value <- prettyNum(property$Value, big.mark = ",")

i4 <- ichoropleth2(Value ~ State, data = property, pal="PuRd", animate= "Year")

#i4$show("iframesrc", cdn = TRUE)
i4$addParams(height = 350, width = 510)
i4$save('property.html', cdn = TRUE)
#i4
