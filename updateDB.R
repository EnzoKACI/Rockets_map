library(httr)
library(jsonlite)
library(tidyverse)

len <- nchar(getwd())
if (substr(getwd(),len-4,len) != "/data"){
  setwd(dir = "data/")
}

load("updatedRocketsDB.RData")


url = "https://ll.thespacedevs.com/2.2.0/launch/upcoming/?limit=7000"


x <- GET(url)
apiresponse <- fromJSON(rawToChar(x$content))


data <- apiresponse
updateUpcoming <- tibble("NAME" = as.character(data$results$rocket$configuration$name), "MISSION" = as.character(data$results$mission$name),"MISSION_TYPE" = as.character(data$results$mission$type), "ORBIT" = as.character(data$results$mission$orbit$name), "NET_DATE" = parse_datetime(as.character(data$results$net)),"LATITUDE" = as.numeric(as.character(data$results$pad$latitude)), "LONGITUDE" = as.numeric(as.character(data$results$pad$longitude)), "LAUNCH_LOCATION" = as.character(data$results$pad$location$name),  "LAUNCHER_NAME"=as.character(data$results$pad$name), "ID" = as.character(data$results$id))


upcomingRocketsDF <- rows_upsert(updateUpcoming, upcomingRocketsDF, by="ID")

url = "https://ll.thespacedevs.com/2.2.0/launch/previous/?limit=7000"

x <- GET(url)
apiresponse <- fromJSON(rawToChar(x$content))


data <- apiresponse

updatePrevious <- tibble("NAME" = as.character(data$results$rocket$configuration$name), "MISSION" = as.character(data$results$mission$name),"MISSION_TYPE" = as.character(data$results$mission$type), "ORBIT" = as.character(data$results$mission$orbit$name), "NET_DATE" = as.Date(as.character(data$results$net), format = "%Y-%m-%dT%H:%M:%SZ"),"LATITUDE" = as.numeric(as.character(data$results$pad$latitude)), "LONGITUDE" = as.numeric(as.character(data$results$pad$longitude)), "LAUNCH_LOCATION" = as.character(data$results$pad$location$name),  "LAUNCHER_NAME"=as.character(data$results$pad$name), "LAUNCH_RESULT" = as.character(data$results$status$abbrev), "ID" = as.character(data$results$id))

previousRocketsDF <- rows_upsert(x=updatePrevious, y=previousRocketsDF, by="ID")

save(upcomingRocketsDF, previousRocketsDF, file = "updatedRocketsDB.RData")
