#' This is a package that interacts with OpenStreetmaps API
#'  
#'  
#' @param place (string)
#' @return latitude and longitude
#' @export
#' @import tidyverse
#' @import magrittr
#' @import leaflet
#' @import rvest
#' @import knitr
#' @import dplyr
#' @import httr
#' @import jsonlite
#' @import shiny





# place<-"Lund"  #place input!


#function to capture latitude and longitude of place
latlong<-function(place)
{
  place <- gsub(" ", "+", place) # replace space with + to retrieve the location
  z<-list(address=place) 
  url <- "https://nominatim.openstreetmap.org/search?q="
  get_coord = paste0(url, place, "&format=geojson")

  response <- read_html(get_coord) %>%
    html_node("p") %>%
    html_text() %>%
    fromJSON()
  
  lon <- response$features$geometry$coordinates[[1]][1]
  lat <- response$features$geometry$coordinates[[1]][2]
  
  list("latitude"=lat, "longitude" = lon)
}

