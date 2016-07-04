library('sp')
library('dplyr')
library('magrittr')
library('ggmap')
library('leaflet')
erum_data <- readRDS("data/erum_data.rds")

# data preparation and geocoding
cities <- erum_data %>% group_by(City) %>% summarise(count = n())
cities <- cities %>% as.data.frame() %>%
        mutate_geocode(City)

poznan <- cities[cities$City == 'Poznań', ]
cities_rest <- cities[!(cities$City == 'Poznań'), ]

cities_all <- cbind(cities_rest, poznan)
names(cities_all) <- c('City', 'count', 'lon', 'lat', 'Poznan', 'Poznan_count', 'Poznan_lon', 'Poznan_lat')

# static point map
p <- ggmap(get_map('Poznań', zoom = 4)) +
        geom_point(data = cities_rest, aes(x = lon, y = lat), color = "red", size = 1, shape = 3) +
        geom_point(data = poznan, aes(x = lon, y = lat), color = "blue", size = 4, shape = 18)
p 

# interactive point map
picture <- 'http://spotkania-entuzjastow-r.weebly.com/uploads/3/2/2/9/32290495/681824454.png'
content <- paste(sep = " ",
                 paste0("<img src = ", picture, " width=42>"),
                 "<b><a href='http://erum.ue.poznan.pl'>european R users meeting 2016</a></b>")

leaflet() %>% addCircles(data=cities_rest, lng = cities_rest$lon , lat = cities_rest$lat) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addMarkers(data=poznan, popup=content)

# raw list to Lines objects
row.names(cities_all) <- 1:(nrow(cities_all))
cities_all_lines <- vector("list", nrow(cities_all))
for (i in seq_along(cities_all_lines)) {
        cities_all_lines[[i]] <- Lines(list(Line(rbind(cities_all[i, 3:4], cities_all[i, 7:8]))), as.character(i))
}

lines_to_poznan <- SpatialLines(cities_all_lines)
lines_to_poznan <- SpatialLinesDataFrame(lines_to_poznan, cities_rest, match.ID = FALSE)

# static line map
p <- ggmap(get_map('Poznań', zoom = 4)) +
        geom_point(data = cities_rest, aes(x = lon, y = lat), color = "red", size = 1, shape = 3) +
        geom_point(data = poznan, aes(x = lon, y = lat), color = "blue", size = 4, shape = 18) + 
        geom_leg(data = cities_all, aes(x = lon, y = lat, xend = Poznan_lon, yend = Poznan_lat), color= 'blue')
p 

# inteactive line map
leaflet() %>% addPolylines(data=lines_to_poznan, weight=sqrt(lines_to_poznan$count)+1) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPopups(data=poznan, popup=content,
                  options = popupOptions(closeButton = TRUE))
