library('googlesheets')
library('tidyverse')
library('ggmap')
library('ggplot2')
library('sp')

# gs_auth(new_user = TRUE)
# gap_url <- "https://docs.google.com/spreadsheets/d/1rVTUrcsiZLb-9FgGmZ1Yna_Yexu8E3gRkYNkhJpzc2c/edit#gid=321233721"
# sheet <- gap_url %>% 
#         gs_url() %>% 
#         gs_read(.) %>% 
#         mutate(City=trimws(City)) %>%
#         group_by(City) %>%
#         summarise(count = n()) %>% 
#         as.data.frame() %>% 
#         mutate_geocode(City)
# saveRDS(sheet, 'data/sheet_updated.rds')

sheet <- readRDS('data/sheet_updated.rds')

poznan <- sheet[sheet$City == 'Poznań', ]
cities_rest <- sheet[!(sheet$City == 'Poznań'), ]
cities_all <- cbind(cities_rest, poznan)

### lines prepare
row.names(cities_all) <- 1:(nrow(cities_all))
cities_all_lines <- vector("list", nrow(cities_all))
for (i in seq_along(cities_all_lines)) {
        cities_all_lines[[i]] <- Lines(list(Line(rbind(cities_all[i, 3:4], cities_all[i, 7:8]))), as.character(i))
}

lines_to_poznan <- SpatialLines(cities_all_lines)
lines_to_poznan <- SpatialLinesDataFrame(lines_to_poznan, cities_rest, match.ID = FALSE)
names(cities_all) <- c('City', 'count', 'lon', 'lat', 'Poznan', 'Poznan_count', 'Poznan_lon', 'Poznan_lat')

# static path map
poz_map <- get_map('Poznań', zoom = 4)
p <- ggmap(poz_map) +
        geom_point(data = cities_rest, aes(x = lon, y = lat), color = "black", size = 1, shape = 1) +
        geom_segment(data = cities_all, aes(x = lon, y = lat, xend = Poznan_lon, yend = Poznan_lat), size = 0.2, color = '#472b29') + 
        geom_point(data = poznan, aes(x = lon, y = lat), color = "black", size = 4, shape = 19) + 
        coord_map()
p

ggsave(p)