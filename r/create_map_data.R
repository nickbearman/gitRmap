#GOAL: process the list of cities for the Traveling GIS Chat Book to 
#   1. get lat/long coords
#   2. make a geojson file
#   3. make the javascript file the webmap needs.


# Set Up ------------------------------------------------------------------

# Libraries
library(tidygeocoder)
library(sf)
library(geojsonsf)


# read the data
locations<-read.csv("data/locations.csv")

# duplicate data
all_data <- locations
# add id to all_data
all_data$id<-c(1:dim(all_data)[[1]])

# only keep city, state, country, lat, long columns
locations <- locations[,1:5]

#add id column
#locations$id<-c(1:dim(locations)[[1]])



#  Geocode ---------------------------------------------------------------

# a table of rows that already have been geocoded
has_latlong<-locations[which(!is.na(locations$lat)),]

# a table of rows that need to be geocoded
to_geocode<-locations[which(is.na(locations$lat)), 1:3]

if (dim(to_geocode)[1]>0){ #if there is more than 0 rows to geocode
  # geocode the new rows
  new_latlong<-to_geocode %>% 
    geocode(city=city,
            state=state,
            country=country,
            method="osm"
    )
  # combine the two tables to make one list of all the cities with their lat/longs
  to_plot<-rbind(has_latlong, new_latlong)
  
}else{to_plot<-has_latlong}

#join extra columns back on

to_plot <- cbind(to_plot,all_data[,6:dim(locations)[[1]]])


# write the data back to the csv so don't need to geocode every line every time it updates
write.csv(to_plot, file="data/locations.csv", row.names = FALSE)


# Create GeoJSON & Javascript ---------------------------------------------

# turn the points into an SF object
map_points<-st_as_sf(to_plot, coords=c("long", "lat"))

# convert the SF object to GeoJSON
geojson_map_points<-sf_geojson(map_points)

# wrap the GeoJSON text in the javascript declaration Leaflet needs to read GeoJSON
js_map_points<-paste0(
  "var locations=",
  geojson_map_points
)

# write the text to the javascript file
file_connection<-file("docs/locations.js")
writeLines(text=js_map_points, con=file_connection)
close(file_connection)


# Write Coordinates to a File ---------------------------------------------

book_coords<-st_coordinates(map_points)

coords.js<-c()

for (i in 1:nrow(book_coords)){
  new.row<-paste0("[",book_coords[i,2], "," ,book_coords[i,1],"]")
  coords.js<-c(coords.js, new.row)
}

coords_to_write<-paste0(
  "var book_coords = [",
  paste(as.character(coords.js), sep=",", collapse=","),
  "]"
)

# write the text to the javascript file
file_connection<-file("docs/book_coords.js")
writeLines(text=coords_to_write, con=file_connection)
close(file_connection) 


# Make the Lines ----------------------------------------------------------

map_line_df<-as.data.frame(matrix(c("book travel path"), nrow = 1, ncol=1))
map_line_sfc<-st_linestring(st_coordinates(map_points))
map_line_df$geometry<-st_geometry(map_line_sfc)
map_line<-st_sf(map_line_df)

geojson_map_line<-sf_geojson(map_line)

# wrap the GeoJSON text in the javascript declaration Leaflet needs to read GeoJSON
js_map_line<-paste0(
  "var book_line=",
  geojson_map_line
)

# write the text to the javascript file
file_connection<-file("docs/book_line.js")
writeLines(text=js_map_line, con=file_connection)
close(file_connection)




