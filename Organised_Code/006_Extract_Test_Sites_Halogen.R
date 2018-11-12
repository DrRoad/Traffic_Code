# Extract Wanted Sites using oneminutetrafficdata package.

# stat19 = as.data.frame(readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Geometries/", "Stat19_2016_2km_Subset.csv")))
stat19 = as.data.frame(readr::read_csv(file.path("Data/Geometries/Stat19_2016_2km_Subset.csv")))

# Extract Severe Accidents on Main Roads, Not Near Complex Networks eg Junctions

severe = c(1,2)

stat19 = as.data.frame(stat19[stat19$Accident_Severity %in% severe & stat19$Road_Type == 3  & stat19$Junction_Detail == 0, ])

# Make Spatial

stat19_spatial = st_as_sf(stat19, coords = c("X", "Y"), crs = 27700)

stat19_buffer = st_buffer(stat19_spatial, 500)

#Extract Sites

sites = readr::read_csv(file.path("Data/Geometries/", "Site_Locations.csv"))

ac_sites = st_as_sf(sites, coords = c("Longitude", "Latitude"), crs = 4326)

osgb_sites = st_transform(ac_sites, crs = 27700)
mapview(osgb_sites)

osgb_sites = osgb_sites[stat19_buffer, ]
sites_subset = osgb_sites %>% 
  filter(grepl(pattern = "M621", `Geographic Address`))

summary(df1$geographic_address %in% sites_subset$`Geographic Address`)
summary(sites_subset$`Geographic Address` %in% df1$geographic_address)

Sites_accidents = unique(sites_subset$`Geographic Address`)
midas_sites = sites_subset
names(midas_sites) = snakecase::to_snake_case(names(midas_sites))
readr::write_csv(midas_sites, "midas_sites.csv")
sf::write_sf(midas_sites, "midas_sites.geojson")

# Extract Wanted Dates

dates = as.data.frame(unique(stat19_spatial$Date))
colnames(dates) = "Date"
dates_formatted = as.Date(dates[,1], "%d/%m/%Y")
month = dates_formatted[months(dates_formatted) == "December"]
month = as.character(month)

####################Remove after run

#'%!in%' <- function(x,y)!('%in%'(x,y)) # https://stackoverflow.com/questions/5831794/opposite-of-in
#days_done = c("2016-03-02",
#              "2016-03-06",
#               "2016-03-08",
#               "2016-03-11",
#               "2016-03-12",
#               "2016-03-13",
#               "2016-03-16",
#               "2016-03-19",
#               "2016-03-21",
#               "2016-03-26",
#               "2016-03-31")
# 
#month = month[month %!in% days_done]

###################################

# single month
ValidateArguments(month, month, sites = Sites_accidents)
# set month manually:
month_orig = month
month_start = "2017-12-01"
month_end = "2017-12-07"

rd = RoadData(startDate = month_start, endDate = month_end, tcdFileRoot = "Data/Original/Auto/", Sites_accidents)


class(rd)
str(rd)
df_list = lapply(seq(length(rd)),function(i){
  df = as.data.frame(rd[i], stringsAsFactors = FALSE)
})
df1 = as.data.frame(data.table::rbindlist(df_list, use.names=TRUE, fill=TRUE))
class(df1)

# clean-up names
names_clean = snakecase::to_snake_case(names(df1))
names(df1) = names_clean

# df1 = df1 %>% 
#   mutate_at(vars(matches("year|month")), as.numeric)
sapply(df1, class)
summary(df1)
unique(df1$geographic_address)
csv_file = paste0("Data/Original/Auto//", month_start, "--", month_end, ".csv")

summary(df1$average_speed_lane_1)
write_csv(df1, csv_file)
df1 = read_csv(csv_file)

for(j in seq(length(month))){
  
  
  rd = RoadData(startDate = month[j], endDate = month[j], tcdFileRoot = "Data/Original/Auto/", Sites_accidents)
  
  # Make Dataframe
  
  df_list = lapply(seq(length(rd)),function(i){
    df = as.data.frame(rd[i], stringsAsFactors = FALSE)
  })
  
  head(do.call(bind_rows,df_list))
  
  df1 = as.data.frame(data.table::rbindlist(df_list, use.names=TRUE, fill=TRUE))
  
  csv_file = paste0("Data/Original/Auto//", month[j], ".csv")
  
  # Output to csv
  
  write.csv(df1, file = csv_file, row.names=FALSE)
  
  # Clean up Memory Usage
  
  rm(df1)
  rm(df_list)
  rm(rd)
  gc()
}

# next-step: read-in .csvs - see
file.edit("Organised_Code/006_1_Load_Month_Of_Data.R")
