# Speed over time
halo_all_0 = halo_spatial[halo_spatial$Before == halo_spatial$After,]
halo_before_1 = halo_spatial[halo_spatial$Before == 1,]
halo_after_1 = halo_spatial[halo_spatial$After == 1,]

# Points

# g = ggplot(mapping = aes(x = Time_GMT, y = AveSpeed)) + 
#   geom_point(data = halo_all_0, colour = "green", alpha = (0.2))
# 
# o = ggplot(mapping = aes(x = Time_GMT, y = AveSpeed)) + 
#   geom_point(data = halo_before_1, colour = "orange", alpha = (0.2))
# 
# r = ggplot(mapping = aes(x = Time_GMT, y = AveSpeed)) +
#   geom_point(data = halo_after_1, color = "red", alpha = (0.2))
#   
# grid.arrange(g, o, r, nrow = 1, ncol = 3)

# AveSpeed

g = ggplot(mapping = aes(x = Time_GMT, y = AveSpeed)) + 
  geom_boxplot(data = halo_all_0, mapping= aes(group = cut_width(Time_GMT, 1800)), color = "green", alpha = (0.2)) + 
  ylim(0, 250) + 
  scale_x_continuous(breaks=c(0, 21600, 43200, 64800, 86399), labels = c("00:00:00", "06:00:00","12:00:00","18:00:00","23:59:59")) + 
  ggtitle("Farther than an Hour Before or After a \n Collision")
  

o = ggplot(mapping = aes(x = Time_GMT, y = AveSpeed)) + 
  geom_boxplot(data = halo_before_1, mapping= aes(group = cut_width(Time_GMT, 1800)), color = "orange", alpha = (0.2)) + 
  ylim(0, 250)  + 
  scale_x_continuous(breaks=c(0, 21600, 43200, 64800, 86399), labels = c("00:00:00", "06:00:00","12:00:00","18:00:00","23:59:59")) + 
  ggtitle("Within One Hour Before a Collision \n")

r = ggplot(mapping = aes(x = Time_GMT, y = AveSpeed)) +
  geom_boxplot(data = halo_after_1, mapping= aes(group = cut_width(Time_GMT, 1800)), color = "red", alpha = (0.2)) + 
  ylim(0, 250) + 
  scale_x_continuous(breaks=c(0, 21600, 43200, 64800, 86399), labels = c("00:00:00", "06:00:00","12:00:00","18:00:00","23:59:59")) + 
  ggtitle("Within One Hour After a Collision \n")

grid.arrange(g, o, r, nrow = 1, ncol = 3)

# AveOccupancy

g = ggplot(mapping = aes(x = Time_GMT, y = AveOccupancy)) + 
  geom_boxplot(data = halo_all_0, mapping= aes(group = cut_width(Time_GMT, 1800)), color = "green", alpha = (0.2)) + 
  ylim(0, 100) + 
  scale_x_continuous(breaks=c(0, 21600, 43200, 64800, 86399), labels = c("00:00:00", "06:00:00","12:00:00","18:00:00","23:59:59")) + 
  ggtitle("Farther than an Hour Before or After a \n Collision")


o = ggplot(mapping = aes(x = Time_GMT, y = AveOccupancy)) + 
  geom_boxplot(data = halo_before_1, mapping= aes(group = cut_width(Time_GMT, 1800)), color = "orange", alpha = (0.2)) + 
  ylim(0, 100)  + 
  scale_x_continuous(breaks=c(0, 21600, 43200, 64800, 86399), labels = c("00:00:00", "06:00:00","12:00:00","18:00:00","23:59:59")) + 
  ggtitle("Within One Hour Before a Collision \n")

r = ggplot(mapping = aes(x = Time_GMT, y = AveOccupancy)) +
  geom_boxplot(data = halo_after_1, mapping= aes(group = cut_width(Time_GMT, 1800)), color = "red", alpha = (0.2)) + 
  ylim(0, 100) + 
  scale_x_continuous(breaks=c(0, 21600, 43200, 64800, 86399), labels = c("00:00:00", "06:00:00","12:00:00","18:00:00","23:59:59")) + 
  ggtitle("Within One Hour After a Collision \n")

grid.arrange(g, o, r, nrow = 1, ncol = 3)

# AveHeadway

g = ggplot(mapping = aes(x = Time_GMT, y = AveHeadway)) + 
  geom_boxplot(data = halo_all_0, mapping= aes(group = cut_width(Time_GMT, 1800)), color = "green", alpha = (0.2)) + 
  ylim(0, 255) + 
  scale_x_continuous(breaks=c(0, 21600, 43200, 64800, 86399), labels = c("00:00:00", "06:00:00","12:00:00","18:00:00","23:59:59")) + 
  ggtitle("Farther than an Hour Before or After a \n Collision")


o = ggplot(mapping = aes(x = Time_GMT, y = AveHeadway)) + 
  geom_boxplot(data = halo_before_1, mapping= aes(group = cut_width(Time_GMT, 1800)), color = "orange", alpha = (0.2)) + 
  ylim(0, 255)  + 
  scale_x_continuous(breaks=c(0, 21600, 43200, 64800, 86399), labels = c("00:00:00", "06:00:00","12:00:00","18:00:00","23:59:59")) + 
  ggtitle("Within One Hour Before a Collision \n")

r = ggplot(mapping = aes(x = Time_GMT, y = AveHeadway)) +
  geom_boxplot(data = halo_after_1, mapping= aes(group = cut_width(Time_GMT, 1800)), color = "red", alpha = (0.2)) + 
  ylim(0, 255) + 
  scale_x_continuous(breaks=c(0, 21600, 43200, 64800, 86399), labels = c("00:00:00", "06:00:00","12:00:00","18:00:00","23:59:59")) + 
  ggtitle("Within One Hour After a Collision \n")

grid.arrange(g, o, r, nrow = 1, ncol = 3)

# Total Flow

g = ggplot(mapping = aes(x = Time_GMT, y = TotalFlow)) + 
  geom_boxplot(data = halo_all_0, mapping= aes(group = cut_width(Time_GMT, 1800)), color = "green", alpha = (0.2)) + 
  ylim(0, 175) + 
  scale_x_continuous(breaks=c(0, 21600, 43200, 64800, 86399), labels = c("00:00:00", "06:00:00","12:00:00","18:00:00","23:59:59")) + 
  ggtitle("Farther than an Hour Before or After a \n Collision")


o = ggplot(mapping = aes(x = Time_GMT, y = TotalFlow)) + 
  geom_boxplot(data = halo_before_1, mapping= aes(group = cut_width(Time_GMT, 1800)), color = "orange", alpha = (0.2)) + 
  ylim(0, 175)  + 
  scale_x_continuous(breaks=c(0, 21600, 43200, 64800, 86399), labels = c("00:00:00", "06:00:00","12:00:00","18:00:00","23:59:59")) + 
  ggtitle("Within One Hour Before a Collision \n")

r = ggplot(mapping = aes(x = Time_GMT, y = TotalFlow)) +
  geom_boxplot(data = halo_after_1, mapping= aes(group = cut_width(Time_GMT, 1800)), color = "red", alpha = (0.2)) + 
  ylim(0, 175) + 
  scale_x_continuous(breaks=c(0, 21600, 43200, 64800, 86399), labels = c("00:00:00", "06:00:00","12:00:00","18:00:00","23:59:59")) + 
  ggtitle("Within One Hour After a Collision \n")

grid.arrange(g, o, r, nrow = 1, ncol = 3)

## Webtris Locations not in MIDAS

MIDAS = osgb_sites[osgb_sites$`Geographic Address` %in% unique(halo_spatial$Geographic_Address),]

'%!in%' <- function(x,y)!('%in%'(x,y))

No_MIDAS = osgb_sites[osgb_sites$`Geographic Address` %!in% unique(halo_spatial$Geographic_Address),]

tm_shape(uk, bbox = st_bbox(c(xmin = 150000, xmax = 650000, ymax = 600000, ymin = 0), crs = st_crs(27700))) +
  tm_polygons(alpha = 0.1) +
  tm_shape(MIDAS) +
  tm_dots(col = "blue", alpha = 1, size = 0.5) + 
  tm_shape(No_MIDAS) +
  tm_dots(col = "red", alpha = 1, siz = 0.5) +
  tm_layout(title = "MIDAS Gold and WebTRIS Sites Inconsistency", main.title.size = 10)

inconsis_report = as.data.frame(unique(No_MIDAS$`Geographic Address`))

write.csv(inconsis_report, file = "D:/Documents/5872M-Dissertation/Data/Potential_Data_Issues/webTRIS_MIDAS_Gold_Inconsistency_Sites.csv", row.names=FALSE)


Faulty_Sites = MIDAS[MIDAS$`Geographic Address` %in% unique(all_missing$Geographic_Address),]

Working_Sites = MIDAS[MIDAS$`Geographic Address` %!in% unique(all_missing$Geographic_Address),]

tm_shape(uk, bbox = st_bbox(c(xmin = 150000, xmax = 650000, ymax = 600000, ymin = 0), crs = st_crs(27700))) +
  tm_polygons(alpha = 0.1) +
  tm_shape(Working_Sites) +
  tm_dots(col = "blue", alpha = 1, size = 0.5) + 
  tm_shape(Faulty_Sites) +
  tm_dots(col = "red", alpha = 1, siz = 0.5) +
  tm_layout(title = "Potentially Faulty MIDAS Gold Sites", main.title.size = 10)

# Generic Day

month = "2016-10-04" # Tuesday
Sites_accidents = unique(halo_spatial$Geographic_Address)

  
  rd = RoadData(startDate = month, endDate = month, tcdFileRoot = "D:/Documents/5872M-Dissertation/Data/Halogen_2016",Sites_accidents)
  
  # Make Dataframe
  
  df_list = lapply(seq(length(rd)),function(i){
    df = as.data.frame(rd[i], stringsAsFactors = FALSE)
  })
  
  head(do.call(bind_rows,df_list))
  
  df1 = as.data.frame(data.table::rbindlist(df_list, use.names=TRUE, fill=TRUE))
  
  csv_file = paste0("D:/Documents/5872M-Dissertation/Data/Average_Day/", month, ".csv")
  
  # Output to csv
  
  write.csv(df1, file = csv_file, row.names=FALSE)
  
  # Clean up Memory Usage
  
  rm(df1)
  rm(df_list)
  rm(rd)
  gc()



