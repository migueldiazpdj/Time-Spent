library(dplyr)
library(ggplot2)
library(gganimate)
library(reshape2)
library(ggthemes)
library(readxl)
library(tidyverse)
library(tidyr)
library(maps)
library(mapproj)

#Import Annual Working Hours and Clean the Data
annual_working_hours <- read.csv("~/Downloads/annual-working-hours-per-worker.csv")
annual_working_hours <- annual_working_hours[,-2]
colnames(annual_working_hours) <- c("Country", "Year","Hours")
annual_working_hours <- annual_working_hours %>%
  mutate(Hours=round(Hours,0))

annual_top<- annual_working_hours %>%
  filter(Year %in% max(Year))
annual_top <- annual_top[,-2]
annual_top <- arrange(annual_top, desc(Hours))
View(annual_top)

ggplot(annual_working_hours, aes(x= Year, y= Hours, color= Country))+
  geom_line()+
  xlim(1950,2020)

#Rounding data and creating a more readable table
annual_working_hours <- annual_working_hours %>%
  mutate(Hours=round(Hours,0))
annual_hours <- pivot_wider(annual_working_hours, names_from = "Country", 
                            values_from = "Hours")
annual_working_hours<- annual_working_hours %>%
  filter(Country %in% c("Spain", "France",
                     "Germany",  "United States",
                      "United Kingdom", "Japan", "Mexico", "India", "South Africa"))
view(annual_working_hours)

#Create map for data
colnames(annual_top) <- c("region", "hours")
annual_top[annual_top=="United States"]<- ("USA")
world <- map_data('world') %>%
  filter(region != "Antarctica") %>% fortify
ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region), fill = "white",color = "black", linewidth = 0.2) +
  geom_map(data = annual_top, map = world,
           aes(fill = hours, map_id = region),color = "black", linewidth = 0.2) +
  coord_map("rectangular", lat0 = 0, xlim = c(-180, 180), ylim = c(-70, 90)) +
  scale_fill_continuous(low = "#8cFFdb", high = "#003C62", guide = "colorbar")+
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="Legend", title="Annual Working Hours by Country (2017)") +
  theme(plot.title = element_text(size=20, face = "bold", hjust=0.5))+
  theme_fivethirtyeight()
ggsave("Map_Work_Hours.jpg", width=3840, height= 2160, units=c("px"))
write_csv(annual_top, "Work_Top.csv")

#Create the GGplot for the data
custom_colors <- c("royalblue2", "black", "pink","green3", "orange2", "brown", "gold1", "navy", "red")
ggplot(annual_working_hours, aes(x= Year, y= Hours, color= Country))+
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  geom_line(size=1)+
  geom_point(size=2)+
  labs(
    x = "Year",
    y = "Time (Hours)",
    title = "Average Annual Working Hours per Year by Country",
    subtitle ="Source:https://ourworldindata.org/time-use"
  ) +
  theme(plot.title = element_text(size=20, face = "bold", hjust=0.5))+
  xlim(1950,2020)+
  ylim(1250,2500)

  ggsave("Working_Hours.jpg", width=3840, height= 2160, units=c("px"))

view(annual_hours)

#save all edited files as CSV
write.csv(annual_hours, file = "~/Desktop/annual_hours.csv", row.names = FALSE)
write.csv(annual_working_hours, file = "~/Desktop/annual_working_hours.csv", row.names = FALSE)



