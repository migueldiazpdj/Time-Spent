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

#Read CSV file
poverty <- read.csv("~/Downloads/pip_dataset.csv", header=TRUE)
View(poverty)

#Transform dataset and filter desired columns and rows
poverty<- poverty %>%
  filter(year %in% 2018,
         welfare_type %in% "income",
         ppp_version %in% 2011)
poverty[poverty=="United States"]<- ("USA")
View(poverty)

poverty_gap <- poverty%>%
  select(country, headcount_ratio_upper_mid_income_povline)
poverty_gap <- poverty_gap %>%
  mutate(headcount_ratio_upper_mid_income_povline = round(headcount_ratio_upper_mid_income_povline,2))
View(poverty_gap)
colnames(poverty_gap) <- c("region", "value")

        

additional_data <- data.frame(
  region = c("Somalia", "Congo Democratic Republic", "South Sudan", "Burundi", "Central African Republic", 
             "Zambia", "Rwanda", "Niger","Tanzania","Turkmenistan", "Uganda", "Zimbabwe", "United Arab Emirates",
             "China","India", "South Africa", "Canada", "Morocco", "Indonesia", "Algeria", "Myanmar", "Malaysia",
             "Bangladesh", "Thailand", "Vietnam", "Pakistan", "South Korea", "Nigeria", "Mauritania", "Senegal",
             "Namibia", "Botswana", "Kazakhstan", "Mongolia", "Venezuela","Ukraine", "Iraq", "Nepal", "Sudan",
             "Chad", "Ghana"),
  value = c(70.73, 69.69, 67.33, 65.13, 61.88, 61.35, 52.01, 80.63, 44.95, 43.12, 42.73, 34.21, 0, 0.43, 11.09,
            20.48, 0.25, 1.44, 5.42,6.42, 68.2, 4.82, 86.90, 13.15, 22.24, 84.53, 1.23, 80,66.76,74,57.32, 63.51,
            14.29, 38.35, 38.23, 7.1, 26.38, 80.40, 86.2, 86.42, 78.5)
)
poverty_gap <- rbind(poverty_gap, additional_data)


#Sort by poverty rate
poverty_top <- arrange(poverty_gap, desc(value))
View(poverty_top)

#ggmap of data

world <- map_data('world') %>%
  filter(region != "Antarctica") %>% fortify
ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region), fill = "white",color = "darkgrey", linewidth = 0.5) +
  geom_map(data = poverty_gap, map = world,
           aes(fill = value, map_id = region),color = "darkgrey", linewidth = 0.2) +
  coord_map("rectangular", lat0 = 0, xlim = c(-180, 180), ylim = c(-70, 90)) +
  scale_fill_continuous(low = "#F5F5DC", high = "red4", guide = "colorbar")+
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="Legend", title="Poverty Percentage by Country (2018)", subtitle="Less Than $6.25 per day") +
  theme(plot.title = element_text(size=20, face = "bold", hjust=0.5))+
  theme_fivethirtyeight()

ggsave("Poverty_Percentage.jpg", width=3840, height= 2160, units=c("px"))
write_csv(poverty_gap, "poverty_gap.csv")
write_csv(poverty_top, "poverty_top.csv")
