library(dplyr)
library(ggplot2)
library(gganimate)
library(reshape2)
library(ggthemes)
library(readxl)
library(tidyverse)
library(tidyr)
library(PupillometryR)

#Import Excel file
Time_Use<- read_excel("~/Desktop/UNAV/2023-2024/Statistics Project/Excel Files/Time-Use-in-OECD-Countries-OECD.xlsx")
colnames(Time_Use) <- c("Country", "Activity", "Minutes")
Time_Use <-Time_Use %>%
        mutate(Minutes = floor(Minutes))
Time_Use <-Time_Use %>%
  mutate(Hours = Minutes/60)
Time_Use <- Time_Use %>%
  mutate(Hours=round(Hours,1))
view(Time_Use)

#Flat Violin Graph of hours spent by Activity

custom_colors <- c("maroon1", "mediumorchid1", "plum", "rosybrown2", "seashell4","paleturquoise3", 
                   "mediumaquamarine","#85b22c", "olivedrab1",  "#FFFF00", "#FFCC00", "peru", "salmon", "tomato2")
ggplot(Time_Use, aes(x=Activity, y=Hours, fill=Activity))+
  geom_flat_violin(aes(fill=Activity),position=position_nudge(x=0,y=0),
                   adjust=2, trim=FALSE, colour=NA) +
  labs(x= "Activity", y="Hours", title= "Distribution of Time Spent per Day by Activity",
       subtitle= "Source: https://ourworldindata.org/time-use",
       size=14)+
  coord_flip() +
  theme_minimal()+
  scale_fill_manual(values=custom_colors)+
  theme(plot.title = element_text(size=20, face = "bold", hjust=0.5),
        axis.text.y = element_text(size=10))+
  stat_summary(geom = "point", fun = mean, shape = 19, size = 1, 
               aes(group = Activity))
ggsave("Flat_Violin.jpg", width=3840, height= 2160, units=c("px"))

#Creating GGplot with the specified file

custom_colors <- c("maroon1", "mediumorchid1", "plum", "rosybrown2", "seashell4","paleturquoise3", 
                   "mediumaquamarine","#85b22c", "olivedrab1",  "#FFFF00", "#FFCC00", "peru", "salmon", "tomato2")
ggplot(Time_Use, aes(x = Country, y = Hours, fill = Activity,label=Hours)) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  geom_bar(stat = "identity") +
  geom_text(size=3, position=position_stack(vjust=0.5))+
  labs(
    x = "Country",
    y = "Time (Hours)",
    title = "Time Use by Country (aged 15 to 64)",
    subtitle ="Source:https://ourworldindata.org/time-use",
    size=14,
    face="bold"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(size=20, face = "bold", hjust=0.5),
        axis.text.y = element_text(size=10))+
  coord_flip()

ggsave("World_Data.jpg", width=3840, height= 2160, units=c("px"))

# Widening data to create a more legible table and removing hours column
?pivot_wider
Time_Use_Wide <-Time_Use[,-4]
Time_Use_Wide <- pivot_wider(Time_Use_Wide, names_from = "Activity", 
                             values_from = "Minutes")
colnames(Time_Use_Wide)<- c("Country", "Work", "Education", "Care_For_Household_Members","Housework",
                             "Shopping", "Unpaid_Work/Volunteering", "Sleep","Eating", "Personal_Care",
                             "Sports","Attending_Events","Seeing_Friends","TV_&_Radio","Other_Leisure")

#Creating the statistics for the data (mean, median, mode, min, max and std)

Activity_mean_median <- Time_Use %>%
  select(-Country,-Minutes) %>%
  group_by(Activity) %>%
  summarise(Mean=mean(Hours), Max=max(Hours), Min=min(Hours), Median=median(Hours), Std=sd(Hours), Var= var(Hours))
Activity_mean_median <- Activity_mean_median %>%
  mutate(round(Activity_mean_median[,c(2:7)],2))
Activity_mean_median <- Activity_mean_median[order(Activity_mean_median$Mean,decreasing=TRUE),]
View(Activity_mean_median)


#Save all edited files as CSV
write.csv(Time_Use, file = "~/Desktop/Time_Use.csv", row.names = FALSE)
write.csv(Time_Use_Wide, file = "~/Desktop/Time_Use_Wide.csv", row.names = FALSE)
write.csv(Activity_mean_median, file = "~/Desktop/Activity_mean_median.csv", row.names = FALSE)
