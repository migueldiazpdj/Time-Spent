library(dplyr)
library(ggplot2)
library(gganimate)
library(reshape2)
library(gapminder)
library(ggthemes)
library(readxl)



#Importing time_spent and cleaning the data
time_spent <- read_excel("~/Desktop/UNAV/2023-2024/Statistics Project/Excel Files/Country Times.xlsx")
View(time_spent)
time_spent_edited <- time_spent[,-1]
time_spent_edited
colnames(time_spent_edited) <-c("Age","Alone","Friends","Children","Family","Partner","Coworkers")
rownames(time_spent_edited) <- time_spent_edited$Age
time_spent_edited <- time_spent_edited %>%
  mutate(time_spent_edited[,2:7]/60)
time_spent_edited <- time_spent_edited %>%
  mutate(round(time_spent_edited[,2:7],2))
View(time_spent_edited)

# Melting data to form the necessary column names and numbers
time_spent_long <- melt(time_spent_edited, id.vars = "Age", variable.name = "Activity", value.name = "Hours")
View(time_spent_long)

#Creating a line graph with each representation of time spend based on age
custom_colors <- c("#85b22c", "olivedrab1",  "#FFFF00", "#FFCC00", "peru", "salmon")
ggplot(time_spent_long, aes(x = Age, y= Hours, color = Activity)) +
  geom_line(linewidth=1) +
  theme_minimal()+
  scale_color_manual(values = custom_colors) +
  labs(x = "Age", y = "Hours Spent") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))+
  ggtitle("Time Spent During Life")+
  scale_x_continuous(limits = c(15, 90))+
  geom_point()

linegraph_animation <- linegraph +
  transition_reveal(Age)+
  view_follow(fixed_y=TRUE)
linegraph_animation

animate(linegraph_animation, height=700, width= 1000, fps =33, duration= 10,
        end_pause=60, res=150)
anim_save("Time_Spent_Line.gif")

#Graphing line plots of each variable in their own graph
  ggplot(time_spent_long, aes(x = Age, y = Hours, color = Activity)) +
    geom_line() +
    labs(x = "Age", y = "Hours Spent") +
    scale_x_continuous(limits = c(15, max(time_spent_long$Age))) +
    facet_wrap(~Activity)
  
#Graphing histogram of each age based on Age
  graph1 <- ggplot(time_spent_long, aes(x = Activity, y = Hours,fill = Activity)) +
    geom_col(position=position_dodge(width=0.2)) +
    labs(title= "Time Spent During Life (in hours)") +
    scale_y_continuous(limits = 
                        c(0, max(time_spent_long$Hours)))+
    coord_flip() +
    theme_fivethirtyeight()+
  theme(axis.ticks = element_blank(),  
        legend.position = "none")

graph1.animation <- graph1 +
  transition_time(Age)+
  labs(subtitle="Age: {frame_time}")+
  xlab("Hours")+
  exit_fade()
graph1.animation

animate(graph1.animation, height=900, width= 1800, fps =30, duration= 35,
        end_pause=60, res=200)
anim_save("Time_Spent_Hist.gif")

#Creating a line graph with each representation of time spend based on age
custom_colors <- c("#FF0000", "#FFFF00","#00FF00", "#00FFFF", "#0000FF", "#FF00FF")
migueline <- ggplot(time_spent_long, aes(x = Age, y = Hours, color = Activity)) +
  geom_line(size = 1) +
  scale_color_manual(values = custom_colors) +
  labs(x = "Age", y = "Hours Spent") +
  scale_x_continuous(limits = c(15, max(time_spent_edited$Age),
                                breaks=0:100)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Georgia", size = 12),
    axis.title = element_text(size = 12),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)  
  ) +
  ggtitle("Time Spent During Life")+
  geom_point()

migueline_animation <- migueline +
  transition_reveal(Age)+
  view_follow(fixed_y=TRUE)
linegraph_animation

animate(migueline_animation, height=700, width= 1400, fps =30, duration= 10,
        end_pause=60, res=100)
anim_save("Time_Spent_Migueline.gif")
