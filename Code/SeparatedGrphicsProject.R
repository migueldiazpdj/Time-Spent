library(dplyr)
library(ggplot2)
library(reshape2)
library(extrafont)

#Graphing line plots of each variable in their own graph
ggplot(time_spent_long, aes(x = Age, y = Minutes, color = Activity)) +
  geom_line() +
  labs(x = "Age", y = "Minutes Spent") +
  scale_x_continuous(limits = c(15, max(time_spent_long$Age))) +
  facet_wrap(~Activity)

#Filtering for Alone. Making a table and a graph
Alone_Table <- time_spent_edited %>%
  summarize(Age,Alone)
Alone_Table
ggplot(time_spent_edited, aes(x=Age,y=Alone))+
  geom_line(size = 1.5, color = "#e03c31") +
  theme_minimal() +
  theme(
    text = element_text(family = "Lucida Sans Unicode", size = 12),
    axis.title = element_text(size = 15, color = "#e03c31"),
    legend.position = "none",  # Remove the legend
    plot.title = element_text(size = 24, color = "#e03c31", hjust = 0.5)  # Título grande en el centro
  ) +
  labs(
    x = "Age",
    y = "Hours Alone",
    title = "Time Alone"
  )
ggsave("Alone.jpg",width = 3840, height = 2160, units = c("px"))

#Filtering for With_Friends. Making a table and a graph
Friends_Table <- time_spent_edited %>%
  summarize(Age,Friends)
Friends_Table
ggplot(time_spent_edited, aes(x=Age,y=Friends))+
  geom_line(size = 1.5, color = "#f7ea48") +
  theme_minimal() +
  theme(
    text = element_text(family = "Lucida Sans Unicode", size = 12),
    axis.title = element_text(size = 15, color = "#f7ea48"),
    legend.position = "none",  # Remove the legend
    plot.title = element_text(size = 24, color = "#f7ea48", hjust = 0.5)  # Título grande en el centro
  ) +
  labs(
    x = "Age",
    y = "Hours with Friends",
    title = "Time with Friends"
  )
ggsave("Friends.jpg",width = 3840, height = 2160, units = c("px"))

#Filtering for With_Children. Making a table and a graph
Children_Table <- time_spent_edited %>%
  summarize(Age,Children)
Children_Table
ggplot(time_spent_edited, aes(x=Age,y=Children))+
  geom_line(size = 1.5, color = "#2dc84d") +
  theme_minimal() +
  theme(
    text = element_text(family = "Lucida Sans Unicode", size = 12),
    axis.title = element_text(size = 15, color = "#2dc84d"),
    legend.position = "none",  # Remove the legend
    plot.title = element_text(size = 24, color = "#2dc84d", hjust = 0.5)  # Título grande en el centro
  ) +
  labs(
    x = "Age",
    y = "Hours with Children",
    title = "Time with your Children"
  )
ggsave("Children.jpg",width = 3840, height = 2160, units = c("px"))

mean(time_spent_edited$Children)
median(time_spent_edited$Children)

#Filtering for With_Immediate_Family. Making a table and a graph
Family_Table <-time_spent_edited %>%
  summarize(Age,Family)
Family_Table
ggplot(time_spent_edited, aes(x=Age,y=Family))+
  geom_line()
ggplot(time_spent_edited, aes(x=Age,y=Family))+
  geom_line(size = 1.5, color = "#90e0ef") +
  theme_minimal() +
  theme(
    text = element_text(family = "Lucida Sans Unicode", size = 12),
    axis.title = element_text(size = 15, color = "#90e0ef"),
    legend.position = "none",  # Remove the legend
    plot.title = element_text(size = 24, color = "#90e0ef", hjust = 0.5)  # Título grande en el centro
  ) +
  labs(
    x = "Age",
    y = "Hours with Family",
    title = "Time with your Family"
  )

ggsave("Family.jpg",width = 3840, height = 2160, units = c("px"))

#Filtering for With_Partner. Making a table and a graph
Partner_Table <-time_spent_edited %>%
  summarize(Age,Partner)
Partner_Table
ggplot(time_spent_edited, aes(x=Age,y=Partner))+
  geom_line()
ggplot(time_spent_edited, aes(x=Age,y=Partner))+
  geom_line(size = 1.5, color = "#147bd1") +
  theme_minimal() +
  theme(
    text = element_text(family = "Lucida Sans Unicode", size = 12),
    axis.title = element_text(size = 15, color = "#147bd1"),
    legend.position = "none",  # Remove the legend
    plot.title = element_text(size = 24, color = "#147bd1", hjust = 0.5)  # Título grande en el centro
  ) +
  labs(
    x = "Age",
    y = "Hours with Patner",
    title = "Time with your Partner"
  )
ggsave("Partner.jpg",width = 3840, height = 2160, units = c("px"))

#Filtering for Coworker. Making a table and a graph
Partner_Table <-time_spent_edited %>%
  summarize(Age,Coworkers)
Partner_Table
ggplot(time_spent_edited, aes(x=Age,y=Coworkers))+
  geom_line()
ggplot(time_spent_edited, aes(x=Age,y=Coworkers))+
  geom_line(size = 1.5, color = "#FF00FF") +
  theme_minimal() +
  theme(
    text = element_text(family = "Lucida Sans Unicode", size = 12),
    axis.title = element_text(size = 15, color = "#FF00FF"),
    legend.position = "none",  # Remove the legend
    plot.title = element_text(size = 24, color = "#FF00FF", hjust = 0.5)  # Título grande en el centro
  ) +
  labs(
    x = "Age",
    y = "Hours with Coworkers",
    title = "Time with your Coworkers"
  )
ggsave("Coworker.jpg",width = 3840, height = 2160, units = c("px"))