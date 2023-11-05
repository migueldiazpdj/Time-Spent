library(dplyr)
library(ggplot2)
library(reshape2)
library(extrafont)

# Importing time_spent and cleaning the data (transforming to hours)
time_spent
time_spent_edited <- time_spent[,-c(1,2)]
colnames(time_spent_edited) <- c("Age","Alone","Friends","Children","Family","Partner","Coworkers")
time_spent_edited <- time_spent_edited %>%
  mutate(time_spent_edited[,c(2:7)]/60)
time_spent_edited <- time_spent_edited %>%
  mutate(round(time_spent_edited[,c(2:7)],2))
rownames(time_spent_edited) <- time_spent_edited$Age
View(time_spent_edited)

# Melting data to form the necessary column names and numbers
time_spent_long <- melt(time_spent_edited, id.vars = "Age", variable.name = "Activity", value.name = "Hours")

# Define custom colors for each activity (total of 6 colors)
custom_colors <- c("#e03c31", "#f7ea48","#2dc84d", "#90e0ef", "#147bd1", "#FF00FF")

# Create a faceted plot with custom line colors and modified titles
facet_plot <- ggplot(time_spent_long, aes(x = Age, y = Hours, color = Activity)) +
  geom_line(size = 1.75) +
  labs(x = "Age", y = "Hours Spent") +
  scale_x_continuous(limits = c(15, max(time_spent_long$Age))) +
  facet_wrap(~Activity, ncol = 3) +  # Display facets in 3 columns
  scale_color_manual(values = custom_colors) +  # Assign custom colors to each facet
  theme_minimal() +
  theme(
    text = element_text(family = "Lucida Sans Unicode", size = 12),
    axis.title = element_text(size = 15),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray"),
    strip.text = element_text(size = 17, color = "black"),
    strip.background = element_rect(size=0.8, linetype='solid'),
    legend.position = "none"  # Remove the legend
  )
ggsave("Facet_plot.jpg",width = 3840, height = 2160, units = c("px"))
# Print the faceted plot
print(facet_plot)

#Creating a line graph with each representation of time spend based on age
ggplot(time_spent_long, aes(x = Age, y = Hours, color = Activity)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = custom_colors) +
  labs(x = "Age", y = "Hours Spent") +
  scale_x_continuous(limits = c(15, max(time_spent_edited$Age))) +
  theme_minimal() +
  theme(
    text = element_text(family = "Lucida Sans Unicode", size = 12),
    axis.title = element_text(size = 12),
    plot.background = element_rect(fill = "white"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)  # Center the title horizontally
  ) +
  ggtitle("Time Spent during life")
ggsave("Time_spent.jpg",width = 3840, height = 2160, units = c("px"))
