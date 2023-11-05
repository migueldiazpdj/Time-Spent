library(usmap)
library(ggplot2)
library(extrafont)

# Create a data frame with state names and average retirement age
retirement_data <- data.frame(
  state = c("District of Columbia", "Hawaii", "Massachusetts", "South Dakota", "Maryland", "Connecticut", "New Jersey", "Vermont", "Rhode Island", "New Hampshire",
            "Colorado", "Virginia", "North Dakota", "Minnesota", "Utah", "Nebraska", "Iowa", "Texas", "Kansas", "California",
            "New York", "Washington", "Pennsylvania", "Wisconsin", "Florida", "Montana", "Illinois", "Idaho", "Wyoming", "Tennessee",
            "Oregon", "Maine", "Nevada", "Delaware", "Arizona", "North Carolina", "South Carolina", "Ohio", "Indiana", "Missouri", "Georgia", "Mississippi",
            "Louisiana", "New Mexico", "Michigan", "Kentucky", "Alabama", "Oklahoma", "Arkansas", "Alaska", "West Virginia"),
  values = c(67, 66, 66, 66, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 63,
                             63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 62, 62, 62, 62, 62, 62, 62, 61, 61)
)

# Your existing code for creating the plot
Retirement_plot <- plot_usmap(data = retirement_data, color = "#0061ff") +
  scale_fill_continuous(
    low = "#60efff", high = "#0061ff", name = "", label = scales::comma
  ) + theme(legend.position = "right",
            legend.text = element_text(size = 16)) +
  labs(title = "Average Retirement Age in Every State")

# Change the title font
Retirement_plot + theme(
  plot.title = element_text(family = "Lucida Sans Unicode", size = 16, face = "bold", hjust = 0.5, vjust = - 10)
) + 
  annotate(geom = "text", x = 0.5, y = -0.5, label = "Mean: 63.86 and Median: 64", size = 5, hjust = -0.4, vjust = 30)

ggsave("Retirement_plot.jpg",width = 3840, height = 2160, units = c("px"))

# Mean and median of the plot:
Median <- median(retirement_data$values)
Median
Mean <- mean(retirement_data$values)
Mean


