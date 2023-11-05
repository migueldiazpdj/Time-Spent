# Load the ggplot2 library
library(ggplot2)
library(extrafont)
# Create a data frame from the provided data
data <- data.frame(
  Country = c("Netherlands", "Iceland", "Australia", "New Zealand", "Switzerland", "Norway", "Canada", "Denmark",
              "United Kingdom", "Austria", "Germany", "United States", "Ireland", "G7", "Japan", "Finland",
              "Sweden", "Israel", "OECD - Total", "Mexico", "Colombia", "Estonia", "Türkiye", "Euro area",
              "France", "European Union", "Slovenia", "Luxembourg", "Latvia", "Costa Rica", "Portugal",
              "Lithuania", "Poland", "Korea", "Belgium", "Hungary", "Czech Republic", "Russia", "Spain", "Chile",
              "Slovak Republic", "Italy", "Greece", "South Africa"),
  Employment_Rate = c(77.05, 69.35, 65.18, 62.81, 61.39, 59.04, 57.80, 56.27, 54.38, 53.83, 51.35, 51.28, 48.84, 48.23, 47.85, 47.02, 46.37,
                      43.84, 43.75, 41.95, 37.45, 37.45, 36.97, 36.51, 35.41, 35.39, 33.08, 30.45, 30.23, 30.02, 29.23,
                      29.10, 28.18, 27.76, 26.90, 26.50, 25.63, 25.16, 23.78, 23.65, 21.82, 20.30, 18.25, 10.45)
)

# Highlight "Netherlands" and "Iceland" with different colors
highlighted_countries <- c("Spain", "United States")
data$Highlight <- ifelse(data$Country %in% highlighted_countries, "Highlighted", "Other")

# Create a ggplot
plot <- ggplot(data, aes(x = reorder(Country, Employment_Rate), y = Employment_Rate, fill = Highlight)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Employment Rate") +
  ggtitle("Employment Rate by Country (15-24 years-olds)") +
  scale_fill_manual(values = c("Highlighted" = c("#d62828"), "Other" = "#003049"))

# Rotate the x-axis labels for better readability
plot + theme_minimal() +
  theme(
    text = element_text(family = "Lucida Sans Unicode", size = 12),
    legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        plot.title = element_text(size = 16, hjust = 0.5))  

ggsave("EmplymentRate.jpg",width = 3840, height = 2160, units = c("px"))

spain_employment_rate <- data[data$Country == "Spain", "Employment_Rate"]
print(spain_employment_rate)

usa_employment_rate <- data[data$Country == "United States", "Employment_Rate"]
print(usa_employment_rate)