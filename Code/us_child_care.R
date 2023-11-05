library(ggplot2)

#Data frame
data <- data.frame(
  Year = c(1965, 1975, 1985, 1998),
  Married_Fathers = c(17, 17, 26, 51),
  Married_Mothers = c(84, 67, 80, 99),
  Single_Mothers = c(59, 63, 67, 85)
)

# Line ggplot
ggplot(data, aes(x = Year)) +
  geom_line(aes(y = Married_Fathers, color = "Married Fathers"), size = 1) +
  geom_line(aes(y = Married_Mothers, color = "Married Mothers"), size = 1) +
  geom_line(aes(y = Single_Mothers, color = "Single Mothers"), size = 1) +
  geom_point(aes(y = Married_Fathers, color = "Married Fathers"), size=1.5)+
  geom_point(aes(y = Married_Mothers, color = "Married Mothers"), size = 1.5)+
  geom_point(aes(y = Single_Mothers, color = "Single Mothers"), size =1.5)+
  labs(title = "Child Care Time per day (with children age 18 or under)",
       x = "Year",
       y = "minutes") +
  scale_color_manual(
    values = c("Married Fathers" = "#00a6ed", "Married Mothers" = "#f6511d", "Single Mothers" = "#ffb400")) +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        plot.title = element_text(size = 20, hjust = 0.5), 
        legend.text = element_text(size = 18),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12)) 


ggsave("UsChildCare_plot.jpg",width = 3840, height = 2160, units = c("px"))


