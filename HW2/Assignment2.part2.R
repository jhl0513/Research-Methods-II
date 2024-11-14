# Load required libraries
install.packages(c("readxl", "fixest", "stargazer", "ggplot2", "car"))
library(readxl)
library(fixest)
library(stargazer)
library(ggplot2)
library(car)

# 1. Load the data
data <- read.csv("assignment_2.csv")

# Convert variables to appropriate types
data$State.Id <- as.factor(data$State.Id)
data$Year <- as.numeric(data$Year)

# 2. Define the treatment group: States that implemented the ban from 2021 onwards
data$Treatment <- ave(data$Vaping.Ban, data$State.Id, FUN = max)

# 3. Create a post-2021 indicator variable
data$Post2021 <- ifelse(data$Year >= 2021, 1, 0)

# 4. Interaction model to test parallel trends and estimate treatment effect
dnd_model <- lm(Lung.Hospitalizations ~ Year * Treatment + Post2021 * Treatment, data = data)
summary(dnd_model)

# 5. Create a Difference-in-Difference (DnD) plot
ggplot(data, aes(x = Year, y = Lung.Hospitalizations, color = as.factor(Treatment), group = State.Id)) +
  geom_line(alpha = 0.7) +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "red") +
  labs(title = "Difference-in-Difference Analysis of Lung Hospitalizations",
       subtitle = "Testing Parallel Trends Before 2021 and Divergence After 2021",
       x = "Year",
       y = "Number of Lung Hospitalizations",
       color = "Treatment Group") +
  theme_minimal()

# 6. Output the regression results to a publication-quality table
stargazer(dnd_model,
          type = "text",
          title = "Difference-in-Difference Regression Results with Parallel Trends Test",
          out = "DnD_Results_Parallel_Trends.txt",
          covariate.labels = c("Year", "Treatment", "Year * Treatment", "Post-2021 * Treatment"),
          dep.var.labels = "Lung Hospitalizations",
          notes = "The model tests for parallel trends before 2021 and estimates the treatment effect after 2021.")


