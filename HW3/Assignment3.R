# install necessary packages 
install.packages("dplyr")
install.packages("gt")
install.packages("lmtest")
install.packages("sandwich")
install.packages("stargazer")
install.packages("kableExtra")


# Load necessary libraries
library(dplyr)
library(gt)
library(lmtest)   
library(sandwich) 
library(stargazer) 
library(kableExtra)

# Load the data
data <- read.csv("sports-and-education.csv")

#####2

# Perform t-tests for each variable
t_test_results <- data.frame(
  Variable = c("Academic Quality", "Athletic Quality", "Near Big Market"),
  Mean_Ranked = c(
    mean(data$Academic.Quality[data$Ranked.2017 == 1], na.rm = TRUE),
    mean(data$Athletic.Quality[data$Ranked.2017 == 1], na.rm = TRUE),
    mean(data$Near.Big.Market[data$Ranked.2017 == 1], na.rm = TRUE)
  ),
  Mean_NotRanked = c(
    mean(data$Academic.Quality[data$Ranked.2017 == 0], na.rm = TRUE),
    mean(data$Athletic.Quality[data$Ranked.2017 == 0], na.rm = TRUE),
    mean(data$Near.Big.Market[data$Ranked.2017 == 0], na.rm = TRUE)
  ),
  P_Value = c(
    t.test(data$Academic.Quality ~ data$Ranked.2017)$p.value,
    t.test(data$Athletic.Quality ~ data$Ranked.2017)$p.value,
    t.test(data$Near.Big.Market ~ data$Ranked.2017)$p.value
  )
)

# Add significance levels based on p-values
t_test_results <- t_test_results %>%
  mutate(
    Significance = case_when(
      P_Value < 0.001 ~ "***",
      P_Value < 0.01 ~ "**",
      P_Value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# Create a formatted table
t_test_results %>%
  kable("html", 
        col.names = c("Variable", "Mean (Ranked)", "Mean (Not Ranked)", "P-Value", "Significance"), 
        caption = "Balance Table with Statistical Tests for Ranked vs Not Ranked Colleges") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed")) %>%
  add_header_above(c(" " = 1, "Means" = 2, " " = 2))

#####4
# Fit a logistic regression model to predict "Ranked.2017"
propensity_model <- glm(
  Ranked.2017 ~ Academic.Quality + Athletic.Quality + Near.Big.Market, 
  data = data, 
  family = binomial(link = "logit")
)

# Create a table of the result of logistic regression
stargazer(propensity_model, 
          type = "html",        
          title = "Logistic Regression Results",
          dep.var.labels = c("Ranked in 2017"),
          covariate.labels = c("Academic Quality", "Athletic Quality", "Near Big Market"),
          out = "/Users/jhl2226/data/logistic_regression.html",
          align = TRUE)

# Predict the probability of treatment (being ranked) for each observation
data <- data %>%
  mutate(propensity_score = predict(propensity_model, type = "response"))

# View the first few rows of the updated dataset
head(data)

#####5
# Load necessary libraries
library(ggplot2)

# Plot the stacked histogram
ggplot(data, aes(x = propensity_score, fill = as.factor(Ranked.2017))) +
  geom_histogram(binwidth = 0.05, position = "stack", alpha = 0.7, color = "black") +
  scale_fill_manual(
    values = c("blue", "red"), 
    name = "Ranking Status",
    labels = c("Not Ranked", "Ranked")
  ) +
  labs(
    title = "Propensity Score Overlap Between Ranked and Not Ranked Schools",
    x = "Propensity Score",
    y = "Count"
  ) +
  theme_minimal()

####6
# Define the block size
block_size <- 0.1  

# Ensure the propensity score column exists
if (!"propensity_score" %in% colnames(data)) {
  stop("Propensity score column is missing. Please run the propensity score model first.")
}

# Define the common support region based on your earlier steps
data_common_support <- data %>%
  filter(propensity_score >= 0.30 & propensity_score <= 0.90)

# Group observations into blocks
data_common_support <- data_common_support %>%
  mutate(
    block = cut(
      propensity_score,
      breaks = seq(0, 1, by = block_size), 
      include.lowest = TRUE,
      labels = FALSE  
    )
  )

# Summarize the data by blocks
block_summary <- data_common_support %>%
  group_by(block) %>%
  summarise(
    count = n(),
    avg_propensity_score = mean(propensity_score, na.rm = TRUE),
    ranked_count = sum(Ranked.2017),
    not_ranked_count = sum(1 - Ranked.2017)
  )

data_common_support$block <- data_common_support$block - 3

#####7
# Run the regression model
model <- lm(
  Alumni.Donations.2018 ~ Ranked.2017 + Academic.Quality + Athletic.Quality + Near.Big.Market + factor(block),
  data = data_common_support
)

# Summary of the model with robust standard errors
summary_robust <- coeftest(model, vcov = vcovHC(model, type = "HC1"))

# Export regression table to an HTML file
stargazer(
  model,
  type = "html",
  title = "Treatment Effect of Being Ranked on Alumni Donations",
  dep.var.labels = "Alumni Donations (in $1,000s)",
  covariate.labels = c(
    "Ranked (2017)", 
    "Academic Quality", 
    "Athletic Quality", 
    "Near Big Market" 
  ),
  notes = "Robust standard errors are reported in parentheses. The dependent variable is alumni donations in $1,000s.",
  out = "/Users/jhl2226/data/regression_table.html"
)









