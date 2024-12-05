#download
install.packages("dplyr")
install.packages("stargazer")
install.packages("ivreg")

# Load necessary libraries
library(dplyr)
library(stargazer)
library(ivreg)

# Load the data
data <- read.csv("crime.csv")

#4 
variables_to_test <- c("Severity.Of.Crime", "Months.In.Jail", "Recidivates")
balance_results <- data.frame(Variable = character(),
                              T_Statistic = numeric(),
                              P_Value = numeric(),
                              stringsAsFactors = FALSE)

for (var in variables_to_test) {
  # Perform t-test for the variable
  t_test <- t.test(data[[var]] ~ data$Republican.Judge)
  
  # Store results in the dataframe
  balance_results <- balance_results %>%
    add_row(Variable = var,
            T_Statistic = t_test$statistic,
            P_Value = t_test$p.value)
}

# Prepare balance test results for stargazer
balance_table <- balance_results %>%
  mutate(
    T_Statistic = round(T_Statistic, 3),
    P_Value = formatC(P_Value, format = "e", digits = 2)
  )

# Create a stargazer table
stargazer(
  balance_table,
  type = "text",  # Change to "latex" for LaTeX or "html" for HTML output
  title = "Balance Test Results",
  summary = FALSE,
  rownames = FALSE,
  out = "Balance_Test_Results_Table.txt"
)

#5

# First stage regression: Effect of Republican Judge on Months in Jail
first_stage_model <- lm(Months.In.Jail ~ Republican.Judge + Severity.Of.Crime, data = data)

# Create a HTML table for the first stage
stargazer(
  first_stage_model,
  type = "html",  # HTML output for easy viewing
  title = "First Stage Regression: Effect of Judge's Political Affiliation on Sentence Length",
  dep.var.labels = "Months in Jail",
  covariate.labels = c("Republican Judge", "Severity of Crime"),
  digits = 3,
  out = "First_Stage_Regression_Table.html"  # Output HTML file
)

#7

# Reduced Form 
reduced_form <- lm(Recidivates ~ Republican.Judge + Severity.Of.Crime, data = data)
summary(reduced_form)


#8

reduced_form$coefficients
first_stage_model$coefficients

0.1426641/3.221876

#9

# Perform the IV regression
iv_model <- ivreg(Recidivates ~ Months.In.Jail + Severity.Of.Crime | Republican.Judge + Severity.Of.Crime, data = data)

# Extract the first-stage F-statistic and p-value
diagnostics <- summary(iv_model)$diagnostics
f_stat <- diagnostics["Weak instruments", "statistic"]  
p_value <- diagnostics["Weak instruments", "p-value"]  

# Determine statistical significance stars based on the p-value
if (p_value < 0.001) {
  sig <- "***"
} else if (p_value < 0.01) {
  sig <- "**"
} else if (p_value < 0.05) {
  sig <- "*"
} else {
  sig <- ""
}

# Format the F-statistic with stars only
f_stat_label <- sprintf("First-stage F-statistic: %.2f%s", f_stat, sig)

# Create a stargazer table for the IV regression with the F-statistic
stargazer(
  iv_model,
  type = "html",  # Use "html" for an HTML file
  title = "Second Stage Regression: Effect of Sentence Length on Recidivism",
  dep.var.labels = "Recidivates",
  covariate.labels = c("Months in Jail", "Severity of Crime"),
  digits = 3,
  add.lines = list(c("Note", f_stat_label)),  # Add the F-statistic with stars as a note
  out = "Second_Stage_Regression_Result.html"  # Output file name
)