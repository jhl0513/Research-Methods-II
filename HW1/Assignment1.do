cd "/Users/jhl2226/data"

* Load the data
import delimited "assignment1-research-methods.csv", delimiter(tab) clear


* Perform the OLS regression to measure the effect of attending an elite school
regress calledback eliteschoolcandidate

ssc install estout

* Output the regression results into a publication-quality table
esttab using "regression_results_ols.rtf", replace
