# This file is available at https://ebmgt.github.io/copyright_case_studies/
# Author: rbadgett@kumc.edu
# Permission: GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
# Last edited 2024-09-13

# Notes:
# 2024-09-13: removed multiplier by 100 as this was already done in the "Citations over time.../R" file
library(tcltk)  # Required for tk_select.list

# Function to calculate and print summary based on selected comparison
comparison.groups <- tk_select.list(c(
    'PHQ9 versus MBI',
    'PHQ9 versus MMSE',
    'SLUMS versus MBI',
    'SLUMS versus MMSE', 
    'APGAR versus MBI', 
    'APGAR versus MMSE', 
    'CAGE versus MBI', 
    'CAGE versus MMSE'), 
preselect = 'SLUMS versus MMSE', multiple = FALSE,
                                    title = "\n\nWhat comparison are we studying?\n\n")

# Define coefficient, standard error,variables (years already adjusted for by the prior script)
coefficients <- list(
  PHQ9 = c(4.341326 ,  0.07800662),
  SLUMS = c(0.004021742 ,  0.0009746716),
  MBI = c(-0.5965183 ,  0.1799254),
  MMSE = c(0.008086851, 0.04590435),
  APGAR = c(0.1463098 ,  0.02121964),
  CAGE = c( 0.00622469 ,  0.006303041)
)

# Parse var1 and var2 from comparison.groups
selected_comparison <- unlist(strsplit(comparison.groups, " versus "))
var1 <- selected_comparison[1]
var2 <- selected_comparison[2]

# Assign coefficients and standard errors based on var1 and var2
coeff_var1 <- coefficients[[var1]][1]
se_var1 <- coefficients[[var1]][2]
coeff_var2 <- coefficients[[var2]][1]
se_var2 <- coefficients[[var2]][2]

# Function to print summary calculations
function_print <- function(var1, var2, coeff_var1, se_var1, coeff_var2, se_var2) {
  # Z value
  diff_coeff <- coeff_var1 - coeff_var2
  se_diff <- sqrt(se_var1^2 + se_var2^2)
  z_value <- diff_coeff / se_diff
  
  # P-value
  p_value <- 2 * pnorm(-abs(z_value))
  
  # Calculate confidence intervals
  ci_lower_var1 <- coeff_var1 - (1.96 * se_var1)  # 95% CI for var1
  ci_upper_var1 <- coeff_var1 + (1.96 * se_var1)
  
  ci_lower_var2 <- coeff_var2 - (1.96 * se_var2)  # 95% CI for var2
  ci_upper_var2 <- coeff_var2 + (1.96 * se_var2)
  
  # Print results
  cat(paste("Summary for", var1, "and", var2, "comparison:\n"))
  cat(paste("95% CI for", var1, "coefficient:", sprintf("%.2f", coeff_var1*1), " per 1 topic citations per year (95% CI:", round(ci_lower_var1 * 1, 2), "to", round(ci_upper_var1 * 1, 2), ")\n"))
  cat(paste("95% CI for", var2, "coefficient:", sprintf("%.2f", coeff_var2*1), " per 1 topic citations per year (95% CI:", round(ci_lower_var2 * 1, 2), "to", round(ci_upper_var2 * 1, 2), ")\n"))
  cat(paste("Z value:", round(z_value, 3), "\n"))
  cat(paste("P-value:", sprintf("%.3f", p_value), "\n"))
}

# Call function_print with selected variables
function_print(var1, var2, coeff_var1, se_var1, coeff_var2, se_var2)
