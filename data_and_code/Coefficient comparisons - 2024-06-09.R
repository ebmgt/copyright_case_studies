# PHQ-9 vs MMSE: ------
# MMSE spline
coeff_mmse <- -0.6836287
se_mmse <- 0.161361

# PHQ-9 spline
coeff_phq9 <- 25.83813
se_phq9 <- 0.4218007

# Calculate the lower and upper bounds of the 95% CI for MMSE
ci_lower_mmse <- coeff_mmse - (z_value * se_mmse)
ci_upper_mmse <- coeff_mmse + (z_value * se_mmse)
cat("95% CI for MMSE coefficient:", ci_lower_mmse, "to", ci_upper_mmse, "\n")

# Calculate the lower and upper bounds of the 95% CI for PHQ9
ci_lower_phq9 <- coeff_phq9 - (z_value * se_phq9)
ci_upper_phq9 <- coeff_phq9 + (z_value * se_phq9)
cat("95% CI for PHQ-9 coefficient:", ci_lower_phq9, "to", ci_upper_phq9, "\n")

# Difference in coefficients
diff_coeff <- coeff_mmse - coeff_phq9

# Standard error of the difference
se_diff <- sqrt(se_mmse^2 + se_phq9^2)

# Z value
z_value <- diff_coeff / se_diff

# P-value
p_value <- 2 * pnorm(-abs(z_value))

# Print results
print(paste("Z value:", z_value))
print(paste("P-value:", p_value))

#___________________----------
# PHQ-9 vs MBI: ---------------
coeff_mbi <- -3.145627
se_mbi <- 1.222504

# PHQ-9 spline
coeff_phq9 <- 25.83813
se_phq9 <- 0.4218007

# Calculate the lower and upper bounds of the 95% CI for MBI
ci_lower_mbi <- coeff_mbi - (z_value * se_mbi)
ci_upper_mbi <- coeff_mbi + (z_value * se_mbi)
cat("95% CI for MBI coefficient:", ci_lower_mbi, "to", ci_upper_mbi, "\n")
# Calculate the lower and upper bounds of the 95% CI for PHQ9
ci_lower_phq9 <- coeff_phq9 - (z_value * se_phq9)
ci_upper_phq9 <- coeff_phq9 + (z_value * se_phq9)
cat("95% CI for PHQ-9 coefficient:", ci_lower_phq9, "to", ci_upper_phq9, "\n")

# Difference in coefficients
diff_coeff <- coeff_mbi - coeff_phq9

# Standard error of the difference
se_diff <- sqrt(se_mbi^2 + se_phq9^2)

# Z value
z_value <- diff_coeff / se_diff

# P-value
p_value <- 2 * pnorm(-abs(z_value))

# Print results
print(paste("Z value:", z_value))
print(paste("P-value:", p_value))
