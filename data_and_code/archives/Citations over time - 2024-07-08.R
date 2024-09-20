# This file is available at https://github.com/ebmgt/pending/
# Author: rbadgett@kumc.edu
# Permission: GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
# Last edited 2024-06-09

# If Rstudio
if (Sys.getenv("RSTUDIO") != "1"){
  tk_messageBox(type = "ok", paste('1. ', 'Working directory:\n', getwd(), sepo=''), caption = paste("Hello and good",daypart))
}else{
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  #ScriptsDir <- paste(getwd(),'/Scripts',sep='')
}

# Functions ------
function_libraries_install <- function(packages){
  install.packages(setdiff(packages, rownames(installed.packages())), repos = "https://cloud.r-project.org/")
  for(package_name in packages)
  {
    library(package_name, character.only=TRUE, quietly = FALSE);
    cat('Installing package: ', package_name)
  }
}

function_plot_print <- function (plotname, plotheight, plotwidth){
  plotname <- gsub(":|\\s|\\n|\\?|\\!|\\'", "", plotname)
  rstudioapi::savePlotAsImage(
    paste(plotname,' -- ',current.date(),'.png',sep=''),
    format = "png", width = plotwidth, height = plotheight)
}

current.date <- function(){
  return (as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE)))
}

# Packages/libraries -----
packages_essential <- c("tcltk",'stringr','openxlsx','readr')
function_libraries_install(packages_essential)
packages_splines <- c('splines')
function_libraries_install(packages_splines)
packages_dplyr <- c('dplyr')
function_libraries_install(packages_dplyr)

# ____________________ ------
# Parameters ------
main_title <- NULL
filename <- NULL
y_label <- NULL

#topic <- 'MMSE'
#topic <- 'SLUMS'
#topic <- 'MBI'
#topic <- 'PHQ-9'
test_name <- tk_select.list(c('MMSE','SLUMS','MBI','PHQ-9'), preselect = 'PHQ-9', multiple = FALSE,
                                    title = "\n\nWhat comparison are we studying?\n\n")
destination <- "Journal" # "Other" "Journal" 

if (test_name == 'MMSE'){
  #* MMSE and dementia ------
  filename <- "PubMed_Timeline_Results_by_Year - ‘Mini-mental status’[tiab] - 'dementia'[tiab] - medline - FINAL - 2024-06-11.csv"
  main_title <- paste0("Citations per year with ",
                       "'mini-mental status'[tiab]\n",
                       "in 'dementia' and MEDLINE since 1990")
  topic_search_terms <- "'dementia'[TIAB]"
  factor <- 100
  note <- paste0("2001: PAR\u2021 acquires rights to the MMSE\u2020\n",
                 "2004: NIH drafts Public Access Policy\n",
                 "2006: NIH Public Access Policy implemented\n",
                 "2007: VA seeks alternatives to MMSE\n")
  note1 <- "MMSE. Mini-mental status examinataion."
  note2 <- "PAR, Inc. Psychological Assessment Resources."
}
if (test_name == 'SLUMS'){
  #* SLUMS and dementia ------
  filename <- "PubMed_Timeline_Results_by_Year - ‘Saint Louis University Mental Status’[tiab] - 'dementia'[tiab] - medline - FINAL - 2024-07-04.csv"
  main_title <- paste0("Citations per year with ",
                       "'Saint Louis University Mental Status'[tiab]\n",
                       "in 'dementia' and MEDLINE since 1990")
  topic_search_terms <- "'dementia'[TIAB]"
  factor <- 100
  note <- paste0("2006: SLUMS*. Saint Louis University\npublishes and releases the SLUMS*\n")
  note1 <- "SLUMS. Saint Louis University Mental Status."
  note2 <- ""
}
if (test_name == 'MBI'){
  #* MBI and burnout ------
  filename <- "PubMed_Timeline_Results_by_Year - ‘Maslach burnout’[tiab] - 'burnout'[tiab] - medline - FINAL - 2024-06-11.csv"
  main_title <- paste0("Citations per year with ",
                       "'Maslach burnout'[tiab]\n",
                       "in 'burnout' and MEDLINE since 1990")
  topic_search_terms <- "'Maslach Burnout'[TIAB]"
  factor <- 100
  note <- paste0("1996: MGI\u2020 acquires rights to the MBI.\n",
                 "2004: NIH drafts Public Access Policy\n",
                 "2006: NIH Public Access Policy implemented")
  note1 <- "MBI. Maslach Burnout Inventory."
  note2 <- "MGI Mind Garden, Inc."
}

if (test_name == 'PHQ-9'){
  #* PHQ-9 and depression ------
  filename <- "PubMed_Timeline_Results_by_Year - ‘PHQ-9’[tiab] - 'depressive disorder'[mesh] - medline - FINAL - 2024-06-11.csv"
  main_title <- paste0("Citations per year with 'PHQ-9'[tiab]\n",
                       "in 'depressive disorders' and MEDLINE since 1990")
  topic_search_terms <- "'depressive disorder'[MeSH]"
  factor <- 100
  note <- paste0("The PHQ-9 was published in 1999.\nPfizer\u2021 owns the copyright to the PHQ-9 and in\n",
                 " 2005 creates phqscreeners.com to host free access.")
  note1 <- "PHQ-9. Patient Health Questionnaire."
  note2 <- "Pfizer Inc."
}

# For journal publication
if (destination == "Journal"){main_title <- ""}

# ____________________ ------
# GET DATA -----
#data.medline <- read_csv("PubMed_Timeline_Results_by_Year - 'mini-mental status'[tiab] - 2024-06-08.csv")
# Denominator: burnout + medline
data.medline <- read_csv(filename)

data.medline <- data.medline[data.medline$Year < 2023, ]

data.medline$Survey.rate <- factor * data.medline$survey_topic / data.medline$topic_medline

# Spline analysis ------
# Initialize variables to store the best model and its adjusted R-squared
best_adj_r_squared <- -Inf
best_df <- NULL
best_model <- NULL

# Initialize the data.summary data frame
rm(list = "data.summary")
data.summary <- data.frame(df = integer(), knots_interior = integer(), adj_r_squared = numeric(), 
                           AIC = numeric(), BIC = numeric(), MSE = numeric(), 
                           P_value_versus_one_less_knot = numeric(), 
                           P_value_versus_0_knots = numeric(),
                           biggest_knot = integer(), 
                           biggest_knot_slope_before = numeric(), 
                           biggest_knot_slope_after = numeric(), 
                           biggest_knot_slope_change = numeric(), 
                           first_negative_slope_year = integer(), stringsAsFactors = FALSE)

# Loop over different degrees of freedom to fit spline models
# BIG LOOP START -----
df <- 1
for (df in 1:10) {  # Start from 1 to include the linear model
  # Little loop start start -----
  if (df == 1) {
    # Fit the initial linear model (no knots)
    model <- lm(Survey.rate ~ Year, data = data.medline)  # Linear model
    initial_model <- model
    initial_adj_r_squared <- summary(initial_model)$adj.r.squared
    adj_r_squared <- initial_adj_r_squared
    # P_value_versus_one_less_knot will come from the following later
    p_value_initial_model <- summary(initial_model)$coefficients[2,4]
    P_value_versus_one_less_knot <- p_value_initial_model
    P_value_versus_0_knots <- NA
    knots <- numeric(0)
    biggest_knot <- NA
    biggest_knot_slope_before <- NA
    biggest_knot_slope_after <- NA
    most_important_slope_change <- NA
    first_negative_slope_year <- NA
  } else {
    # Fit the spline model using natural splines
    model <- lm(Survey.rate ~ ns(Year, df = df), data = data.medline)
    knots <- attr(ns(data.medline$Year, df = df), "knots")
    
    # Calculate first derivatives at each knot
    if (length(knots) > 0) {
      first_derivatives <- numeric(length(knots))
      biggest_knot_slope_before <- numeric(length(knots))
      biggest_knot_slope_after <- numeric(length(knots))
      first_negative_slope_year <- NA
      for (i in seq_along(knots)) {
        #if (i==1){stop()}
        knot <- knots[i]
        delta <- 1 # was 0.01 until 2024-07-08
        # Not sure about following line helpful
        slope_before <- (predict(model, newdata = data.frame(Year = knot)) - 
                           predict(model, newdata = data.frame(Year = knot - delta))) / delta
        slope_after <- (predict(model, newdata = data.frame(Year = knot + delta)) - 
                          predict(model, newdata = data.frame(Year = knot))) / delta
        # Store the slopes
        slopes_before[i] <- slope_before
        slopes_after[i] <- slope_after
        # Calculate the change in slope
        first_derivatives[i] <- slope_after - slope_before
        
        # Check for the first negative slope
        if (is.na(first_negative_slope_year) && slope_after < 0) {
          first_negative_slope_year <- round(knot)
        }
      }
      
      # Identify the knot with the largest change in slope
      max_slope_change_index <- which.max(abs(first_derivatives))
      biggest_knot <- round(knots[max_slope_change_index])
      biggest_knot_slope_before <- biggest_knot_slopes_before[max_slope_change_index]
      biggest_knot_slope_after <- biggest_knot_slopes_after[max_slope_change_index]
      most_important_slope_change <- first_derivatives[max_slope_change_index]
      
      # Ensure first_negative_slope_year is NA if all slopes are positive
      if (all(biggest_knot_slopes_after >= 0)) {
        first_negative_slope_year <- NA
      }
    } else {
      biggest_knot <- NA
      most_important_slope_change <- NA
      biggest_knot_slope_before <- NA
      biggest_knot_slope_after <- NA
      first_negative_slope_year <- NA
    }
    
    # Perform ANOVA to compare the model with the linear model
    anova_result_0_knots <- anova(initial_model, model)
    P_value_versus_0_knots <- anova_result_0_knots$`Pr(>F)`[2]
  }
  # Little loop start end -----
  
  # Get the adjusted R-squared value
  adj_r_squared <- summary(model)$adj.r.squared
  
  # Get AIC, BIC, and MSE
  aic <- AIC(model)
  bic <- BIC(model)
  mse <- mean(model$residuals^2)
  
  # Perform ANOVA to compare models if df > 0
  if (df > 1) {
    anova_result <- anova(previous_model, model)
    P_value_versus_one_less_knot <- anova_result$`Pr(>F)`[2]  # P-value for the comparison with one less knot
  } else {
    P_value_versus_one_less_knot <- p_value_initial_model  # No comparison for the initial linear model
  }
  
  # Update the previous model for the next comparison
  # Append the results to the data.summary data frame

  data.summary <- rbind(data.summary, data.frame(df = df, 
       knots_interior = df - 1, 
       adj_r_squared = adj_r_squared, 
       AIC = aic, BIC = bic, MSE = mse, 
       P_value_versus_one_less_knot = P_value_versus_one_less_knot, 
       P_value_versus_0_knots = P_value_versus_0_knots, 
       biggest_knot = biggest_knot, 
       biggest_knot_slope_before = biggest_knot_slope_before, 
       biggest_knot_slope_after = biggest_knot_slope_after, 
       biggest_knot_slope_change = most_important_slope_change, 
       first_negative_slope_year = first_negative_slope_year)
       )
  previous_model <- model
}
# BIG LOOP END -----
# ____________________ ------

# Format the p-values and other metrics to avoid scientific notation
data.summary$P_value_versus_one_less_knot <- as.numeric(data.summary$P_value_versus_one_less_knot)
data.summary$P_value_versus_0_knots <- as.numeric(data.summary$P_value_versus_0_knots)
data.summary$adj_r_squared <- round(data.summary$adj_r_squared, 3)
data.summary$biggest_knot_slope_before <- round(data.summary$biggest_knot_slope_before, 4)
data.summary$biggest_knot_slope_after <- round(data.summary$biggest_knot_slope_after, 4)
data.summary$biggest_knot_slope_change <- round(data.summary$biggest_knot_slope_change, 4)

P_value_versus_0_knots <- ifelse(P_value_versus_0_knots < 0.001, "<0.001", format(round(P_value_versus_0_knots, digits = 3), nsmall = 3))

# Determine the best model -----
#* Is anything significant? -----
if (min(data.summary$P_value_versus_one_less_knot[1:10],na.rm = TRUE) > 0.5){
  print ("No significant rows!")
  stop()
}
#* Based on adjusted R-squared, AIC, and BIC -----
# Filter out rows where P_value_versus_one_less_knot is NA
#data.summary <- data.summary %>% filter(!is.na(P_value_versus_one_less_knot))

#* Based on the last sig P-value compared to one less knot -----
#** Linear is sig? ----- 
if (data.summary$P_value_versus_one_less_knot[1] < 0.05){
  best_df <- which(data.summary$P_value_versus_one_less_knot[1:10] >= 0.05)[1] - 1
}else{
#** Linear is insig?-----
  best_df <- which(data.summary$P_value_versus_one_less_knot[2:10] >= 0.05)[1] # No minus 1 as already removed the first row
}

if (is.na(best_df) || best_df < 1) {
  best_df <- 1  # Fallback to the first model if no significant row found
}

best_model_row <- data.summary[best_df, ]

best_model <- lm(Survey.rate ~ ns(Year, df = best_df), data = data.medline)
best_model_summary <- summary(best_model)
data.summary[data.summary$df==best_df,]
p_value_overall <- best_model_summary$coefficients[nrow(best_model_summary$coefficients),4]
p_value_overall <- sprintf("%.3f", p_value_overall)

best_adj_r_squared <- best_model_row$adj_r_squared

# Check if this model has the best adjusted R-squared so far
if (adj_r_squared > best_adj_r_squared) {
  # But this value is not used
  best_adj_r_squared <- adj_r_squared
  #best_df <- df
  #best_model <- model
}
best_adj_r_squared <- sprintf("%.3f", best_adj_r_squared)

# Extract key knots from the best model -----
#* Best knot ------
if (best_df > 1) {
  knotS_interior_best <- attr(ns(data.medline$Year, df = best_df), "knots")
} else {
  knotS_interior_best <- NA
}

#*  Final knot and penultimate knot -----
if (best_df == 1) { # Linear model case
  penultimate_knot <- min(data.medline$Year)
} else {
  #penultimate_knot <- knotS_interior_best[length(knotS_interior_best)]
  penultimate_knot <- knotS_interior_best[best_df - 1]
} 
best_model_knot <- best_model_row$biggest_knot

# Nonlinearity test -----
best_model_p_value_versus_0_knots <- as.numeric(best_model_row$P_value_versus_0_knots)
best_model_p_value_versus_0_knots <- ifelse(best_model_p_value_versus_0_knots < 0.001, "<0.001", format(round(best_model_p_value_versus_0_knots, digits = 3), nsmall = 3))

# ____________________ -----
# Slope of final segment -----
final_year <- 2022

# *Using coefficient of final segment: -----
coef_summary <- coef(summary(best_model))
num_coeffs <- nrow(coef_summary)  # Number of coefficients
final_slope <- coef_summary[num_coeffs, "Estimate"]
final_slope_se <- coef_summary[num_coeffs, "Std. Error"]
adjusted_final_slope  <- final_slope / (final_year- penultimate_knot)
adjusted_final_slope_se  <- final_slope_se / (final_year- penultimate_knot)

# Calculate the 95% confidence interval for the slope
ci_lower_slope_final_segment <- adjusted_final_slope - 1.96 * adjusted_final_slope_se
ci_upper_slope_final_segment <- adjusted_final_slope + 1.96 * adjusted_final_slope_se

#* Double check with predicted locations of surrounding knots -----
# Predict the values at the penultimate knot and the final year
predict_at_penultimate_knot <- predict(best_model, newdata = data.frame(Year = penultimate_knot))
predict_at_final_year <- predict(best_model, newdata = data.frame(Year = final_year))
predicted_final_slope  <- (predict_at_final_year - predict_at_penultimate_knot) / (final_year - penultimate_knot)

# _____________________ ------
# PLOT with the data points ------
top_margin_add <- 0
bottom_margin_add <- 3
if (destination == "Journal"){
  bottom_margin_add = 3 # normally 5.1
  top_margin_add = -3.6
  main_title <- ""
}
par(mar = c(5.1 + bottom_margin_add, 4.1, 4.1 + top_margin_add, 2.1), mfrow = c(1, 1)) # (bottom, left, top, right)

plot(data.medline$Year, data.medline$Survey.rate, type = "p", 
     ylim = c(0, 1.5*max(data.medline$Survey.rate)),
     main = main_title,
     xlab = "Year",
     ylab =  paste0(test_name, "* Citations per ", factor," topic\u2020 citations"))

xmin <- par("usr")[1] + strwidth("A")
xmax <- par("usr")[2] - strwidth("A")
ymin <- par("usr")[3] + 1.2*strheight("A")
ymax <- par("usr")[4] - strheight("A")

# Add fitted spline from the best model
lines(data.medline$Year, fitted(best_model), col = "blue", lwd = 2)

# Add the linear regression line
lines(data.medline$Year, fitted(initial_model), col = "black", lwd = 2, lty = 2)

# Extract the knots from the best model
if (best_df > 1) {
  knots <- attr(ns(data.medline$Year, df = best_df), "knots")
  # Plot the knots
  points(knots, predict(best_model, newdata = data.frame(Year = knots)), pch = 16, col = "red")
  # Add a dashed vertical line at the most important knot
  abline(v = best_model_knot, col = "red", lty = 2)
}

#* Add bold "Notes" ------
text(x = min(data.medline$Year), 
     y = 1.5*max(data.medline$Survey.rate), 
     adj = c(0, 1), 
     labels = "Notes:", 
     cex = 0.8, font = 2)

# Add the rest of the upper notes
notes_upper <- ''
if (best_df > 1){
  notes_upper = paste0(notes_upper,
                       "\u2022 P (nonlinear): ", p_value_overall, "\n",
                       "\u2022 R\u00B2 (Optimal spline regression, ", best_df - 1, " knots): R\u00B2 = ", best_adj_r_squared, "\n",
                       #"\u2022 P (ANOVA compared to one less knot): ", best_model_p_value_versus_0_knots, "\n",
                       "\u2022 Year with largest slope change: ", best_model_knot, "\n",
                       "\u2022 Slope of final segment: ",  round(adjusted_final_slope,3), " citations/year\n"
                       #,"\u2022 First negative slope year: ", ifelse(is.na(best_model_row$first_negative_slope_year), "NA", best_model_row$first_negative_slope_year)
  )
}else{
  notes_upper = paste0(notes_upper,
                       "\u2022 P (linear): ", p_value_overall, " (non-linear is insignificant)\n",
                       "\u2022 R\u00B2 (linear): = ", round(initial_adj_r_squared, 3), "\n",
                       "\u2022 Slope: ",  round(adjusted_final_slope,3), " citations/year\n"
  )
}

text(x = min(data.medline$Year), 
     y = 1.5*max(data.medline$Survey.rate) - 0.06 * max(data.medline$Survey.rate), 
     adj = c(0, 1), 
     labels = notes_upper, 
     cex = 0.8, font = 1)

# Add additional text
num_lines <- length(unlist(gregexpr("\n", note))) # + 1 - 2

text(x = ifelse(test_name=='PHQ-9', 1999, pmax(best_model_knot, (xmin + xmax)/2, na.rm = TRUE) + 1), 
     y = ifelse(test_name=='PHQ-9', 14, min(num_lines*strheight('A'),100)), # num_lines*strheight('A') #max(data.medline$Survey.rate),  
     labels = note,
     adj = c(0, 1), # 0 for left/bottom, 1 for right/top, and 0.5 for centered
     cex = 0.8, font = 1)

#* Add footnotes -----
mtext("Footnotes:", side = 1, line = 4, adj = 0, cex = 0.8, font = 2)
mtext(paste0("* ",note1), side = 1, line = 5, adj = 0, cex = 0.8)
mtext(paste0("\u2020 Topic search terms at PubMed.gov: ", topic_search_terms, " AND MEDLINE[SB]"), test_name, side = 1, line = 6, adj = 0, cex = 0.8)
if (nchar(note2) > 1){mtext(paste0("\u2021 ",note2), side = 1, line = 7, adj = 0, cex = 0.8)}

#* Print plot-----
function_plot_print(filename, 600, 800)

# _____________________ ------
# Save data.summary-----
write_csv (data.summary, paste0(test_name,' - search - data.summary - ', current.date(),'.csv'))

# Print data.summary ------
print(data.summary)

# Print slopes summary------
cat('Best model, final spline:
Calculated slope from final coefficient/year: ', adjusted_final_slope, 'se: ' , adjusted_final_slope_se, '
Predicted slope from final coefficient/year: ', predicted_final_slope)

