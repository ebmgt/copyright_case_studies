# This file is available at https://github.com/ebmgt/copyright_case_studies/
# Author: rbadgett@kumc.edu
# Permission: GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
# Last edited 2024-06-09

version
citation(package = "base", lib.loc = NULL, auto = NULL)

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
  (current.date <- as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE)))
  rstudioapi::savePlotAsImage(
    paste(plotname,' -- ',current.date,'.png',sep=''),
    format = "png", width = plotwidth, height = plotheight)
}

# Packages/libraries -----
packages_essential <- c("tcltk",'stringr','openxlsx','readr')
function_libraries_install(packages_essential)
packages_splines <- c('splines')
function_libraries_install(packages_splines)
packages_dplyr <- c('dplyr')
function_libraries_install(packages_dplyr)


# Variables ------
main_title <- NULL
filename <- NULL
y_label <- NULL

# Parameters ------
topic <- 'MMSE'
#topic <- 'MBI'
#topic <- 'PHQ-9'

if (topic == 'MMSE'){
  #* MMSE and dementia ------
  filename <- "PubMed_Timeline_Results_by_Year - ‘Mini-mental status’tiab - 'dementia'tiab - medline - FINAL - 2024-06-09.csv"
  main_title <- paste0("Citations per year with ",
                       "'mini-mental status'[tiab]\n",
                       "in 'dementia' and MEDLINE since 1990")
  factor <- 100
  y_label <- "MMSE\u2020 Citations per 1k MEDLINE citations with 'dementia'[tiab]"
  note <- paste0("2001: PAR* acquires rights to the MMSE\u2020\n",
                 "2004: NIH drafts Public Access Policy\n",
                 "2006: NIH Public Access Policy implemented\n",
                 "2007: VA starts seeking alternatives to MMSE\n")
  note1 <- "* PAR, Inc.Psychological Assessment Resources."
  note2 <- "\u2020 MMSE. Mini-mental status examination."
}
if (topic == 'MBI'){
  #* MBI and burnout ------
  filename <- "PubMed_Timeline_Results_by_Year - ‘Maslach burnout’tiab - 'burnout'tiab - medline - FINAL - 2024-06-09.csv"
  main_title <- paste0("Citations per year with ",
                       "'Maslach burnout'[tiab]\n",
                       "in 'burnout' and MEDLINE since 1990")
  factor <- 100
  y_label <- "MGI*"
  note <- paste0("1996: MGI* acquires rights to the MBI.\n",
                 "2004: NIH drafts Public Access Policy\n",
                 "2006: NIH Public Access Policy implemented")
  y_label <- "MBI\u2020 Citations per 1k MEDLINE citations with 'burnout'[tiab]"
  note1 <- "* MGI Mind Garden, Inc."
  note2 <- "\u2020MBI. Maslach Burnout Inventory."
}

if (topic == 'PHQ-9'){
  #* PHQ-9 and depression ------
  filename <- "PubMed_Timeline_Results_by_Year - ‘PHQ-9’tiab - depressive disorder'tiab' - medline - FINAL - 2024-06-08.csv"
  main_title <- paste0("Citations per year with 'PHQ-9'[tiab]\n",
                       "in 'depressive disorders' and MEDLINE since 1990")
  factor <- 100
  y_label <- "PHQ-9*"
  note <- paste0("Pfizer* owns the copyright to the PHQ-9 and in\n",
                 " 2005 creates phqscreeners.com to host free access.")
  y_label <- "PHQ-9\u2020 Citations per 1k MEDLINE citations with 'depressive disorder'[MeSH]"
  note1 <- "* Pfizer Inc."
  note2 <- "\u2020PHQ-9. Patient Health Questionnaire."
}

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
data.summary <- data.frame(df = integer(), knots_interior = integer(), adj_r_squared = numeric(), 
                           AIC = numeric(), BIC = numeric(), MSE = numeric(), 
                           P_value_compared_to_one_less_knot = numeric(), 
                           P_value_compared_to_0_knots = numeric(),
                           most_important_knot = integer(), slope_change = numeric(), 
                           slope_before = numeric(), slope_after = numeric(), 
                           first_negative_slope_year = integer(), stringsAsFactors = FALSE)

# Fit the initial linear model (no knots)
initial_model <- lm(Survey.rate ~ Year, data = data.medline)
initial_adj_r_squared <- summary(initial_model)$adj.r.squared

# Loop over different degrees of freedom to fit spline models
for (df in 0:10) {  # Start from 0 to include the linear model
  if (df == 0) {
    model <- initial_model  # Linear model
    knots <- numeric(0)
    most_important_knot <- NA
    most_important_slope_change <- NA
    slope_before <- NA
    slope_after <- NA
    p_value_0_knots <- NA
    first_negative_slope_year <- NA
  } else {
    # Fit the spline model using natural splines
    model <- lm(Survey.rate ~ ns(Year, df = df), data = data.medline)
    knots <- attr(ns(data.medline$Year, df = df), "knots")
    
    # Calculate first derivatives at each knot
    if (length(knots) > 0) {
      first_derivatives <- numeric(length(knots))
      slopes_before <- numeric(length(knots))
      slopes_after <- numeric(length(knots))
      first_negative_slope_year <- NA
      for (i in seq_along(knots)) {
        knot <- knots[i]
        delta <- 0.01
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
      most_important_knot <- round(knots[max_slope_change_index])
      most_important_slope_change <- first_derivatives[max_slope_change_index]
      slope_before <- slopes_before[max_slope_change_index]
      slope_after <- slopes_after[max_slope_change_index]
      
      # Ensure first_negative_slope_year is NA if all slopes are positive
      if (all(slopes_after >= 0)) {
        first_negative_slope_year <- NA
      }
    } else {
      most_important_knot <- NA
      most_important_slope_change <- NA
      slope_before <- NA
      slope_after <- NA
      first_negative_slope_year <- NA
    }
    
    # Perform ANOVA to compare the model with the linear model
    anova_result_0_knots <- anova(initial_model, model)
    p_value_0_knots <- anova_result_0_knots$`Pr(>F)`[2]
  }
  
  # Get the adjusted R-squared value
  adj_r_squared <- summary(model)$adj.r.squared
  
  # Get AIC, BIC, and MSE
  aic <- AIC(model)
  bic <- BIC(model)
  mse <- mean(model$residuals^2)
  
  # Perform ANOVA to compare models if df > 0
  if (df > 0) {
    anova_result <- anova(previous_model, model)
    p_value <- anova_result$`Pr(>F)`[2]  # P-value for the comparison with one less knot
  } else {
    p_value <- NA  # No comparison for the initial linear model
  }
  
  # Append the results to the data.summary data frame
  data.summary <- rbind(data.summary, data.frame(df = df, knots_interior = df - 1, adj_r_squared = adj_r_squared, 
                                                 AIC = aic, BIC = bic, MSE = mse, 
                                                 P_value_compared_to_one_less_knot = p_value, 
                                                 P_value_compared_to_0_knots = p_value_0_knots, 
                                                 most_important_knot = most_important_knot, slope_change = most_important_slope_change, 
                                                 slope_before = slope_before, slope_after = slope_after, 
                                                 first_negative_slope_year = first_negative_slope_year))
  
  # Check if this model has the best adjusted R-squared so far
  if (adj_r_squared > best_adj_r_squared) {
    best_adj_r_squared <- adj_r_squared
    best_df <- df
    best_model <- model
  }
  
  # Update the previous model for the next comparison
  previous_model <- model
}

# Format the p-values and other metrics to avoid scientific notation
data.summary$P_value_compared_to_one_less_knot <- as.numeric(data.summary$P_value_compared_to_one_less_knot)
data.summary$P_value_compared_to_one_less_knot[data.summary$P_value_compared_to_one_less_knot < 0.001] <- "< 0.001"
data.summary$P_value_compared_to_0_knots <- as.numeric(data.summary$P_value_compared_to_0_knots)
data.summary$P_value_compared_to_0_knots[data.summary$P_value_compared_to_0_knots < 0.001] <- "< 0.001"
data.summary$adj_r_squared <- round(data.summary$adj_r_squared, 3)
data.summary$slope_change <- round(data.summary$slope_change, 4)
data.summary$slope_before <- round(data.summary$slope_before, 4)
data.summary$slope_after <- round(data.summary$slope_after, 4)

# Determine the best model based on adjusted R-squared, AIC, and BIC -----
# Filter out rows where P_value_compared_to_one_less_knot is NA
data.summary <- data.summary %>% filter(!is.na(P_value_compared_to_one_less_knot))

# Determine the best model based on the last significant P-value compared to one less knot
best_model_row_index <- which(data.summary$P_value_compared_to_one_less_knot >= 0.05)[1] - 1
if (is.na(best_model_row_index) || best_model_row_index < 1) {
  best_model_row_index <- 1  # Fallback to the first model if no significant row found
}

best_model_row <- data.summary[best_model_row_index, ]

best_df <- best_model_row$df
best_model <- lm(Survey.rate ~ ns(Year, df = best_df), data = data.medline)
best_model_summary <- summary(best_model)

# Get the p-value for the comparison of the best model with the linear model
best_model_p_value_0_knots <- best_model_row$P_value_compared_to_0_knots
best_model_p_value_0_knots <- as.numeric(best_model_p_value_0_knots)
best_model_knot <- best_model_row$most_important_knot

# PLOT with the data points ------
par(mar = c(5.1 + 2, 4.1, 4.1, 2.1), mfrow = c(1, 1)) # (bottom, left, top, right)

#* Plot ------
plot(data.medline$Year, data.medline$Survey.rate, type = "p", 
     ylim = c(0, 1.5*max(data.medline$Survey.rate)),
     main = main_title,
     xlab = "Year",
     ylab = y_label)

xmin <- par("usr")[1] + strwidth("A")
xmax <- par("usr")[2] - strwidth("A")
ymin <- par("usr")[3] + 1.2*strheight("A")
ymax <- par("usr")[4] - strheight("A")

# Add fitted spline from the best model
lines(data.medline$Year, fitted(best_model), col = "blue", lwd = 2)

# Add the linear regression line
lines(data.medline$Year, fitted(initial_model), col = "black", lwd = 2, lty = 2)

# Extract the knots from the best model
if (best_df > 0) {
  knots <- attr(ns(data.medline$Year, df = best_df), "knots")
  # Plot the knots
  points(knots, predict(best_model, newdata = data.frame(Year = knots)), pch = 16, col = "red")
}

# Add a dashed vertical line at the most important knot
abline(v = best_model_knot, col = "red", lty = 2)

# Add bold "Notes"
text(x = min(data.medline$Year), 
     y = ifelse(topic=='PHQ-9', 1.5*max(data.medline$Survey.rate), 1.5*max(data.medline$Survey.rate)), 
     adj = c(0, 1), 
     labels = "Notes:", 
     cex = 0.8, font = 2)

# Add the rest of the upper notes
text(x = min(data.medline$Year), 
     y = 1.5*max(data.medline$Survey.rate) - 0.05 * max(data.medline$Survey.rate), 
     adj = c(0, 1), 
     labels = paste0(
       "\u2022 Linear regression (0 knots): R\u00B2 = ", round(initial_adj_r_squared, 3), "\n",
       "\u2022 Optimal spline regression (", best_df - 1, " knots): R\u00B2 = ", round(best_adj_r_squared, 3), "\n",
       "\u2022 P (ANOVA compared to linear regression): ", round(best_model_p_value_0_knots,3), "\n",
       "\u2022 Knot with largest slope change: Year ", best_model_knot, "\n"
       #,"\u2022 First negative slope year: ", ifelse(is.na(best_model_row$first_negative_slope_year), "NA", best_model_row$first_negative_slope_year)
     ), 
     cex = 0.8, font = 1)

#* Add additional text ------
num_lines <- length(unlist(gregexpr("\n", note))) + 1

text(x = ifelse(topic=='PHQ-9', 2000, best_model_knot + 1), 
     y = ifelse(topic=='PHQ-9', 10, 0), #max(data.medline$Survey.rate),  
     labels = note,
     adj = c(0, 0), # 0 for left/bottom, 1 for right/top, and 0.5 for centered
     cex = 0.8, font = 1)

# Add footnotes
mtext("Footnotes:", side = 1, line = 4, adj = 0, cex = 0.8, font = 2)
mtext(note1, side = 1, line = 5, adj = 0, cex = 0.8)
mtext(note2, side = 1, line = 6, adj = 0, cex = 0.8)

# Print -----
function_plot_print(filename, 600, 800)
# Print the data.summary data frame
print(data.summary)

cat('Best model, final spline: ')
best_model_summary$coefficients[best_df+1,1]
best_model_summary$coefficients[best_df+1,2]

