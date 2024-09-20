The files that start with "PubMed_Timeline_Results_by_Year..." are the datafiles downloaded from PubMed.

The files that start with the name of the survey or scale and contain "search - data.summary" are the outputs of the R script, "Citations over time - 2024-xx-xx.R" and are provided here for checking for processing errors.

There are two R scripts. 
• "Citations over time - 2024-xx-xx.R" processes the PubMed files and creates the data for the manuscript and supplement tables. This script also creates the regression plots.
• "Coefficient comparisons - 2024-xx-xx.R" uses the coefficients and their SEs from the script above (after you hard code the values into this script), to create the significance testing for comparing citation rates in the final splines of the regression plots.

Questions to rbadgett@kumc.edu.

2024-09-19