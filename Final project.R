---
	title: "Final project"
format:
	html:
---
library(tidyverse)
library(gtsummary)
library(readr)
library(here)

tb <- read_csv(here("strep_tb.csv"))

# Create a {gtsummary} table of descriptive statistics about your data (1 pt)

table1<-tbl_summary(
	tb,
	by = gender,
	include = c(gender, arm, dose_strep_g, baseline_condition, baseline_cavitation, strep_resistance, improved),
	label = list(
		arm ~ "Group",
		dose_strep_g ~ "Dose of Streptomycin (g)",
		baseline_condition ~ "Patient Condition at Baseline",
		baseline_cavitation ~ "Lung Cavitation at Baseline",
		strep_resistance ~ "Streptomycin Resistance at 6m",
		improved ~ "Outcome of Improved"
	))

#Fit a regression and present well-formatted results from the regression (1 pt)
library(broom)

linear_model_int <- lm(improved ~ gender*arm + baseline_condition + baseline_cavitation + strep_resistance,
											 data = tb)
tbl_regression(
	linear_model_int,
	intercept = TRUE,
	label = list(
		gender ~ "Gender",
		arm ~ "Group",
		baseline_condition ~ "Condition of the Patient at Baseline",
		baseline_cavitation ~ "Cavitation of the Lungs at baseline",
		strep_resistance ~ "Resistance to Streptomycin at 6m"
	))

#Create a figure (1 pt)

hist(tb$rad_num, main = "Rating of Chest X-ray at Month 6",
		 xlab = "Rating",
		 ylab = "Frequency")

#Write and use a function that does something with the data (1 pt)
new_table_function <- function(model) {
	tbl_regression(
		model,
		intercept = TRUE,
		label = list(
			gender ~ "Gender",
			arm ~ "Group",
			baseline_condition ~ "Condition of the Patient at Baseline",
			baseline_cavitation ~ "Cavitation of the Lungs at baseline",
			strep_resistance ~ "Resistance to Streptomycin at 6m"
		)
	)
}

linear_model_int <- lm(improved ~ gender*arm + baseline_condition + baseline_cavitation + strep_resistance,
											 data = tb)

new_table_function(linear_model_int)

#mode of Rating of Chest X-ray at Month 6

mode_value<- mode(tb$rad_num)

#file path for final project
project_file_path<- here("R","Final project.html")
