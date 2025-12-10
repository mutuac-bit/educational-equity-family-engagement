### 01_data_preparation.R
### Educational Equity Through Family Engagement
#### Prepared by: Cynthia Mutua
Setup
# Setup
library(tidyverse)      
library(readxl)
library(janitor)
library(skimr)
library(naniar)
library(here)

# Prevent namespace conflicts
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

# Set theme for all plots
theme_set(theme_minimal(base_size = 12))

# Custom color palette (colorblind-friendly)
project_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                    "#0072B2", "#D55E00", "#CC79A7")

### Import Data
# Read 2019 data
pfi_2019 <- read_excel("pfi-data.xlsx", sheet = "curated 2019")

# Read 2016 data  
pfi_2016 <- read_excel("pfi-data.xlsx", sheet = "curated 2016")

# Check dimensions
cat("2019 data:", nrow(pfi_2019), "rows,", ncol(pfi_2019), "columns\n")

cat("2016 data:", nrow(pfi_2016), "rows,", ncol(pfi_2016), "columns\n")

# Check if column names match
all_match <- identical(names(pfi_2019), names(pfi_2016))
cat("\nColumn names match:", all_match, "\n")

Variable Selection & Creation
# Select key variables for analysis
# (We'll expand this based on what we see in the data)

# Select key variables for analysis
key_vars <- c(
  # Identifiers
  "BASMID",
  
  # Outcome variables
  "SEGRADES",        # Student grades (primary outcome)
  "SEGRADEQ",        # Work quality
  "SEENJOY",         # Enjoys school
  "SEABSNT",         # Days absent (Poisson outcome)
  "SEREPEAT",        # Grade repetition
  
  # School engagement frequency (Poisson outcome alternative)
  "FSFREQ",          # Times participated in school meetings
  
  # School engagement activities (individual indicators)
  "FSSPORTX",        # Attended school event
  "FSVOL",           # Volunteered
  "FSMTNG",          # Attended meeting
  "FSPTMTNG",        # PTA meeting
  "FSATCNFN",        # Parent-teacher conference
  "FSFUNDRS",        # Fundraising
  "FSCOMMTE",        # Committee
  "FSCOUNSLR",       # Guidance counselor
  
  # Homework involvement
  "FHHOME",          # Days per week doing homework
  "FHWKHRS",         # Hours per week on homework
  "FHCHECKX",        # Checks homework
  "FHHELP",          # Days help with homework
  
  # Cultural enrichment - weekly
  "FOSTORY2X",       # Told story
  "FOCRAFTS",        # Arts/crafts
  "FOGAMES",         # Board games
  "FOBUILDX",        # Projects
  "FOSPORT",         # Sports
  "FODINNERX",       # Family dinners
  
  # Cultural enrichment - monthly
  "FOLIBRAYX",       # Library
  "FOBOOKSTX",       # Bookstore
  
  # Socioeconomic variables
  "TTLHHINC",        # Household income
  "PARGRADEX",       # Parent education (derived)
  "P2GUARD",         # Second parent present
  
  # School type
  "EDCPUB",          # Public school
  
  # School choice
  "SCCHOICE",        # Has school choice
  "SPUBCHOIX",       # District allows choice
  "SCONSIDR",        # Considered other schools
  
  # Student characteristics
  "ALLGRADEX",       # Current grade
  "CSEX",            # Sex
  "RACEETH",         # Race/ethnicity (derived)
  
  # Disability
  "DSBLTY",          # Has disability (derived)
  
  # Family structure
  "NUMSIBSX"         # Number siblings (derived)
)

# Select and add year indicator 
pfi_2019_selected <- pfi_2019 %>%
  dplyr::select(tidyselect::any_of(key_vars)) %>%  # Explicit namespace
  mutate(survey_year = 2019)

pfi_2016_selected <- pfi_2016 %>%
  dplyr::select(tidyselect::any_of(key_vars)) %>%  # Explicit namespace
  mutate(survey_year = 2016)

# ALLGRADEX has different types across years (likely numeric vs character)
# We convert both to character to allow bind_rows() to work

# Stack datasets
pfi_2019_selected$ALLGRADEX <- as.character(pfi_2019_selected$ALLGRADEX)
pfi_2016_selected$ALLGRADEX <- as.character(pfi_2016_selected$ALLGRADEX)

pfi_combined <- bind_rows(pfi_2019_selected, pfi_2016_selected)

cat("Combined dataset:", nrow(pfi_combined), "rows,", 
    ncol(pfi_combined), "columns\n")

Initial Data Inspection
#### DATA QUALITY ASSESSMENT Before any analysis, we need to understand our data structure, types, missing patterns, and value distributions.

# Look at first few rows
head(pfi_combined)

# Check structure
glimpse(pfi_combined)

# Summary statistics
skim(pfi_combined)

Data Cleaning & Recoding
# Data Cleaning & Recoding

pfi_clean <- pfi_combined %>%
  mutate(
    # OUTCOME VARIABLES 
    #PRIMARY OUTCOME: Grade Categories (for Multinomial Logistic & LDA)
    # Original coding: 1=Mostly A's, 2=B's, 3=C's, 4=D's/F's, 5=No grades We create meaningful labels and exclude "No grades" (code 5) as       NA because we can't assess academic performance without grade data. Primary outcome: Grades (1=Mostly A's, 2=B's, 3=C's, 4=D/F's,        5=No grades)
    grades_cat = case_when(
      SEGRADES == 1 ~ "High_Achievers",
      SEGRADES == 2 ~ "Solid_Performers", 
      SEGRADES == 3 ~ "Struggling",
      SEGRADES == 4 ~ "At_Risk",
      TRUE ~ NA_character_
    ),
    grades_cat = factor(grades_cat, 
                       levels = c("High_Achievers", "Solid_Performers", 
                                 "Struggling", "At_Risk")),
    
    # Binary outcome: At-Risk indicator(for Binary Logistic Regression)
    # At-risk if C's or worse AND (high absence OR doesn't enjoy school).
    # This operationalizes "at-risk" as students who:
    # (1) Have C's or worse grades AND
    # (2) Either high absenteeism (>10 days) OR don't enjoy school
    # This captures students needing immediate intervention, not just poor grades
    
    at_risk = case_when(
      SEGRADES >= 3 & (SEABSNT > 10 | SEENJOY >= 3) ~ 1,
      SEGRADES <= 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # COUNT OUTCOMES: For Poisson Regression
    days_absent = as.numeric(SEABSNT),
    meetings_attended = as.numeric(FSFREQ),
    
    # SCHOOL ENGAGEMENT ACTIVITIES
    #SCHOOL ENGAGEMENT SCORE CONSTRUCTION (0-8 scale)
    # The PFI survey codes activities as 1=Yes, 2=No
    # We recode to 1=Yes, 0=No so we can sum them into a count
    
    attend_event = if_else(FSSPORTX == 1, 1, 0),
    volunteer = if_else(FSVOL == 1, 1, 0),
    general_meeting = if_else(FSMTNG == 1, 1, 0),
    pta_meeting = if_else(FSPTMTNG == 1, 1, 0),
    parent_teacher_conf = if_else(FSATCNFN == 1, 1, 0),
    fundraising = if_else(FSFUNDRS == 1, 1, 0),
    committee = if_else(FSCOMMTE == 1, 1, 0),
    counselor = if_else(FSCOUNSLR == 1, 1, 0),
    
    # Create school engagement composite (count 0-8)
    school_engagement = attend_event + volunteer + general_meeting + 
                       pta_meeting + parent_teacher_conf + fundraising + 
                       committee + counselor,
    # Sum all 8 activities: Score ranges from 0 (no engagement) to 8 (all activities)
    
    # HOMEWORK INVOLVEMENT 
    # Create composite score (standardized)
    # We combine three related measures of homework involvement. Standardization (z-scores) makes the coefficient interpretable as:
    # "effect of a 1 SD increase in homework involvement"
    homework_days = as.numeric(FHHOME),      # Days per week
    homework_hours = as.numeric(FHWKHRS),    # Hours per week
    homework_check = as.numeric(FHCHECKX),   # Frequency of checking
    homework_help = as.numeric(FHHELP),      # Days help per week
    
    # Average the three components and standardize (mean=0, SD=1)
    # The [,1] extracts the vector from scale()'s matrix output
    homework_involvement = scale(
      (homework_days + homework_hours + homework_help) / 3
    )[,1],
    
    
    #CULTURAL ENRICHMENT SCORE (Weighted Composite)
    # This captures the educational environment at home beyond formal schoolwork.
    # We weight monthly activities lower (divide by 4) since they're less frequent.
    # Weekly activities (1=Yes, 2=No to 1=Yes, 0=No)
    
    
    story = if_else(FOSTORY2X == 1, 1, 0),
    crafts = if_else(FOCRAFTS == 1, 1, 0),
    games = if_else(FOGAMES == 1, 1, 0),
    projects = if_else(FOBUILDX == 1, 1, 0),
    sports_home = if_else(FOSPORT == 1, 1, 0),
    dinners = as.numeric(FODINNERX),  # Already 0-7 scale
    
    # Monthly activities (1=Yes, 2=No to 1=Yes, 0=No)
    library = if_else(FOLIBRAYX == 1, 1, 0),
    bookstore = if_else(FOBOOKSTX == 1, 1, 0),
    
    # Create cultural enrichment composite
    # Weekly activities + monthly activities (weighted as 1/4 week) + dinners proportion
    cultural_enrichment = (story + crafts + games + projects + sports_home) +
                         (library + bookstore) / 4 +
                         dinners / 7,
    
    # SOCIOECONOMIC VARIABLES 
    # These are CENTRAL to our compensatory hypothesis, not just controls.
    # We test if engagement effects DIFFER by SES level.
    
    # Income as ordered factor (original 12-category scale)
    income_level = factor(TTLHHINC, ordered = TRUE),
    
    # Simplified 3-category income for interaction testing
    # Thresholds based on distribution: 1-3=Low, 4-6=Middle, 7+=High
    # Income (keep as ordered factor)
    income_level = factor(TTLHHINC, ordered = TRUE),
    
    # Create 3-category income (need to check distribution first, but assuming standard coding)
    income_3cat = case_when(
      TTLHHINC <= 3 ~ "Low",      # Adjust thresholds based on actual distribution
      TTLHHINC <= 6 ~ "Middle",
      TTLHHINC > 6 ~ "High",
      TRUE ~ NA_character_
    ),
    income_3cat = factor(income_3cat, 
                        levels = c("Low", "Middle", "High"),
                        ordered = TRUE),
    
    # Parent education (keep as ordered factor)
    parent_education = factor(PARGRADEX, ordered = TRUE),
    
    # Create 3-category education for easier interpretation
    parent_ed_3cat = case_when(
      PARGRADEX <= 2 ~ "HS_or_less",     # High school or less
      PARGRADEX == 3 ~ "Some_college",    # Some college
      PARGRADEX >= 4 ~ "College_grad",    # Bachelor's or higher
      TRUE ~ NA_character_
    ),
    parent_ed_3cat = factor(parent_ed_3cat,
                           levels = c("HS_or_less", "Some_college", "College_grad"),
                           ordered = TRUE),
    
    # CONTROL VARIABLES
    # These isolate the true effect of engagement from confounding factors
    
    # Two-parent household (1=Yes, 2=No)
    two_parent = if_else(P2GUARD == 1, 1, 0),
    
    # Public school indicator (we only have this one school type variable)
    public_school = if_else(EDCPUB == 1, 1, 0),
    
    # SCHOOL CHOICE 
    has_school_choice = if_else(SCCHOICE == 1 | SPUBCHOIX == 1, 1, 0),
    considered_other_schools = if_else(SCONSIDR == 1, 1, 0),
    
    # STUDENT CHARACTERISTICS 
    grade_level = as.numeric(ALLGRADEX),
    
    male = if_else(CSEX == 1, 1, 0),
    
    race_ethnicity = factor(RACEETH),
    
    has_disability = if_else(DSBLTY == 1, 1, 0),
    
    num_siblings = as.numeric(NUMSIBSX),
    
    # Survey year as factor
    year = factor(survey_year)
  )

# CREATE FINAL ANALYSIS DATASET
# Select only the variables we need for modelling
pfi_analysis <- pfi_clean %>%
  select(
    # ID
    BASMID,
    
    # Outcomes
    grades_cat, at_risk, days_absent, meetings_attended,
    
    # Engagement composites
    school_engagement, homework_involvement, cultural_enrichment,
    
    # Engagement components (keep for detailed analysis)
    attend_event, volunteer, general_meeting, pta_meeting, 
    parent_teacher_conf, fundraising, committee, counselor,
    
    # Socioeconomic
    income_level, income_3cat, parent_education, parent_ed_3cat, 
    two_parent, public_school,
    
    # School choice
    has_school_choice, considered_other_schools,
    
    # Student characteristics  
    grade_level, male, race_ethnicity, has_disability, num_siblings,
    
    # Year
    year
  )

# Remove rows with missing primary outcome
pfi_analysis <- pfi_analysis %>%
  filter(!is.na(grades_cat))

cat("Final analysis dataset:", nrow(pfi_analysis), "observations\n")

cat("\nVariables in final dataset:\n")

print(names(pfi_analysis))
       
MISSING DATA PATTERN ANALYSIS
Understanding missingness is crucial for: 1. Deciding which variables to include 2. Choosing appropriate handling strategies 3. Assessing potential bias 4. Check structure

cat("\n MISSING DATA BY VARIABLE \n")


 MISSING DATA BY VARIABLE 
pfi_analysis %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(pct_missing = round(n_missing / nrow(pfi_analysis) * 100, 1)) %>%
  arrange(desc(n_missing)) %>%
  print(n = Inf)

KEY FINDINGS: - race_ethnicity: 46.7% missing - Include with “Unknown” category - has_school_choice: 31.3% missing - EXCLUDE (confounded with year) - at_risk: 7.6% missing - Acceptable for binary logistic model - grade_level: 1.3% missing - Minimal impact - All engagement variables: 0% missing - No issues

# Check structure
cat("\nVariables in final dataset:\n")

print(names(pfi_analysis))
               
# Summary of key variables
cat("\n OUTCOME VARIABLE DISTRIBUTION \n")


 OUTCOME VARIABLE DISTRIBUTION 
print(table(pfi_analysis$grades_cat, useNA = "always"))

cat("\n AT-RISK INDICATOR \n")


 AT-RISK INDICATOR 
print(table(pfi_analysis$at_risk, useNA = "always"))

cat("\n INCOME DISTRIBUTION \n")


 INCOME DISTRIBUTION 
print(table(pfi_analysis$income_3cat, useNA = "always"))

cat("\n PARENT EDUCATION DISTRIBUTION \n")


 PARENT EDUCATION DISTRIBUTION 
print(table(pfi_analysis$parent_ed_3cat, useNA = "always"))


 ENGAGEMENT SCORE SUMMARY 
print(summary(pfi_analysis$school_engagement))

INTERPRETATION: - Not at-risk: 22,140 (94.4%) - At-risk: 1,327 (5.7%) WARNING: Severe class imbalance will likely cause overfitting in binary logistic!

Save Cleaned Data
# Save as RDS for easy loading
saveRDS(pfi_analysis, "pfi_analysis_data.rds")

cat("Data saved \n")

Data saved 
library(GGally)
library(patchwork)
library(scales)

# Load saved data
pfi_analysis <- readRDS("pfi_analysis_data.rds")

# Create analysis version with numeric grades for correlations
pfi_explore <- pfi_analysis %>%
  mutate(
    # Convert to numeric for correlation
    grades_numeric = as.numeric(grades_cat),
    # Make race unknown explicit
    race_ethnicity = fct_explicit_na(race_ethnicity, na_level = "Unknown"),
    # Handle school choice missing
    has_school_choice = replace_na(has_school_choice, 0)
  ) %>%
  filter(!is.na(at_risk))

cat("Data prepared for exploratory analysis:", nrow(pfi_explore), "observations\n")
Quick Summary
# Summary of key variables
summary(pfi_analysis %>% select(grades_cat, at_risk, school_engagement,
                                income_3cat, parent_education))
