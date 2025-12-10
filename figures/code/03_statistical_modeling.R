#### STATISTICAL TESTS FOR KEY RELATIONSHIPS
##### STATISTICAL SIGNIFICANCE TESTS

```{r}
# 1. School engagement vs grades
cat("\n1. School Engagement vs Grades:\n")
engagement_test <- cor.test(pfi_explore$school_engagement, 
                            pfi_explore$grades_numeric)
cat(sprintf("   Correlation: %.3f, p-value: %.2e\n", 
           engagement_test$estimate, engagement_test$p.value))

# 2. Homework involvement vs grades
cat("\n2. Homework Involvement vs Grades:\n")
homework_test <- cor.test(pfi_explore$homework_involvement, 
                         pfi_explore$grades_numeric)
cat(sprintf("   Correlation: %.3f, p-value: %.2e\n", 
           homework_test$estimate, homework_test$p.value))

# 3. Cultural enrichment vs grades
cat("\n3. Cultural Enrichment vs Grades:\n")
cultural_test <- cor.test(pfi_explore$cultural_enrichment, 
                         pfi_explore$grades_numeric)
cat(sprintf("   Correlation: %.3f, p-value: %.2e\n", 
           cultural_test$estimate, cultural_test$p.value))

# 4. Test if engagement matters MORE for low-income
cat("\n4. Interaction Test: Does engagement matter more for low-income?\n")
cat("   (Comparing correlations across income groups)\n")

by_income <- pfi_explore %>%
  group_by(income_3cat) %>%
  summarise(
    cor_engagement_grades = cor(school_engagement, grades_numeric, 
                               use = "complete.obs"),
    n = n()
  )
print(by_income)
```
##### INTERPRETATION:
The correlations are similar across groups in this simple analysis.We need formal interaction testing in regression to properly assess
the compensatory hypothesis (which controls for other variables).

#### Stastical Modeling

```{r modeling-libraries}
#| label: modeling libraries


library(tidymodels)
library(discrim)      # For LDA/QDA
library(poissonreg)   # For Poisson
library(nnet)         # For multinomial
```

#### DATA PREPARATION FOR MODELING

```{r modeling-data-prep}
#| label: Modeling data prep
# Handle missing data based on ggpairs findings
pfi_modeling <- pfi_analysis %>%
  mutate(
    # Race: Include with "Unknown" category (ggpairs showed 47% missing but variable is important)
    race_ethnicity = fct_explicit_na(race_ethnicity, na_level = "Unknown"),
    
    # School choice: Exclude due to 31% missing (entire 2016 dataset)
    # Decision: Too much missingness, confounds with year
    
    # Convert outcomes to appropriate types
    at_risk = factor(at_risk, levels = c("0", "1")),
    days_absent = as.numeric(days_absent)
  ) %>%
  # Remove rows with missing at_risk (needed for binary logistic)
  filter(!is.na(at_risk))

cat("Final modeling dataset:", nrow(pfi_modeling), "observations\n")
cat("Variables included:", ncol(pfi_modeling), "\n")
```

#### VARIABLE SELECTION JUSTIFICATION

```{r variable-justification}
#| label: Variable Justification

cat("\nRESPONSE VARIABLES:\n")
cat("  • grades_cat: Primary outcome (multinomial, LDA)\n")
cat("  • at_risk: Binary screening tool (logistic)\n")
cat("  • days_absent: Attendance outcome (Poisson)\n")

cat("\nMAIN PREDICTORS (justified by ggpairs):\n")
cat("  • school_engagement: r=-0.136*** (significant)\n")
cat("  • homework_involvement: r=-0.139*** (strongest engagement predictor)\n")
cat("  • cultural_enrichment: r=-0.131*** (significant)\n")
cat("  → All three retained: low multicollinearity (max r=0.326)\n")

cat("\nSES MODERATORS:\n")
cat("  • income_3cat, parent_ed_3cat, two_parent\n")
cat("  → Essential for testing compensatory effects\n")

cat("\nCONTROL VARIABLES (from ggpairs):\n")
cat("  • has_disability: r=0.207*** (STRONGEST predictor!)\n")
cat("  • male: r=0.143*** (moderate effect)\n")
cat("  • grade_level: r=0.095*** (weak but significant)\n")
cat("  • num_siblings: r=-0.039*** (weak, included for completeness)\n")
cat("  • public_school: r=0.079*** (weak but theoretically important)\n")
cat("  • year: controls for temporal effects between 2016 and 2019\n")
cat("  • race_ethnicity: with 'Unknown' category for missing\n")

cat("\nINTERACTIONS (core hypothesis):\n")
cat("  • income_3cat × school_engagement\n")
cat("  • parent_ed_3cat × homework_involvement\n")
cat("  → Tests if engagement helps disadvantaged students MORE\n")

cat("\nEXCLUDED VARIABLES:\n")
cat("  • has_school_choice: 31% missing (entire 2016 wave)\n")
cat("  • considered_other_schools: related missing pattern\n")
cat("  • individual activities: using composites instead for parsimony\n")
```

#### TRAIN/TEST SPLIT
Stratified sampling ensures proportional representation of grade categories

```{r train-test-split}
#| label: Train/Test Split

set.seed(2024) #For Reproducibility

# Stratified split (75/25) on primary outcome
data_split <- initial_split(pfi_modeling, 
                            prop = 0.75, 
                            strata = grades_cat)

train_data <- training(data_split)
test_data <- testing(data_split)

cat("Training set:", nrow(train_data), "observations\n")
cat("Test set:", nrow(test_data), "observations\n")

# 10-fold cross-validation
cv_folds <- vfold_cv(train_data, v = 10, strata = grades_cat)
cat("Cross-validation: 10 folds\n\n")
```
####RECIPE DEFINITIONS
Recipes define preprocessing steps applied consistently to train and test data.
We create separate recipes for different outcomes.

```{r recipes}
#| label: Recipe Defintions

# Recipe for MULTINOMIAL and LDA (grades_cat outcome)
multinom_recipe <- recipe(grades_cat ~ 
                           school_engagement + homework_involvement + cultural_enrichment +
                           income_3cat + parent_ed_3cat + two_parent +
                           grade_level + male + has_disability + num_siblings + 
                           public_school + year + race_ethnicity,
                         data = train_data) %>%
  # Create dummy variables for categorical predictors
  step_dummy(all_nominal_predictors()) %>%
  
  # KEY INTERACTIONS (our research innovation!)
  step_interact(~ starts_with("income_3cat"):school_engagement) %>%
  step_interact(~ starts_with("parent_ed_3cat"):homework_involvement) %>%
  
  # Normalize numeric predictors (important for LDA)
  step_normalize(all_numeric_predictors()) %>%
  
  # Remove zero-variance predictors
  step_zv(all_predictors())

# Recipe for BINARY LOGISTIC (at_risk outcome)
logistic_recipe <- recipe(at_risk ~ 
                           school_engagement + homework_involvement + cultural_enrichment +
                           income_3cat + parent_ed_3cat + two_parent +
                           grade_level + male + has_disability + num_siblings + 
                           public_school + year + race_ethnicity,
                         data = train_data) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_interact(~ starts_with("income_3cat"):school_engagement) %>%
  step_interact(~ starts_with("parent_ed_3cat"):homework_involvement) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_zv(all_predictors())

# Recipe for POISSON (days_absent outcome)
poisson_recipe <- recipe(days_absent ~ 
                          school_engagement + homework_involvement + cultural_enrichment +
                          income_3cat + parent_ed_3cat + two_parent +
                          grade_level + male + has_disability + num_siblings + 
                          public_school + year + race_ethnicity,
                        data = train_data) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_interact(~ starts_with("income_3cat"):school_engagement) %>%
  step_interact(~ starts_with("parent_ed_3cat"):homework_involvement) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_zv(all_predictors())

print(multinom_recipe)
print(logistic_recipe)
print(poisson_recipe)
```
