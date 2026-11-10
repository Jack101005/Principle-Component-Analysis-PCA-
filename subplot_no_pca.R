# ============================================================
# PCA + Logistic Regression: AI subscription among CSE students
# Dataset: AI Usage Habits Survey for CSE Students in Vietnam
# ============================================================
# Pipeline:
# 1. Read the Excel file
# 2. Keep only respondents who answered Yes to the CSE question
# 3. Exclude Timestamp and the CSE screening question from predictors
# 4. Encode ordinal/binary variables and keep institution type nominal
# 5. Stratified 80/20 train-validation split
# 6. Impute, dummy-encode, standardize and fit PCA on TRAIN only
# 7. Apply the fitted preprocessing/PCA to validation
# 8. Fit Logistic Regression on the principal components
# 9. Evaluate and save metrics, predictions, PCA variance and plots
# ============================================================

# ---------------------------
# 0. Install/load packages
# ---------------------------
required_packages <- c(
  "readxl", "dplyr", "stringr", "rsample", "recipes",
  "pROC", "plotly", "htmlwidgets"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

invisible(lapply(required_packages, library, character.only = TRUE))


# ---------------------------
# 1. Read Excel data
# ---------------------------
# A file-selection window will open. Select the updated .xlsx file.
file_path <- file.choose()
raw_data <- readxl::read_excel(file_path, sheet = 1, .name_repair = "minimal")

# The expected survey layout is A:S (19 columns).
if (ncol(raw_data) < 19) {
  stop("The Excel file has fewer than 19 columns. Please check the survey layout.")
}

cat("Total records in Excel (excluding header):", nrow(raw_data), "\n")

# Helper functions for numeric and ordinal answers.
parse_number <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

# Answers such as '2 · Somewhat confident' become 2.
parse_ordinal <- function(x) {
  suppressWarnings(as.numeric(stringr::str_extract(as.character(x), "^\\s*\\d+")))
}

# ---------------------------
# 2. Filter CSE respondents
# ---------------------------
# Column B is the screening question:
# Are you currently studying CS/SE/IT or a closely related major?
# Keep Yes only. This column will NOT be used as a predictor.
cse_answer <- stringr::str_trim(as.character(raw_data[[2]]))
cse_mask <- stringr::str_detect(cse_answer, stringr::regex("^(Có|Yes)", ignore_case = TRUE))

cse_data <- raw_data[cse_mask %in% TRUE, , drop = FALSE]

cat("CSE records kept:", nrow(cse_data), "\n")
cat("Non-CSE records excluded:", sum(stringr::str_detect(cse_answer, stringr::regex("^(Không|No)", ignore_case = TRUE)), na.rm = TRUE), "\n")

# ---------------------------
# 3. Create target variable
# ---------------------------
# Column G is the target question about a monthly AI subscription.
# Yes = pays for a monthly AI subscription; No = does not pay.
target_raw <- stringr::str_trim(as.character(cse_data[[7]]))

target <- dplyr::case_when(
  stringr::str_detect(target_raw, stringr::regex("^(Có|Yes)", ignore_case = TRUE)) ~ "Yes",
  stringr::str_detect(target_raw, stringr::regex("^(Không|No)", ignore_case = TRUE)) ~ "No",
  TRUE ~ NA_character_
)

if (anyNA(target)) {
  bad_target_values <- unique(target_raw[is.na(target)])
  stop(paste(
    "Unexpected or missing target values found:",
    paste(bad_target_values, collapse = " | ")
  ))
}

paid_ai <- factor(target, levels = c("No", "Yes"))
cat("\nTarget distribution after CSE filtering:\n")
print(table(paid_ai))

# ---------------------------
# 4. Prepare predictor variables
# ---------------------------
# Excel column positions:
# A = Timestamp (excluded)
# B = CSE screening question (used for filtering only; excluded)
# C = Age
# D = Type of institution
# E = Year of study
# F = Programming confidence (ordinal 1-3)
# G = Target (monthly AI subscription)
# H = When the respondent started using AI for studying
# I = Average AI usage hours per day
# J = Frequency of using AI to learn concepts (ordinal 1-3)
# K = Frequency of using AI to generate/write code (ordinal 1-3)
# L = Percentage of bugs fixed with AI
# M = Whether the respondent reviews AI-generated code (binary)
# N = AI-assisted percentage of reports/documentation
# O = Estimated productivity improvement percentage
# P = Estimated grade improvement percentage
# Q = Concern about coding/problem-solving skills (ordinal 1-3)
# R = Concern about academic integrity (ordinal 1-3)
# S = Hours spent solving a difficult problem before asking AI

review_answer <- stringr::str_trim(as.character(cse_data[[13]]))
review_binary <- dplyr::case_when(
  stringr::str_detect(review_answer, stringr::regex("^(Có|Yes)", ignore_case = TRUE)) ~ 1,
  stringr::str_detect(review_answer, stringr::regex("^(Không|No)", ignore_case = TRUE)) ~ 0,
  TRUE ~ NA_real_
)

predictors <- dplyr::tibble(
  age = parse_number(cse_data[[3]]),
  institution_type = factor(stringr::str_trim(as.character(cse_data[[4]]))),
  study_year = parse_number(cse_data[[5]]),
  programming_confidence = parse_ordinal(cse_data[[6]]),
  years_using_ai = parse_number(cse_data[[8]]),
  daily_ai_hours = parse_number(cse_data[[9]]),
  frequency_learning = parse_ordinal(cse_data[[10]]),
  frequency_code_generation = parse_ordinal(cse_data[[11]]),
  bug_fix_percentage = parse_number(cse_data[[12]]),
  review_ai_code = review_binary,
  report_ai_percentage = parse_number(cse_data[[14]]),
  productivity_improvement = parse_number(cse_data[[15]]),
  grade_improvement = parse_number(cse_data[[16]]),
  concern_coding_skills = parse_ordinal(cse_data[[17]]),
  academic_integrity_concern = parse_ordinal(cse_data[[18]]),
  self_solve_hours = parse_number(cse_data[[19]])
)

model_data <- dplyr::bind_cols(
  dplyr::tibble(paid_ai = paid_ai),
  predictors
)


# ---------------------------
# 5. Stratified train-validation split
# ---------------------------
set.seed(2026)

split_obj <- rsample::initial_split(
  model_data,
  prop = 0.80,
  strata = paid_ai
)

train_data <- rsample::training(split_obj)
valid_data <- rsample::testing(split_obj)

cat("\nTrain records:", nrow(train_data), "\n")
cat("Validation records:", nrow(valid_data), "\n")
cat("\nTarget distribution in training set:\n")
print(prop.table(table(train_data$paid_ai)))
cat("\nTarget distribution in validation set:\n")
print(prop.table(table(valid_data$paid_ai)))

print(train_data)


x11() # This is for Linux

library(GGally)

# Colour by group + custom panels
ggpairs(train_data, columns = 4:9,
        aes(colour = paid_ai, alpha = 0.5),
        upper = list(continuous = wrap("dot_no_facet", alpha = 0.5)), #, size = 4)),
        lower = list(continuous = wrap("density", alpha = 0.5)),
        diag  = list(continuous = wrap("densityDiag", alpha = 0.5)),
        title = "Scatterplot Matrix") +
  scale_colour_manual(values = c("#3a86d4", "#f4a259", "#5fad56")
)   
locator(1)
dev.off()

# Example: Mapping 5 variables (var1-var5)
plot_ly(
  train_data,
  x = ~report_ai_percentage,
  y = ~daily_ai_hours,
  z = ~productivity_improvement,
  opacity = 0.7,
  symbols = c("circle", "square", "diamond", "cross", "x"),
  symbol = ~factor(years_using_ai),
  color = ~frequency_code_generation,      # 4th dimension: Color
  size = ~bug_fix_percentage,       # 5th dimension: Marker Size
  type = "scatter3d",
  mode = "markers"
) %>%
  layout(scene = list(
    xaxis = list(title = "Var 1"),
    yaxis = list(title = "Var 2"),
    zaxis = list(title = "Var 3")
  )
)
