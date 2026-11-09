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

# Benchmark switch:
# TRUE  = Logistic Regression on principal components (PCA model)
# FALSE = Logistic Regression on the standardized original predictors (no PCA)
# Run the script once with each setting; results go to separate folders.
USE_PCA <- FALSE
result_folder <- if (USE_PCA) "PCA_Logistic_Results" else "NoPCA_Logistic_Results"
cat("Model mode:", if (USE_PCA) "WITH PCA" else "WITHOUT PCA", "\n")

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

# ---------------------------
# 6. Preprocess + PCA (fit on TRAIN only)
# ---------------------------
# Important:
# - Median imputation is learned from training data only.
# - institution_type is nominal, so it is converted to dummy variables.
# - Ordinal answers were converted to 1, 2, 3 above.
# - All predictors are standardized before PCA.
# - PCA keeps enough components to explain at least 90% of TRAIN variance.
# - Validation data is transformed using the already-fitted training recipe.

pca_recipe <- recipes::recipe(paid_ai ~ ., data = train_data) %>%
  recipes::step_impute_median(recipes::all_numeric_predictors()) %>%
  recipes::step_dummy(recipes::all_nominal_predictors()) %>%
  recipes::step_normalize(recipes::all_predictors())

if (USE_PCA) {
  pca_recipe <- pca_recipe %>%
    recipes::step_pca(recipes::all_predictors(), threshold = 0.90)
}

prepared_recipe <- recipes::prep(
  pca_recipe,
  training = train_data,
  retain = TRUE,
  verbose = FALSE
)

train_pca <- recipes::bake(prepared_recipe, new_data = NULL)
valid_pca <- recipes::bake(prepared_recipe, new_data = valid_data)

# pc_columns = the model inputs: the PCs (with PCA) or all predictors (without PCA).
if (USE_PCA) {
  pc_columns <- grep("^PC[0-9]+$", names(train_pca), value = TRUE)
} else {
  pc_columns <- setdiff(names(train_pca), "paid_ai")
}

if (length(pc_columns) == 0) {
  stop("No predictor columns were produced. Check the predictor data.")
}

cat("\nNumber of model inputs:", length(pc_columns), "\n")

pca_variance_table <- NULL
if (USE_PCA) {
  # Variance explained by each principal component.
  pca_step_index <- which(vapply(
    prepared_recipe$steps,
    function(step) inherits(step, "step_pca"),
    logical(1)
  ))

  pca_fit <- prepared_recipe$steps[[pca_step_index]]$res
  pc_variance <- (pca_fit$sdev^2) / sum(pca_fit$sdev^2)

  pca_variance_table <- data.frame(
    Principal_Component = paste0("PC", seq_along(pc_variance)),
    Variance_Explained = pc_variance,
    Cumulative_Variance = cumsum(pc_variance)
  )

  cat("\nPCA variance explained:\n")
  print(pca_variance_table)
}

# ---------------------------
# 7. Logistic Regression using PCs
# ---------------------------
logistic_formula <- stats::as.formula(
  paste("paid_ai ~", paste(pc_columns, collapse = " + "))
)

logistic_model <- stats::glm(
  formula = logistic_formula,
  data = train_pca,
  family = stats::binomial(link = "logit")
)

cat("\nLogistic Regression model summary:\n")
print(summary(logistic_model))

if (!isTRUE(logistic_model$converged)) {
  warning("Logistic Regression did not converge. Review the model and retained PCs.")
}

# ---------------------------
# 8. Validation predictions
# ---------------------------
valid_probability <- stats::predict(
  logistic_model,
  newdata = valid_pca,
  type = "response"
)

# Default classification threshold = 0.50.
valid_prediction <- factor(
  ifelse(valid_probability >= 0.50, "Yes", "No"),
  levels = c("No", "Yes")
)

# ---------------------------
# 9. Evaluate the model
# ---------------------------
confusion_matrix <- table(
  Actual = factor(valid_pca$paid_ai, levels = c("No", "Yes")),
  Predicted = factor(valid_prediction, levels = c("No", "Yes"))
)

cat("\nConfusion Matrix (rows = actual, columns = predicted):\n")
print(confusion_matrix)

TN <- unname(confusion_matrix["No", "No"])
FP <- unname(confusion_matrix["No", "Yes"])
FN <- unname(confusion_matrix["Yes", "No"])
TP <- unname(confusion_matrix["Yes", "Yes"])

safe_divide <- function(numerator, denominator) {
  if (denominator == 0) NA_real_ else numerator / denominator
}

accuracy <- safe_divide(TP + TN, TP + TN + FP + FN)
precision <- safe_divide(TP, TP + FP)
recall <- safe_divide(TP, TP + FN)          # Sensitivity for the Yes class
specificity <- safe_divide(TN, TN + FP)
f1_score <- safe_divide(2 * precision * recall, precision + recall)

roc_object <- pROC::roc(
  response = valid_pca$paid_ai,
  predictor = valid_probability,
  levels = c("No", "Yes"),
  direction = "<",
  quiet = TRUE
)
auc_value <- as.numeric(pROC::auc(roc_object))

metrics <- data.frame(
  Metric = c("Accuracy", "Precision (Yes)", "Recall / Sensitivity (Yes)",
             "Specificity (No)", "F1-score (Yes)", "ROC-AUC"),
  Value = c(accuracy, precision, recall, specificity, f1_score, auc_value)
)

cat("\nValidation metrics:\n")
print(metrics)

# ---------------------------
# 10. PCA 3D visualization (training data)
# ---------------------------
# This plot is for visual exploration only. The model still uses every PC
# retained by the 90% variance threshold, not only PC1-PC3.
if (USE_PCA && length(pc_columns) >= 3) {

  pca_3d <- plotly::plot_ly(
    data = train_pca,
    x = train_pca[[pc_columns[1]]],
    y = train_pca[[pc_columns[2]]],
    z = train_pca[[pc_columns[3]]],
    color = train_pca$paid_ai,
    colors = c("No" = "#E76F51", "Yes" = "#2A9D8F"),
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 4, opacity = 0.8)
  ) %>%
    plotly::layout(
      title = "Training Data: First 3 Principal Components",
      scene = list(
        xaxis = list(title = pc_columns[1]),
        yaxis = list(title = pc_columns[2]),
        zaxis = list(title = pc_columns[3])
      ),
      legend = list(title = list(text = "Monthly AI subscription"))
    )

  output_dir <- file.path(
    dirname(file_path),
    result_folder
  )

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  htmlwidgets::saveWidget(
    pca_3d,
    file = file.path(output_dir, "pca_3d_training.html"),
    selfcontained = FALSE
  )
}

# ---------------------------
# 11. Save outputs
# ---------------------------
output_dir <- file.path(dirname(file_path), result_folder)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

validation_predictions <- data.frame(
  Actual = valid_pca$paid_ai,
  Predicted = valid_prediction,
  Probability_Pay_AI = valid_probability
)

write.csv(
  validation_predictions,
  file = file.path(output_dir, "validation_predictions.csv"),
  row.names = FALSE
)

write.csv(
  metrics,
  file = file.path(output_dir, "validation_metrics.csv"),
  row.names = FALSE
)

if (!is.null(pca_variance_table)) {
  write.csv(
    pca_variance_table,
    file = file.path(output_dir, "pca_variance_explained.csv"),
    row.names = FALSE
  )
}

# Save the fitted preprocessing recipe and Logistic Regression model.
saveRDS(
  list(
    preprocessing_recipe = prepared_recipe,
    logistic_regression = logistic_model,
    principal_components = pc_columns,
    classification_threshold = 0.50
  ),
  file = file.path(output_dir, "pca_logistic_model.rds")
)

# Save ROC curve as PNG.
png(
  filename = file.path(output_dir, "roc_curve_logistic_pca.png"),
  width = 900,
  height = 700,
  res = 120
)
plot(
  roc_object,
  legacy.axes = TRUE,
  main = paste0("ROC Curve - ", if (USE_PCA) "PCA + " else "No PCA, ",
                "Logistic Regression (AUC = ",
                round(auc_value, 3), ")"),
  lwd = 2,
  col = "blue"
)
abline(a = 0, b = 1, lty = 2, col = "gray")
dev.off()

cat("\nAll outputs saved in:\n", output_dir, "\n")
cat("Files created:\n")
cat("- validation_predictions.csv\n")
cat("- validation_metrics.csv\n")
cat("- pca_variance_explained.csv\n")
cat("- pca_3d_training.html (if at least 3 PCs were retained)\n")
cat("- roc_curve_logistic_pca.png\n")
cat("- pca_logistic_model.rds\n")
