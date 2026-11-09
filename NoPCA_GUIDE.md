# Running the No-PCA Logistic Regression Baseline

This guide explains how to run `pca_logistic_ai_subscription_noPCA.R`, the benchmark model that predicts whether a CSE student pays for a monthly AI subscription **without** PCA, so its results can be compared with the PCA model in `pca_logistic_ai_subscription.R`.

## What the script does

1. Reads the survey Excel file and keeps only respondents who answered Yes to the CSE screening question (150 of 158 records).
2. Uses Q5 (monthly AI subscription, Yes or No) as the target and the other 16 survey questions as predictors.
3. Splits the data 80/20 with stratification and `set.seed(2026)`: 119 training records and 31 validation records. The split is identical to the PCA model, so the comparison is fair.
4. Imputes missing values with the training median, dummy-encodes institution type and standardizes all predictors. All of this is learned on the training set only.
5. **Skips PCA** and fits Logistic Regression directly on the 17 standardized predictors.
6. Evaluates on the validation set and saves the results.

## The PCA switch

Near the top of the script there is one switch:

```r
USE_PCA <- FALSE
```

| Setting | Model | Results folder |
|---|---|---|
| `USE_PCA <- FALSE` | Logistic Regression on the 17 standardized predictors | `NoPCA_Logistic_Results/` |
| `USE_PCA <- TRUE` | Logistic Regression on the principal components (90% variance threshold) | `PCA_Logistic_Results/` |

Everything else in the pipeline is the same in both modes.

## Requirements

1. R 4.x installed (download from https://cran.r-project.org).
2. The survey file `Khảo sát thói quen sử dụng AI của sinh viên CSE Việt Nam _ AI Usage Habits Survey for CSE Students in Vietnam (Responses).xlsx` in the repository folder.
3. R packages: `readxl`, `dplyr`, `stringr`, `rsample`, `recipes`, `pROC`, `plotly`, `htmlwidgets`. The script installs any that are missing.

The first run can take **5 to 15 minutes** because some packages are compiled from source. Later runs take a few seconds.

## How to run

1. Open a terminal in the repository folder and start R:
   ```bash
   R
   ```
2. Tell R to pick up the Excel file automatically, so you do not have to type its long name:
   ```r
   file.choose <- function(...) list.files(pattern = "\\.xlsx$")[1]
   ```
3. Run the script:
   ```r
   source("pca_logistic_ai_subscription_noPCA.R")
   ```
4. Check that the console prints `Model mode: WITHOUT PCA` and `Number of model inputs: 17`.

If you skip step 2, R asks `Enter file name:`. Type the full file name **including** `.xlsx` (type `Khảo` and press Tab to autocomplete).

## Output files

The script writes these files to `NoPCA_Logistic_Results/`:

| File | Content |
|---|---|
| `validation_metrics.csv` | Accuracy, precision, recall, specificity, F1 and ROC-AUC |
| `validation_predictions.csv` | Actual class, predicted class and predicted probability of paying for each validation student |
| `roc_curve_logistic_pca.png` | ROC curve of the no-PCA model (the file name is shared with the PCA version) |
| `pca_logistic_model.rds` | Fitted preprocessing recipe and Logistic Regression model |

No PCA variance table or 3D plot is created in this mode.

## Comparing with the PCA model

Run the PCA model first if `PCA_Logistic_Results/` does not exist yet, then paste this into R:

```r
pca <- read.csv("PCA_Logistic_Results/validation_metrics.csv")
nopca <- read.csv("NoPCA_Logistic_Results/validation_metrics.csv")
data.frame(Metric = pca$Metric, PCA = round(pca$Value, 3), NoPCA = round(nopca$Value, 3))
```

Results from our run on 150 CSE respondents:

| Metric | With PCA (12 PCs) | Without PCA (17 predictors) |
|---|---|---|
| Accuracy | 0.677 | 0.645 |
| Precision (Yes) | 0.667 | 0.650 |
| Recall (Yes) | 0.824 | 0.765 |
| Specificity (No) | 0.500 | 0.500 |
| F1 (Yes) | 0.737 | 0.703 |
| ROC-AUC | 0.702 | 0.634 |

## How to read the results

1. The PCA model is slightly better on every metric except specificity, which is equal.
2. The gap is small. The validation set has only 31 students, and the accuracy difference (21 versus 20 correct) is one student. Report it as "PCA reduces 17 inputs to 12 components without losing predictive performance", not as a clear improvement.
3. Both models predict "Yes" too often: half of the non-paying students are classified as paying.
4. For a stronger conclusion, compare the two ROC curves with a DeLong test (`pROC::roc.test`) or use repeated cross-validation instead of a single split.

## Troubleshooting

| Problem | Fix |
|---|---|
| ``Error: `path` does not exist`` | The file name was typed without `.xlsx`. Use step 2 of "How to run", or type the full name. |
| `cannot open file 'NoPCA_Logistic_Results/validation_metrics.csv'` | The no-PCA run did not finish. Fix the error above and run the script again. |
| Console shows `Model mode: WITH PCA` | The switch is still `TRUE`. Set `USE_PCA <- FALSE`, save the file and run again. |
| Package installation fails | Run `install.packages(c("readxl", "dplyr", "stringr", "rsample", "recipes", "pROC", "plotly", "htmlwidgets"))` and read the first error it prints. |
