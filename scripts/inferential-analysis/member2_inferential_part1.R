# Load cleaned dataset 
hr_data <- read.csv("~/Desktop/tpsm-D3/data/cleaned_data.csv")

# Quick verification
cat("Dimensions:", dim(hr_data), "\n")
cat("Columns present:\n")
print(colnames(hr_data))

# Check the key columns we need
str(hr_data[, c("Respect_Index", "JobSatisfaction",
                "Satisfaction_Category", "Respect_Category")])

# Normality Testing

# Split Respect_Index into Low and High Satisfaction groups
low_sat  <- hr_data$Respect_Index[hr_data$JobSatisfaction %in% c(1, 2)]
high_sat <- hr_data$Respect_Index[hr_data$JobSatisfaction %in% c(3, 4)]

cat("Low Satisfaction group size :", length(low_sat), "\n")
cat("High Satisfaction group size:", length(high_sat), "\n")

# --- Shapiro-Wilk: Low Satisfaction group ---
# H0: Respect_Index in the Low Satisfaction group is normally distributed
# H1: Respect_Index in the Low Satisfaction group is NOT normally distributed
shapiro_low <- shapiro.test(low_sat)
cat("\nShapiro-Wilk Test — Low Satisfaction Group:\n")
print(shapiro_low)

# --- Shapiro-Wilk: High Satisfaction group ---
# H0: Respect_Index in the High Satisfaction group is normally distributed
# H1: Respect_Index in the High Satisfaction group is NOT normally distributed
shapiro_high <- shapiro.test(high_sat)
cat("\nShapiro-Wilk Test — High Satisfaction Group:\n")
print(shapiro_high)

# --- Decision ---
cat("\n--- Normality Decision ---\n")
cat("Low Sat  p-value:", round(shapiro_low$p.value, 6),
    "->", ifelse(shapiro_low$p.value < 0.05,
                 "NOT normal (reject H0)",
                 "Normal (fail to reject H0)"), "\n")
cat("High Sat p-value:", round(shapiro_high$p.value, 6),
    "->", ifelse(shapiro_high$p.value < 0.05,
                 "NOT normal (reject H0)",
                 "Normal (fail to reject H0)"), "\n")
cat("Note: Both groups have n > 30, so t-test is still robust\n")
cat("      due to the Central Limit Theorem.\n")

# --- Q-Q Plots for both groups ---
png("~/Desktop/tpsm-D3/plots/inferential-analysis/infer_qq_plots.png", width = 900, height = 450)
par(mfrow = c(1, 2))

qqnorm(low_sat,
       main = "Q-Q Plot: Respect Index\n(Low Satisfaction Group)",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       col  = "tomato",
       pch  = 16)
qqline(low_sat, col = "darkred", lwd = 2)

qqnorm(high_sat,
       main = "Q-Q Plot: Respect Index\n(High Satisfaction Group)",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       col  = "steelblue",
       pch  = 16)
qqline(high_sat, col = "darkblue", lwd = 2)

par(mfrow = c(1, 1))
dev.off()

cat("Q-Q plots saved to ~/Desktop/tpsm-D3/plots/inferential-analysis/infer_qq_plots.png\n")

# Variance Ratio (F) Test 

# H0: Variances of Respect_Index are equal between the two groups
# H1: Variances of Respect_Index are NOT equal between the two groups

var_test <- var.test(high_sat, low_sat)
cat("\nVariance Ratio (F) Test:\n")
print(var_test)

# Store decision for use in t-test below
equal_var <- var_test$p.value >= 0.05

cat("\n--- Variance Decision ---\n")
cat("p-value:", round(var_test$p.value, 6), "\n")
cat("Equal variances assumed:", equal_var, "\n")
cat(ifelse(equal_var,
           "-> Will use var.equal = TRUE in t-test\n",
           "-> Will use var.equal = FALSE (Welch t-test)\n"))

# Two-Sample t-Test 
# One-tailed: High Satisfaction group has HIGHER Respect_Index

# H0: Mean Respect_Index is equal in both groups (μ_high = μ_low)
# H1: Mean Respect_Index is higher in High Satisfaction group (μ_high > μ_low)
# One-tailed test, α = 0.05

t_test_result <- t.test(high_sat, low_sat,
                        alternative = "greater",
                        var.equal   = equal_var)

cat("\nTwo-Sample t-Test (one-tailed: μ_high > μ_low):\n")
print(t_test_result)

cat("\n--- Group Means ---\n")
cat("Mean Respect_Index — High Satisfaction:", round(mean(high_sat), 4), "\n")
cat("Mean Respect_Index — Low  Satisfaction:", round(mean(low_sat), 4), "\n")
cat("Difference (High - Low)               :", round(mean(high_sat) - mean(low_sat), 4), "\n")

# --- Cohen's d ---
pooled_sd <- sqrt(((length(high_sat) - 1) * var(high_sat) +
                     (length(low_sat)  - 1) * var(low_sat)) /
                    (length(high_sat) + length(low_sat) - 2))

cohens_d <- (mean(high_sat) - mean(low_sat)) / pooled_sd

cat("\n--- Effect Size ---\n")
cat("Cohen's d:", round(cohens_d, 4), "\n")
cat("Benchmarks: <0.2 negligible | 0.2-0.5 small | 0.5-0.8 medium | >0.8 large\n")
cat("Effect size interpretation:",
    ifelse(abs(cohens_d) < 0.2, "Negligible",
           ifelse(abs(cohens_d) < 0.5, "Small",
                  ifelse(abs(cohens_d) < 0.8, "Medium", "Large"))), "\n")

# One-Sample t-Test 
# Does overall mean Respect_Index differ from midpoint 2.5?

# H0: Population mean Respect_Index = 2.5 (midpoint of scale 1 to 4)
# H1: Population mean Respect_Index ≠ 2.5 (two-tailed)

one_sample_t <- t.test(hr_data$Respect_Index,
                       mu          = 2.5,
                       alternative = "two.sided")

cat("\nOne-Sample t-Test (Respect_Index vs midpoint 2.5):\n")
print(one_sample_t)

cat("\nSample Mean Respect_Index:", round(mean(hr_data$Respect_Index), 4), "\n")
cat("Test against mu =", 2.5, "\n")
cat("Decision:", ifelse(one_sample_t$p.value < 0.05,
                        "Reject H0 — mean significantly differs from 2.5",
                        "Fail to reject H0 — mean not significantly different from 2.5"), "\n")

# Summary Results Table

cat("\n")
cat(strrep("=", 108), "\n")
cat(sprintf("%-33s | %-24s | %-10s | %-10s | %-20s\n",
            "Test", "Variables", "Statistic", "p-value", "Decision (α = 0.05)"))
cat(strrep("-", 108), "\n")

results <- list(
  list(
    name  = "Shapiro-Wilk (Low Sat)",
    vars  = "Respect_Index",
    stat  = shapiro_low$statistic,
    pval  = shapiro_low$p.value,
    reject = shapiro_low$p.value < 0.05
  ),
  list(
    name  = "Shapiro-Wilk (High Sat)",
    vars  = "Respect_Index",
    stat  = shapiro_high$statistic,
    pval  = shapiro_high$p.value,
    reject = shapiro_high$p.value < 0.05
  ),
  list(
    name  = "F Variance Ratio Test",
    vars  = "Respect_Index (H vs L)",
    stat  = var_test$statistic,
    pval  = var_test$p.value,
    reject = var_test$p.value < 0.05
  ),
  list(
    name  = "Two-Sample t-Test (1-tail)",
    vars  = "Respect_Index (H vs L)",
    stat  = t_test_result$statistic,
    pval  = t_test_result$p.value,
    reject = t_test_result$p.value < 0.05
  ),
  list(
    name  = "One-Sample t-Test (=2.5)",
    vars  = "Respect_Index",
    stat  = one_sample_t$statistic,
    pval  = one_sample_t$p.value,
    reject = one_sample_t$p.value < 0.05
  )
)

for (r in results) {
  cat(sprintf("%-33s | %-24s | %-10s | %-10s | %-20s\n",
              r$name,
              r$vars,
              round(r$stat, 4),
              formatC(r$pval, format = "f", digits = 6),
              ifelse(r$reject, "Reject H0", "Fail to Reject H0")))
}

cat(strrep("=", 108), "\n")
cat("\nCohen's d for Two-Sample t-Test:", round(cohens_d, 4), "\n")