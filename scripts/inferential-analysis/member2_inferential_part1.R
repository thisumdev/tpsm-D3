# Load the cleaned dataset
hr_data = read.csv("~/Desktop/tpsm-D3/data/cleaned_data.csv")

# Inspect structure
head(hr_data)
str(hr_data)
dim(hr_data)

# Normality Testing

# H0: Respect_Index in the Low Satisfaction group is normally distributed
# H1: Respect_Index in the Low Satisfaction group is NOT normally distributed
shapiro.test(low_sat)

# H0: Respect_Index in the High Satisfaction group is normally distributed
# H1: Respect_Index in the High Satisfaction group is NOT normally distributed
shapiro.test(high_sat)

# Q-Q Plot for Low Satisfaction group
qqnorm(low_sat,
       main = "Q-Q Plot: Respect Index (Low Satisfaction Group)",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       col  = "tomato",
       pch  = 16)
qqline(low_sat, col = "darkred", lwd = 2)

# Q-Q Plot for High Satisfaction group
qqnorm(high_sat,
       main = "Q-Q Plot: Respect Index (High Satisfaction Group)",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       col  = "steelblue",
       pch  = 16)
qqline(high_sat, col = "darkblue", lwd = 2)

# Save Q-Q plots to file
png("~/Desktop/tpsm-D3/plots/inferential-analysis/infer_qq_plots.png",
    width = 900, height = 450)

par(mfrow = c(1, 2))

qqnorm(low_sat,
       main = "Q-Q Plot: Respect Index (Low Satisfaction Group)",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       col  = "tomato",
       pch  = 16)
qqline(low_sat, col = "darkred", lwd = 2)

qqnorm(high_sat,
       main = "Q-Q Plot: Respect Index (High Satisfaction Group)",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       col  = "steelblue",
       pch  = 16)
qqline(high_sat, col = "darkblue", lwd = 2)

par(mfrow = c(1, 1))
dev.off()

# Variance Ratio Test 

# H0: Variances of Respect_Index are equal between Low and High Satisfaction groups
# H1: Variances of Respect_Index are NOT equal between the two groups
var.test(high_sat, low_sat)

# Store variance test result to use in t-test
var_result = var.test(high_sat, low_sat)
equal_var   = var_result$p.value >= 0.05
equal_var

# Two-Sample t-Test 

# H0: Mean Respect_Index is equal in both groups (mu_high = mu_low)
# H1: Mean Respect_Index is higher in High Satisfaction group (mu_high > mu_low)
# One-tailed test, alpha = 0.05
t.test(high_sat, low_sat, alternative = "greater", var.equal = equal_var)

# Store t-test result
t_result = t.test(high_sat, low_sat, alternative = "greater", var.equal = equal_var)

# Group means
mean(high_sat)
mean(low_sat)

# Cohen's d — Effect Size
pooled_sd = sqrt(((length(high_sat) - 1) * var(high_sat) +
                    (length(low_sat)  - 1) * var(low_sat)) /
                   (length(high_sat) + length(low_sat) - 2))

cohens_d = (mean(high_sat) - mean(low_sat)) / pooled_sd
cohens_d
# Benchmarks: < 0.2 negligible | 0.2-0.5 small | 0.5-0.8 medium | > 0.8 large

# One-Sample t-Test

# H0: Population mean Respect_Index = 2.5 (midpoint of scale 1 to 4)
# H1: Population mean Respect_Index is not equal to 2.5 (two-tailed)
t.test(hr_data$Respect_Index, mu = 2.5)

mean(hr_data$Respect_Index)

# Store all test results
shapiro_low  = shapiro.test(low_sat)
shapiro_high = shapiro.test(high_sat)
var_result   = var.test(high_sat, low_sat)
t_result     = t.test(high_sat, low_sat, alternative = "greater", var.equal = equal_var)
one_t_result = t.test(hr_data$Respect_Index, mu = 2.5)

# Summary Table
results = data.frame(
  Test = c(
    "Shapiro-Wilk (Low Sat)",
    "Shapiro-Wilk (High Sat)",
    "F Variance Ratio Test",
    "Two-Sample t-Test (1-tail)",
    "One-Sample t-Test (vs 2.5)"
  ),
  Variables = c(
    "Respect_Index",
    "Respect_Index",
    "Respect_Index (H vs L)",
    "Respect_Index (H vs L)",
    "Respect_Index"
  ),
  Statistic = c(
    round(shapiro_low$statistic,  4),
    round(shapiro_high$statistic, 4),
    round(var_result$statistic,   4),
    round(t_result$statistic,     4),
    round(one_t_result$statistic, 4)
  ),
  P_Value = c(
    round(shapiro_low$p.value,  6),
    round(shapiro_high$p.value, 6),
    round(var_result$p.value,   6),
    round(t_result$p.value,     6),
    round(one_t_result$p.value, 6)
  ),
  Decision = c(
    ifelse(shapiro_low$p.value  < 0.05, "Reject H0", "Fail to Reject H0"),
    ifelse(shapiro_high$p.value < 0.05, "Reject H0", "Fail to Reject H0"),
    ifelse(var_result$p.value   < 0.05, "Reject H0", "Fail to Reject H0"),
    ifelse(t_result$p.value     < 0.05, "Reject H0", "Fail to Reject H0"),
    ifelse(one_t_result$p.value < 0.05, "Reject H0", "Fail to Reject H0")
  )
)

print(results)