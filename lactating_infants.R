
# Energy adjustment from breastfeeding adjusted by prevalence of breastfeeding 
# In the country/region and by infant age (months).


## From 0 to 12 months

milk_per_month <- as.data.frame(cbind(c(3:12), c(568,636,574,634,714,611,688,635,516,565)))

names(milk_per_month) <- c("months", "milk_g_day")

milk_per_month[, "kcal_milk"] <- round(milk_per_month$milk_g_day*(2.8/4.18))

rnorm(nrow(df_12months), 0, p)


## From 13 to 17 monthds



# From 18 to 23 monnths.


# Test dataset:
# 
 df <- data.frame(
   id = 1:6, 
  # age_y = c(0, 1, 2, 3, 4, 5), 
   age_months = c(16, 12, 3, 13, 10, 7), 
   sex = c("1", "2", "1", "2", "1", "2"),  # "M" = Male, "F" = Female
   weight = c(NA, 12, 15, NA, 20, NA) , # Missing weights to be filled
  Y_bf = rep(250, 6), 
  Y_nonbf = rep(0,6),
  w =rep(1, 6))


set.seed(1)

R <- 1000

res <- numeric(R)

for (r in 1:R) {

  B <- rep(NA_integer_, nrow(df))
  idx <- which(df$age_months >= 12 & df$age_months <= 17)
  B[idx] <- rbinom(length(idx), 1, 0.8)
  Y <- ifelse(B == 1, df$Y_bf, df$Y_nonbf)
  res[r] <- weighted.mean(Y, df$w, na.rm = TRUE)
}

mean_res <- mean(res)
se_res <- sd(res)            # MC standard error including status uncertainty
ci <- mean_res + c(-1,1)*1.96*se_res

