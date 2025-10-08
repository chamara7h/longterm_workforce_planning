## Regression model for Eng

## Load libraries 

library(tidyverse)
library(tsibble)
library(gtsummary)


## Load data

nurse_df <- read_rds("data/eng_workforce_master.rds")



## Scale population data. Convert it to thousand

nurse_reg <- nurse_df %>% mutate(age_15_19 = age_15_19/1000,
                                 age_20_24 = age_20_24/1000,
                                 age_25_29 = age_25_29/1000,
                                 age_30_34 = age_30_34/1000,
                                 age_35_39 = age_35_39/1000,
                                 age_40_44 = age_40_44/1000,
                                 age_45_49 = age_45_49/1000,
                                 age_50_54 = age_50_54/1000,
                                 age_55_59 = age_55_59/1000,
                                 age_60_64 = age_60_64/1000) %>%
  
  select(!c(org_name, ics_name, nhse_region_name, hc_lag1, hc_lag2, hc_lag3, hc_lag4))



## Convert categorical variables as factors. So, we can treat categorical variables easily

months_lable <- c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")
months_level <- unique(nurse_reg$month)

nurse_reg_tidy <- nurse_reg %>%
  
  mutate(month = factor(month, levels = months_level, labels = months_lable),
         
         org_code = as.factor(org_code),
         
         nhse_region_code = as.factor(nhse_region_code), 
         
         ics_code = as.factor(ics_code)) %>%
  
  select(!c(date, month))


nurse_reg_tidy_train <- nurse_reg_tidy %>% filter(year <= 2022)

nurse_reg_tidy_future <- nurse_reg_tidy %>% filter(year > 2022)

## model fitting

fit_reg <- lm(hc ~ ., data = nurse_reg_tidy_train)

summary(fit_reg)

confint(fit_reg)
 
## remove nhse_code, ICS_code and total_nhs_spend_on_mental_health due to singularities

nurse_reg_updated <- nurse_reg_tidy %>% select(!c(nhse_region_code, ics_code, total_nhs_spend_on_mental_health,
                                                  graduates_per_1000_inhabitants_lag1, garduates_per_1000_inhabitants, SFTN, IFTN))

fit_reg <- lm(hc ~ ., data = nurse_reg_updated)

summary(fit_reg)

car::Anova(fit_reg, type = 3)



# Combine all of the output into a single table


fit_reg %>%
  tbl_regression(intercept = T,
                 estimate_fun = function(x) style_sigfig(x, digits = 3),
                 pvalue_fun   = function(x) style_pvalue(x, digits = 3)) %>%
  add_global_p(keep = T) 



## residuals

r1  <- resid(fit_reg)
r2  <- rstandard(fit_reg)
r3  <- rstudent(fit_reg)



## Compare means and standard deviations

COMPARE <- round(data.frame(Mean = c(mean(r1), mean(r2), mean(r3)),
                            SD   = c(sd(r1),   sd(r2),   sd(r3))), 5)

rownames(COMPARE) <- c("Unstandardized", "Standardized", "Studentized")




## ploting residuals

RESID <- fit_reg$residuals

par(mfrow=c(1,2))

hist(RESID, xlab = "Residuals", probability = T,
     
     # Adjust ylim to be able to see more of the curves if needed
     
     ylim = c(0, 0.002))

# Superimpose empirical density (no normality assumption, for comparison)

lines(density(RESID, na.rm=T), lwd = 2, col = "red")

# Superimpose best fitting normal curve

curve(dnorm(x, mean = mean(RESID, na.rm=T), sd = sd(RESID, na.rm=T)),
      lty = 2, lwd = 2, add = TRUE, col = "blue")



# Normal quantile-quantile (QQ) plot

qqnorm(as.numeric(RESID), col="red", pch=20)

qqline(as.numeric(RESID), col="blue", lty=2, lwd=2)



## Visualizing the adjusted relationships

# To plot for all the predictors

#car::avPlots(fit_reg, ask = F, layout = c(2,3))

#car::avPlots(fit_reg, terms = . ~ org_code + hc_leavers + hc_joiners)



## Make predictions

nurse_reg_tidy_future <- nurse_reg_tidy_future %>% select(!c(nhse_region_code, ics_code, total_nhs_spend_on_mental_health,
                                    graduates_per_1000_inhabitants_lag1, garduates_per_1000_inhabitants, SFTN, IFTN))


fc_reg <- predict(fit_reg, nurse_reg_tidy_future, interval = "confidence")

fc_reg_df <- as.data.frame(fc_reg)



## combine predicted values to original dataframe


nurse_reg_tidy_pred <- nurse_reg_tidy_future %>% select(year, org_code, hc) %>%
  
  cbind(fc_reg_df)



## CIs for regression coefficients

confint(fit_reg)











