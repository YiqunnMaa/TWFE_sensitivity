### reference model and alternative specifications
library(dplyr)
library(fixest)
library(splines)

### (1) run the reference model (Model 0)
model = feglm(n_deaths ~ popw_mean_pm2.5 + popw_monthly_mean_tempF + popw_total_precip| fipsihme+year, 
              data = data.all,
              offset = log(data.all$pop),
              vcov = "iid",
              family = "quasipoisson")


### (2) models with different data aggregation methods (Model 0, Models 1-1 to 1-7)
Exposure_weightings <- c("popw_mean_pm2.5 + popw_monthly_mean_tempF + popw_total_precip", # reference
                         "popw_mean_pm2.5 + popw_monthly_mean_tempF + areaw_total_precip",
                         "popw_mean_pm2.5 + areaw_monthly_mean_tempF + areaw_total_precip",
                         "popw_mean_pm2.5 + areaw_monthly_mean_tempF + popw_total_precip",
                         "areaw_mean_pm2.5 + popw_monthly_mean_tempF + popw_total_precip",
                         "areaw_mean_pm2.5 + popw_monthly_mean_tempF + areaw_total_precip",
                         "areaw_mean_pm2.5 + areaw_monthly_mean_tempF + popw_total_precip",
                         "areaw_mean_pm2.5 + areaw_monthly_mean_tempF + areaw_total_precip")

formulas <- c(paste0("n_deaths ~ ",Exposure_weightings, "| fipsihme+year"))

for (i in 1:length(formulas)){
  formula.i <- formulas[i]
  model = feglm(as.formula(formula.i), 
                data = data.all,
                offset = log(data.all$pop),
                vcov = "iid",
                family = "quasipoisson")
}

### (3) models with different temperature adjustments (Model 0, Models 2-1 to 2-4)
TEMPs <- c("popw_monthly_mean_tempF",
           "",
           "ns(popw_monthly_mean_tempF, df=3)",
           "ns(popw_monthly_mean_tempF, df=5)",
           "popw_days_u30 + popw_days_30to40 + popw_days_40to50 + popw_days_50to60 + popw_days_60to70 + popw_days_70to80 + popw_days_80to90 + popw_days_90to100 + popw_days_o100")
formulas <- c(paste0("n_deaths ~ popw_mean_pm2.5 + popw_total_precip +",
                     TEMPs,
                     "| fipsihme+year"))
formulas[2] <- "n_deaths ~ popw_mean_pm2.5 + popw_total_precip| fipsihme+year"

for (i in 1:length(formulas)){
  formula.i <- formulas[i]
  model = feglm(as.formula(formula.i), 
                data = data.all,
                offset = log(data.all$pop),
                vcov = "iid",
                family = "quasipoisson")
}

### (4) models with different precipitation adjustments (Model 0, Models 3-1 to 3-4)
PRCPs <- c("popw_total_precip",
           "",
           "ns(popw_total_precip, df=3)",
           "ns(popw_total_precip, df=5)",
           "popw_days_above_30mm")
formulas <- c(paste0("n_deaths ~ popw_mean_pm2.5 + popw_monthly_mean_tempF +",
                     PRCPs,
                     "| fipsihme+year"))
formulas[2] <- "n_deaths ~ popw_mean_pm2.5 + popw_monthly_mean_tempF| fipsihme+year"

for (i in 1:length(formulas)){
  formula.i <- formulas[i]
  model = feglm(as.formula(formula.i), 
                data = data.all,
                offset = log(data.all$pop),
                vcov = "iid",
                family = "quasipoisson")
}

### (5) models with different cominations of fixed effects (Model 0, Models 4-1 to 4-48)
FEs <- c(
  "fipsihme + year",
  "fipsihme + month",
  "fipsihme + year + month",
  "fipsihme + year^month",
  "fipsihme + statefips^year",
  "fipsihme + statefips^month",
  "fipsihme + statefips^year + statefips^month",
  "fipsihme + statefips^year^month",
  "fipsihme + fipsihme^year",
  "fipsihme + fipsihme^month",
  "fipsihme + fipsihme^year + fipsihme^month",
  "fipsihme + statefips^year + fipsihme^month",
  "fipsihme + fipsihme^year + statefips^month",
  "fipsihme + year + statefips^year",
  "fipsihme + year + statefips^month",
  "fipsihme + year + statefips^year + statefips^month",
  "fipsihme + year + statefips^year^month",
  "fipsihme + year + fipsihme^year",
  "fipsihme + year + fipsihme^month",
  "fipsihme + year + fipsihme^year + fipsihme^month",
  "fipsihme + year + statefips^year + fipsihme^month",
  "fipsihme + year + fipsihme^year + statefips^month",
  "fipsihme + month + statefips^year",
  "fipsihme + month + statefips^month",
  "fipsihme + month + statefips^year + statefips^month",
  "fipsihme + month + statefips^year^month",
  "fipsihme + month + fipsihme^year",
  "fipsihme + month + fipsihme^month",
  "fipsihme + month + fipsihme^year + fipsihme^month",
  "fipsihme + month + statefips^year + fipsihme^month",
  "fipsihme + month + fipsihme^year + statefips^month",
  "fipsihme + year + month + statefips^year",
  "fipsihme + year + month + statefips^month",
  "fipsihme + year + month + statefips^year + statefips^month",
  "fipsihme + year + month + statefips^year^month",
  "fipsihme + year + month + fipsihme^year",
  "fipsihme + year + month + fipsihme^month",
  "fipsihme + year + month + fipsihme^year + fipsihme^month",
  "fipsihme + year + month + statefips^year + fipsihme^month",
  "fipsihme + year + month + fipsihme^year + statefips^month",
  "fipsihme + year^month + statefips^year",
  "fipsihme + year^month + statefips^month",
  "fipsihme + year^month + statefips^year + statefips^month",
  "fipsihme + year^month + statefips^year^month",
  "fipsihme + year^month + fipsihme^year",
  "fipsihme + year^month + fipsihme^month",
  "fipsihme + year^month + fipsihme^year + fipsihme^month",
  "fipsihme + year^month + statefips^year + fipsihme^month",
  "fipsihme + year^month + fipsihme^year + statefips^month"
)

formulas <- c(paste0("n_deaths ~ popw_mean_pm2.5 + popw_monthly_mean_tempF + popw_total_precip|",
                     FEs))

for (i in 1:length(formulas)){
  formula.i <- formulas[i]
  model = feglm(as.formula(formula.i), 
                data = data.all,
                offset = log(data.all$pop),
                vcov = "iid",
                family = "quasipoisson")
}