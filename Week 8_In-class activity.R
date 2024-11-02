
### WEEK 8 IN-CLASS ACTITVITY ####
### Date: Mon. Oct. 28, 2024 ####

library(readr)
final_data <- read_csv("Version Control/Armed-conflict/data/final_data.csv")
View(final_data)

finaldata$pctpopdens <-finaldata$popdens / 100

# Fit a panel model with fixed effect for country

matmormod <- lm(Maternal_Mortality ~ -1 + armconf1 + gdp1000 + OECD + pctpopdens + urban +
              agedep + male_edu + temp + rainfall1000 + earthquake + drought +
              ISO, data = finaldata)



library(plm)

matmorplm <- plm(Maternal_Mortality ~ armconf1 + gdp1000 + OECD +pctpopdens + urban + agedep + 
                   male_edu + temp + rainfall1000 + earthquake + drought,
                 index = c("ISO"), 
                 model = "within",
                data = finaldata)
  

# Compare the coefficients from the two models
install.packages("texreg")
library(texreg)

screenreg(list(matmormod,matmorplm))

##### Fit models adding year (fixed effects model for country and time) #####

lmmod <- lm(Maternal_Mortality ~ -1 + armconf1 + gdp1000 + OECD + popdens + urban +
              agedep + male_edu + temp + rainfall1000 + earthquake + drought +
              ISO + as.factor(Year), 
            data = finaldata)

summary(lmmod)


plmmod <- plm(Maternal_Mortality ~ armconf1 + gdp1000 + OECD + popdens + urban + 
                agedep + male_edu + temp + rainfall1000 + earthquake + drought,
              index = c("ISO", "Year"),
              effect = "twoways",
              model = "within",
              data = finaldata)

summary(plmmod)

# Compare the coefficients from the two models
screenreg(list(lmmod,plmmod))


# Fit models using the the other mortality covariates as main predictors

preds <- as.formula(" ~ armconf1 + gdp1000 + OECD + popdens + urban + 
                  agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                  ISO + as.factor(Year)")

matmormod <- lm(update.formula(preds, Maternal_Mortality ~ .), data = finaldata)
un5mormod <- lm(update.formula(preds, Under5_Mortality ~ .), data = finaldata)
infmormod <- lm(update.formula(preds, Infant_Mortality ~ .), data = finaldata)
neomormod <- lm(update.formula(preds, Neonatal_Mortality ~ .), data = finaldata)


# Log transform the GDP variable
finaldata$log_gdp1000 <- log(finaldata$gdp1000)

#Fit new plm models with the new GDP variable

preds2 <- as.formula(" ~ armconf1 + log_gdp1000 + OECD + popdens + urban + 
                  agedep + male_edu + temp + rainfall1000 + earthquake + drought,  
                  index = c("ISO", "Year"), effect = "twoways",
                     model = "within",
                     data = finaldata)

matmormod2 <- plm(update.formula(preds2, Maternal_Mortality ~ .), data = finaldata)
un5mormod2 <- plm(update.formula(preds2, Under5_Mortality ~ .), data = finaldata)
infmormod2 <- plm(update.formula(preds2, Infant_Mortality ~ .), data = finaldata)
neomormod2 <- plm(update.formula(preds2, Neonatal_Mortality ~ .), data = finaldata)


# Compare the coefficients from the two models
screenreg(list(matmormod2,un5mormod2,infmormod2,neomormod2))


