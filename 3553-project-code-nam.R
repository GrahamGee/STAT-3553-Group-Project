library(tidyverse) # this is for ggplot2 and all of that stuff

library(readxl) # This library is used to read the excel table in. Prevents the need to convert over to csv as we received the files in xlsx format.
library(lars)
library(faraway)

# Loading in the data ------------------------------------------------

m_basket <- read_excel(path="~/3553-final-project/unclean-data/cmj/Men's Basketball_CMJ.xlsx")
w_basket<- read_excel(path="~/3553-final-project/unclean-data/cmj/Women's Basketball_CMJ.xlsx")
m_soc<- read_excel(path="~/3553-final-project/unclean-data/cmj/Men's Soccer_CMJ.xlsx")
w_soc<- read_excel(path="~/3553-final-project/unclean-data/cmj/Women's Soccer_CMJ.xlsx")
m_hock<- read_excel(path="~/3553-final-project/unclean-data/cmj/Men's Hockey_CMJ.xlsx")
w_hock<- read_excel(path="~/3553-final-project/unclean-data/cmj/Women's Hockey_CMJ.xlsx")
m_foot<- read_excel(path="~/3553-final-project/unclean-data/cmj/Football_CMJ.xlsx")
w_rug<- read_excel(path="~/3553-final-project/unclean-data/cmj/Women's Rugby_CMJ.xlsx")

# Cleaning data -----------------------------------------------------------

# Here I've used the maggritr pipe (|> or %>%) operator from the dyplr package. This allows me to efficiently declare commands. It can be read as "and then". I've also used a flurry of dplyr subsetting functions that allow me to perform data cleaning operations. 

# The first cleaning operation can be read the following: 

# Initialize a dataframe called m_basket and then read in data from m_basket `and then (|>)` keep any rows that have the Type column equal to "Countermovement Jump" and have NA values in the Tags column - essentially get rid of any rows that have CMJ arms or whatever - `and then` create two new columns called sex and sport with the values "m" and "basketball" respectively `and then` move those columns before system weight `and then` convert all columns after system weight to be numeric. The last step is necessary because "N/A"s in the excel form are registered as characters in the R. 

m_basket <- m_basket |>
  filter((Type == "Countermovement Jump") & is.na(Tags)) |>
  mutate(sex = "m",
         sport = "basketball") |>
  relocate(sex, sport, .before = `System Weight`) |>
  mutate(across(12:90, as.numeric)) #across lets me perform the mutate function across multiple columns 

w_basket <- w_basket |>
  filter((Type == "Countermovement Jump") & is.na(Tags)) |>
  mutate(sex = "w",
         sport = "basketball") |>
  relocate(sex, sport, .before = `System Weight`) |>
  mutate(across(12:90, as.numeric))

m_soc <- m_soc |>
  filter((Type == "Countermovement Jump") & is.na(Tags)) |>
  mutate(sex = "m",
         sport = "soccer") |>
  relocate(sex, sport, .before = `System Weight`) |>
  mutate(across(12:90, as.numeric))

w_soc <- w_soc |>
  filter((Type == "Countermovement Jump") & is.na(Tags)) |>
  mutate(sex = "w",
         sport = "soccer") |>
  relocate(sex, sport, .before = `System Weight`) |>
  mutate(across(12:90, as.numeric))

m_hock <- m_hock |>
  filter((Type == "Countermovement Jump") & is.na(Tags)) |>
  mutate(sex = "m",
         sport = "hockey") |>
  relocate(sex, sport, .before = `System Weight`) |>
  mutate(across(12:90, as.numeric))

w_hock <- w_hock |>
  filter((Type == "Countermovement Jump") & is.na(Tags)) |>
  mutate(sex = "w",
         sport = "hockey") |>
  relocate(sex, sport, .before = `System Weight`) |>
  mutate(across(12:90, as.numeric))

w_rug <- w_rug |>
  filter((Type == "Countermovement Jump") & is.na(Tags)) |>
  mutate(sex = "w",
         sport = "rugby") |>
  relocate(sex, sport, .before = `System Weight`) |>
  mutate(across(12:90, as.numeric))

m_foot <- m_foot |>
  filter((Type == "Countermovement Jump") & is.na(Tags)) |>
  mutate(sex = "m",
         sport = "football") |>
  relocate(sex, sport, .before = `System Weight`) |>
  mutate(across(12:90, as.numeric))


# Here I am binding all the dataframes together into one large dataframe.
data <- bind_rows(m_basket, w_basket, m_soc, w_soc, m_hock, w_hock, w_rug, m_foot)


# Here I've subsetted our large data set into a smaller one that contains only the columns from sex on wards. Clearly we don't need the jump id and so forth. 
data2 <- data |> 
  select(sex:`Relative Propulsive Net Impulse`)

# I'm rearranging the columns here so that they are listed alphabetically to better match the data dictionary
data2 <- data2 |> 
  mutate(
    sex = as.factor(sex), # 
    sport = as.factor(sport)
  ) |> 
  select(order(colnames(data2))) |> 
  relocate(where(is.numeric), .after = where(is.factor)) |> 
  na.omit() # this last line omits rows with NA's in them (need to look at this again)

rm(data, m_basket, w_basket, m_soc, w_soc, m_hock, w_hock, w_rug, m_foot)

# Multicollinearity checks ------------------------------------------------

library(reshape2)
cor_2 <- round(cor(data2[3:81], use="complete.obs"),2)

CM <- cor_2
CM[lower.tri(CM, diag=TRUE)] <- NA

threshold <- 0.30

cor_predictors <- subset(melt(CM, na.rm=TRUE), abs(value) <= (threshold))

cor_predictors_2 <- cor_predictors |> 
  filter(Var1 == "Jump Height")

data3 <- data2 |> 
  select(-c(cor_predictors_2$Var2))


# VIF Check ---------------------------------------------------------------

for (i in 1:ncol(data3)) {
  vif(lm(`Jump Height` ~ data3[,1:i], data=data3))
}

mod_3 <- lm(`Jump Height` ~ sex + sport + data3$`Avg. Landing Force` + data3$`Avg. Braking Force` + data3$`Avg. Braking Power` + data3$`Avg. Braking Velocity`+ data3$`Avg. Propulsive Force` + data3$`Avg. Propulsive Power` +data3$`Avg. Propulsive Velocity` + data3$`Avg. Relative Braking Force` + data3$`Avg. Relative Braking Power` + data3$`Right Avg. Braking Force` + data3$`Takeoff Velocity` + data3$RSI + data3$`Right Force at Peak Propulsive Force` + data3$`Right Force at Peak Landing Force` +data3$`Right Force at Peak Braking Force`+data3$`Right Avg. Propulsive Force`+data3$`Right Avg. Braking RFD`+data3$`Right Avg. Braking Force`+data3$`Relative Propulsive Net Impulse` + data3$`Relative Propulsive Impulse` +data3$`Relative Peak Landing Force`+data3$`Relative Force at Min Displacement`+data3$`Relative Braking Net Impulse`+data3$`Propulsive Net Impulse`+data3$`Peak Velocity`+data3$`Peak Relative Propulsive Power`+data3$`Peak Relative Propulsive Force`+data3$`Peak Relative Braking Power`+data3$`Peak Relative Braking Force`+data3$`Peak Propulsive Power`+data3$`Peak Propulsive Force`+data3$`Peak Landing Force`+data3$`Peak Braking Velocity`+data3$`Peak Braking Power`+data3$`Peak Braking Force`+data3$mRSI+data3$`Jump Momentum`+, data = data3)

mod_3 <- lm(data3$`Jump Height` ~ . - data3$`Left Force at Peak Propulsive Force` - data3$`Left Force at Peak Braking Force` - data3$`Left Avg. Propulsive Force` - data3$`Left Avg. Braking RFD` - data3$`Left Avg. Braking Force`, data = data3)
# left force at peak propulsive force is perfectly correlated with some other predictors

vif(mod_3)
alias(mod_4)

library(car)  # Ensure the 'car' package is loaded for vif()

# Initialize a list to store results
vif_results <- list()

# Iterate over each predictor
for (i in 1:ncol(data3)) {
  predictor_subset <- colnames(data3)[1:i]  # Get predictor names up to column i
  
  # Try to calculate VIF for the current subset
  tryCatch({
    # Fit the model
    model <- lm(`Jump Height` ~ ., data = data3[, c("Jump Height", predictor_subset)])
    
    # Calculate VIF and store it
    vif_results[[i]] <- vif(model)
  }, error = function(e) {
    # Print error message and problematic predictors
    message("Error in iteration ", i, ": ", conditionMessage(e))
    message("Predictors causing the issue: ", paste(predictor_subset, collapse = ", "))
  })
}

# View the VIF results
vif_results


# LASSO  -----------------------------------------------------------------

which(colnames(design_matrix) == "`Jump Height`")

design_matrix <- model.matrix(~ . -1, data = data2)

lmod <- lars(design_matrix[,-27], data2$`Jump Height`)
lmod

# Model Diagnostics After MC and LASSO -------------------------------------------------------

mod_1 <- lm(`Jump Height` ~ ., data = data2)
mod_residuals <- residuals(mod_1)
mod_fitted <- fitted(mod_1)

length(mod_residuals)
length(mod_fitted)

plot <- ggplot(data2, aes(x = mod_fitted, y = mod_residuals)) +
                 geom_point() + xlab("Model Fitted Values") +ylab("Model Residuals") + 
                 geom_hline(yintercept = 0)
plot

# Check the normality assumption ------------------------------------------

qqnorm(mod_residuals,
       ylab = "Residuals",
       main = "",
       lwd = 2)
qqline(mod_residuals) # clearly this doesn't look normal. 

shapiro.test(mod_residuals) # This won't work because Shapiro Test only takes sample size between 3 and 5000. 


# Check for autocorrelation -----------------------------------------------


# Check for influential points --------------------------------------------


# PCA Test ----------------------------------------------------------------


