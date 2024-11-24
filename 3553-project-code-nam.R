library(tidyverse)
library(readxl)
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
    sex = as.factor(sex),  
    sport = as.factor(sport)
  ) |> 
  select(order(colnames(data2))) |> 
  relocate(where(is.numeric), .after = where(is.factor)) |> 
  na.omit() # this last line omits rows with NA's in them (need to look at this again)

rm(data, m_basket, w_basket, m_soc, w_soc, m_hock, w_hock, w_rug, m_foot)

# Multicollinearity checks ------------------------------------------------

library(reshape2)
cor_2 <- round(cor(data2[3:81], use="complete.obs"), 2)

CM <- cor_2

CM[lower.tri(CM, diag=TRUE)] <- NA

threshold <- 0.20

cor_predictors <- subset(melt(CM, na.rm=TRUE), abs(value) <= (threshold))

cor_predictors_2 <- cor_predictors |> 
  filter(Var1 == "Jump Height")

 
data3 <- data2 |> 
  select(-c(cor_predictors_2$Var2))


# VIF Check ---------------------------------------------------------------

# mod_3 <- lm(`Jump Height` ~ sex + sport + data3$`Avg. Landing Force` + data3$`Avg. Braking Force` + data3$`Avg. Braking Power` + data3$`Avg. Braking Velocity`+ data3$`Avg. Propulsive Force` + data3$`Avg. Propulsive Power` +data3$`Avg. Propulsive Velocity` + data3$`Avg. Relative Braking Force` + data3$`Avg. Relative Braking Power` + data3$`Right Avg. Braking Force` + data3$`Takeoff Velocity` + data3$RSI + data3$`Right Force at Peak Propulsive Force` + data3$`Right Force at Peak Landing Force` +data3$`Right Force at Peak Braking Force`+data3$`Right Avg. Propulsive Force`+data3$`Right Avg. Braking RFD`+data3$`Right Avg. Braking Force`+data3$`Relative Propulsive Net Impulse` + data3$`Relative Propulsive Impulse` +data3$`Relative Peak Landing Force`+data3$`Relative Force at Min Displacement`+data3$`Relative Braking Net Impulse`+data3$`Propulsive Net Impulse`+data3$`Peak Velocity`+data3$`Peak Relative Propulsive Power`+data3$`Peak Relative Propulsive Force`+data3$`Peak Relative Braking Power`+data3$`Peak Relative Braking Force`+data3$`Peak Propulsive Power`+data3$`Peak Propulsive Force`+data3$`Peak Landing Force`+data3$`Peak Braking Velocity`+data3$`Peak Braking Power`+data3$`Peak Braking Force`+data3$mRSI+data3$`Jump Momentum`+ data3$`Avg. Relative Propulsive Force`  + data3$`Avg. Relative Propulsive Power` + data3$`Braking Impulse` + data3$`Braking Net Impulse` + data3$`Braking Phase` + data3$`Braking Phase %` + data3$`Braking RFD` + data3$`Countermovement Depth` + data3$`Flight Time` + data3$`Force at Min Displacement` + data3$`Impulse Ratio`, data = data3)



data4 <- data3 |> 
  select(-c(`Left Force at Peak Propulsive Force`, `Left Force at Peak Braking Force`, `Left Avg. Propulsive Force`, `Left Avg. Braking RFD`, `Left Avg. Braking Force`, `Left Force at Peak Landing Force`))

mod_1 <- lm(`Jump Height` ~ . -sex -sport, data = data4)

vif(mod_1)

list_of_vif <- list()
high_vif <- str_remove_all(names(which.max(vif(mod_1))),"`")
list_of_vif <- append(list_of_vif , high_vif)

data4 <- data4 |> 
  select(-c(high_vif))
  
mod_2 <- lm(`Jump Height` ~ . -sex -sport, data = data4)

repeat {
  vif(mod_2)
  high_vif <- str_remove_all(names(which.max(vif(mod_2))),"`")
  list_of_vif <- append(list_of_vif , high_vif)
  
  data4 <- data4 |> 
    select(-c(high_vif))
  
  mod_2 <- lm(`Jump Height` ~ . -sex -sport, data = data4)
  vif(mod_2)
  
  if (max(vif(mod_2)) < 4) {
    break
  }
}
vif(mod_2)

# Model Diagnostics After MC and LASSO -------------------------------------------------------

mod_3 <- lm(`Jump Height` ~ -sex -sport., data = data4)
mod_residuals <- residuals(mod_3)
mod_fitted <- fitted(mod_3)

length(mod_residuals)
length(mod_fitted)

plot <- ggplot(data4, aes(x = mod_fitted, y = mod_residuals)) +
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
# LASSO  -----------------------------------------------------------------

design_matrix <- model.matrix(~ . -1, data = data4)

which(colnames(design_matrix) == "`Jump Height`")


lmod <- lars(design_matrix[,-10], data2$`Jump Height`)
lmod
round(lmod$Cp,2)
# PCA Test ----------------------------------------------------------------


