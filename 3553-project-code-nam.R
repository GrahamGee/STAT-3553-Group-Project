
library(tidyverse) # this is for ggplot2 and all of that stuff
library(readxl) # This library is used to read the excel table in. Prevents the need to convert over to csv as we received the files in xlsx format.

library(faraway) # for vif function
library(corrplot) # for nice correlation plots
library(lubridate)

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

# Here I've used the maggritr pipe (|> or %>%) operator from the dyplr package. This allows me to efficiently declare cleaning operators. It can be read as "and then". I've also used a flurry of dplyr subsetting functions that allow me to perform data cleaning operations. 

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

# I am clearing up the environment of the old dataframes. 
rm(m_basket, w_basket, m_soc, w_soc, m_hock, w_hock, w_rug, m_foot)


# Multicollinearity Checks ------------------------------------------------

# Here I've subsetted our large dataset into a smaller one that contains only the columns from sex onwards. Clearly we don't need the jump id and so forth. 
data2 <- data |> 
  select(sex:`Relative Propulsive Net Impulse`) 

# I'm rearranging the columns here so that they are listed alphabetically to better match the data dictionary
data2 <- data2 |> 
  select(order(colnames(data2))) |> 
  relocate(where(is.numeric), .after = where(is.character)) 

mod_1 <- lm(`Jump Height` ~ ., data = data2[, !(names(data2) %in% c("Left Force at Peak Propulsive Force", "Left Avg. Propulsive Force", "Left Force at Peak Landing Force", "Left Force at Peak Braking Force", "Left Avg. Landing Force", "Left Avg. Braking Force", "Left Avg. Braking RFD", "Right Force at Peak Propulsive Force", "Right Force at Peak Landing Force", "Right Force at Peak Braking Force", "Right Avg. Propulsive Force", "Right Avg. Propulsive Force", "Right Avg. Landing Force", "Right Avg. Braking RFD", "Right Avg. Braking Force", "sex", "sport"))]) # lm(y ~ . , data=data) the . allows you to fit all predictors in your dataframe without having to explicitly type them all out. 

summary(mod_1) # This is the summary of the model.

vif(mod_1)

