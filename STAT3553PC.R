library(tidyverse)
library(readxl)
library(lars)
library(faraway)
library(MASS)

# Loading in the data ------------------------------------------------

m_basket <- read_excel(path="~/Data Analytics/STAT 3553/Project/CMJ_IMTP Data/Men's Basketball_CMJ.xlsx")
w_basket<- read_excel(path="~/Data Analytics/STAT 3553/Project/CMJ_IMTP Data/Women's Basketball_CMJ.xlsx")
m_soc<- read_excel(path="~/Data Analytics/STAT 3553/Project/CMJ_IMTP Data/Men's Soccer_CMJ.xlsx")
w_soc<- read_excel(path="~/Data Analytics/STAT 3553/Project/CMJ_IMTP Data/Women's Soccer_CMJ.xlsx")
m_hock<- read_excel(path="~/Data Analytics/STAT 3553/Project/CMJ_IMTP Data/Men's Hockey_CMJ.xlsx")
w_hock<- read_excel(path="~/Data Analytics/STAT 3553/Project/CMJ_IMTP Data/Women's Hockey_CMJ.xlsx")
m_foot<- read_excel(path="~/Data Analytics/STAT 3553/Project/CMJ_IMTP Data/Football_CMJ.xlsx")
w_rug<- read_excel(path="~/Data Analytics/STAT 3553/Project/CMJ_IMTP Data/Women's Rugby_CMJ.xlsx")

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

mod_3 <- lm(`Jump Height` ~ .-sex -sport, data = data4)
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

#shapiro.test(mod_residuals) # This won't work because Shapiro Test only takes sample size between 3 and 5000. 

par(mfrow=c(2,2))
plot(mod_3)
#We can observe that the model is linear, but normality is not satisfied.
#The Scale-Location graph we can see is not linear, so there is likely some homoscedasticity.
#The Residuals vs Leverage plot has some extreme points so some influential points present and possible outliers.
par(mfrow=c(1,1))

# Remove Outliers  ------------------------------------------

removeOutl<-function(dat){ #Function to remove outliers
  mod <- lm(`Jump Height` ~ .-sex -sport, data = dat)
  studresiduals<-rstudent(mod)
  
  dat <- dat[abs(studresiduals)<3, ]
  print(range(rstudent(mod)))
  
  return(dat)
  
}
data5<-data4 #New dataset without outliers
mod_4<-lm(`Jump Height` ~ .-sex -sport, data = data5)

while(TRUE){
  
  data5<-removeOutl(data5)
  mod_4<-lm(`Jump Height` ~ .-sex -sport, data = data5)
  if(abs(range(rstudent(mod_4))[1])>3 || abs(range(rstudent(mod_4))[2])>3) {
    print("Continue loop to eliminate more outliers")
    
  } else {break}
  
}
print("Removed all outliers")
print(range(rstudent(mod_4)))

par(mfrow=c(2,2))
plot(mod_4) #Observe new model without outliers

# Check for autocorrelation -----------------------------------------------



# Check for influential points --------------------------------------------

data5<-data5[c(-911,-771,-243,-758,-8415,-7301,-755,-839,-8084,-897,-99,-4475,-4615,-1932,-6888),] #Identified 15 points that reduced max cook's distance from

mod_4<-lm(`Jump Height` ~ .-sex -sport, data = data5) #Refit model

par(mfrow=c(1,1))
cookDat<-cooks.distance(mod_4)#find Cook’s distance
#half-normal plot of Cook’s distance
halfnorm(cookDat,3,ylab="Cook’s distances",xlim=c(0,4)) #Given the removed points, this should be reasonable, without comprimising results.

par(mfrow=c(2,2))
plot(mod_4)
#Eliminating influential points did not result in any significant change, likely due to the size of the dataset.

# Box-Cox Transformation  -----------------------------------------------------------------

#par(mfrow=c(1,1))
#mod_5<-mod_4 #New model mod_5 for box-cox transformation
#boxcoxTransform<-boxcox(mod_5, plotit = TRUE,lambda=seq(0,1,by=0.1))
#Is closest to 0.9 so will try lambda=0.9

#data5$`Jump HeightBX` <- (data5$`Jump Height`^0.9 - 1) / 0.9
#mod_5<-lm(`Jump HeightBX` ~ .-sex -sport, data = data5) #Fitting model with Box-cox transformation.
#par(mfrow=c(2,2))
#plot(mod_5)#We can conclude this section as a Box-Cox Transformation is overfitting the data.
#R^2 increaded to 0.9995, but now all other metrics are worse.
#mod_5 is deemed irrelevant

# Square root Transformation  -----------------------------------------------------------------

#mod_6<-lm(sqrt(`Jump Height`) ~ .-sex -sport, data = data5) #Fitting model with transformation.
#plot(mod_6) #We can also conclude that this transformation is overfitting in a similar way to box-cox.

# LASSO  -----------------------------------------------------------------

design_matrix <- model.matrix(~ . -1, data = data5)
#modeDat5<-mode(design_matrix)

indexJH<-which(colnames(design_matrix) == "`Jump Height`")


lmod <- lars(design_matrix[,-indexJH], data5$`Jump Height`)
lmod
round(lmod$Cp,2)

# PCA Test ----------------------------------------------------------------

#Will start by using data3
data3Scaled<-scale(data3[,c(-1:-2,-23)])
pcaTestData3<-prcomp(data3Scaled,center=TRUE,scale.=TRUE)
print(summary(pcaTestData3))

#Try with data4 after vif eliminations
data4Scaled<-scale(data4[c(-1:-2,-6)])
pcaTestData4<-prcomp(data4Scaled,center=TRUE,scale.=TRUE)
print(summary(pcaTestData4))

#Try with data5 after eliminating outliers
data5Scaled<-scale(data5[c(-1:-2,-6)])
pcaTestData5<-prcomp(data5Scaled,center=TRUE,scale.=TRUE)
print(summary(pcaTestData5))

#Plotting the standard deviation of principal components
par(mfrow=c(2,2))
plot(pcaTestData3$sdev,type="l",ylab="SD of PC", xlab="PC number",main="57 Variables")
plot(pcaTestData4$sdev,type="l",ylab="SD of PC", xlab="PC number",main="7 Variables")
plot(pcaTestData5$sdev,type="l",ylab="SD of PC", xlab="PC number",main="7 Variables, no Outliers")

par(mfrow=c(1,1))
robData3 <- cov.rob(data3Scaled)#find the center and Sigma
md <- mahalanobis(data3Scaled, center=robData3$center, cov=robData3$cov)
n <- nrow(data3Scaled);p <- ncol(data3Scaled)
plot(qchisq(1:n/(n+1),p), sort(md), xlab=expression(paste(chi^2,"
quantiles")), ylab="Sorted Mahalanobis distances")
abline(0,1) #This doesn't work as the x's are collinear

robData4 <- cov.rob(data4Scaled)#find the center and Sigma
md <- mahalanobis(data4Scaled, center=robData4$center, cov=robData4$cov)
n <- nrow(data4Scaled);p <- ncol(data4Scaled)
plot(qchisq(1:n/(n+1),p), sort(md), xlab=expression(paste(chi^2,"
quantiles")), ylab="Sorted Mahalanobis distances")
abline(0,1) #This works

robData5 <- cov.rob(data5Scaled)#find the center and Sigma
md <- mahalanobis(data5Scaled, center=robData5$center, cov=robData5$cov)
n <- nrow(data5Scaled);p <- ncol(data5Scaled)
plot(qchisq(1:n/(n+1),p), sort(md), xlab=expression(paste(chi^2,"
quantiles")), ylab="Sorted Mahalanobis distances")
abline(0,1)

#Rotation Matrices
data3Rotation<-pcaTestData3$rotation
data4Rotation<-pcaTestData4$rotation
data5Rotation<-pcaTestData5$rotation

print(round(data3Rotation[,5],2))
print(round(data4Rotation[,5],2))
print(round(data5Rotation[,5],2))


# Comparing 5th pca values for response vars in dataset 4 with themselves in dataset 3 ------------------------

# Ensure row names are consistent between the two matrices
rows_to_extract <- rownames(data4Rotation)

# Extract corresponding rows from data3Rotation
filtered_data3Rotation <- data3Rotation[rows_to_extract, ]

# Combine the two for comparison
comparison <- list(
  data3 = filtered_data3Rotation,
  data4 = data4Rotation
)

# Create a data frame for side-by-side comparison
comparison_df <- data.frame(
  Variable = rows_to_extract,
  data3_PC5 = round(filtered_data3Rotation[, "PC5"], 4),
  data4_PC5 = round(data4Rotation[, "PC5"], 4)
)

print(comparison_df)

#Lets also look at the top 10 values of the pca test of dataset 3
temp<-order(data3Rotation[,"PC5"],decreasing = TRUE)
bestVals<-rownames(temp[1:10])
print(bestVals)