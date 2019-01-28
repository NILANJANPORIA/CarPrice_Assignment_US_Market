# Cleaning up the environment
remove (list=ls())

setwd("D:/Module3/Assignment- Linear Regression")

# importing libraries
library(ggplot2)
library(lubridate)
library(MASS)
library(car)
library(dplyr)
library(stringr)
library(scales)
library(zoo) 
library(gridExtra)

automobile_dataset <- read.csv('CarPrice_Assignment.csv',header = T, na.strings=c("NA", ""))
#--"#DIV/0!",,"NaN"



##############################################################################
###
### Data Understanding & preparation
###
##############################################################################
str(automobile_dataset)

###############
## Observation 1: No NA values in Dataset. No duplicate records present in Dataset.
############### 
sum(is.na(automobile_dataset))    #-- 0
nrow(unique(automobile_dataset))  #-- Count 205
nrow(automobile_dataset)          #-- Count 205

###############
## Observation 2: variable named CarName contains 'Company Name' and 'Car Model'.
##                In this analysis only 'company name' should be condidered.
############### 
automobile_dataset$CompanyName<-  sub("\\s.*","",automobile_dataset$CarName)
#automobile_dataset<-automobile_dataset[,-3]
automobile_dataset<-subset( automobile_dataset, select = -CarName)


###############
## Observation 3: Data Correction
##                CompanyName contains same name in different spelling. Those data need to fixed
############### 
unique(automobile_dataset$CompanyName)

## "vokswagen"   "volkswagen"  "vw" -> same company
##  "maxda"       "mazda"           -> same company
## "toyota"      "toyouta"          -> same company
## "Nissan"      "nissan"           -> same company
## "porsche"     "porcshce"         -> same company

automobile_dataset$CompanyName[automobile_dataset$CompanyName %in% c("vokswagen","vw")]<-"volkswagen"
automobile_dataset$CompanyName[automobile_dataset$CompanyName == "maxda"]<-"mazda"
automobile_dataset$CompanyName[automobile_dataset$CompanyName == "toyouta"]<-"toyota"
automobile_dataset$CompanyName[automobile_dataset$CompanyName == "Nissan"]<-"nissan"
automobile_dataset$CompanyName[automobile_dataset$CompanyName == "porsche"]<-"porcshce"
###############
## Observation 4: <Dummy Variable Creation>
##                Many Catagorical attributes are present.Those need to be converted as Numeric columns if needed.
##                factors with 2 levels: fueltype,aspiration,doornumber,enginelocation
##                factors with more than 2 levels:  carbody,drivewheel,enginetype,cylindernumber,fuelsystem,symboling,CompanyName
##                Out of these, only symboling & CompanyName (derived column) not treated as factor in currentdataset
############### 
automobile_dataset$symboling<-as.factor(automobile_dataset$symboling)

#DUMMY VARIABLE CREATION. 

## Converting factors with 2 levels to numerical variables

## Assuming Price will be high for Diesel Car leveling that as 1
summary(automobile_dataset$fueltype)
levels(automobile_dataset$fueltype)<-c(1,0)
automobile_dataset$fueltype<- as.numeric(levels(automobile_dataset$fueltype))[automobile_dataset$fueltype]
summary(automobile_dataset$fueltype)

## Assuming Price will be high for Turbo then Standard,leveling Turbo as 1
summary(automobile_dataset$aspiration)
levels(automobile_dataset$aspiration)<-c(0,1)
automobile_dataset$aspiration<- as.numeric(levels(automobile_dataset$aspiration))[automobile_dataset$aspiration]
summary(automobile_dataset$aspiration)

## Assuming Price will be high for Four doors leveling that as 1
summary(automobile_dataset$doornumber)
levels(automobile_dataset$doornumber)<-c(1,0)
automobile_dataset$doornumber<- as.numeric(levels(automobile_dataset$doornumber))[automobile_dataset$doornumber]
summary(automobile_dataset$doornumber)

## Assuming Price will be high for rear engine (mainly used for sports car) leveling that as 1
summary(automobile_dataset$enginelocation)
levels(automobile_dataset$enginelocation)<-c(0,1)
automobile_dataset$enginelocation<- as.numeric(levels(automobile_dataset$enginelocation))[automobile_dataset$enginelocation]
summary(automobile_dataset$enginelocation)


## Converting factors with multi levels to numerical variables
dummy_carbody <- data.frame(model.matrix( ~carbody, data = automobile_dataset))
dummy_carbody <- dummy_carbody[,-1]
automobile_dataset <- cbind(subset( automobile_dataset, select = -carbody), dummy_carbody)

dummy_drivewheel <- data.frame(model.matrix( ~drivewheel, data = automobile_dataset))
dummy_drivewheel <- dummy_drivewheel[,-1]
automobile_dataset <- cbind(subset( automobile_dataset, select = -drivewheel), dummy_drivewheel)

dummy_enginetype <- data.frame(model.matrix( ~enginetype, data = automobile_dataset))
dummy_enginetype <- dummy_enginetype[,-1]
automobile_dataset <- cbind(subset( automobile_dataset, select = -enginetype), dummy_enginetype)

dummy_cylindernumber <- data.frame(model.matrix( ~cylindernumber, data = automobile_dataset))
dummy_cylindernumber <- dummy_cylindernumber[,-1]
automobile_dataset <- cbind(subset( automobile_dataset, select = -cylindernumber), dummy_cylindernumber)

dummy_fuelsystem <- data.frame(model.matrix( ~fuelsystem, data = automobile_dataset))
dummy_fuelsystem <- dummy_fuelsystem[,-1]
automobile_dataset <- cbind(subset( automobile_dataset, select = -fuelsystem), dummy_fuelsystem)

dummy_symboling <- data.frame(model.matrix( ~symboling, data = automobile_dataset))
dummy_symboling <- dummy_symboling[,-1]
automobile_dataset <- cbind(subset( automobile_dataset, select = -symboling), dummy_symboling)

dummy_CompanyName <- data.frame(model.matrix( ~CompanyName, data = automobile_dataset))
dummy_CompanyName <- dummy_CompanyName[,-1]
automobile_dataset <- cbind(subset( automobile_dataset, select = -CompanyName), dummy_CompanyName)

###############
## Observation 5: <Removing Unnecessary Columns>
##                Few columns are not significant for Analysis like Car_ID
##                Those columns should be removed from Dataset
##                Company-Name shouldn't be removed, as Brand-name may matters for price
###############  
automobile_dataset<-subset( automobile_dataset, select = -car_ID)









######################################################################################################
################################### Model building and evaluation ####################################
######################################################################################################
# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(automobile_dataset), 0.7*nrow(automobile_dataset))
train = automobile_dataset[trainindices,]
test = automobile_dataset[-trainindices,]


## 'Model 1' for identifying which attributes are good indicator
model_1 <-lm(price~.,data=train)
summary(model_1)

step <- stepAIC(model_1, direction="both")
step
###############
## Step 1: Few columns are found significant by stepAIC. Create model from those columns only
##                
###############


model_2 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               curbweight + enginesize + stroke + peakrpm + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
               enginetyperotor + cylindernumberfive + cylindernumberthree + 
               fuelsystem2bbl + symboling.1 + symboling0 + symboling3 + 
               CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
               CompanyNamejaguar + CompanyNamemazda + CompanyNamemercury + 
               CompanyNamemitsubishi + CompanyNamenissan + CompanyNameplymouth + 
               CompanyNamerenault + CompanyNamesaab + CompanyNametoyota + 
               CompanyNamevolkswagen, data = train)
summary(model_2)

vif(model_2)

###############
## Observation: High R squared values got as expected. But huge no of column used in model.
## Result     : Multiple R-squared:  0.9804,	Adjusted R-squared:  0.9735  
## Step 2     : Remove all the variables one by one having high VIF and high P-Value.
##                
###############

##Step 2.1  : Remove enginetypeohc (having high VIF and high P-Value)
model_3 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               curbweight + enginesize + stroke + peakrpm + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginetypedohcv + enginetypel +  enginetypeohcf + 
               enginetyperotor + cylindernumberfive + cylindernumberthree + 
               fuelsystem2bbl + symboling.1 + symboling0 + symboling3 + 
               CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
               CompanyNamejaguar + CompanyNamemazda + CompanyNamemercury + 
               CompanyNamemitsubishi + CompanyNamenissan + CompanyNameplymouth + 
               CompanyNamerenault + CompanyNamesaab + CompanyNametoyota + 
               CompanyNamevolkswagen, data = train)

summary(model_3)

vif(model_3)
## Result     : Multiple R-squared:  0.9797,	Adjusted R-squared:  0.9728
## Observation: Adjusted R-squared value not affected much


##Step 2.2  : Remove CompanyNamehonda (having high VIF and high P-Value)
model_4 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               curbweight + enginesize + stroke + peakrpm + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginetypedohcv + enginetypel +  enginetypeohcf + 
               enginetyperotor + cylindernumberfive + cylindernumberthree + 
               fuelsystem2bbl + symboling.1 + symboling0 + symboling3 + 
               CompanyNamebmw + CompanyNamebuick + CompanyNamedodge +  
               CompanyNamejaguar + CompanyNamemazda + CompanyNamemercury + 
               CompanyNamemitsubishi + CompanyNamenissan + CompanyNameplymouth + 
               CompanyNamerenault + CompanyNamesaab + CompanyNametoyota + 
               CompanyNamevolkswagen, data = train)

summary(model_4)

vif(model_4)
## Result     : Multiple R-squared:  0.9787,	Adjusted R-squared:  0.9717 
## Observation: Adjusted R-squared value not affected much


##Step 2.3  : Remove fuelsystem2bbl (having high VIF and high P-Value)
model_5 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               curbweight + enginesize + stroke + peakrpm + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginetypedohcv +  enginetypeohcf + 
               enginetyperotor + cylindernumberfive + cylindernumberthree + 
               symboling.1 + symboling0 + symboling3 + 
               CompanyNamebmw + CompanyNamebuick + CompanyNamedodge +  
               CompanyNamejaguar + CompanyNamemazda + CompanyNamemercury + 
               CompanyNamemitsubishi + CompanyNamenissan + CompanyNameplymouth + 
               CompanyNamerenault + CompanyNamesaab + CompanyNametoyota + 
               CompanyNamevolkswagen, data = train)

summary(model_5)

vif(model_5)
## Result     : Multiple R-squared:  0.9767,	Adjusted R-squared:  0.9697 
## Observation: Adjusted R-squared value not affected much

##Step 2.4  : Remove symboling3 (having high VIF and high P-Value)
model_6 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               curbweight + enginesize + stroke + peakrpm + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginetypedohcv +  enginetypeohcf + 
               enginetyperotor + cylindernumberfive + cylindernumberthree + 
               symboling.1 + symboling0 +
               CompanyNamebmw + CompanyNamebuick + CompanyNamedodge +  
               CompanyNamejaguar + CompanyNamemazda + CompanyNamemercury + 
               CompanyNamemitsubishi + CompanyNamenissan + CompanyNameplymouth + 
               CompanyNamerenault + CompanyNamesaab + CompanyNametoyota + 
               CompanyNamevolkswagen, data = train)

summary(model_6)

vif(model_6)
## Result     : MuMultiple R-squared:  0.976,	Adjusted R-squared:  0.969
## Observation: Adjusted R-squared value not affected much


##Step 2.4  : Remove symboling0 (having high VIF and high P-Value)
model_6 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               curbweight + enginesize + stroke + peakrpm + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginetypedohcv +  enginetypeohcf + 
               enginetyperotor + cylindernumberfive + cylindernumberthree + 
               symboling.1 +
               CompanyNamebmw + CompanyNamebuick + CompanyNamedodge +  
               CompanyNamejaguar + CompanyNamemazda + CompanyNamemercury + 
               CompanyNamemitsubishi + CompanyNamenissan + CompanyNameplymouth + 
               CompanyNamerenault + CompanyNamesaab + CompanyNametoyota + 
               CompanyNamevolkswagen, data = train)

summary(model_6)

vif(model_6)
## Result     : Multiple R-squared:  0.976,	Adjusted R-squared:  0.969
## Observation: Adjusted R-squared value not affected much

##Step 2.5  : Remove CompanyNametoyota (having high VIF and high P-Value)
model_7 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               enginesize + stroke + peakrpm + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginetypedohcv +  enginetypeohcf + 
               enginetyperotor + cylindernumberfive + cylindernumberthree + 
               symboling.1 +
               CompanyNamebmw + CompanyNamebuick + CompanyNamedodge +  
               CompanyNamejaguar + CompanyNamemazda + CompanyNamemercury + 
               CompanyNamemitsubishi + CompanyNamenissan + CompanyNameplymouth + 
               CompanyNamerenault + CompanyNamesaab +  
               CompanyNamevolkswagen, data = train)

summary(model_7)

vif(model_7)
## Result     : Multiple R-squared:  0.973,	Adjusted R-squared:  0.9661 
## Observation: Adjusted R-squared value not affected much


###############
## Observation: 'Adjusted R-squared' values not affected much even though all the 
##              attributes having high VIF and P Value removed.
##              But huge no of columns used in model.
## Result     : Multiple R-squared:  0.973,	Adjusted R-squared:  0.9661
## Step 3     : Remove variables (one by one) having high P-Value.
##                
###############

## Step 3.1   : Remove CompanyNamesaab (having high P-Value)
model_8 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               enginesize + stroke + peakrpm + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginetypedohcv +  enginetypeohcf + 
               enginetyperotor + cylindernumberfive + cylindernumberthree + 
               symboling.1 +
               CompanyNamebmw + CompanyNamebuick + CompanyNamedodge +  
               CompanyNamejaguar + CompanyNamemazda + CompanyNamemercury + 
               CompanyNamemitsubishi + CompanyNamenissan + CompanyNameplymouth + 
               CompanyNamerenault + 
               CompanyNamevolkswagen, data = train)
summary(model_8)

vif(model_8)
## Result     : Multiple R-squared:  0.973,	Adjusted R-squared:  0.9663 
## Observation: Adjusted R-squared value not affected much


## Step 3.2   : Remove cylindernumberfive (having high P-Value)
model_9 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               enginesize + stroke + peakrpm + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginetypedohcv +  enginetypeohcf + 
               enginetyperotor + cylindernumberthree + 
               symboling.1 +
               CompanyNamebmw + CompanyNamebuick + CompanyNamedodge +  
               CompanyNamejaguar + CompanyNamemazda + CompanyNamemercury + 
               CompanyNamemitsubishi + CompanyNamenissan + CompanyNameplymouth + 
               CompanyNamerenault + 
               CompanyNamevolkswagen, data = train)
summary(model_9)

vif(model_9)
## Result     : Multiple R-squared:  0.9729,	Adjusted R-squared:  0.9666  
## Observation: Adjusted R-squared value not affected much


## Step 3.3   : Remove symboling.1 (having high P-Value)
model_10 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               enginesize + stroke + peakrpm + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginetypedohcv +  enginetypeohcf + 
               enginetyperotor + cylindernumberthree + 
               CompanyNamebmw + CompanyNamebuick + CompanyNamedodge +  
               CompanyNamejaguar + CompanyNamemazda + CompanyNamemercury + 
               CompanyNamemitsubishi + CompanyNamenissan + CompanyNameplymouth + 
               CompanyNamerenault + 
               CompanyNamevolkswagen, data = train)
summary(model_10)

vif(model_10)
## Result     : Multiple R-squared:  0.9729,	Adjusted R-squared:  0.9668  
## Observation: Adjusted R-squared value not affected much

## Step 3.4   : Remove CompanyNamemercury (having high P-Value)
model_11 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
                enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypedohcv +  enginetypeohcf + 
                enginetyperotor + cylindernumberthree + 
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge +  
                CompanyNamejaguar + CompanyNamemazda + 
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNameplymouth + 
                CompanyNamerenault + 
                CompanyNamevolkswagen, data = train)
summary(model_11)

vif(model_11)
## Result     : R-squared:  0.9728,	Adjusted R-squared:  0.967   
## Observation: Adjusted R-squared value not affected much


## Step 3.5   : Remove CompanyNamevolkswagen (having high P-Value)
model_12 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
                enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypedohcv +  enginetypeohcf + 
                enginetyperotor + cylindernumberthree + 
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge +  
                CompanyNamejaguar + CompanyNamemazda + 
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNameplymouth + 
                CompanyNamerenault, data = train)
summary(model_12)

vif(model_12)
## Result     : Multiple R-squared:  0.9721,	Adjusted R-squared:  0.9665   
## Observation: Adjusted R-squared value not affected much


## Step 3.6   : Remove CompanyNamerenault (having high P-Value)
model_13 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
                enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypedohcv +  enginetypeohcf + 
                enginetyperotor + cylindernumberthree + 
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge +  
                CompanyNamejaguar + CompanyNamemazda + 
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNameplymouth , data = train)
summary(model_13)

vif(model_13)
## Result     : Multiple R-squared:  0.9721,	Adjusted R-squared:  0.9665   
## Observation: Adjusted R-squared value not affected much


## Step 3.7   : Remove CompanyNamerenault (having high P-Value)
model_14 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
                enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypedohcv +  enginetypeohcf + 
                enginetyperotor + cylindernumberthree + 
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge +  
                CompanyNamejaguar + CompanyNamemazda + 
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNameplymouth , data = train)
summary(model_14)

vif(model_14)
## Result     : Multiple R-squared:  0.9716,	Adjusted R-squared:  0.9661   
## Observation: Adjusted R-squared value not affected much

## Step 3.8   : Remove CompanyNamenissan (having high P-Value)
model_15 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
                enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypedohcv +  enginetypeohcf + 
                enginetyperotor + cylindernumberthree + 
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge +  
                CompanyNamejaguar + CompanyNamemazda + 
                CompanyNamemitsubishi + CompanyNameplymouth , data = train)
summary(model_15)

vif(model_15)
## Result     : Multiple R-squared:  0.971,	Adjusted R-squared:  0.9657 
## Observation: Adjusted R-squared value not affected much


## Step 3.9   : Remove CompanyNamenissan (having high P-Value)
model_16 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
                enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypedohcv +  enginetypeohcf + 
                enginetyperotor + cylindernumberthree + 
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge +  
                CompanyNamejaguar + CompanyNamemazda + 
                CompanyNamemitsubishi + CompanyNameplymouth , data = train)
summary(model_16)

vif(model_16 )
## Result     : Multiple R-squared:  0.971,	Adjusted R-squared:  0.9657 
## Observation: Adjusted R-squared value not affected much


## Step 3.10   : Remove CompanyNamemazda (having high P-Value)
model_17 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
                enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypedohcv +  enginetypeohcf + 
                enginetyperotor + cylindernumberthree + 
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge +  
                CompanyNamejaguar +  
                CompanyNamemitsubishi + CompanyNameplymouth , data = train)
summary(model_17)

vif(model_17 )
## Result     : Multiple R-squared:  0.9702,	Adjusted R-squared:  0.9651 
## Observation: Adjusted R-squared value not affected much

## Step 3.11   : Remove CompanyNameplymouth (having high P-Value)
model_18 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
                enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypedohcv +  enginetypeohcf + 
                enginetyperotor + cylindernumberthree + 
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge +  
                CompanyNamejaguar +  
                CompanyNamemitsubishi , data = train)
summary(model_18)

vif(model_18 )
## Result     : Multiple R-squared:  0.9693,	Adjusted R-squared:  0.9643 
## Observation: Adjusted R-squared value not affected much


## Step 3.12   : Remove CompanyNameplymouth (having high P-Value)
model_19 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
                enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypedohcv +  enginetypeohcf + 
                enginetyperotor + cylindernumberthree + 
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge +  
                CompanyNamejaguar +  
                CompanyNamemitsubishi , data = train)
summary(model_19)

vif(model_19 )
## Result     : Multiple R-squared:  0.9693,	Adjusted R-squared:  0.9643 
## Observation: Adjusted R-squared value not affected much


## Step 3.13   : Remove CompanyNamedodge (having high P-Value)
model_20 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
                enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypedohcv +  enginetypeohcf + 
                enginetyperotor + cylindernumberthree + 
                CompanyNamebmw + CompanyNamebuick +
                CompanyNamejaguar +  
                CompanyNamemitsubishi , data = train)
summary(model_20)

vif(model_20 )
## Result     : Multiple R-squared:  0.9683,	Adjusted R-squared:  0.9634  
## Observation: Adjusted R-squared value not affected much


## Step 3.14   : Remove enginetypedohcv (having high P-Value)
model_21 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
                enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypeohcf + 
                enginetyperotor + cylindernumberthree + 
                CompanyNamebmw + CompanyNamebuick +
                CompanyNamejaguar +  
                CompanyNamemitsubishi , data = train)
summary(model_21)

vif(model_21 )
## Result     : Multiple R-squared:  0.9666,	Adjusted R-squared:  0.9617  
## Observation: Adjusted R-squared value not affected much



###############
## Observation: 'Adjusted R-squared' values not affected much even though many attributes removed
##               But large no of columns used in model.
## Result     : Multiple R-squared:  0.9666,	Adjusted R-squared:  0.9617 
## Step 4     : Compare if multiple variable have high VIF and low p-Value.They could be correlated.
##              In that case, Less significant column need to be removed.  
###############

## Observation: carwidth,enginesize have higher vif. So those attributes may be correlated.
cor(train$carwidth,train$enginesize)  ##  0.7637016

## Step 4.1   : Remove enginesize and (carwidth retained)
model_22 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
                stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypeohcf + 
                enginetyperotor + cylindernumberthree + 
                CompanyNamebmw + CompanyNamebuick +
                CompanyNamejaguar +  
                CompanyNamemitsubishi , data = train)
summary(model_22)

vif(model_22 )
## Result     : Multiple R-squared:  0.9206,	Adjusted R-squared:  0.9098  
## Observation: Adjusted R-squared value affected, many more columns become insignificant


## Step 4.2   : Add enginesize and remove carwidth
model_23 <-lm(formula = price ~ aspiration + enginelocation + 
                enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypeohcf + 
                enginetyperotor + cylindernumberthree + 
                CompanyNamebmw + CompanyNamebuick +
                CompanyNamejaguar +  
                CompanyNamemitsubishi , data = train)
summary(model_23)

vif(model_23 )
## Result     : Multiple R-squared:  0.9493,	Adjusted R-squared:  0.9424  
## Observation: Adjusted R-squared value affected (but less than Model 22), many more columns become insignificant



###############################################################################################
## Observation: Model23 is better than Model 22
##              1. Adjusted R-squared value less affected
##              2. Difference between 'Multiple R-squared' and 'Adjusted R-squared' less
##              3. All though P-Value is very less for both enginesize and carwidth. But enginesize is more less
##              4. In Model 23, more columns having higher vif become insignificant
###############################################################################################




## Step 4.3   : remove carbodysedan
model_24 <-lm(formula = price ~ aspiration + enginelocation + 
                enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodywagon + drivewheelrwd + 
                enginetypeohcf + 
                enginetyperotor + cylindernumberthree + 
                CompanyNamebmw + CompanyNamebuick +
                CompanyNamejaguar +  
                CompanyNamemitsubishi , data = train)
summary(model_24)

vif(model_24 )
## Result     : Multiple R-squared:  0.9489,	Adjusted R-squared:  0.9424   
## Observation: Adjusted R-squared value not affected much


## Step 4.4   : remove carbodywagon
model_25 <-lm(formula = price ~ aspiration + enginelocation + 
                enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + drivewheelrwd + 
                enginetypeohcf + 
                enginetyperotor + cylindernumberthree + 
                CompanyNamebmw + CompanyNamebuick +
                CompanyNamejaguar +  
                CompanyNamemitsubishi , data = train)
summary(model_25)

vif(model_25 )
## Result     : Multiple R-squared:  0.9489,	Adjusted R-squared:  0.9428   
## Observation: Adjusted R-squared value not affected much


## Step 4.5   : remove cylindernumberthree
model_26 <-lm(formula = price ~ aspiration + enginelocation + 
                enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + drivewheelrwd + 
                enginetypeohcf + 
                enginetyperotor + 
                CompanyNamebmw + CompanyNamebuick +
                CompanyNamejaguar +  
                CompanyNamemitsubishi , data = train)
summary(model_26)

vif(model_26 )
## Result     : Multiple R-squared:  0.948,	Adjusted R-squared:  0.9423 
## Observation: Adjusted R-squared value not affected much


## Step 4.6   : remove drivewheelrwd
model_27 <-lm(formula = price ~ aspiration + enginelocation + 
                enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback +  
                enginetypeohcf + 
                enginetyperotor + 
                CompanyNamebmw + CompanyNamebuick +
                CompanyNamejaguar +  
                CompanyNamemitsubishi , data = train)
summary(model_27)

vif(model_27)
## Result     : Multiple R-squared:  0.946,	Adjusted R-squared:  0.9405 
## Observation: Adjusted R-squared value not affected much


## Step 4.7   : remove carbodyhardtop
model_28 <-lm(formula = price ~ aspiration + enginelocation + 
                enginesize + stroke + peakrpm +  
                carbodyhatchback +  
                enginetypeohcf + 
                enginetyperotor + 
                CompanyNamebmw + CompanyNamebuick +
                CompanyNamejaguar +  
                CompanyNamemitsubishi , data = train)
summary(model_28)

vif(model_28)
## Result     : Multiple R-squared:  0.9437,	Adjusted R-squared:  0.9385  
## Observation: Adjusted R-squared value not affected much




Corr_automobile_dataset<-subset( train, select = c(aspiration,enginelocation,enginesize,stroke,peakrpm,carbodyhatchback,enginetypeohcf,enginetyperotor,CompanyNamebmw,CompanyNamebuick,CompanyNamejaguar,CompanyNamemitsubishi))
corr_Table<-cor(Corr_automobile_dataset)
write.csv(corr_Table,file="corr_Table.csv")


###############################################################################################
## Observation: enginesize has greater vif. enginesize has moderately correlated with CompanyNamebuick. 
##              However for a rival company, enginesize has more relevant.

## Step 4.8   : remove CompanyNamebuick
model_29 <-lm(formula = price ~ aspiration + enginelocation + 
                enginesize + stroke + peakrpm +  
                carbodyhatchback +  
                enginetypeohcf + 
                enginetyperotor + 
                CompanyNamebmw + 
                CompanyNamejaguar +  
                CompanyNamemitsubishi , data = train)
summary(model_29)

vif(model_29)
## Result     : Multiple R-squared:  0.9249,	Adjusted R-squared:  0.9185  
## Observation: Adjusted R-squared value not affected much. vif of enginesize reduced and some more companyparameters are less significant


## Step 4.9   : remove CompanyNamejaguar
model_30 <-lm(formula = price ~ aspiration + enginelocation + 
                enginesize + stroke + peakrpm +  
                carbodyhatchback +  
                enginetypeohcf + 
                enginetyperotor + 
                CompanyNamebmw + 
                CompanyNamemitsubishi , data = train)
summary(model_30)

vif(model_30)
## Result     : Multiple R-squared:  0.9228,	Adjusted R-squared:  0.917 
## Observation: Adjusted R-squared value not affected much. 

## Step 4.10   : remove CompanyNamemitsubishi
model_31 <-lm(formula = price ~ aspiration + enginelocation + 
                enginesize + stroke + peakrpm +  
                carbodyhatchback +  
                enginetypeohcf + 
                enginetyperotor + 
                CompanyNamebmw , data = train)
summary(model_31)

vif(model_31)
## Result     : Multiple R-squared:  0.9194,	Adjusted R-squared:  0.914  
## Observation: Adjusted R-squared value not affected much. 



###############################################################################################
## Observation: enginetypeohcf,enginelocation AND stroke have comparatively greater vif (although <2). 
##              From Corralation Matrix, found that
##              enginetypeohcf has moderately correlated with other two. 
##              Allthough enginelocation AND stroke are not much correlated.
###############################################################################################          

## Step 4.11   : remove enginetypeohcf
model_32 <-lm(formula = price ~ aspiration + enginelocation + 
                enginesize + stroke + peakrpm +  
                carbodyhatchback +  
                enginetyperotor + 
                CompanyNamebmw , data = train)
summary(model_32)

vif(model_32)
## Result     : Multiple R-squared:  0.9122,	Adjusted R-squared:  0.907 
## Observation: Adjusted R-squared value not affected much. 


## Step 4.12   : remove CompanyNamebmw for checking
model_33 <-lm(formula = price ~ aspiration + enginelocation + 
                enginesize + stroke + peakrpm +  
                carbodyhatchback perotor, data = train)+  
                enginety
summary(model_33)

vif(model_33)
## Result     : Multiple R-squared:  0.9021,	Adjusted R-squared:  0.897 
## Observation: Adjusted R-squared value affected much. 

##Observation:
##            CompanyNamebmw have less correlation with other variable. Removing the value also hampers the model.
##            So BMW tag have some significance to USA customers hence 'model 33' discarded


## So Model-32 is final for this approach



###############################################################################################
## Test Record: . 
##              Test the Model with Test-Dataset 
##Observation:  R Square value reduced. 
##              Train Data value was 0.9122,  Test data value reduced to 0.7913848
##              Model can be overfitted.
###############################################################################################          

# predicting the results in test dataset
Predict_1 <- predict(model_32,test[,-1])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared


###############################################################################################
## Observation: There was a conflict on Model 22 and Model 23
##              Because enginesize and carwidth both have high vif but less P-Value. 
##              That time Model was continued removing carwidth.
##              Create Second approach by removing enginesize instead of carwidth.
###############################################################################################
## Step 5.0   : Remove enginesize and (carwidth retained) (Started from Model 21)
model_34 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
                stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypeohcf + 
                enginetyperotor + cylindernumberthree + 
                CompanyNamebmw + CompanyNamebuick +
                CompanyNamejaguar +  
                CompanyNamemitsubishi , data = train)
summary(model_34)

vif(model_34 )
## Result     : Multiple R-squared:  0.9206,	Adjusted R-squared:  0.9098  
## Observation: Adjusted R-squared value affected, many more columns become insignificant


## Step 5.1   : Remove enginetyperotor
model_35 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
                stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypeohcf + 
                cylindernumberthree + 
                CompanyNamebmw + CompanyNamebuick +
                CompanyNamejaguar +  
                CompanyNamemitsubishi , data = train)
summary(model_35)

vif(model_35 )
## Result     : Multiple R-squared:  0.9202,	Adjusted R-squared:  0.9101 
## Observation: Adjusted R-squared value not much  affected


## Step 5.2   : Remove carbodysedan
model_36 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
                stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback +  carbodywagon + drivewheelrwd + 
                enginetypeohcf + 
                cylindernumberthree + 
                CompanyNamebmw + CompanyNamebuick +
                CompanyNamejaguar +  
                CompanyNamemitsubishi , data = train)
summary(model_36)

vif(model_36)
## Result     : Multiple R-squared:  0.9135,	Adjusted R-squared:  0.9032  
## Observation: Adjusted R-squared value not much  affected

## Step 5.3   : Remove carbodyhardtop
model_37 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
                stroke + peakrpm + 
                carbodyhatchback +  carbodywagon + drivewheelrwd + 
                enginetypeohcf + 
                cylindernumberthree + 
                CompanyNamebmw + CompanyNamebuick +
                CompanyNamejaguar +  
                CompanyNamemitsubishi , data = train)
summary(model_37)

vif(model_37)
## Result     : Multiple R-squared:  0.9135,	Adjusted R-squared:  0.904 
## Observation: Adjusted R-squared value not much  affected

## Step 5.4   : Remove carbodyhatchback
model_38 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
                stroke + peakrpm + 
                carbodywagon + drivewheelrwd + 
                enginetypeohcf + 
                cylindernumberthree + 
                CompanyNamebmw + CompanyNamebuick +
                CompanyNamejaguar +  
                CompanyNamemitsubishi , data = train)
summary(model_38)

vif(model_38)
## Result     : Multiple R-squared:  0.9132,	Adjusted R-squared:  0.9045 
## Observation: Adjusted R-squared value not much  affected

## Step 5.5   : Remove carbodywagon
model_39 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
                stroke + peakrpm + 
                drivewheelrwd + 
                enginetypeohcf + 
                cylindernumberthree + 
                CompanyNamebmw + CompanyNamebuick +
                CompanyNamejaguar +  
                CompanyNamemitsubishi , data = train)
summary(model_39)

vif(model_39)
## Result     : Multiple R-squared:  0.9121,	Adjusted R-squared:  0.904 
## Observation: Adjusted R-squared value not much  affected

## Step 5.6   : Remove CompanyNamemitsubishi
model_40 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
                stroke + peakrpm + 
                drivewheelrwd + 
                enginetypeohcf + 
                cylindernumberthree + 
                CompanyNamebmw + CompanyNamebuick +
                CompanyNamejaguar, data = train)
summary(model_40)

vif(model_40)
## Result     : Multiple R-squared:  0.9111,	Adjusted R-squared:  0.9037 
## Observation: Adjusted R-squared value not much  affected

## Step 5.7   : Remove enginetypeohcf
model_41 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
                stroke + peakrpm + 
                drivewheelrwd + 
                cylindernumberthree + 
                CompanyNamebmw + CompanyNamebuick +
                CompanyNamejaguar, data = train)
summary(model_41)

vif(model_41)
## Result     : Multiple R-squared:  0.9094,	Adjusted R-squared:  0.9025 
## Observation: Adjusted R-squared value not much  affected

## Step 5.8   : Remove aspiration
model_42 <-lm(formula = price ~ enginelocation + carwidth + 
                stroke + peakrpm + 
                drivewheelrwd + 
                cylindernumberthree + 
                CompanyNamebmw + CompanyNamebuick +
                CompanyNamejaguar, data = train)
summary(model_42)

vif(model_42)
## Result     : Multiple R-squared:  0.9081,	Adjusted R-squared:  0.9019 
## Observation: Adjusted R-squared value not much  affected


## Step 5.9   : Remove stroke
model_43 <-lm(formula = price ~ enginelocation + carwidth + 
                peakrpm + 
                drivewheelrwd + 
                cylindernumberthree + 
                CompanyNamebmw + CompanyNamebuick +
                CompanyNamejaguar, data = train)
summary(model_43)

vif(model_43)
## Result     : Multiple R-squared:  0.9057,	Adjusted R-squared:  0.9001 
## Observation: Adjusted R-squared value not much  affected

## Step 5.10   : Remove cylindernumberthree
model_44 <-lm(formula = price ~ enginelocation + carwidth + 
                peakrpm + 
                drivewheelrwd + 
                CompanyNamebmw + CompanyNamebuick +
                CompanyNamejaguar, data = train)
summary(model_44)

vif(model_44)
## Result     : Multiple R-squared:  0.9023,	Adjusted R-squared:  0.8972 
## Observation: Adjusted R-squared value not much  affected

## Step 5.11   : Remove peakrpm
model_45 <-lm(formula = price ~ enginelocation + carwidth + 
                drivewheelrwd + 
                CompanyNamebmw + CompanyNamebuick +
                CompanyNamejaguar, data = train)
summary(model_45)

vif(model_45)
## Result     : Multiple R-squared:  0.8974,	Adjusted R-squared:  0.8928 
## Observation: Adjusted R-squared value   affected but value is good

## Step 5.12   : Remove drivewheelrwd
model_46 <-lm(formula = price ~ enginelocation + carwidth + 
                CompanyNamebmw + CompanyNamebuick +
                CompanyNamejaguar, data = train)
summary(model_46)

vif(model_46)
## Result     : Multiple R-squared:  0.8894,	Adjusted R-squared:  0.8853 
## Observation: Adjusted R-squared value   affected so discard the model_46

## So Model-45 is finalized for this approach

###############################################################################################
## Test Record: . 
##              Test the Model with Test-Dataset 
##Observation:  R Square value is closer with expectation. 
##              Train Data value was 0.8974,  Test data value reduced to 0.8498306
##              Model-45 gives better result than previous Model-32.
###############################################################################################  

# predicting the results in test dataset
Predict_1 <- predict(model_45,test[,-1])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared




##########################################################
##Analysis Result
##
## Two Models are finalized (Model-32 and Model-45). Model-45 looks more suitable for dataset.
##
## But Model-45 contains more CompanyName Parameters, what suggest Brand Name matters a lot in price to USA 
## May be Customers looks for best brand. However that is not scope in this analysis
## As a competitor 'Geely Auto' shouldn't be directly related with rival brand
## They should check concentrate below parameters as those affect Car-price
## aspiration      
## enginelocation
## enginesize
## stroke
## peakrpm
## carbodyhatchback
## enginetyperotor
## carwidth 
## drivewheelrwd 