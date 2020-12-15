# STATS506 Final Project Script 
# The R script 
# Author: Dongyang Zhao

#79:---------------------------------------------------------------------------
library(readr)
library(tidyverse)
library(pander)
library(survey)
Data <- read_csv("Data/2012_public_use_data_aug2016.csv")

# The function we used to convert the code into the actual name
region_map = function(x){
  if(x == "1"){
    return("Northeast")
  }
  else if(x=="2"){
    return("Midwest")
  }
  else if(x == "3"){
    return("South")
  }
  else{
    return("West")
  }
}

Energy_map = function(x){
  if(x == "ELHT1"){
    return("Electricity")
  }
  else if (x == "NGHT1"){
    return("Natural gas")
  }
  else if (x == "FKHT1"){
    return("Fuel oil")
  }
  else if (x == "PRHT1"){
    return("Propane")
  }
  else if (x == "STHT1"){
    return("District steam")
  }
  else if(x == "HWHT1"){
    return("District hot water")
  }
  else if(x == "WOHT1"){
    return("Wood")
  }
  else if(x == "COHT1"){
    return("Coal")
  }
  else if(x == "SOHT1"){
    return("Solar")
  }
  else{
    return("Others")
  }
}

# select the weights and heat source variables
# create an indicator to determine if the building use this source
# 1 for yes 0 for no or missing 

Data_clean = Data %>% select(
  REGION,
  ELHT1,
  NGHT1,
  FKHT1,
  PRHT1,
  STHT1,
  HWHT1,
  WOHT1,
  COHT1,
  SOHT1,
  OTHT1,
  starts_with("FINALWT")
)

Data_clean$REGION  = 
  Data_clean$REGION %>% map(region_map) %>% unlist()
colname_energy = c(  "ELHT1",
                     "NGHT1",
                     "FKHT1",
                     "PRHT1",
                     "STHT1",
                     "HWHT1",
                     "WOHT1",
                     "COHT1",
                     "SOHT1",
                     "OTHT1")


for(i in colname_energy){
  eval(parse(text = paste("Data_clean$",
                          i,
                          "=ifelse(Data_clean$",
                          i,
                          "==2,0,1)",sep = "")))
  eval(parse(text = paste("Data_clean$",
                          i,
                          "=ifelse(is.na(Data_clean$",
                          i,
                          "),0,Data_clean$",
                          i,
                          ")",sep = "")))
  
}

# Create a new variable `MainHeatSource` for each record

Data_pivot = Data_clean %>% 
  pivot_longer(
    !c(contains("FINALWT"),REGION),
    names_to = "MainHeatSource",
    values_to = "indicator"
  ) 

# store the weights
sampweights = Data_pivot$FINALWT
brrwts = Data_pivot %>% select(contains("FINALWT"),-FINALWT)




des = svrepdesign(weights=sampweights, 
                  repweights=brrwts, 
                  type="Fay", 
                  rho=0.5, 
                  mse=TRUE,
                  data = Data_pivot)

# calculate the proportion
svyresult = svyby(~indicator, by = ~REGION + MainHeatSource,des,svymean)

svyresult$lwr = svyresult$indicator - 1.96 * svyresult$se
svyresult$upr = svyresult$indicator + 1.96 * svyresult$se



Data_pivot$MainHeatSource =  Data_pivot$MainHeatSource %>%
  map(Energy_map) %>% unlist()
Data_pivot$MainHeatSource = as.factor(Data_pivot$MainHeatSource)
Data_pivot$MainHeatSource = relevel(Data_pivot$MainHeatSource, "Natural gas")

# test the significance 
des = svrepdesign(weights=sampweights, 
                  repweights=brrwts, 
                  type="Fay", 
                  rho=0.5, 
                  mse=TRUE,
                  data = Data_pivot)
test_output = svyglm(indicator ~ MainHeatSource:REGION +REGION, des)
summary(test_output)
#79:---------------------------------------------------------------------------
