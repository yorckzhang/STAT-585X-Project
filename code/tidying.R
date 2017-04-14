library(tidyverse)
library(readxl)
library(lubridate)

library(plotly)
library(plyr)
library(maps)
library(mapproj)



##Functions to clean the data using the tidy techniques we learned

educationTidy <- function(education){
  
  #selecting the important variables
  education_tidy <- education %>% select (1,2,3,8,9,10,11,16,17,18,19,24,25,26,27,32,33,34,35,40,41,42,43) %>% data.frame()
  
  #here we fix the column names
  names(education_tidy) <- gsub("..","-",names(education_tidy),fixed=TRUE)
  
  #renaming some columns to common pattern
  education_tidy <- education_tidy %>% rename("FIPS"=`FIPS.Code`,"Area_Name"=`Area.name`,"Some.college.or.associate.s.degree-1970"=`Some.college-1.3.years-.1970`,"Some.college.or.associate.s.degree-1980"=`Some.college-1.3.years-.1980`)
  
  #here we gather the data and split to give us a tidy format of our data 
  education_tidy <- education_tidy %>% gather(key="TypeofEducation-Year",value="Number",4:23) %>% 
    separate(`TypeofEducation-Year`,into=c("TypeofEducation","Year"),sep="-")
  
  row.names(education_tidy) <- NULL
  
  return (education_tidy)
}

populationTidy <- function(population){
 
  #selecting the important variables
  population_tidy <- population %>% select(1,2,3,11,12,13,14,15,16,23,24,25,26,27,28,29,30,31,32,33,34)%>%data.frame()
  
  #renaming some columns to common pattern
  names(population_tidy)[4:9] <- c("Population-2010","Population-2011","Population-2012","Population-2013","Population-2014","Population-2015")
  
  #here we gather the data and split to give us a tidy format of our data 
  population_tidy<- population_tidy %>% gather(key="Type-Year",value="Number",4:21) %>% separate(`Type-Year`,into=c("Type","Year"))
  
  row.names(population_tidy) <- NULL
  
  return(population_tidy)
  
}

povertyTidy <- function(poverty){
  
  #selecting the important variables
  poverty_tidy_2015 <- poverty %>% select(1,2,3,8,14,26)%>% data.frame()
  
  #renaming some columns to common pattern
  names(poverty_tidy_2015) <- c("FIPS","State","Area_Name","Overall_Poverty","age_0-17_poverty","Median_Household_Income")
  
  #here we gather the data us a tidy format of our data 
  poverty_tidy_2015 <- poverty_tidy_2015 %>% gather(key="Type",value="Number",4:6)
  
  row.names(poverty_tidy_2015) <- NULL
  
  return (poverty_tidy_2015)
  
}

unemploymentTidy <- function(unemployment){
  
  #selecting the important variables
  unemployment_tidy <- unemployment %>% select(1,2,3,9,10,13,14,17,18,21,22,25,26,29,30,33,34,37,38,41,42) %>% data.frame()
  
  #renaming some columns to common pattern
  unemployment_tidy <- unemployment_tidy %>% rename("FIPS"=`FIPStxt`,"Area_Name"=`Area_name`)
  
  columnNames <- colnames(unemployment_tidy)[4:21]
  #here we parse the data into numeric since it was in character format
  unemployment_tidy[columnNames] <- sapply(unemployment_tidy[columnNames],parse_number)
  
  #here we gather the data and split to give us a tidy format of our data 
  unemployment_tidy <- unemployment_tidy %>% gather(key="EmploymentType-year", value="Number",4:21) %>% 
    separate(`EmploymentType-year`,into=c("Employment Type", "Year"))
  
  row.names(unemployment_tidy) <- NULL
  
  return(unemployment_tidy)
  
}


#this function return data of States only
stateData <- function(data){
  states <- state.name
  return(data[data$Area_Name %in% states,])
}







#reading data
education <- read_excel(path="data/Education.xls",sheet = 1)
population <- read_excel(path="data/PopulationEstimates.xls",sheet = 1)
poverty <- read_excel(path="data/PovertyEstimates.xls", sheet = 1)
unemployment <- read_excel(path="data/Unemployment.xls", sheet=1)
names(education)
names(population)
names(poverty)
names(unemployment)

education_tidy <- educationTidy(education)
population_tidy <- populationTidy(population)
poverty_tidy <- povertyTidy(poverty)
unemployment_tidy <- unemploymentTidy(unemployment)

education_StatesOnly <- stateData(education_tidy)
population_StatesOnly <- stateData(population_tidy)
poverty_StatesOnly <- stateData(poverty_tidy)
unemployment_StatesOnly <- stateData(unemployment_tidy)

education_US <- education_StatesOnly %>% group_by(TypeofEducation,Year) %>% summarize(count = sum(Number))
population_US <- population_StatesOnly %>% group_by(Type,Year) %>% summarize(count=sum(Number))
poverty_US <- poverty_StatesOnly %>% group_by(Type) %>% summarize(count=sum(Number))
unemployment_US <- unemployment_StatesOnly %>% group_by(`Employment Type`,Year) %>% summarize(count=sum(Number))











