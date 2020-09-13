#libraries

library(tidyverse)
library(readr)
library(knitr)
library(ggplot2)
library(psych)
library(stringdist)
library(qwraps2)
library(stargazer)

#database

orig_database <- read_csv("https://raw.githubusercontent.com/danielortim/db_public/master/results-survey783831%20(9).csv")

#Creation of a function to sort numeric and string data as its propper type
orig_database[] <- lapply(orig_database, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})

#cleaning of edcuation and prof status in a single col with corresponding numbers

orig_database <- orig_database %>%
  mutate(education_level= (
      ((Education_1)*1) + 
      ((Education_2)*2) + 
      ((Education_3)*3) +
      ((Education_4)*4) +  
      ((Education_5)*5) + 
      ((Education_6)*6)
  ))%>%
  select(-Education_1, -Education_2, -Education_3, -Education_4, -Education_5, -Education_6)%>%
  mutate(professional_status = (
      ((ProfStatus_1)*1)+
      ((ProfStatus_2)*2)+
      ((ProfStatus_3)*3)+
      ((ProfStatus_4)*4)+
      ((ProfStatus_5)*5)+
      ((ProfStatus_6)*6)+
      ((ProfStatus_7)*7)
  ))%>%
  select(-ProfStatus_1, -ProfStatus_2, -ProfStatus_3, -ProfStatus_4, -ProfStatus_5, -ProfStatus_6, -ProfStatus_7)


summary(orig_database$professional_status)

colmumbers <- data.frame(colnames(orig_database))


 #replacing values in nationality to homogenize
orig_database$Nationality[grep("fr[a|e]n[c|ç]", tolower(orig_database$Nationality))] <- "France"
orig_database$Nationality[grep("col", tolower(orig_database$Nationality))] <- "Colombia"
orig_database$Nationality[grep("amer", tolower(orig_database$Nationality))] <- "United States"
orig_database$Nationality[grep("usa", tolower(orig_database$Nationality))] <- "United States"
orig_database$Nationality[grep("united", tolower(orig_database$Nationality))] <- "United States"
orig_database$Nationality[grep("alg", tolower(orig_database$Nationality))] <- "Algeria"
orig_database$Nationality[grep("liban", tolower(orig_database$Nationality))] <- "Lebanon"
orig_database$Nationality[grep("maroc", tolower(orig_database$Nationality))] <- "Moroco"
orig_database$Nationality[grep("per", tolower(orig_database$Nationality))] <- "Peru"
orig_database$Nationality[grep("pol", tolower(orig_database$Nationality))] <- "Poland"
orig_database$Nationality[grep("turq", tolower(orig_database$Nationality))] <- "Turkey"

nat<-data.frame(table(orig_database$Nationality))

#replacing values in countries of residence to homogenize

orig_database$Residence[grep("fr[a|e]n[c|ç]", tolower(orig_database$Residence))] <- "France"
orig_database$Residence[grep("col", tolower(orig_database$Residence))] <- "Colombia"
orig_database$Residence[grep("amer", tolower(orig_database$Residence))] <- "United States"
orig_database$Residence[grep("usa", tolower(orig_database$Residence))] <- "United States"
orig_database$Residence[grep("united", tolower(orig_database$Residence))] <- "United States"
orig_database$Residence[grep("alg", tolower(orig_database$Residence))] <- "Algeria"
orig_database$Residence[grep("liban", tolower(orig_database$Residence))] <- "Lebanon"
orig_database$Residence[grep("maroc", tolower(orig_database$Residence))] <- "Moroco"
orig_database$Residence[grep("per", tolower(orig_database$Residence))] <- "Peru"
orig_database$Residence[grep("pol", tolower(orig_database$Residence))] <- "Poland"
orig_database$Residence[grep("turq", tolower(orig_database$Residence))] <- "Turkey"
    ##made an educated guess with the only yes value
orig_database$Residence[grep("yes", tolower(orig_database$Residence))] <- "Norway"

res<-data.frame(table(orig_database$Residence))


#creation of replacement function for scales 

gsub.num <- function(x) as.double(gsub("[a-zA-Z]", "", x))

#Cleaning of esi R scale

orig_database[13:44] <- lapply(orig_database[13:44], gsub.num)

#cleaning of PANAS

orig_database[51:60] <- lapply(orig_database[51:50], gsub.num)

#cleaning of VDA 1

orig_database[81:83] <- lapply(orig_database[81:83], gsub.num)

#cleaning of VDA 2

orig_database[115:117] <- lapply(orig_database[115:117], gsub.num)

#cleaning of  wtp

orig_database[183:205] <- lapply(orig_database[183:205], gsub.num)

#loading of the clean database without exclusion criteria or NA management (file that is uploaded to gHub)

#write.csv(orig_database, "C:\\Users\\daort\\Documents\\M1 economics and Pychology\\Thesis\\raw_clean.csv")