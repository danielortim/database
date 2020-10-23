#libraries

library(tidyverse)
library(readr)
library(knitr)
library(ggplot2)
library(psych)
library(stringdist)
library(qwraps2)
library(stargazer)
library(formattable)
library(dplyr)
library(pastecs)

################
#Cleaning,scoring and joining an original database
################

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


#creation of replacement function for scales to remove letter

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

#adding of VRAT score database and binding it to current one

       
vrat_scores <- read.csv("https://raw.githubusercontent.com/danielortim/database/master/Copy%20of%20Result%20vRAT_%20(003).csv")
vrat_scores <-vrat_scores %>%
  select(-c(2:5))%>%
  replace(is.na(.), 0) %>%
  mutate(vrat_total = rowSums(.[2:21])) %>%
  rename(id = Response.ID)
 ##This function binds the database only if the ID numbers match, otherwise displays NA
  orig_database <- left_join(orig_database, vrat_scores, by = c("id" = "id"))


#adding DTT scoring database

dtt_score <- read.csv("https://raw.githubusercontent.com/danielortim/database/master/Copy%20of%20Results%20DT_%20(004).csv")
  colnames(dtt_score)
  dtt_score <- dtt_score %>%
  select(c(1,7,8,10,14)) %>%
  rename(id = Response.ID)
  
  orig_database <- left_join(orig_database, dtt_score, by = c("id" = "id"))

#checking for the final order of colnames and updating the list, as well as summary table

colmumbers <- data.frame(colnames(orig_database))

summary_table<- data.frame(do.call(cbind, lapply(orig_database, summary)))

#Scoring the EsiR scale 
#RECODEq3 q8 q13 q18 q19 q23 q28 (MISSING=SYSMIS) (0=4) (1=3) (2=2) (3=1) (4=0) .EXECUTE .

esi_r <- orig_database %>%
  select(c(13:44)) %>%
  mutate_at(c(3,8,13,18,19,23,28), funs(recode(., `0`=4, `1`=3,`2`=2,`3`=1,`4`=0))) %>%
 #*ESI Paranormal Beliefs COMPUTE ParaNB = SUM(q4, q9, q14, q19, q24, q29) .VARIABLE LABELS ParaNB "ESI Paranormal Beliefs " 
  mutate(para_nb = ESIR_SQ004 + ESIR_SQ009 + ESIR_SQ014 + ESIR_SQ019 + ESIR_SQ024 + ESIR_SQ029) %>%
 #ESI Existential Well-being (maybe interesting to see if "awe& wonder people" are happier, but could drop this dimension)
 #COMPUTE ExWell = SUM(q3, q8, q13, q18, q23, q28) .
  mutate(ex_well = ESIR_SQ003 + ESIR_SQ008 + ESIR_SQ013 + ESIR_SQ018 + ESIR_SQ023 + ESIR_SQ028) %>%
 #ESI Cognitive Orientation Towards Spirituality (Really is just spirituality)
 # CogOr = SUM(q1, q6, q11, q16, q21, q26) .  
  mutate(cog_or = ESIR_SQ001 + ESIR_SQ006 + ESIR_SQ011 + ESIR_SQ016 + ESIR_SQ021 + ESIR_SQ026) %>%
 # ESI Religiousness
 #COMPUTE Relness = SUM(q5, q10, q15, q20, q25, q30) .
  mutate(rel_ness = ESIR_SQ005 + ESIR_SQ010 + ESIR_SQ015 + ESIR_SQ020 + ESIR_SQ025 + ESIR_SQ030)

orig_database <- cbind(orig_database, esi_r$para_nb, esi_r$ex_well, esi_r$cog_or, esi_r$rel_ness)

#Scoring AWE-s scale

awe_s <- orig_database %>%
  select(c(153:182)) %>%
  mutate(time_percept = rowSums(.[1:5])) %>%
  mutate(self_loss = rowSums(.[6:10])) %>%
  mutate(conectedness = rowSums(.[11:15])) %>%
  mutate(vastness = rowSums(.[16:20])) %>%
  mutate(physiologgical = rowSums(.[21:25])) %>%
  mutate(acommodation = rowSums(.[25:30]))

orig_database <- cbind(orig_database, awe_s$time_percept, awe_s$self_loss, awe_s$conectedness, awe_s$vastness, awe_s$physiologgical, awe_s$acommodation)

colmumbers <- data.frame(colnames(orig_database))

descriptives_orig_database<- data.frame(t(stat.desc(orig_database)))


       
################
#Excluding cases and writing a clean database
################

clean_database <- orig_database %>%
  filter(lastpage >= 13)

descriptives_clean_database<- data.frame(t(stat.desc(clean_database)))

summary_table_clean<- data.frame(do.call(cbind, lapply(clean_database, summary)))

#write.csv(orig_database, "C:\\Users\\daort\\Documents\\M1 economics and Pychology\\Thesis\\raw_clean.csv")

#formattable(orig_database)


