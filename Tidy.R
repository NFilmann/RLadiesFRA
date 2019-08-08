library(tidyverse)
#Import the data
labdata <- read_delim("https://raw.githubusercontent.com/NFilmann/RLadiesFRA/master/labdata.csv", delim=";")
basedata <- read_delim("https://raw.githubusercontent.com/NFilmann/RLadiesFRA/master/basedata.csv", delim=";")

#Have a look at the data
str(labdata)
str(basedata)


# 1. Join both datasets
# The primary key in each table is the variable PatientID

# At first we assure, that we have no missing values in PatientID
summary(unique(labdata$PatientID))
summary(basedata$PatientID)
#-> no missings, and the IDs match

HIVdata <- full_join(labdata, basedata)

str(HIVdata)


# 2. Make sure that each observation corresponds to one row

HIVdata=split(labdata, labdata$Time_weeks) %>% 
  reduce(left_join, by = "PatientID") %>%
  full_join(basedata, .)
str(HIVdata)

HIVdata <- HIVdata %>% rename_all(funs(str_replace(., ".y.y.y", ".24"))) %>% 
  rename_all(funs(str_replace(., ".x.x.x", ".16"))) %>% 
  rename_all(funs(str_replace(., ".y.y", ".12"))) %>% 
  rename_all(funs(str_replace(., ".x.x", ".8"))) %>% 
  rename_all(funs(str_replace(., ".y", ".4"))) %>% 
  rename_all(funs(str_replace(., ".x", ".0"))) 

str(HIVdata)

HIVdata <- HIVdata %>% rename_all(funs(str_replace(., "Start_thera.4", "Start_therapy"))) %>% 
  rename_all(funs(str_replace(., "Stu.4Center", "StudyCenter"))) 
str(HIVdata)


# 3. Calculate the patient age at start of therapy
HIVdata <-HIVdata %>% mutate(HIVdata, Age=((difftime(as.Date(HIVdata$Start_therapy, format="%d.%m.%Y"), as.Date(HIVdata$DateOfBirth, format="%d.%m.%Y"))))) %>%
  mutate(Age=as.numeric(round(Age/365.25,0)))
str(HIVdata)


# 4&5. Calculate a new variable „VirResponse“ indicating if thrapeutic success (i.e. viral load <20 copies/ml) is reached 24 weeks after start of therapy
# Recode HBVpos and HCVpos in a meaningful way

#For VirResponse you could use simply the variable "TErgNumOperator.24", where an "<" indicates that the
#treatment was succesful
HIVdata <-HIVdata  %>% mutate(
                    VirResponse = as.numeric(!is.na(TErgNumOperator.24)),
                    HBVpos = as.numeric(!is.na(HBVpos)),
                    HCVpos = as.numeric(!is.na(HCVpos)))
            

#labeling the variables in a meaningful way
HIVdata <-HIVdata %>%
  mutate(VirResponse = recode(VirResponse,  "0" = "no", "1" = "yes"),
         HBVpos = recode(HBVpos,  "0" = "neg", "1" = "pos"),
         HCVpos = recode(HCVpos,  "0" = "neg", "1" = "pos"))

# #Labeling the variable names
HIVdata <-HIVdata %>%
  mutate(VirResponse = structure(VirResponse, label = "Virologic Response"),
  TErgNum.0 = structure(TErgNum.0, label = "Viral load at baseline"),
  Value.0 = structure(Value.0, label = "CD4 at baseline"),
  HBVpos = structure(HBVpos, label = "HBV pos"),
  HCVpos = structure(HCVpos, label = "HCV pos"))

str(HIVdata)
#(How do you deal with patients that died before end of treatment?)
#They don't have any lab values at week 24, so they are already taken into account. 


#Of course we could go on here, for example the variables, that contain the medication are a mess,
#but we rather look at some plots now.


