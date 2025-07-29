#============================================================================
# Author: Bryan Luke Shabroski
# Purpose: In order for the UI of textInputSliders() to look clean,
# we must abbreviate some of our incoming data.
#============================================================================

library(dplyr)

# read our rds
dfFinalStuType <- readRDS("studentData/dfFinalStuType.rds")

# For student pop we simply make UG transfer Transfer.
dfFinalStuType$STUDENT_POPULATION_DESC[dfFinalStuType$STUDENT_POPULATION_DESC == "UG Transfer"] <- "Transfer"

# For housing we abbreviate
dfFinalStuType$HOUSING_TYPE[dfFinalStuType$HOUSING_TYPE == "On-Campus"] <- "On"
dfFinalStuType$HOUSING_TYPE[dfFinalStuType$HOUSING_TYPE == "Off-Campus"] <- "Off"

# For communities we abbreviate
dfFinalStuType$COMMUNITY[dfFinalStuType$COMMUNITY == "College-in-the-Woods"] <- "CitW"
dfFinalStuType$COMMUNITY[dfFinalStuType$COMMUNITY == "Dickinson Community"] <- "Dick" 
dfFinalStuType$COMMUNITY[dfFinalStuType$COMMUNITY == "Hillside Community"] <- "Hill"
dfFinalStuType$COMMUNITY[dfFinalStuType$COMMUNITY == "Hinman College"] <- "Hin"
dfFinalStuType$COMMUNITY[dfFinalStuType$COMMUNITY == "Mountainview College"] <- "Mtn"
dfFinalStuType$COMMUNITY[dfFinalStuType$COMMUNITY == "Newing College"] <- "New"
dfFinalStuType$COMMUNITY[dfFinalStuType$COMMUNITY == "Susquehanna Community"] <- "Susq"

# For colleges we remove UG and abbreviate if necessary
dfFinalStuType$COLLEGE_DESC[dfFinalStuType$COLLEGE_DESC == "UG CCPA"] <- "CCPA"
dfFinalStuType$COLLEGE_DESC[dfFinalStuType$COLLEGE_DESC == "UG CEO"] <- "CEO"
dfFinalStuType$COLLEGE_DESC[dfFinalStuType$COLLEGE_DESC == "UG Harpur"] <- "Harpur"
dfFinalStuType$COLLEGE_DESC[dfFinalStuType$COLLEGE_DESC == "UG Management"] <- "Mgmt"
dfFinalStuType$COLLEGE_DESC[dfFinalStuType$COLLEGE_DESC == "UG Nursing"] <- "Nursing"
dfFinalStuType$COLLEGE_DESC[dfFinalStuType$COLLEGE_DESC == "UG Watson"] <- "Watson"

#For testing
#print(unique(dfFinalStuType$STUDENT_POPULATION_DESC))
#print(unique(dfFinalStuType$HOUSING_TYPE))
#print(unique(dfFinalStuType$COMMUNITY))
#print(unique(dfFinalStuType$COLLEGE_DESC))

saveRDS(dfFinalStuType, "studentData/dfFinalStuType.rds")


