#==============================================
#Author: Bryan Luke Shabroski
#Purpose: Transform our excel data into rds.
#==============================================


library(readxl)

#Read excel data
student_info <- read_excel("C:\\Users\\bryan\\Downloads\\pde_sim_final_2023-11-29.xlsx")
survey_info <- read_excel("C:\\Users\\bryan\\Downloads\\or_survey_sim_final_2023-11-29.xlsx")

#Display Data
print(colnames(student_info))

for(col in colnames(student_info)[-1]) {
  cat("\n", col, ":\n")
  print(unique(student_info[[col]]))
}



print(colnames(survey_info))

for(col in colnames(survey_info)[-1]) {
  cat("\n", col, ":\n")
  print(unique(survey_info[[col]]))
}

#Save data as rds
saveRDS(student_info, "studentData/dfFinalStuType.rds")
saveRDS(survey_info, "surveyData/dfFinalSurveyAnswers.rds")


