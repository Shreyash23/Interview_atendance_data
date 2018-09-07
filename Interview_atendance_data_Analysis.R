library(readr)
library(lubridate)
library(corrplot)
library(caret)
library(keras)
library(recipes)
install_keras()
Interview_Attendance_Data <- read_csv("C:/Users/shrey/Desktop/Interview_atendance_data/Interview_Attendance_Data.csv")
#replacing spaces with "_" in the column names
names(Interview_Attendance_Data) <- gsub(" ", "_", names(Interview_Attendance_Data))

#Taking care of the date column
str(Interview_Attendance_Data$Date_of_Interview)
Interview_Attendance_Data$Date_of_Interview <- gsub("&.*","",Interview_Attendance_Data$Date_of_Interview)
Interview_Attendance_Data$Date_of_Interview <- parse_date_time(x= Interview_Attendance_Data$Date_of_Interview,
                orders = c("%d.%m.%Y","%d.%m.%y","%d/%m/%y","%d/%m/%Y","%d-%b-%y","%d-%b-%Y"))
Interview_Attendance_Data$Date_of_Interview <- as.Date(Interview_Attendance_Data$Date_of_Interview)
str(Interview_Attendance_Data$Date_of_Interview)

#Taking care of the Client Name column
str(Interview_Attendance_Data$Client_name)
Interview_Attendance_Data$Client_name <- as.factor(Interview_Attendance_Data$Client_name)
str(Interview_Attendance_Data$Client_name)
Interview_Attendance_Data[which(Interview_Attendance_Data$Client_name=='Aon hewitt Gurgaon'),]$Client_name <- 'Aon Hewitt'
Interview_Attendance_Data[which(Interview_Attendance_Data$Client_name=='Hewitt'),]$Client_name <- 'Aon Hewitt'
Interview_Attendance_Data[which(Interview_Attendance_Data$Client_name=='Standard Chartered Bank Chennai'),]$Client_name <- 'Standard Chartered Bank'
Interview_Attendance_Data$Client_name <- factor(Interview_Attendance_Data$Client_name)
table(Interview_Attendance_Data$Client_name)

#Taking care of the Industry column
str(Interview_Attendance_Data$Industry)
Interview_Attendance_Data$Industry <- as.factor(Interview_Attendance_Data$Industry)
str(Interview_Attendance_Data$Industry)
table(Interview_Attendance_Data$Industry)

#Taking care of the Location column
str(Interview_Attendance_Data$Location)
Interview_Attendance_Data$Location <- as.factor(Interview_Attendance_Data$Location)
str(Interview_Attendance_Data$Location)
table(Interview_Attendance_Data$Location)
Interview_Attendance_Data[which(Interview_Attendance_Data$Location=='Gurgaonr'),]$Location <- 'Gurgaon'
Interview_Attendance_Data[which(Interview_Attendance_Data$Location=='chennai'),]$Location <- 'Chennai'
Interview_Attendance_Data[which(Interview_Attendance_Data$Location=='CHENNAI'),]$Location <- 'Chennai'
Interview_Attendance_Data$Location <- factor(Interview_Attendance_Data$Location)
table(Interview_Attendance_Data$Location)

#Taking care of the Position_to_be_closed column
str(Interview_Attendance_Data$Position_to_be_closed)
Interview_Attendance_Data$Position_to_be_closed <- as.factor(Interview_Attendance_Data$Position_to_be_closed)
str(Interview_Attendance_Data$Position_to_be_closed)
table(Interview_Attendance_Data$Position_to_be_closed)

#Taking care of the Nature_of_Skillset column
str(Interview_Attendance_Data$Nature_of_Skillset)
Interview_Attendance_Data$Nature_of_Skillset <- as.factor(Interview_Attendance_Data$Nature_of_Skillset)
str(Interview_Attendance_Data$Nature_of_Skillset)
table(Interview_Attendance_Data$Nature_of_Skillset)
Interview_Attendance_Data$Nature_of_Skillset <- tolower(Interview_Attendance_Data$Nature_of_Skillset)
#Making a new column instead of Nature_of_Skillset
Interview_Attendance_Data$Nature_of_Skillset_Group <- NA
Interview_Attendance_Data[which(grepl("SCCM",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'SCCM'
Interview_Attendance_Data[which(grepl("MEDNET",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'MEDNET'
Interview_Attendance_Data[which(grepl("JAVA",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'JAVA'
Interview_Attendance_Data[which(grepl("ANALYST|ANALYTICAL",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'ANALYST'
Interview_Attendance_Data[which(grepl("SAS",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'SAS'
Interview_Attendance_Data[which(grepl("ORACLE",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'ORACLE'
Interview_Attendance_Data[which(grepl("AML|KYC|CDD|REGULATORY",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'AML'
Interview_Attendance_Data[which(grepl("biosimilars|biosimillar|biosimiliars",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'ORACLE'
Interview_Attendance_Data[which(grepl("accounting",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'Accounting'
Interview_Attendance_Data[which(grepl("lending|liablities|liabilities|liability",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'lending&liabilities'
Interview_Attendance_Data[which(grepl("testing",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'Testing'
Interview_Attendance_Data[which(grepl("operations",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'Operations'
Interview_Attendance_Data[which(grepl("cots",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'COTS'
Interview_Attendance_Data[which(grepl("dot net",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- '.Net'
Interview_Attendance_Data[which(grepl("emea",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'EMEA'
Interview_Attendance_Data[which(grepl("etl",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'ETL'
Interview_Attendance_Data[which(grepl("hadoop",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'Hadoop'
Interview_Attendance_Data[which(grepl("routine",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'Routine'
Interview_Attendance_Data[which(grepl("manager|management|lead|tl",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'Manager'
Interview_Attendance_Data[which(grepl("t-24",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'T-24'
Interview_Attendance_Data[which(grepl("fresher",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'Fresher'
Interview_Attendance_Data[which(grepl("labelling",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'Global labelling'
Interview_Attendance_Data[which(grepl("#name?|10.00 am|11.30 am|12.30 pm|9.00 am|9.30 am",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'Global labelling'
Interview_Attendance_Data[which(grepl("publishing",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'Publishing'
Interview_Attendance_Data[which(grepl("production",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'Production'
Interview_Attendance_Data[which(grepl("label|licensing",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'Label & Licensing'
Interview_Attendance_Data[which(grepl("product control",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'Product control'
Interview_Attendance_Data[which(grepl("generic drugs",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'Generic drugs'

Interview_Attendance_Data$Nature_of_Skillset <- gsub("l & l","ll",Interview_Attendance_Data$Nature_of_Skillset)
Interview_Attendance_Data[which(grepl("ll",Interview_Attendance_Data$Nature_of_Skillset,ignore.case = TRUE)),]$Nature_of_Skillset_Group <- 'lending&liabilities'

Interview_Attendance_Data$Nature_of_Skillset_Group <- as.factor(Interview_Attendance_Data$Nature_of_Skillset_Group)
table(Interview_Attendance_Data$Nature_of_Skillset_Group)
#Deleting the old nature of skillset column
Interview_Attendance_Data$Nature_of_Skillset <- NULL

#Taking care of the interview type column
str(Interview_Attendance_Data$Interview_Type)
Interview_Attendance_Data$Interview_Type <- as.factor(Interview_Attendance_Data$Interview_Type)
table(Interview_Attendance_Data$Interview_Type)
Interview_Attendance_Data$Interview_Type = as.character(Interview_Attendance_Data$Interview_Type)
inds = grep("Sch?eduled [Ww]alk ?[Ii]n",Interview_Attendance_Data$Interview_Type)
Interview_Attendance_Data[inds,"Interview_Type"] = "Scheduled Walk In"
inds = grep("Walkin ?",Interview_Attendance_Data$Interview_Type)
Interview_Attendance_Data[inds,"Interview_Type"] = "Walk In"
inds = grep("Scheduled $",Interview_Attendance_Data$Interview_Type)
Interview_Attendance_Data[inds,"Interview_Type"] = "Scheduled"
str(Interview_Attendance_Data$Interview_Type)
Interview_Attendance_Data$Interview_Type <- as.factor(Interview_Attendance_Data$Interview_Type)
table(Interview_Attendance_Data$Interview_Type)

#Taking care of the Gender column
str(Interview_Attendance_Data$Gender)
Interview_Attendance_Data$Gender <- as.factor(Interview_Attendance_Data$Gender)
table(Interview_Attendance_Data$Gender)

#Taking care of the Candidate_Current_Location column
str(Interview_Attendance_Data$Candidate_Current_Location)
Interview_Attendance_Data$Candidate_Current_Location <- as.factor(Interview_Attendance_Data$Candidate_Current_Location)
table(Interview_Attendance_Data$Candidate_Current_Location)
Interview_Attendance_Data[which(Interview_Attendance_Data$Candidate_Current_Location=='chennai'),]$Candidate_Current_Location <- 'Chennai'
Interview_Attendance_Data[which(Interview_Attendance_Data$Candidate_Current_Location=='CHENNAI'),]$Candidate_Current_Location <- 'Chennai'
Interview_Attendance_Data$Candidate_Current_Location <- factor(Interview_Attendance_Data$Candidate_Current_Location)
#Interview_Attendance_Data[which(Interview_Attendance_Data$Candidate_Current_Location=='- Cochin-'),]$Candidate_Current_Location <- 'Cochin'
table(Interview_Attendance_Data$Location,Interview_Attendance_Data$Candidate_Current_Location)
#Both these columns are identical, so we can delete one of these
Interview_Attendance_Data$Location <- NULL

#Taking care of the Candidate_Job_Location column
str(Interview_Attendance_Data$Candidate_Job_Location)
Interview_Attendance_Data$Candidate_Job_Location <- as.factor(Interview_Attendance_Data$Candidate_Job_Location)
table(Interview_Attendance_Data$Candidate_Job_Location)

#Taking care of the Interview_Venue column
str(Interview_Attendance_Data$Interview_Venue)
Interview_Attendance_Data$Interview_Venue <- as.factor(Interview_Attendance_Data$Interview_Venue)
table(Interview_Attendance_Data$Interview_Venue)

#Taking care of the Candidate_Native_location column
str(Interview_Attendance_Data$Candidate_Native_location)
Interview_Attendance_Data$Candidate_Native_location <- as.factor(Interview_Attendance_Data$Candidate_Native_location)
table(Interview_Attendance_Data$Candidate_Native_location)
Interview_Attendance_Data[which(Interview_Attendance_Data$Candidate_Native_location=='Delhi /NCR'),]$Candidate_Native_location <- 'Delhi'
Interview_Attendance_Data[which(Interview_Attendance_Data$Candidate_Native_location=='- Cochin-'),]$Candidate_Native_location <- 'Cochin'
#Interview_Attendance_Data$Candidate_Native_location <- factor(Interview_Attendance_Data$Candidate_Native_location)
table(Interview_Attendance_Data$Candidate_Native_location)

#Taking care of the Have_you_obtained_the_necessary_permission_to_start_at_the_required_time column
str(Interview_Attendance_Data$Have_you_obtained_the_necessary_permission_to_start_at_the_required_time)
Interview_Attendance_Data$Have_you_obtained_the_necessary_permission_to_start_at_the_required_time <- as.factor(Interview_Attendance_Data$Have_you_obtained_the_necessary_permission_to_start_at_the_required_time)
table(Interview_Attendance_Data$Have_you_obtained_the_necessary_permission_to_start_at_the_required_time)
Interview_Attendance_Data[which(Interview_Attendance_Data$Have_you_obtained_the_necessary_permission_to_start_at_the_required_time=='No'),]$Have_you_obtained_the_necessary_permission_to_start_at_the_required_time <- 'NO'
Interview_Attendance_Data[which(Interview_Attendance_Data$Have_you_obtained_the_necessary_permission_to_start_at_the_required_time=='Not yet'),]$Have_you_obtained_the_necessary_permission_to_start_at_the_required_time <- 'NO'
Interview_Attendance_Data[which(Interview_Attendance_Data$Have_you_obtained_the_necessary_permission_to_start_at_the_required_time=='Yet to confirm'),]$Have_you_obtained_the_necessary_permission_to_start_at_the_required_time <- 'Na'
Interview_Attendance_Data[which(is.na(Interview_Attendance_Data$Have_you_obtained_the_necessary_permission_to_start_at_the_required_time)),]$Have_you_obtained_the_necessary_permission_to_start_at_the_required_time <- 'Na'
Interview_Attendance_Data[which(Interview_Attendance_Data$Have_you_obtained_the_necessary_permission_to_start_at_the_required_time=='yes'),]$Have_you_obtained_the_necessary_permission_to_start_at_the_required_time <- 'Yes'
Interview_Attendance_Data$Have_you_obtained_the_necessary_permission_to_start_at_the_required_time <- factor(Interview_Attendance_Data$Have_you_obtained_the_necessary_permission_to_start_at_the_required_time)
table(Interview_Attendance_Data$Have_you_obtained_the_necessary_permission_to_start_at_the_required_time)

#Taking care of the Hope_there_will_be_no_unscheduled_meetings column
str(Interview_Attendance_Data$Hope_there_will_be_no_unscheduled_meetings)
Interview_Attendance_Data$Hope_there_will_be_no_unscheduled_meetings <- as.factor(Interview_Attendance_Data$Hope_there_will_be_no_unscheduled_meetings)
table(Interview_Attendance_Data$Hope_there_will_be_no_unscheduled_meetings)
Interview_Attendance_Data[which(Interview_Attendance_Data$Hope_there_will_be_no_unscheduled_meetings=='Not sure'),]$Hope_there_will_be_no_unscheduled_meetings <- 'Not Sure'
Interview_Attendance_Data[which(Interview_Attendance_Data$Hope_there_will_be_no_unscheduled_meetings=='yes'),]$Hope_there_will_be_no_unscheduled_meetings <- 'Yes'
Interview_Attendance_Data[which(Interview_Attendance_Data$Hope_there_will_be_no_unscheduled_meetings=='Not Sure'),]$Hope_there_will_be_no_unscheduled_meetings <- 'No'
Interview_Attendance_Data[which(Interview_Attendance_Data$Hope_there_will_be_no_unscheduled_meetings=='cant Say'),]$Hope_there_will_be_no_unscheduled_meetings <- 'No'
Interview_Attendance_Data[which(is.na(Interview_Attendance_Data$Hope_there_will_be_no_unscheduled_meetings)),]$Hope_there_will_be_no_unscheduled_meetings <- 'Na'
Interview_Attendance_Data$Hope_there_will_be_no_unscheduled_meetings <- factor(Interview_Attendance_Data$Hope_there_will_be_no_unscheduled_meetings)
table(Interview_Attendance_Data$Hope_there_will_be_no_unscheduled_meetings)

#Taking care of the Can_I_Call_you_three_hours_before_the_interview_and_follow_up_on_your_attendance_for_the_interview column
str(Interview_Attendance_Data$Can_I_Call_you_three_hours_before_the_interview_and_follow_up_on_your_attendance_for_the_interview)
Interview_Attendance_Data$Can_I_Call_you_three_hours_before_the_interview_and_follow_up_on_your_attendance_for_the_interview <- as.factor(Interview_Attendance_Data$Can_I_Call_you_three_hours_before_the_interview_and_follow_up_on_your_attendance_for_the_interview)
table(Interview_Attendance_Data$Can_I_Call_you_three_hours_before_the_interview_and_follow_up_on_your_attendance_for_the_interview)
Interview_Attendance_Data[which(Interview_Attendance_Data$Can_I_Call_you_three_hours_before_the_interview_and_follow_up_on_your_attendance_for_the_interview=='yes'),]$Can_I_Call_you_three_hours_before_the_interview_and_follow_up_on_your_attendance_for_the_interview <- 'Yes'
Interview_Attendance_Data[which(Interview_Attendance_Data$Can_I_Call_you_three_hours_before_the_interview_and_follow_up_on_your_attendance_for_the_interview=='No Dont'),]$Can_I_Call_you_three_hours_before_the_interview_and_follow_up_on_your_attendance_for_the_interview <- 'No'
Interview_Attendance_Data[which(is.na(Interview_Attendance_Data$Can_I_Call_you_three_hours_before_the_interview_and_follow_up_on_your_attendance_for_the_interview)),]$Can_I_Call_you_three_hours_before_the_interview_and_follow_up_on_your_attendance_for_the_interview <- 'Na'
Interview_Attendance_Data$Can_I_Call_you_three_hours_before_the_interview_and_follow_up_on_your_attendance_for_the_interview <- factor(Interview_Attendance_Data$Can_I_Call_you_three_hours_before_the_interview_and_follow_up_on_your_attendance_for_the_interview)
table(Interview_Attendance_Data$Can_I_Call_you_three_hours_before_the_interview_and_follow_up_on_your_attendance_for_the_interview)

#Taking care of the Can_I_have_an_alternative_number/_desk_number._I_assure_you_that_I_will_not_trouble_you_too_much column
str(Interview_Attendance_Data$`Can_I_have_an_alternative_number/_desk_number._I_assure_you_that_I_will_not_trouble_you_too_much`)
Interview_Attendance_Data$`Can_I_have_an_alternative_number/_desk_number._I_assure_you_that_I_will_not_trouble_you_too_much` <- as.factor(Interview_Attendance_Data$`Can_I_have_an_alternative_number/_desk_number._I_assure_you_that_I_will_not_trouble_you_too_much`)
table(Interview_Attendance_Data$`Can_I_have_an_alternative_number/_desk_number._I_assure_you_that_I_will_not_trouble_you_too_much`)
Interview_Attendance_Data[which(Interview_Attendance_Data$`Can_I_have_an_alternative_number/_desk_number._I_assure_you_that_I_will_not_trouble_you_too_much`=='na'),]$`Can_I_have_an_alternative_number/_desk_number._I_assure_you_that_I_will_not_trouble_you_too_much` <- "Na"
Interview_Attendance_Data[which(is.na(Interview_Attendance_Data$`Can_I_have_an_alternative_number/_desk_number._I_assure_you_that_I_will_not_trouble_you_too_much`)),]$`Can_I_have_an_alternative_number/_desk_number._I_assure_you_that_I_will_not_trouble_you_too_much` <- "Na"
Interview_Attendance_Data[which(Interview_Attendance_Data$`Can_I_have_an_alternative_number/_desk_number._I_assure_you_that_I_will_not_trouble_you_too_much`=='yes'),]$`Can_I_have_an_alternative_number/_desk_number._I_assure_you_that_I_will_not_trouble_you_too_much` <- "Yes"
Interview_Attendance_Data[which(Interview_Attendance_Data$`Can_I_have_an_alternative_number/_desk_number._I_assure_you_that_I_will_not_trouble_you_too_much`=='No I have only thi number'),]$`Can_I_have_an_alternative_number/_desk_number._I_assure_you_that_I_will_not_trouble_you_too_much` <- "No"
Interview_Attendance_Data$`Can_I_have_an_alternative_number/_desk_number._I_assure_you_that_I_will_not_trouble_you_too_much` <- factor(Interview_Attendance_Data$`Can_I_have_an_alternative_number/_desk_number._I_assure_you_that_I_will_not_trouble_you_too_much`)
table(Interview_Attendance_Data$`Can_I_have_an_alternative_number/_desk_number._I_assure_you_that_I_will_not_trouble_you_too_much`)

#Taking care of the Have_you_taken_a_printout_of_your_updated_resume._Have_you_read_the_JD_and_understood_the_same column
str(Interview_Attendance_Data$Have_you_taken_a_printout_of_your_updated_resume._Have_you_read_the_JD_and_understood_the_same)
Interview_Attendance_Data$Have_you_taken_a_printout_of_your_updated_resume._Have_you_read_the_JD_and_understood_the_same <- as.factor(Interview_Attendance_Data$Have_you_taken_a_printout_of_your_updated_resume._Have_you_read_the_JD_and_understood_the_same)
table(Interview_Attendance_Data$Have_you_taken_a_printout_of_your_updated_resume._Have_you_read_the_JD_and_understood_the_same)
Interview_Attendance_Data[which(Interview_Attendance_Data$Have_you_taken_a_printout_of_your_updated_resume._Have_you_read_the_JD_and_understood_the_same=='yes'),]$Have_you_taken_a_printout_of_your_updated_resume._Have_you_read_the_JD_and_understood_the_same <- 'Yes'
Interview_Attendance_Data[which(Interview_Attendance_Data$Have_you_taken_a_printout_of_your_updated_resume._Have_you_read_the_JD_and_understood_the_same=='Not yet'),]$Have_you_taken_a_printout_of_your_updated_resume._Have_you_read_the_JD_and_understood_the_same <- 'No'
Interview_Attendance_Data[which(Interview_Attendance_Data$Have_you_taken_a_printout_of_your_updated_resume._Have_you_read_the_JD_and_understood_the_same=='Not Yet'),]$Have_you_taken_a_printout_of_your_updated_resume._Have_you_read_the_JD_and_understood_the_same <- 'No'
Interview_Attendance_Data[which(Interview_Attendance_Data$Have_you_taken_a_printout_of_your_updated_resume._Have_you_read_the_JD_and_understood_the_same=='No- will take it soon'),]$Have_you_taken_a_printout_of_your_updated_resume._Have_you_read_the_JD_and_understood_the_same <- 'No'
Interview_Attendance_Data[which(Interview_Attendance_Data$Have_you_taken_a_printout_of_your_updated_resume._Have_you_read_the_JD_and_understood_the_same=='na'),]$Have_you_taken_a_printout_of_your_updated_resume._Have_you_read_the_JD_and_understood_the_same <- 'Na'
Interview_Attendance_Data[which(is.na(Interview_Attendance_Data$Have_you_taken_a_printout_of_your_updated_resume._Have_you_read_the_JD_and_understood_the_same)),]$Have_you_taken_a_printout_of_your_updated_resume._Have_you_read_the_JD_and_understood_the_same <- 'Na'
Interview_Attendance_Data$Have_you_taken_a_printout_of_your_updated_resume._Have_you_read_the_JD_and_understood_the_same <- factor(Interview_Attendance_Data$Have_you_taken_a_printout_of_your_updated_resume._Have_you_read_the_JD_and_understood_the_same)
table(Interview_Attendance_Data$Have_you_taken_a_printout_of_your_updated_resume._Have_you_read_the_JD_and_understood_the_same)

#Taking care of the Are_you_clear_with_the_venue_details_and_the_landmark. column
str(Interview_Attendance_Data$Are_you_clear_with_the_venue_details_and_the_landmark.)
Interview_Attendance_Data$Are_you_clear_with_the_venue_details_and_the_landmark. <- as.factor(Interview_Attendance_Data$Are_you_clear_with_the_venue_details_and_the_landmark.)
table(Interview_Attendance_Data$Are_you_clear_with_the_venue_details_and_the_landmark.)
Interview_Attendance_Data[which(Interview_Attendance_Data$Are_you_clear_with_the_venue_details_and_the_landmark.=='yes'),]$Are_you_clear_with_the_venue_details_and_the_landmark. <- 'Yes'
Interview_Attendance_Data[which(Interview_Attendance_Data$Are_you_clear_with_the_venue_details_and_the_landmark.=='na'),]$Are_you_clear_with_the_venue_details_and_the_landmark. <- 'Na'
Interview_Attendance_Data[which(is.na(Interview_Attendance_Data$Are_you_clear_with_the_venue_details_and_the_landmark.)),]$Are_you_clear_with_the_venue_details_and_the_landmark. <- 'Na'
Interview_Attendance_Data[which(Interview_Attendance_Data$Are_you_clear_with_the_venue_details_and_the_landmark.=='no'),]$Are_you_clear_with_the_venue_details_and_the_landmark. <- 'No'
Interview_Attendance_Data[which(Interview_Attendance_Data$Are_you_clear_with_the_venue_details_and_the_landmark.=='No- I need to check'),]$Are_you_clear_with_the_venue_details_and_the_landmark. <- 'No'
Interview_Attendance_Data$Are_you_clear_with_the_venue_details_and_the_landmark. <- factor(Interview_Attendance_Data$Are_you_clear_with_the_venue_details_and_the_landmark.)
table(Interview_Attendance_Data$Are_you_clear_with_the_venue_details_and_the_landmark.)


#Taking care of the Has_the_call_letter_been_shared column
str(Interview_Attendance_Data$Has_the_call_letter_been_shared)
Interview_Attendance_Data$Has_the_call_letter_been_shared <- as.factor(Interview_Attendance_Data$Has_the_call_letter_been_shared)
table(Interview_Attendance_Data$Has_the_call_letter_been_shared)
Interview_Attendance_Data[which(Interview_Attendance_Data$Has_the_call_letter_been_shared=='yes'),]$Has_the_call_letter_been_shared <- 'Yes'
Interview_Attendance_Data[which(Interview_Attendance_Data$Has_the_call_letter_been_shared=='na'),]$Has_the_call_letter_been_shared <- 'Na'
Interview_Attendance_Data[which(is.na(Interview_Attendance_Data$Has_the_call_letter_been_shared)),]$Has_the_call_letter_been_shared <- 'Na'
Interview_Attendance_Data[which(Interview_Attendance_Data$Has_the_call_letter_been_shared=='no'),]$Has_the_call_letter_been_shared <- 'No'
Interview_Attendance_Data[which(Interview_Attendance_Data$Has_the_call_letter_been_shared=='Havent Checked'),]$Has_the_call_letter_been_shared <- 'No'
Interview_Attendance_Data[which(Interview_Attendance_Data$Has_the_call_letter_been_shared=='Need To Check'),]$Has_the_call_letter_been_shared <- 'No'
Interview_Attendance_Data[which(Interview_Attendance_Data$Has_the_call_letter_been_shared=='Not sure'),]$Has_the_call_letter_been_shared <- 'No'
Interview_Attendance_Data[which(Interview_Attendance_Data$Has_the_call_letter_been_shared=='Not Sure'),]$Has_the_call_letter_been_shared <- 'No'
Interview_Attendance_Data[which(Interview_Attendance_Data$Has_the_call_letter_been_shared=='Not yet'),]$Has_the_call_letter_been_shared <- 'No'
Interview_Attendance_Data[which(Interview_Attendance_Data$Has_the_call_letter_been_shared=='Yet to Check'),]$Has_the_call_letter_been_shared <- 'No'
Interview_Attendance_Data$Has_the_call_letter_been_shared <- factor(Interview_Attendance_Data$Has_the_call_letter_been_shared)
table(Interview_Attendance_Data$Has_the_call_letter_been_shared)

#Taking care of the Marital_Status column
str(Interview_Attendance_Data$Marital_Status)
Interview_Attendance_Data$Marital_Status <- as.factor(Interview_Attendance_Data$Marital_Status)
table(Interview_Attendance_Data$Marital_Status)

#Taking care of the Marital_Status column
str(Interview_Attendance_Data$Observed_Attendance)
Interview_Attendance_Data$Observed_Attendance <- as.factor(Interview_Attendance_Data$Observed_Attendance)
table(Interview_Attendance_Data$Observed_Attendance)
Interview_Attendance_Data[which(Interview_Attendance_Data$Observed_Attendance=='NO'),]$Observed_Attendance <- 'No'
Interview_Attendance_Data[which(Interview_Attendance_Data$Observed_Attendance=='no'),]$Observed_Attendance <- 'No'
Interview_Attendance_Data[which(Interview_Attendance_Data$Observed_Attendance=='yes'),]$Observed_Attendance <- 'Yes'
Interview_Attendance_Data$Observed_Attendance <- factor(Interview_Attendance_Data$Observed_Attendance)
table(Interview_Attendance_Data$Observed_Attendance)

#Checking for null values
apply(Interview_Attendance_Data,2,function(x) round(sum(is.na(x)),2))

#Deleting rows with all NA columns
Interview_Attendance_Data$X23 <- NULL
Interview_Attendance_Data$X24 <- NULL
Interview_Attendance_Data$X25 <- NULL
Interview_Attendance_Data$X26 <- NULL
Interview_Attendance_Data$X27 <- NULL

#Deleting last row wihch has NA
Interview_Attendance_Data <- Interview_Attendance_Data[-1234,]


#Taking care of the Name/ID column
Candidate_ID <- Interview_Attendance_Data[is.na(Interview_Attendance_Data$Observed_Attendance),6]
Interview_Attendance_Data$`Name(Cand_ID)` <- NULL

#Some Additional Data Engineering on Date column
#Converting the date to seasons might give us more meaningful insights
#Creating a Seasons Columns
getSeason <- function(DATES) {
  d<-day(DATES)
  m<-month(DATES)
  ifelse (((m==12 && d>=15  ) || m==1 || m==2 || (d<15 && m==3)), "Winter",
          ifelse ((( m==3 && d>=15 ) ||m==4|| m==5|| (d<15 && m==6)), "Spring",
                  ifelse (((m==6 && d>=15 ) || m==7 ||m==8 || (m==9 && d<15  )), "Summer", "Fall")))
}
for(i in 1:nrow(Interview_Attendance_Data)){
  Interview_Attendance_Data$season[i] <- getSeason(Interview_Attendance_Data$Date_of_Interview[i])
}
Interview_Attendance_Data$season <- as.factor(Interview_Attendance_Data$season)
table(Interview_Attendance_Data$season)
#Deleting the date column
Interview_Attendance_Data$Date_of_Interview <- NULL
#Going through all the variables again
summary(Interview_Attendance_Data)
str(Interview_Attendance_Data)

#Creating training and testing datasets as per the question
Train_data <- Interview_Attendance_Data[-which(is.na(Interview_Attendance_Data$Observed_Attendance)),]
Test_data <- Interview_Attendance_Data[which(is.na(Interview_Attendance_Data$Observed_Attendance)),]
Test_data_old <- Test_data

#Lets rename the columns which might cause trouble 
names(Train_data) <- gsub("\\/", "", names(Train_data))
names(Test_data) <- gsub("\\/", "", names(Test_data))
names(Test_data_old) <- gsub("\\/", "", names(Test_data_old))


###Lets start building the models
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     classProbs = TRUE)


RF_Model <- train(Observed_Attendance~.,Train_data,method="rf",metric="ROC",trControl=ctrl)

glm_Model <- train(Observed_Attendance~.,Train_data,method="glm",metric="ROC",trControl=ctrl)

svmLinear_Model <- train(Observed_Attendance~.,Train_data,method="svmLinear",metric="ROC",trControl=ctrl)

nnet_Model <- train(Observed_Attendance~.,Train_data,method="nnet",metric="ROC",trControl=ctrl)

xgbDART_Model <- train(Observed_Attendance~.,Train_data,method="xgbDART",metric="ROC",trControl=ctrl)

svmradial_Model <- train(Observed_Attendance~.,Train_data,method="svmRadial",metric="ROC",trControl=ctrl)

#Validating models
RF_Model
glm_Model
svmLinear_Model
nnet_Model
xgbDART_Model
svmradial_Model

#So far xgb has the highest training accuracy among the models created
#Lets try if ANN makes any difference
#Recipe to make dummy variables and scale them.
rec_obj <- recipe(Observed_Attendance ~ ., data = Train_data) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep(data = Train_data)

x_train_tbl <- bake(rec_obj, newdata = Train_data) %>% select(-Observed_Attendance)
x_test_tbl  <- bake(rec_obj, newdata = Test_data) %>% select(-Observed_Attendance)

y_train_vec <- ifelse(pull(Train_data, Observed_Attendance) == "Yes", 1, 0)
y_test_vec  <- ifelse(pull(Test_data, Observed_Attendance) == "Yes", 1, 0)

#Imlementing Keras Tensorflow to include ANN
#Building ANN
model_keras <- keras_model_sequential()
set.seed(123)
model <- model_keras %>%
  layer_dense(units = 256, kernel_initializer = "uniform", activation = "relu",
              input_shape = ncol(x_train_tbl)) %>%
  layer_dropout(rate = 0.10) %>%
  layer_dense(units = 128, kernel_initializer = "uniform", activation = "relu") %>%
  layer_dropout(rate = 0.10) %>%
  layer_dense(units = 128, kernel_initializer = "uniform", activation = "relu") %>%
  layer_dropout(rate = 0.10) %>%
  layer_dense(units = 64, kernel_initializer = "uniform", activation = "relu") %>%
  layer_dropout(rate = 0.10) %>%
  layer_dense(units = 1, kernel_initializer = "uniform", activation = "sigmoid")
# Compile 
compile2 <- model_keras %>%
  compile(optimizer = "adam", loss = "binary_crossentropy", metrics = c("accuracy"))

compile2

kemodel <- fit(object = model,
               x = as.matrix(x_train_tbl),
               y = y_train_vec,
               batch_size = 128,
               epochs = 50,
               validation_split = 0.30,
               shuffle=TRUE)

#Inspite of many attempts tuning the keras ANN, the performance still seems to be poor than the models we had trained earlier.
#xgboost still gives us the best training accuracy.
#Lets try the using the models with one-hot encoded and scaled data
#Baking the dataset again
Train_data_new <- bake(rec_obj, newdata = Train_data)
Test_data_new  <- bake(rec_obj, newdata = Test_data_old)

#BUilding the models
RF_Model_new <- train(Observed_Attendance~.,Train_data_new,method="rf",metric="accuracy",trControl=ctrl)

glm_Model_new <- train(Observed_Attendance~.,Train_data_new,method="glm",metric="accuracy",trControl=ctrl)

svmLinear_Model_new <- train(Observed_Attendance~.,Train_data_new,method="svmLinear",metric="accuracy",trControl=ctrl)

nnet_Model_new <- train(Observed_Attendance~.,Train_data_new,method="nnet",metric="accuracy",trControl=ctrl)

xgbDART_Model_new <- train(Observed_Attendance~.,Train_data_new,method="xgbDART",metric="accuracy",trControl=ctrl)

svmradial_Model_new <- train(Observed_Attendance~.,Train_data_new,method="svmRadial",metric="accuracy",trControl=ctrl)

#Validating models
summary(RF_Model_new)
glm_Model_new
svmLinear_Model_new
nnet_Model_new
xgbDART_Model_new
svmradial_Model_new

#Lets try balancing the data and then applying models
#SMOTE used for balancing the data while applying the models in the caret library
#Lets modify the the code a bit
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     classProbs = TRUE)
ctrl$sampling <- 'smote'


RF_Model_smote <- train(Observed_Attendance~.,Train_data,method="rf",metric="accuracy",trControl=ctrl)

glm_Model_smote <- train(Observed_Attendance~.,Train_data,method="glm",metric="accuracy",trControl=ctrl)

svmLinear_Model_smote <- train(Observed_Attendance~.,Train_data,method="svmLinear",metric="accuracy",trControl=ctrl)

nnet_Model_smote <- train(Observed_Attendance~.,Train_data,method="nnet",metric="accuracy",trControl=ctrl)

xgbDART_Model_smote <- train(Observed_Attendance~.,Train_data,method="xgbDART",metric="Accuracy",trControl=ctrl)

svmradial_Model_smote <- train(Observed_Attendance~.,Train_data,method="svmRadial",metric="Accuracy",trControl=ctrl)

#Validating models
max(RF_Model_smote$results$Accuracy)
max(glm_Model_smote$results$Accuracy)
max(svmLinear_Model_smote$results$Accuracy)
max(nnet_Model_smote$results$Accuracy)
max(xgbDART_Model_smote$results$Accuracy)
max(svmradial_Model_smote$results$Accuracy)

#As we observe, lets compare the accuarcy of all 3 xgb models
max(xgbDART_Model$results$Accuracy)
max(xgbDART_Model_new$results$Accuracy)
max(xgbDART_Model_smote$results$Accuracy)

#Lets predict the target variable using all the 3 models
xgb_pred <- predict(xgbDART_Model,newdata = Test_data_old,type = "raw")
xgb_prob <- predict(xgbDART_Model,newdata = Test_data_old,type = "prob")

xgb_pred_new <- predict(xgbDART_Model_new,newdata = Test_data_new,type = "raw")
xgb_prob_new <- predict(xgbDART_Model_new,newdata = Test_data_new,type = "prob")

xgb_pred_smote <- predict(xgbDART_Model_smote,newdata = Test_data_old,type = "raw")
xgb_prob_smote <- predict(xgbDART_Model_smote,newdata = Test_data_old,type = "prob")

#FOrmating a table with the concerned columns
Pred_prob <- cbind(Candidate_ID$`Name(Cand_ID)`,xgb_pred,xgb_prob)
Pred_prob_new <- cbind(Candidate_ID$`Name(Cand_ID)`,xgb_pred_new,xgb_prob_new)
Pred_prob_smote <- cbind(Candidate_ID$`Name(Cand_ID)`,xgb_pred_smote,xgb_prob_smote)
colnames(Pred_prob) <- c("Candidate ID","Prediction","Probablity_Yes","Probablity_No")
colnames(Pred_prob_smote) <- c("Candidate ID","Prediction","Probablity_Yes","Probablity_No")
colnames(Pred_prob_new) <- c("Candidate ID","Prediction","Probablity_Yes","Probablity_No")

#Plotting the results of the prediction
par(mfrow=c(1,3))
barplot(table(Pred_prob$Prediction),main="Data With NO Tranformations")
barplot(table(Pred_prob_new$Prediction),main="Scaled and Normalised Data")
barplot(table(Pred_prob_smote$Prediction),main="SMOTEd Data")

#Lets observe how the each model performs on the SEEN data.
Pred_train <- predict(xgbDART_Model,newdata = Train_data,type = "raw")
Pred_train_new <- predict(xgbDART_Model_new,newdata = Train_data_new,type = "raw")
Pred_train_smote <- predict(xgbDART_Model_smote,newdata = Train_data,type = "raw")

#Analysing the confusion matrix of each model
confusionMatrix(Pred_train,Train_data$Observed_Attendance)
confusionMatrix(Pred_train_new,Train_data_new$Observed_Attendance)
confusionMatrix(Pred_train_smote,Train_data$Observed_Attendance)

#Analysing the aggregated confusion matrix
confusionMatrix(xgbDART_Model,norm = "none")
confusionMatrix(xgbDART_Model_new,norm = "none")
confusionMatrix(xgbDART_Model_smote,norm = "none")

#Here we observe that the model with SMOTEd data is giving us very good sepcificity but poor precision
#And normalised and no-transformed data  models are almost same with minute differences
#We have a tie over here
#However, we see that the model with the normalised data is better at predicting the "No" - which we are concerned about
#Hence, we choose the model which is trained on the normalised dataset
write.csv(Pred_prob_new,"Results_new.csv")