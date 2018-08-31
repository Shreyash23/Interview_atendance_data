library(readr)
library(lubridate)
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
table(Interview_Attendance_Data$Client_name)

#Taking care of the Industry column
str(Interview_Attendance_Data$Industry)
Interview_Attendance_Data$Industry <- as.factor(Interview_Attendance_Data$Industry)
str(Interview_Attendance_Data$Industry)
table(Interview_Attendance_Data$Industry)

#Taking care of the Industry column
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





























