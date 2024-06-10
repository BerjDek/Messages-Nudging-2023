
raw_user_data <- read.csv(file="reports_by_uuid.csv", header = TRUE)

#since the beginning of the project, there have been 398,923 registration/downloads (Till Date report recieved from Agusti)
raw_user_data <- raw_user_data %>% 
  rename( User_ID = user_UUID,
          Registered_Participation_Date = registration_time,
          Registered_Total_Reports = n) %>% 
  mutate(Registered_Participation_Date = as.POSIXct(Registered_Participation_Date, format = "%Y-%m-%d %H:%M:%S"),
         Registered_Total_Reports = as.integer(Registered_Total_Reports)) %>%
  replace_na(list(Registered_Total_Reports = 0)) 




# Count of unique users that have submitted at least 1 report
nrow(raw_user_data %>%
       filter(!is.na(Registered_Total_Reports) & Registered_Total_Reports >= 1))


# 71,495 of the registered users have  filled a report between update and end of 2023

nrow(raw_user_data %>%
       filter(!is.na(Registered_Total_Reports) & Registered_Total_Reports >= 1 & 
                Registered_Participation_Date >= as.POSIXct("2020-10-02") & Registered_Participation_Date <= as.POSIXct("2023-12-31")))

# 54,881 registered users since the cutoff date of 2020-10-02 till the end of 2023. 
# There is a small difference in the numbers between the unique users here and ones from the reports data set
#  54,881 here vs 54,853 in reports, even though the data was extracted/requested on the same day probably in the minutes between reports some reporsts were filled.

#the number from reports csv is going to be used



#checking the number of users registered after the update.
nrow(raw_user_data %>%
       filter(Registered_Participation_Date >= as.POSIXct("2020-10-02")))
#   315,015 users in total, this number is going to be used as the pool to attach to the main data set. 


## NOTE there is a cutoff at end of 2024, if needs to be removed
user_data <- raw_user_data %>%
  filter(Registered_Participation_Date >= as.POSIXct("2020-10-02") & Registered_Participation_Date <= as.POSIXct("2023-12-31"))




