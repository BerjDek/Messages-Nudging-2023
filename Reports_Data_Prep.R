raw_reports_data <- read.csv(file="full_reports_table.csv", header = TRUE) %>% 
  mutate(creation_time= as.POSIXct(creation_time, format = "%Y-%m-%d"))


#To create a reports data that can be assessed and merged with a main data set 
#The raw data is cleaned to limit it to the final Update on 2nd of October 2020, after which all User Id's have been reset


reports_data <- raw_reports_data %>%
  dplyr::select(user_id, location_choice, creation_time, type) %>% 
  rename(User_ID = user_id,
         Rprt_Date = creation_time,
         Rprt_Type = type) %>% 
  filter(User_ID != "" & Rprt_Date >= as.POSIXct("2020-10-02") & Rprt_Date <= as.POSIXct("2023-12-31"))%>% 
  mutate(Rprt_Type = as.factor(Rprt_Type))


n_distinct(reports_data$User_ID) # 54853 unique users have filled reports
min(reports_data$Rprt_Date, na.rm = TRUE)


summary(reports_data) #Since 12/10/2020 till the date of the analysis 174,667 reports have been filled.




### for new experiment that was aiming to see if people who were part of the study filled reports in this year as well.
#Skip for now
reports_data_2 <- raw_reports_data %>%
  dplyr::select(user_id, location_choice, creation_time, type) %>% 
  rename(User_ID = user_id,
         Rprt_Date = creation_time,
         Rprt_Type = type) %>% 
  filter(User_ID != "" & Rprt_Date >= as.POSIXct("2024-01-01") & Rprt_Date <= as.POSIXct("2024-12-31"))%>% 
  mutate(Rprt_Type = as.factor(Rprt_Type))

data <- reports_data_2 %>% 
  filter(User_ID %in% message_data$User_ID)


data <- data %>% 
  group_by(User_ID) %>%
  arrange(User_ID, Rprt_Date) %>%
  slice(1L) %>%
  select(User_ID, Rprt_Date, Rprt_Type) %>% 
  ungroup() 






rm(raw_reports_data)
