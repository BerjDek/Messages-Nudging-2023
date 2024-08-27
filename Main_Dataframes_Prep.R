#Unifying Data Sets


#Joining survey data and message data based on user id
Unified <- full_join(survey_data, message_data, by = "User_ID")

#Joining data by reports on user id
Unified <- full_join(Unified, reports_data, by = "User_ID")



#will take a while
data_tall <- Unified %>%
  group_by(User_ID) %>%
  mutate(Got_Msgs = !is.na(First_Msg_Date), # might not be needed
         Total_Rprts_Filled = n(),
         Season_Rprts_Filled_2023 = sum(Rprt_Date >= "2023-05-01" & Rprt_Date <= "2023-10-30"),
         Season_Rprts_Filled_2022 = sum(Rprt_Date >= "2022-05-01" & Rprt_Date <= "2022-10-30"),
         Season_Rprts_Filled_2021 = sum(Rprt_Date >= "2021-05-01" & Rprt_Date <= "2021-10-30"),
         Total_Bite_Rprts_Filled = sum(Rprt_Type == "bite"),
         Total_Adult_Rprts_Filled = sum(Rprt_Type == "adult"),
         Total_Site_Rprts_Filled = sum(Rprt_Type == "site"),
         Rprts_During_Msging = sum(Rprt_Date >= First_Msg_Date & Rprt_Date <= Last_Msg_Date, na.rm = TRUE),
         Rprts_Before_Msging = sum(Rprt_Date >= (First_Msg_Date - days(Msg_Duration_Days)) & Rprt_Date < First_Msg_Date, na.rm = TRUE),
         Rprts_After_Msging = sum(Rprt_Date > Last_Msg_Date & Rprt_Date <= (Last_Msg_Date + days(Msg_Duration_Days)), na.rm = TRUE)) %>% 
  ungroup()



data_tall <- data_tall %>% 
  group_by(User_ID) %>%
  arrange(User_ID, Rprt_Date) %>%
  slice(1L) %>%
  select(User_ID, Participation_Date, Got_Msgs, Message_Group, Age, Gender, Country,  Nmbr_Msgs_Seen, Msg_Type, Reg_Orientation_Cat, 
         Total_Rprts_Filled,Season_Rprts_Filled_2021, Season_Rprts_Filled_2022, Season_Rprts_Filled_2023, Rprts_During_Msging, Rprts_Before_Msging,
         Rprts_After_Msging, Reg_Orientation, Promotion,Prevention,  Total_Bite_Rprts_Filled, Total_Adult_Rprts_Filled, Total_Site_Rprts_Filled) %>% 
  ungroup() 

#54,853 corresponds with the number of users that have filled at least single report with the set dates from the reports page


#adding registered participation date to the data frame
data_tall <- data_tall %>%
  left_join(user_data %>% select(User_ID, Registered_Participation_Date), by = "User_ID")



#data is just for the ones who completed the study wheras data tall has reports information for everyone that has filled a single report from update till 
#end of 2023

data <- data_tall %>% 
  filter(User_ID %in% message_data$User_ID)


summary(data)


write.csv(data, "G:/My Drive/Article about Messages/Messages-Nudging-2023/MainData.csv", row.names = FALSE)
