
#load message Data

raw_message_data <- read.csv(file="all_messages.csv", header = TRUE)
#separate notification column to type language and msg_nmbr
raw_message_data <- raw_message_data %>% separate(notification_label, c('type','language','msg_nmbr'))
#change the message date to simple date
raw_message_data <- raw_message_data %>% 
  rename(User_ID = user_uuid, Msg_Date = date_sent, Msg_Lang = language) %>% 
  mutate(Msg_Nmbr = as.integer(msg_nmbr),
         Msg_Date = as.POSIXct(Msg_Date, format = "%Y-%m-%d %H:%M:%S"),
         Year = format(Msg_Date, "%Y")) %>%
  group_by(User_ID) %>%
  mutate(Repeat_User = n_distinct(Year) > 1)%>%
  mutate(type = tools::toTitleCase(type)) %>% 
  mutate(type = recode(type, "Prevention" = "Vigilant", "Promotion" = "Eager"))%>% 
  ungroup() 

#first the data for messages were loaded into a data set called message_data_raw, the notification label was split in order to have 
#separate columns for type, language and message number, column names changed to fit the style chosen for the survey data, and then 
#The types of message number column and message date column fixed to integer and date format. To see if user has participated in
#survey/experiment in both years Repeat user column is created by checking distinct years of messages sent


#To clean the data for use and joining with survey data, messages are filtered to those sent in 2023, and grouped by user ID.
#extra columns are created to check period of messaging, number of total messages sent and read, and finally the number of the repeated message.

#In the case of users that received a repeated message, all those that got their first message before the 5th of July 2023, had a repeat of the 
#message they received on 30th of June on the 2nd of July, a date on which an invite should have been sent but instead a nudging message was sent.
#However this shouldn't impact the results, as due to random distribution each type of messaging has 38 participants receiving a repeated message.



message_data <- raw_message_data %>%
  filter(year(Msg_Date) == 2023) %>% 
  group_by(User_ID) %>%
  summarize(
    Msg_Type = first(type),
    Msg_Lang = first(Msg_Lang),
    First_Msg_Date = as.POSIXct(format(min(Msg_Date), "%Y-%m-%d")),
    Last_Msg_Date = as.POSIXct(format(max(Msg_Date), "%Y-%m-%d")),
    Nmbr_Msgs_Sent = n(),
    Nmbr_Msgs_Seen = sum(read_notification == "t"),
    Msg_Duration_Days = as.integer(Last_Msg_Date - First_Msg_Date, units = "days")
  ) %>%
  ungroup() %>%
  mutate(Msg_Type = as.factor(Msg_Type))




#creating Messaging group based on date of first message received.
message_data <- message_data %>%
  mutate(
    Message_Group = case_when(
      month(First_Msg_Date) == 6 & day(First_Msg_Date) <= 14 ~ "A-June1",
      month(First_Msg_Date) == 6 & day(First_Msg_Date) > 14 ~ "B-June2",
      month(First_Msg_Date) == 7 & day(First_Msg_Date) <= 14 ~ "C-July1",
      month(First_Msg_Date) == 7 & day(First_Msg_Date) > 14 ~ "D-July2",
      month(First_Msg_Date) == 8 & day(First_Msg_Date) <= 14 ~ "E-Aug1",
      month(First_Msg_Date) == 8 & day(First_Msg_Date) > 14 ~ "F-Aug2",
      month(First_Msg_Date) == 9 & day(First_Msg_Date) <= 14 ~ "G-Sept1",
      month(First_Msg_Date) == 9 & day(First_Msg_Date) > 14 ~ "H-Sept2",
      month(First_Msg_Date) == 10 & day(First_Msg_Date) <= 14 ~ "I-Oct1",
      month(First_Msg_Date) == 10 & day(First_Msg_Date) > 14 ~ "J-Oct2",
      month(First_Msg_Date) == 11 & day(First_Msg_Date) <= 14 ~ "K-Nov1",
      month(First_Msg_Date) == 10 & day(First_Msg_Date) > 14 ~ "L-Nov2",
      TRUE ~ NA_character_
    )
  ) %>% 
  mutate(Message_Group = as.factor(Message_Group)) 



#The final count of users are 238 users that received messages, with 79 participants in each group and 80 for prevention orientation.
#The number corresponds with the Unique users that have initiated the survey and gave their consent






write.csv(message_data, "G:/My Drive/Article about Messages/Messages-Nudging-2023/CleanMessageData.csv", row.names = FALSE)



