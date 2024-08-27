rep_msg_2023 <- reports_data %>% 
  filter(User_ID %in% data$User_ID,
         between(as.Date(Rprt_Date), as.Date("2023-05-01"), as.Date("2023-10-31"))) %>%
  mutate(Rprt_Type = as.character(Rprt_Type),
         Date = as.Date(Rprt_Date)) %>%
  select(-Rprt_Date)

msg_rep_2023 <- raw_message_data %>%
  filter(between(as.Date(Msg_Date), as.Date("2023-05-01"), as.Date("2023-10-31"))) %>%
  select(User_ID, Msg_Date = as.Date(Msg_Date), type, read_notification) %>%
  mutate(Msg_Seen = as.integer(read_notification == "t"),
         Msg_Type = replace_na(as.character(type), "None"),
         Msg_Nmbr = replace_na(Msg_Nmbr, 0)) %>%
  select(-type, -read_notification)

# Sequence generation and expansion using tidyr::expand
expanded_reports <- tidyr::expand(rep_msg_2023, User_ID, Date = seq.Date(as.Date("2023-05-01"), as.Date("2023-10-31"), by = "day"))
expanded_messages <- tidyr::expand(msg_rep_2023, User_ID, Date)

# Joining and mutating datasets
report_msg_long <- expanded_reports %>%
  left_join(rep_msg_2023, by = c("User_ID", "Date")) %>%
  left_join(expanded_messages, by = c("User_ID", "Date")) %>%
  mutate(Rprt_Type = replace_na(Rprt_Type, "None"),
         Msg_Seen = replace_na(Msg_Seen, 0),
         Msg_Type = replace_na(Msg_Type, "None"),
         Msg_Nmbr = replace_na(Msg_Nmbr, 0),
         Rprt_Filled = as.integer(Rprt_Type != "None"),
         Msg_Received = as.integer(Msg_Type != "None"))

# Further processing
report_msg_wide <- report_msg_long %>%
  group_by(User_ID, Date, Rprt_Type) %>%
  summarize(report_indicator = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Rprt_Type, values_from = report_indicator, values_fill = 0) %>%
  mutate(total_reports = adult + bite + site, Report = as.integer(total_reports > 0)) %>%
  select(-None) %>%
  left_join(msg_subset, by = c("User_ID", "Date")) %>%
  left_join(survey_data %>% select(User_ID, Gender, Reg_Orientation_Cat, Reg_Orientation, Promotion, Prevention), by = "User_ID") %>%
  filter(!is.na(Msg_Type))