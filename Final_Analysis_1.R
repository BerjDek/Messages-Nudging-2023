### Paired t-test comparison for seasonal reporting behavior change 

# comparing reporting behavior of participants who were part of the nudging intervention in two consequtive years 2021/2022  2022/2023; and also
# reporting behavior of participants that have also filled at least a single report but were not part of the intervention between 2022/2023


# Intervention 2022/2023
paired_t_test_result <- data_reduced %>%
  filter(Registered_Participation_Date < as.Date('2023-01-01')) %>%
  with(t.test(Season_Rprts_Filled_2023, Season_Rprts_Filled_2022, paired = TRUE))
print(paired_t_test_result)

# mean difference 8.384058;  t = 2.9139, df = 137, p-value = 0.004171



# Intervention 2021/2022
paired_t_test_result <- data_reduced %>%
  filter(Registered_Participation_Date < as.Date('2022-01-01')) %>%
  with(t.test(Season_Rprts_Filled_2022, Season_Rprts_Filled_2021, paired = TRUE))
print(paired_t_test_result)

# mean difference -3.202899; t = -1.0122, df = 68, p-value = 0.315


#No Intervention 2022/2023
paired_t_test_result <- data_reduced_tall %>%
  filter(Registered_Participation_Date < as.Date('2023-01-01')) %>%
  filter(!User_ID %in% data_reduced$User_ID) %>% 
  with(t.test(Season_Rprts_Filled_2023, Season_Rprts_Filled_2022, paired = TRUE))
print(paired_t_test_result)

# mean difference -0.7433673; t = -27.552, df = 29964, p-value < 2.2e-16

#Result : Significant increase in reporting for those who recieved nudges in 2023 season and not in 2022; and non-significant decrease between 2021 and 2022
# Significant decrease for the participants who didnt recive the nudging treatment both in 2023 and 2022



# Regression Analysis


#Does Nudging (general) increase the chance of reporting?
model <- glmer(Report ~ Msg_Received + (1 + Date | User_ID),
               data = report_msg_wide_reduced,
               family = binomial)

summary(model)

# Receiving a nudge significantly increases the likelihood of reporting, with an estimated log-odds increase of 0.23 (SE = 0.081, p = 0.0046).



# Does nudging increase the total number (intensity) of reporting
model <- glmer(total_reports ~ Msg_Received + (1 + Date|User_ID), 
               data = report_msg_wide_reduced, 
               family = poisson) 
summary(model)

# Nudging (via message received) significantly increases the total number of reports, with an estimated log-mean increase of 0.24 (SE = 0.052, p < 0.0001).




#Does the framing of  the nudge matter 
levels(report_msg_wide_reduced$Msg_Type)
report_msg_wide_reduced$Msg_Type <- relevel(report_msg_wide_reduced$Msg_Type, ref = "None") #set the level to none


model <- glmer(Report ~ Msg_Type + (1 + Date | User_ID),
               data = report_msg_wide_reduced,
               family = binomial(link = "logit"))

summary(model)

# Compared to receiving no nudge, the "Vigilant" framing  significantly increases the likelihood of reporting with an estimated log-odds of 
#0.28 (SE = 0.125, p = 0.0246), while the "Eager" and "Neutral" framing positively impact but with  statistically in significant effects (p 0.1093/0.3250)



#Does agreement between framing and regulatory orientation impact reporting

#Vigilant/Prevention
model <- glmer(Report ~ Orientation_Nudge_Agreement + (1 + Date|User_ID), 
               data = report_msg_wide_reduced[report_msg_wide_reduced$Reg_Orientation_Cat == "Prevention", ], 
               family = binomial(link = "logit"))
summary(model) 
# estimated log-odds increase of 0.32 (SE = 0.159, p = 0.044)


#Eager/Promotion
model <- glmer(Report ~ Orientation_Nudge_Agreement + (1 + Date|User_ID), 
               data = report_msg_wide_reduced[report_msg_wide_reduced$Reg_Orientation_Cat == "Promotion", ], 
               family = binomial(link = "logit"))
summary(model)
# estimated log-odds of -0.53 (SE = 0.476, p = 0.269)

#Neutral/Neutral (not sure if important)
model <- glmer(Report ~ Orientation_Nudge_Agreement + (1 + Date|User_ID), 
               data = report_msg_wide_reduced[report_msg_wide_reduced$Reg_Orientation_Cat == "Neutral", ], 
               family = binomial(link = "logit"))
summary(model)
#estimated log-odds of -0.12 (SE = 0.294, p = 0.692)

# For individuals with prevention orientation alignment between regulatory orientation and nudge framing significantly effects their likelihood
# of reporting, while the alignment doesn't seem as significant for individuals with  promotion and  neutral regulatory orientation



report_msg_wide_reduced$Day_of_Year <- yday(report_msg_wide_reduced$Date)

model <- glmer(Report ~ Msg_Received + (1 + poly(Day_of_Year, 2) | User_ID),
               data = report_msg_wide_reduced,
               family = binomial)

summary(model)

