
# Filters the reports_data dataset to include only rows of reports filled by users who were part of the nudging intervention and the reports
# were filled in 2023 and later more specifically in the mosquito season, Converts Rprt_Type to a character type, renames Rprt_Date to Date, 

rep_msg_2023 <- reports_data %>% 
  filter(User_ID %in% data$User_ID) %>% 
  filter(year(as.Date(Rprt_Date)) == 2023) %>%
  mutate(Rprt_Type = as.character(Rprt_Type)) %>% 
  rename(Date = Rprt_Date) %>% 
  filter(Date >= "2023-05-01"& Date <= "2023-10-31")


# sets the same date limits to messages sent and turns the read notification to an integer
msg_rep_2023<- raw_message_data %>%
  filter(year(Msg_Date) == 2023) %>% 
  dplyr::select(-Msg_Lang,-msg_nmbr,-Repeat_User,-id) %>% 
  mutate(read_notification= as.integer(read_notification == "t"), Msg_Date = as.Date(Msg_Date)) %>% 
  dplyr::select(User_ID, Msg_Date, type, read_notification, Msg_Nmbr) %>% 
  rename(Msg_Type = type, Msg_Seen = read_notification, Date = Msg_Date) %>% 
  filter(Date >= "2023-05-01"& Date <= "2023-10-31")

# Creates a sequence of daily dates from May 1, 2023, to October 31, 2023 (for every day of the season)
dates_2023  <- seq.Date(as.Date("2023-05-01"), as.Date("2023-10-31"), by = "day")

#expands the dates on both the reports and messages dataframes so they include every day of the season as a date for each user
#so briefly, each user has a list of all possible dates in the season, where they could have recieved/filled a nudge/report or not
expanded_reports <- expand.grid(User_ID = unique(rep_msg_2023$User_ID), Date = dates_2023)
expanded_messages <- expand.grid(User_ID = unique(msg_rep_2023$User_ID), Date = dates_2023)


#merges the expanded reports with the originals, and fills days where "nothing happened" with NA
full_reports <- merge(expanded_reports, rep_msg_2023, by = c("User_ID", "Date"), all = TRUE)
full_messages <- merge(expanded_messages, msg_rep_2023, by = c("User_ID", "Date"), all = TRUE)


#corrects NA's and designates proper values
full_reports$Rprt_Type[is.na(full_reports$Rprt_Type)] <- "None"
full_messages$Msg_Seen[is.na(full_messages$Msg_Seen)] <- 0
full_messages$Msg_Type[is.na(full_messages$Msg_Type)] <- "None"
full_messages$Msg_Nmbr[is.na(full_messages$Msg_Nmbr)] <- 0



#combines both created data sets so that one data set would have all dates that show both the reception/filling of nudges/reports each day of the season
#for each user
report_msg_long <- left_join(full_reports, full_messages, by = c("User_ID", "Date"))


#adds the columns to indicate if a message or a report were filled with integers 1 for yes, 0 for no
report_msg_long <- report_msg_long %>%
  mutate(Rprt_Filled = ifelse(Rprt_Type != "None", 1, 0), Msg_Received = ifelse(Msg_Type != "None", 1, 0))


#Converts the Rprt_Type and Msg_Type columns in the report_msg_long dataset to factors (categorical data)
report_msg_long$Rprt_Type <- as.factor(report_msg_long$Rprt_Type)
report_msg_long$Msg_Type <- as.factor(report_msg_long$Msg_Type)





#If I want to save the data set
#write.csv(report_msg_long, "reportmsglong.csv", row.names = FALSE)

#delete redundant items to free some memory
rm(full_messages, full_reports, expanded_messages,expanded_reports, dates_2023, msg_rep_2023, rep_msg_2023)





#creating a denser version of the created data set, which first creates a row to indicate a report, and the summing them up per user per date.
# the data frame shows if on a specific date a specific user filled a report, and then how many of each type and total
report_msg_wide <- report_msg_long %>%
  mutate(report_indicator = as.integer(1)) %>%
  group_by(User_ID, Date, Rprt_Type) %>%
  summarize(report_indicator = sum(report_indicator, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(
    names_from = Rprt_Type, 
    values_from = report_indicator, 
    values_fill = list(report_indicator = 0)  
  ) %>% 
  mutate(
    total_reports = adult + bite + site,
    Report = as.numeric(!None)) %>% 
  dplyr::select(-None)


#creates another data set  from the one created which is a subset that lists for each user all the dates of the season along with information if thez 
#have recieved a message or not
msg_subset <- report_msg_long %>%
  group_by(User_ID, Date) %>%
  slice(1) %>%
  dplyr::select(User_ID, Date, Msg_Received, Msg_Type, Msg_Seen) %>%
  ungroup()


# Joined the two newly created datasets
report_msg_wide <- report_msg_wide %>%
  left_join(msg_subset, by = c("User_ID", "Date"))


#adding additional columns from the survey data to show user gender  and regulatory focus
report_msg_wide <- report_msg_wide %>% 
  left_join(survey_data %>% dplyr::select(User_ID, Gender, Reg_Orientation_Cat, Reg_Orientation, Promotion, Prevention), by = "User_ID") %>% 
  filter(!is.na(Msg_Type))


#adding a column for nudge and orientation agreement
report_msg_wide <- report_msg_wide %>%
  mutate(Orientation_Nudge_Agreement = case_when(
    Msg_Type == "Neutral" & Reg_Orientation_Cat == "Neutral" ~ 1,
    Msg_Type == "Eager" & Reg_Orientation_Cat == "Promotion" ~ 1,
    Msg_Type == "Vigilant" & Reg_Orientation_Cat == "Prevention" ~ 1,
    TRUE ~ 0
  ))

write.csv(report_msg_wide, "RegressionData.csv", row.names = FALSE)




#  Generalized Linear Mixed Model 

install.packages("lme4")
library(lme4)
library(Matrix)

#relationship between recieving messages and filling reports


#the model below includes a random intercept for users, as there are individual differences in baseline likleyhood of reporting, and also assumes that 
#each user has a different trend of reporting over time. Its giving a sigular fit warning saying that the random effects structure os overly complex
model <- glmer(Report ~ Msg_Received + (1 + Date|User_ID),
      data = report_msg_wide,
      family = binomial)
summary(model)



# this model uses random intecept for date as well to capture any day specific factors (mosquito density/environmental factors)
model <-  glmer(Report ~ Msg_Received + (1 | User_ID) + (1 | Date),
data = report_msg_wide,
family = binomial)
summary(model)
 
#no significant effect

#
model <- glmer(Report ~ Msg_Received + Date + (1 | User_ID), 
               data = report_msg_wide,
               family = binomial)
summary(model)

#gives a significant results for msg recieved but error since model is too complex and perfect negative correlation for intercept and slope of date



model <- glmer(Report ~ Msg_Received + Date + (1 + Date | User_ID), 
               data = report_msg_wide,
               family = binomial)
summary(model)



#expermienting day as factor (taking way too long do run)
report_msg_wide$Date_factor <- as.factor(report_msg_wide$Date)

model <- glmer(Report ~ Msg_Received + Date_factor + (1 | User_ID), 
               data = report_msg_wide,
               family = binomial)
summary(model)

#DO NOT DO



# starting again with advice from Chat GPT, this time it is only users that are used as random effect (without date) 
model <- glmer(Report ~ Msg_Received + Msg_Type + Reg_Orientation + Gender + (1 | User_ID),
data = report_msg_wide,
family = binomial)
summary(model)


#this showed that there isnt any significant effect for recieving a nudge



#this model goes back to where i started. it allows for a random intercept and a random slope for Date within User_ID. so basically each user has 
#their own baseline probability of reporting so they have a random intercept, but also it accounts for date having a VARIABLE effect on reporting behavior
#accross users, this makes sense since each user is in a different location and some days might be good for reporting for some and bad for others
model <- glmer(Report ~ Msg_Received + Msg_Type + Reg_Orientation + Gender + (1 + Date | User_ID),
               data = report_msg_wide,
               family = binomial)
summary(model)

#this gives a significant result for msg0-recieved and also msg_type Prevention now I want to check if the interaction of orientation
# and framing makes any difference
model <- glmer(Report ~ Msg_Received + Msg_Type * Reg_Orientation_Cat + (1 + Date | User_ID),
               data = report_msg_wide,
               family = binomial)
summary(model)

#the results became insignificant again. so i decided to go ahead but streamline the analysis in smaller batches


#Does receiving a message increase the chance of reporting?
model <- glmer(Report ~ Msg_Received + (1 + Date | User_ID),
                data = report_msg_wide,
                family = binomial)

summary(model)

# Yes very significant result (0.000595) but  warning [ boundary (singular) fit: see help('isSingular') ]


#if i try with the random interceot only for user
model <- glmer(Report ~ Msg_Received + (1 | User_ID),
               data = report_msg_wide,
               family = binomial)

summary(model)
#it is still significant but less so (0.00826) but i dont get the warning [ boundary (singular) fit: see help('isSingular') ]



# Is one type of message better than another to increase the chance of reporting?
model <- glmer(Report ~ Msg_Type + (1 + Date | User_ID),
                data = report_msg_wide,
                family = binomial)

summary(model)

#in general no, no significant results and warnining  [ boundary (singular) fit: see help('isSingular') ]


#again if I drop the date as a random slope
model <- glmer(Report ~ Msg_Type + (1 | User_ID),
               data = report_msg_wide,
               family = binomial)

summary(model) 
# no significant results but no warning



#Does the orientation of the person impact how differently framed messages affect them?

promotion_data <- subset(report_msg_wide, Reg_Orientation_Cat == "Promotion")

model <- glmer(Report ~ Msg_Type + (1 + Date | User_ID),
                          data = promotion_data,
                          family = binomial)


summary(model)
#no significance for promotion  and ofcourse the warning




prevention_data <- subset(report_msg_wide, Reg_Orientation_Cat == "Prevention")

model <- glmer(Report ~ Msg_Type + (1 + Date | User_ID),
                           data = prevention_data,
                           family = binomial)

summary(model)



levels(report_msg_wide$Msg_Type)
report_msg_wide$Msg_Type <- relevel(report_msg_wide$Msg_Type, ref = "None")





#Model incorporating numerical promotion and prevention scores
model <- glmer(Report ~ Msg_Type * Promotion + Msg_Type * Prevention + (1 + Date | User_ID),
                data = report_msg_wide,
                family = binomial)

summary(model)

#no significant result



#trying to do it with the catagories
model <- glmer(Report ~ Msg_Type * Reg_Orientation_Cat+ (1 + Date | User_ID),
               data = report_msg_wide,
               family = binomial)

summary(model)
#no significant results


#tried again with link logit added and it seems it gave significant for promotion and prevention
model <- glmer(Report ~ Msg_Type + (1 + Date | User_ID),
               data = report_msg_wide,
               family = binomial(link = "logit"))

summary(model)


#gong to try again for different orientations

#prevention
model <- glmer(Report ~ Msg_Type + (1 + Date | User_ID),
               data = prevention_data,
               family = binomial(link = "logit"))

summary(model)




#testing for morivation and reg focus experiment

model <- glmer(Report ~  Reg_Orientation_Cat + (1 + Date | User_ID),
               data = report_msg_wide,
               family =  binomial(link = "logit"))
summary(model)


#not significant

model <- glmer(total_reports ~ Reg_Orientation_Cat + (1 + Date|User_ID), 
               data = report_msg_wide, 
               family = poisson) 
summary(model)


#### to check if the message type interacts with someones orientation
model <- glmer(Report ~ Msg_Type * Promotion + Msg_Type * Prevention + (1 + Date | User_ID),
                           data = report_msg_wide,
                           family = binomial)

summary(model)
 

#the only significant result here seems to be that people with promotion orientation are significantly influanced by PREVENTION MESSAGES