# What if we do the analysis but remove the outliers



# Removing the top 5% oof reporters in 2023
report_msg_wide_reduced <- report_msg_wide %>% 
  filter(!User_ID %in% outliers_1$User_ID)

# 5% of 2023 NO, 5% all yes (0.0258 ), 5% all split into two sides (0.00463 )


#Does receiving a message increase the chance of reporting?
model <- glmer(Report ~ Msg_Received + (1 + Date | User_ID),
               data = report_msg_wide_reduced,
               family = binomial)

summary(model)
# 5% of 2023 NO, 5% all yes (0.0258 ), 5% all split into two sides (0.0258)




# Does the nudge increase the total number of reports filed. reporting intensity
model <- glmer(total_reports ~ Msg_Received + (1 + Date|User_ID), 
               data = report_msg_wide_reduced, 
               family = poisson) 
summary(model)

#5% all split into two sides (2.84e-06 )




#does the type of nudge matter
levels(report_msg_wide_reduced$Msg_Type)
report_msg_wide_reduced$Msg_Type <- relevel(report_msg_wide_reduced$Msg_Type, ref = "None") #set the level to none


model <- glmer(Report ~ Msg_Type + (1 + Date | User_ID),
               data = report_msg_wide_reduced,
               family = binomial(link = "logit"))

summary(model)
# Vigilant is the significant one


#looking only at days where messages were sent, This is unnecessary since it looks at the difference between neutral and promotion/prevention
nudge_sent_data <- subset(report_msg_wide_reduced, Msg_Received == 1)






#does a type increase or decrease the intensity?   # AVOID
model <- glmer(total_reports ~ Msg_Type + (1 + Date|User_ID), 
               data = report_msg_wide_reduced, 
               family = poisson) 
summary(model)
# again all significant with prevention being the lowest type of significance for increasing the number of total reports





#checking if agreement plays a role
model <- glmer(Report ~ Orientation_Nudge_Agreement + (1 + Date | User_ID),
               data = report_msg_wide_reduced,
               family = binomial(link = "logit"))

summary(model)

#this says that the agreement is not significant when considering all the days [with or without messages]


# looking only on days where messages are sent since None is never going to have an agreement
model <- glmer(Report ~ Orientation_Nudge_Agreement + (1 + Date | User_ID),
               data = nudge_sent_data,
               family = binomial(link = "logit"))

summary(model)

# the same result if only messages were sent. the significance fell from 0.1 to 0.4

#trying for each orientation alone

#checking the impact of orientation  and nudge agreement
model <- glmer(Report ~ Orientation_Nudge_Agreement + (1 + Date|User_ID), 
               data = report_msg_wide_reduced[report_msg_wide_reduced$Reg_Orientation_Cat == "Prevention", ], 
               family = binomial(link = "logit"))
summary(model) 
#YES Significant



model <- glmer(Report ~ Orientation_Nudge_Agreement + (1 + Date|User_ID), 
               data = report_msg_wide_reduced[report_msg_wide_reduced$Reg_Orientation_Cat == "Promotion", ], 
               family = binomial(link = "logit"))
summary(model)
# NOT Significant


model <- glmer(Report ~ Orientation_Nudge_Agreement + (1 + Date|User_ID), 
               data = report_msg_wide_reduced[report_msg_wide_reduced$Reg_Orientation_Cat == "Neutral", ], 
               family = binomial(link = "logit"))
summary(model)
# Not Significant




#checking the effect of message framing on intensity of reporting
model <- glmer(total_reports ~ Msg_Type + (1 + Date|User_ID), 
               data = report_msg_wide_reduced, 
               family = poisson) 
summary(model)
#ignore for now as all of them are significant with prevention being the lowest
