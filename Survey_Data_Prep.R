# Clear the workspace
rm(list = ls())

# Restart R session
.rs.restartR()

#loading Data

raw_survey_data <- read.csv("Raw_Survey_Results.csv")

raw_survey_data <- raw_survey_data %>% rename( Language = Start.language,
                                               Consent = I.GIVE.MY.CONSENT.to.participate.in.this.study.and.allow.the.use.of.data.generated.in.Mosquito.Alert.on.my.device.to.be.re.used.in.this.research.project..,
                                               User_ID = user_UUID,
                                               Age = How.old.are.you.,
                                               Gender = What.is.your.gender.,
                                               Country = What.is.the.country.you.currently.reside.in.,
                                               Participation_Date = In.what.year.did.you.first.participate.in.Mosquito.Alert.,
                                               Network = How.many.people.do.you.personally.know..acquaintances..friends..family.members.etc...who.are.participating.in.Mosquito.Alert..not.including.yourself..,
                                               Other_Citi_Sci = Are.you.currently.engaged.in.other.citizen.science.projects.,
                                               Self_Direction = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.am.interested.in.the.topic.of.this.project.,
                                               Stimulation = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.challenge.myself.and.do.something.new.,
                                               Hedonism = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....It.s.a.fun.activity.,
                                               Achievement = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....It.s.an.opportunity.to.perform.better.than.others.,
                                               Face = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.enhance.my.reputation.,
                                               Security = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.live.in.safer.surroundings.,
                                               Conformity = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....Other.people.I.know.are.participating..,
                                               Benevolence = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.contribute.to.my.community.,
                                               Universalism_Social = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.make.the.world.a.better.place.,
                                               Universalism_Nature = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.protect.the.environment.,
                                               Routine = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....It.is.part.of.my.routine.,
                                               Social_Expansion = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.be.part.of.this.volunteers..community.,
                                               Power =On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.gain.recognition.,
                                               Help_Science = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.contribute.to.science.,
                                               Teaching = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.use.it.to.teach.others.about.the.topic.,
                                               Dislike = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.dislike.mosquitos..,
                                               Env_Change = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.noticed.an.increase.or.change.in.mosquitos.in.my.surroundings.,
                                               Prom_1 = I.prefer.to.work.without.instructions.from.others.,
                                               Prev_1 = Rules.and.regulations.are.helpful.and.necessary.for.me,
                                               Prev_2 = For.me..it.is.very.important.to.carry.out.the.obligations.placed.on.me.,
                                               Prom_2 = I.generally.solve.problems.creatively.,
                                               Prev_3 = I.m.not.bothered.about.reviewing.or.checking.things.really.closely.,
                                               Prom_3 = I.like.to.do.things.in.a.new.way.,
                                               Prev_4 = I.always.try.to.make.my.work.as.accurate.and.error.free.as.possible.,
                                               Prom_4 = I.like.trying.out.lots.of.different.things..and.am.often.successful.in.doing.so.,
                                               Prom_5 = It.is.important.to.me.that.my.achievements.are.recognized.and.valued.by.other.people.,
                                               Prev_5 = I.often.think.about.what.other.people.expect.of.me.)





raw_survey_data <- raw_survey_data %>% mutate(Country = if_else(Country == "", "I prefer not to say", Country))

raw_survey_data$User_ID <- sub(" target=", "", raw_survey_data$User_ID) #removed target= which was appearing at the end of some uuid's



raw_survey_data <- raw_survey_data %>%
  mutate_at(vars(starts_with("Prom_"), starts_with("Prev_")), list(~as.numeric(str_extract(., "\\d+")))) #turning the Reg Focus Responses to Numeral


raw_survey_data <- raw_survey_data %>%
  mutate(Other_Citi_Sci = ifelse(Other_Citi_Sci == "Yes", 1, 0)) # Converting Other_Citi_Sci to a binary variable


raw_survey_data <- raw_survey_data %>% 
  dplyr::select(Response.ID, Last.page, Language, Consent, User_ID, Age,
                Gender, Country, Participation_Date, Network, 
                Other_Citi_Sci, Security, Teaching, Self_Direction, Stimulation, Hedonism, 
                Achievement, Face, Conformity, Benevolence, Universalism_Social, 
                Universalism_Nature, Routine, Social_Expansion, Power, 
                Help_Science, Prom_1, Prom_2, Prom_3, 
                Prom_4, Prom_5,  Prev_1, Prev_2, Prev_3, Prev_4, Prev_5) %>% 
  mutate( Gender = as.factor(Gender),
          Country = as.factor(Country),
          Network = as.numeric(Network),
          Participation_Date = as.factor(Participation_Date))



####data cleaning and Exploration

#filtering to those who consented and completed the survey, and their UUID has been registered. NOTE REMOVED THIS PART FROM THE BEGGINING OF
#FILTER "Last.page  == 5 &" WHICH REMOVED THOSE THAT DID NOT HAVE GIVEN THEIR INFO REGARDING RF

survey_data <- raw_survey_data %>%
  filter(Consent == "Yes" & nzchar(User_ID) > 0)

#removed the entries by users that have filled the survey twice, maintaining the results of their first attempt and deleting repeats.
survey_data <- survey_data %>%
  group_by(User_ID) %>%
  filter(row_number() == 1) %>%
  mutate(Complt_Survey = TRUE) %>% 
  ungroup()

survey_data <- survey_data %>%
  select(-Response.ID, -Last.page, -Language, -Consent, -Complt_Survey, -Network, 
         - Other_Citi_Sci, - Security, - Teaching, - Self_Direction, - Stimulation, 
         - Hedonism, - Achievement, - Face, - Conformity, - Benevolence, - Universalism_Social, 
         - Universalism_Nature, - Routine, - Social_Expansion, - Power, - Help_Science)         #Removed Everything that doesnt have to do with RF




##NOT DOING THIS NOW, SINCE I ALLOWED INDIVIDUALS TO COME IN THAT WE HAVE NO CLUE WHAT THEIR RF ARE, AS IN THEY ASNWERED NON OF THE QUESSTIONS
#replacing NA's with Median (median imputation is less sensitive to outliers and max 2 missing for each column )
#survey_data <- survey_data %>%
  mutate(across(Security:Prev_5, ~replace(., is.na(.), median(., na.rm = TRUE))))



#creating an average individual Reg Focus
survey_data <- survey_data %>% mutate(Reg_Orientation = (Prom_1+Prom_2+Prom_3+Prom_4+Prom_5 - Prev_1 - Prev_2- Prev_3- Prev_4- Prev_5))


# Adding categorical version of Reg_Orientation
survey_data <- survey_data %>%
  mutate(Reg_Orientation_Cat = case_when(
    Reg_Orientation < -1 ~ "Prevention",
    Reg_Orientation >= -1 & Reg_Orientation <= 1 ~ "Neutral",
    Reg_Orientation > 1 ~ "Promotion",
    TRUE ~ as.character(NA)  # For NAs
  ))
survey_data <- survey_data %>% mutate(Reg_Orientation_Cat = as.factor(Reg_Orientation_Cat))


#adding columns for only promotion and prevention
survey_data <- survey_data %>%
  mutate(Promotion =  (Prom_1+Prom_2+Prom_3+Prom_4+Prom_5),
         Prevention = (Prev_1+Prev_2+Prev_3+Prev_4+Prev_5))

survey_data <- survey_data %>%
  select(-Prom_1,-Prom_2,-Prom_3,-Prom_4,-Prom_5,-Prev_1,-Prev_2,-Prev_3,-Prev_4,-Prev_5)


write.csv(survey_data, "CleanSurveydData.csv", row.names = FALSE)
rm(raw_survey_data)


summary(survey_data)
