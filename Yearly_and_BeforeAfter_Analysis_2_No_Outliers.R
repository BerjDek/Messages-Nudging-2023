

data_reduced <- data %>% 
  filter(!User_ID %in% outliers_1$User_ID)

data_reduced_tall <- data_tall %>% 
  filter(!User_ID %in% outliers_1$User_ID)

paired_t_test_result <- data_reduced %>%
  filter(Registered_Participation_Date < as.Date('2023-01-01')) %>%
  with(t.test(Season_Rprts_Filled_2023, Season_Rprts_Filled_2022, paired = TRUE))
print(paired_t_test_result)

paired_t_test_result <- data_reduced %>%
  filter(Registered_Participation_Date < as.Date('2022-01-01')) %>%
  with(t.test(Season_Rprts_Filled_2022, Season_Rprts_Filled_2021, paired = TRUE))
print(paired_t_test_result)

paired_t_test_result <- data_reduced_tall %>%
  filter(Registered_Participation_Date < as.Date('2023-01-01')) %>%
  filter(!User_ID %in% data_reduced$User_ID) %>% 
  with(t.test(Season_Rprts_Filled_2023, Season_Rprts_Filled_2022, paired = TRUE))
print(paired_t_test_result)

rm(paired_t_test_result)


In a paired t-test comparing the 2023 and 2022 seasons for the participants that have received messages in 2023 and were registered before 2023, a significant mean difference of approximately 12 additional reports was found **(t = 2.9778, df = 140, p-value = 0.003113)**. The 95% confidence interval ranging from **4.1 to 20.1** additional reports.

When repeating the test to compare 2022 and 2021 seasons for the users received messages in 2023 and were registered before 2022, the outcome was different with a non-significant **-0.49** decrease in average number of reports; **(t = -0.12358, df = 70, p-value = 0.902)**.

More importantly comparing the 2023 and 2022 seasons, for all participants bar the ones who were part of the experiment and were registered before 2023, we see a statistically significant drop **(-0.11)** in average number of reports (t = -26.032, df = 186646, p-value < 2.2e-16)

as with the second and third t-tests, it is usual to see that the number of reports reduce on average for the same participants over time, due to attrition and loss of interest.
The fact that the average has increased for those who received messages is promising.






### Difference-in-Differences Analysis

#In order to assess the change in participant behavior during the period of messaging, we compare the number of reports filled by participants before, 
#during and after their respective messaging period. Note that since the users are distributed in the date they recived the first message and the time 
#frame in which they received messages, seasonality is better accounted for. The number of reports for each user and each specific period 
#was calculated by measuring the specific days in between which the specific participant received messages and then adding the number of reports in
#exactly the same amount of days before and after the first and last message respectively



paired_t_test_result <- with(data_reduced, t.test(Rprts_During_Msging, Rprts_Before_Msging,  paired = TRUE))
print(paired_t_test_result)

paired_t_test_result <- with(data_reduced, t.test(Rprts_During_Msging, Rprts_After_Msging,  paired = TRUE))
print(paired_t_test_result)


# i chose not to mention the final comparision between after and before in the paper
paired_t_test_result <- with(data_reduced, t.test(Rprts_After_Msging, Rprts_Before_Msging,  paired = TRUE)) 
print(paired_t_test_result)

