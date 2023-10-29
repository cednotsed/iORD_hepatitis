rm(list = ls())
setwd("c:/git_repos/iORD_hepatitis/")
require(tidyverse)
require(data.table)
require(lubridate)
require(foreach)
require(doParallel)
require(funtimes)

df <- fread("data/IORD_ASHep-UE_34_20230120_LIMS.csv") %>%
  mutate(CollectionDateTime = as.Date(CollectionDateTime))

sex_df <- fread("data/IORD_ASHep-UE_34_20230120_Attendances.csv") %>%
  select(ClusterID, LinkedSex) %>%
  distinct(ClusterID, LinkedSex)

age_df <- fread("data/IORD_ASHep-UE_34_20230120_Attendances.csv") %>%
  mutate(AttendanceStartDate = as.Date(AttendanceStartDate),
         AttendanceEndDate = as.Date(AttendanceEndDate),
         LinkedBirthmonth = as.Date(LinkedBirthmonth)) %>%
  mutate(age_upon_presentation = time_length(AttendanceStartDate -
                                               LinkedBirthmonth, "year"))

# Plot ALT measurements per month
month_df <- df %>%
  mutate(CollectionMonth = cut(CollectionDateTime, breaks = "month")) %>%
  left_join(sex_df)

alt_df <- month_df %>%  
  group_by(CollectionMonth, LinkedSex) %>%
  filter(TestName == "ALT") %>%
  summarise(n_alt = n()) %>%
  mutate(CollectionMonth = as.Date(CollectionMonth)) %>%
  ungroup()

wbc_df <- month_df %>%  
  group_by(CollectionMonth, LinkedSex) %>%
  filter(TestName == "WHITE CELLS") %>%
  summarise(n_wbc = n()) %>%
  mutate(CollectionMonth = as.Date(CollectionMonth)) %>%
  ungroup()

plot_df <- alt_df %>%
  inner_join(wbc_df) %>%
  mutate(alt_wbc = n_alt / n_wbc) %>%
  arrange(desc(CollectionMonth))

plot_df %>%
  filter(LinkedSex %in% c("F", "M"),
         CollectionMonth < "2023-01-01") %>%
  ggplot(aes(x = CollectionMonth, y = alt_wbc, color = LinkedSex)) +
  geom_line() +
  scale_x_date(date_breaks =  "1 year", date_labels = "%Y") +
  geom_point() +
  theme_bw() +
  labs(x = "Year", 
       y = "Ratio of ALT to WBC tests per month",
       color = "Sex")

# Test 
# Change in ALT levels over time
ALT <- month_df %>% 
  mutate(AttendanceStartDate = as.Date(AttendanceStartDate)) %>%
  left_join(age_df) %>%
  mutate(age_upon_presentation = floor(age_upon_presentation)) %>% 
  mutate(age_group = case_when(age_upon_presentation < 7 ~ "<7",
                               age_upon_presentation >= 6 & age_upon_presentation <= 15 ~ "7-15",
                               age_upon_presentation >= 16 ~ ">15"),
           Value = as.numeric(Value)) %>%
  filter(!is.na(Value)) %>%
  filter(TestName == "ALT") %>%
  group_by(CollectionMonth, age_group) %>%
  summarise(IQR_ALT = IQR(Value)) %>%
  mutate(CollectionMonth = as.Date(CollectionMonth))

ALT %>%
  ggplot(aes(x = CollectionMonth, y = IQR_ALT, color = age_group)) +
  geom_point() +
  geom_line()
