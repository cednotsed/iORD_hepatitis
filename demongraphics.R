setwd("c:/git_repos/iORD_hepatitis/")
require(tidyverse)
require(data.table)
require(lubridate)
require(foreach)
require(doParallel)

df <- fread("data/IORD_ASHep-UE_34_20230120_Attendances.csv") %>%
  mutate(AttendanceStartDate = as.Date(AttendanceStartDate),
         AttendanceEndDate = as.Date(AttendanceEndDate),
         LinkedBirthmonth = as.Date(LinkedBirthmonth))

unique_ids <- unique(df$ClusterID)
length(unique_ids)

# No. of months
interval(ymd("2016-03-01"), ymd("2023-01-15")) %/% months(1)

## range of data
df %>%
  mutate(AttendanceStartDate = as.Date(AttendanceStartDate)) %>%
  summarise(range(AttendanceStartDate))

## No. of male and females
df %>%
  distinct(ClusterID, .keep_all = T) %>%
  group_by(LinkedSex) %>%
  summarise(n = n())

## Calculate age of patients based on first visit
# temp <- df %>% 
#   select(ClusterID, AttendanceStartDate, LinkedBirthmonth)
# 
# cl <- makeCluster(14)
# registerDoParallel(cl)
# 
# age_morsels <- foreach(id = unique(df$ClusterID), 
#                        .packages = c("tidyverse", "lubridate")) %dopar% {
#   df %>%
#     filter(ClusterID == id) %>%
#     arrange(AttendanceStartDate) %>%
#     head(1)
# }
# 
# # stopCluster(cl)
# bind_rows(age_morsels) %>%
#   distinct(ClusterID, .keep_all = T) %>%
#   mutate(age_upon_presentation = time_length(AttendanceStartDate - 
#                                                LinkedBirthmonth, "year")) %>%
#   )

age_df <- df %>%
  mutate(age_upon_presentation = time_length(AttendanceStartDate -
                                               LinkedBirthmonth, "year"))
# Age IQR
quantile(age_df$age_upon_presentation, probs = c(0.25, 0.75))  
