setwd("c:/git_repos/iORD_hepatitis/")
require(tidyverse)
require(data.table)


files <- list.files("data", ".csv", full.names = T)

df <- fread("data/IORD_ASHep-UE_34_20230120_Attendances.csv")
unique_ids <- unique(df$ClusterID)

for(file in files) {
  df <- fread(file)
  print(nrow(df %>% distinct(ClusterID)))
  # print(df %>% summarise(min = min))
  print(all(df$ClusterID %in% unique_ids))
}

df %>%
  head(10) %>%
  mutate(AttendanceEndDate_cor = as.Date(AttendanceEndDate)) %>%
  summarise(max(AttendanceEndDate_cor))
df <- fread("data/IORD_ASHep-UE_34_20230120_Attendances.csv")
View(df)

df %>% distinct(ClusterID)
