library(tidyverse)
library(httr)
library(curl)
library(lubridate)

options(scipen = 999)

##################state data ################
#these are only recurring not emergency, says Emma

opened_cases <- read_csv(URLencode(
  "https://data.ny.gov/resource/fivj-j6mz.csv?district='New York City'")
)

closed_cases <- read_csv(URLencode(
  "https://data.ny.gov/resource/4x9s-7y8g.csv?district='New York City'"
))

denied_cases <- read_csv(URLencode(
  "https://data.ny.gov/resource/tyyj-jgv5.csv?district='New York City'"
))


denials_state_clean <- denied_cases %>% 
  select(-starts_with("fa"), - starts_with("sna")) %>% 
  mutate(total_denials = rowSums(.[6:11])) %>% 
  group_by(year, month) %>% 
  summarize(total_denials = max(total_denials))

opens_state_clean <- opened_cases %>% 
  group_by(year, month) %>% 
  summarize(total_openings = max(total_openings))
  

opened_denied <- inner_join(opens_state_clean, denials_state_clean, by = c("year", "month")) %>% 
  mutate(approval_rate = total_openings/(total_openings+total_denials),
         date = lubridate::my(paste(month, year)),
         total_processed = total_openings+total_denials)
  
ggplot(opened_denied)+
  geom_line(mapping = aes(x = date, y = approval_rate))+
  labs(subtitle = "Cash assistance application approval rate by month",
       y = "Cash assistance application approval rate",
       x = "Date",
       title = "Cash Assistance Applications are getting denied more often")

opened_denied %>% 
  pivot_longer(cols = c("total_openings", "total_denials"), values_to = "count", names_to = "type") %>% 
ggplot()+
  geom_area(aes(x = date, y = count, fill = type), position = "fill")

ggplot(opened_denied)+
  geom_line(mapping = aes(x = date, y = total_processed))+
  labs(subtitle = "Monthly Cash Assistance Applications Processed since 2006",
       y = "Cash assistance applications",
       x = "Date",
       title = "The City is Processing more Cash Assistance Applications in Recent Months")

ggplot(opened_denied)+
  geom_line(mapping = aes(x = date, y = total_denials))+
  labs(subtitle = "Monthly Cash Assistance Applications Denied Since 2006",
       y = "Cash assistance applications",
       x = "Date",
       title = "The City is Denying more Cash Assistance Applications")


state_denial_categories <- denied_cases %>%
  select(-starts_with("fa"), - starts_with("sna")) %>% 
  mutate(total = ta_client_req + ta_finance + ta_residence + ta_comp_employ + ta_comp_other + ta_other,
         date = my(paste(month, year))) %>% 
  mutate(across(starts_with("ta_"), ~./total))
  
pivot_longer(state_denial_categories, cols = starts_with("ta_"), names_to = "reason", values_to = "denials") %>% 
  ggplot()+
  geom_line(aes(x = date, y = denials, color = reason))

#################### MMR ###################


cash_assistance_apps <- read_csv(URLencode("https://data.cityofnewyork.us/resource/rbed-zzin.csv?$where=id=10775&$limit=1000000"))

cash_assistance_approval_rate <- read_csv(URLencode("https://data.cityofnewyork.us/resource/rbed-zzin.csv?$where=id=10776&$limit=1000000"))

hra_vars <- read_csv(URLencode("https://data.cityofnewyork.us/resource/rbed-zzin.csv?$where=agency='HRA'&$limit=1000000"))


#################### # apps ##########################

ca_apps_clean <- cash_assistance_apps %>% 
  select(indicator, id, valuedate, acceptedvalue) %>% 
  arrange(valuedate) %>% 
  mutate(acceptedvalue = acceptedvalue * 1000,
         cumulative_apps = cumsum(acceptedvalue))
  
ggplot(ca_apps_clean)+
  geom_line(mapping = aes(x = valuedate, y = acceptedvalue))+
  labs(subtitle = "Monthly Cash Assistance Applications since July 2015",
       y = "Cash assistance applications",
       x = "Date",
       title = "Cash Assistance Applications fose sharply in 2021")

write_csv(ca_apps_clean, "data/applications_monthly.csv")

ggplot(ca_apps_clean)+
  geom_line(mapping = aes(x = valuedate, y = cumulative_apps))+
  labs(subtitle = "Cumulative Monthly Cash Assistance Applications since July 2015",
       y = "Cash assistance applications",
       x = "Date",
       title = "Cash Assistance Applications are piling up")

####################### approval rate #######################

ca_approval_clean <- cash_assistance_approval_rate %>% 
  select(indicator, id, valuedate, acceptedvalue) %>% 
  arrange(valuedate)

ggplot(ca_approval_clean)+
  geom_line(mapping = aes(x = valuedate, y = acceptedvalue))+
  labs(subtitle = "Cash assistance application approval rate by month",
       y = "Cash assistance application approval rate",
       x = "Date",
       title = "Cash Assistance Applications are getting denied more often")

write_csv(ca_approval_clean, "data/approval_rate.csv")


############## rejection reasons ###########################

cash_assistance_rejections <- read_csv(URLencode("https://data.cityofnewyork.us/resource/g6pg-qint.csv?$limit=20000"))

total_rejections <- cash_assistance_rejections %>% 
  separate(date, sep = "-", into = c("quarter_start_date", "quarter_end_date")) %>% 
  separate(nys_wms_rejection_code, sep = "-", into = c("nys_wms_rejection_code", "rejection_code_description")) %>% 
  mutate(quarter_start_date = mdy(quarter_start_date)) %>% 
  filter(nys_wms_rejection_code == "Total", type_of_government_assistance == "Cash Assistance") %>% 
  mutate(total = as.numeric(str_replace(total, "[*,]", "")),
         test = "x") %>% 
  select(quarter_start_date, total)

ggplot(total_rejections)+
  geom_line(mapping = aes(x = quarter_start_date, y = total))+
  labs(subtitle = "Total Quaraterly rejections",
       y = "Cash assistance denials",
       x = "Date",
       title = "Cash Assistance Application denials are way up")

write_csv(arrange(total_rejections, desc(quarter_start_date)), "data/total_denials.csv")

# reasons

rejection_reason_clean <- cash_assistance_rejections %>% 
  separate(date, sep = "-", into = c("quarter_start_date", "quarter_end_date")) %>% 
  separate(nys_wms_rejection_code, sep = "-", into = c("nys_wms_rejection_code", "rejection_code_description")) %>% 
  mutate(quarter_start_date = mdy(quarter_start_date)) %>% 
  filter(nys_wms_rejection_code != "Total", type_of_government_assistance == "Cash Assistance") %>% 
  select(1:7) %>% 
  group_by(quarter_start_date) %>% 
  mutate(count = as.numeric(str_replace(total, "[*,]", "")))%>% 
  select(-total) %>% 
  left_join(total_rejections, by = c("quarter_start_date")) %>% 
  ungroup() %>% 
  replace_na(list("count" = 0)) %>% 
  mutate(proportion_rejections = count/total) %>% 
  group_by(nys_wms_rejection_code) %>% 
  arrange(nys_wms_rejection_code, quarter_start_date) %>% 
  mutate(change_denials = count - lag(count),
         per_change_denials = (count-lag(count))/lag(count),
         pp_change_denials = proportion_rejections - lag(proportion_rejections)) %>% 
  ungroup()

reasons <- rejection_reason_clean %>% 
  select(1:5) %>% 
  arrange(nys_wms_rejection_code, desc(quarter_start_date)) %>% 
  group_by(nys_wms_rejection_code) %>% 
  summarize(rejection_code_description = first(rejection_code_description))

reason_sum <- rejection_reason_clean %>% 
  filter(quarter_start_date > as.Date("2022-01-01")) %>% 
  group_by(nys_wms_rejection_code) %>% 
  summarize(total = sum(count)) %>% 
  arrange(desc(total)) %>% 
  left_join(reasons)

reason_sum %>% head(10) %>% mutate(percent = total/sum(total)*100) %>% knitr::kable()

top10 <- reason_sum %>% 
  head(10) %>% 
  pull(nys_wms_rejection_code)

top5 <- reason_sum %>% 
  head(5) %>% 
  pull(nys_wms_rejection_code)

reason_sum_quarters <- rejection_reason_clean %>% 
  filter(quarter_start_date > as.Date("2022-01-01"),
         nys_wms_rejection_code %in% top10) %>% 
  group_by(nys_wms_rejection_code, quarter_start_date) %>% 
  arrange(nys_wms_rejection_code, quarter_start_date)

rejection_reason_clean %>% filter(nys_wms_rejection_code %in% top5,
                                  quarter_start_date >= as.Date("2020-01-01")
                                  ) %>% 
  select(quarter_start_date, proportion_rejections, nys_wms_rejection_code) %>% 
  left_join(reasons) %>% 
  ggplot()+
  geom_line(aes(x = quarter_start_date, y = proportion_rejections, color = rejection_code_description))


rejection_reason_clean %>% filter(
                                  quarter_start_date >= as.Date("2020-01-01")
) %>% 
  select(quarter_start_date, proportion_rejections, count, nys_wms_rejection_code) %>% 
  left_join(reasons) %>% 
  mutate(color = if_else(nys_wms_rejection_code == "E10", "blue", "gray")) %>% 
  ggplot()+
  geom_line(aes(x = quarter_start_date, y = count, group=rejection_code_description, color = color))+
  scale_color_identity(labels = c(blue = "Failure to Keep/Complete",gray = "Other"),guide = "legend")

denials_wide <- rejection_reason_clean %>% filter(
  quarter_start_date >= as.Date("2020-01-01")
) %>% 
  select(quarter_start_date, proportion_rejections, nys_wms_rejection_code, count) %>% 
  left_join(reasons) %>% 
  mutate(rejection_code_description = if_else(rejection_code_description == "Case Closed/Rejected For Emergency Assistance", "Case Rejected For Emergency Assistance", rejection_code_description)) %>% 
  filter(proportion_rejections != 0) %>% 
  group_by(quarter_start_date, rejection_code_description) %>% 
  summarize(proportion_rejections = max(proportion_rejections)*100,
            count = max(count)) %>% 
  pivot_wider(id_cols = "quarter_start_date",names_from = "rejection_code_description", values_from = "proportion_rejections")




write_csv(denials_wide, "data/denial_reasons.csv")

###################### applications, denials ########################################

apps_denials_series <- ca_apps_clean %>%
  group_by(date = floor_date(valuedate, unit = "quarter")) %>% 
  summarize(value = sum(acceptedvalue)) %>% 
  filter(date >= as.Date("2019-07-01"), date <= as.Date("2023-07-01")) %>% 
  arrange(date) %>% 
  mutate(series = "Applications",
         yoy = value/lag(value, n = 4),
         qoq = value/lag(value, n = 1),
         per_increase = (value/first(value)-1)*100) %>% 
  bind_rows(total_rejections %>% 
              select(date = quarter_start_date,
                     value = total) %>% 
              arrange(date) %>% 
              mutate(series = "Denials",
                      yoy = value/lag(value, n = 4),
                      qoq = value/lag(value, n = 1),
                     per_increase = (value/first(value)-1)*100)) %>% 
  filter(month(date) %in% c(1, 4, 7, 10))

ggplot(apps_denials_series)+
  geom_line(mapping = aes(x = date, y = per_increase, color = series))
