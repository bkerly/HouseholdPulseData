library(tidyverse)
library(lubridate)
library(ggthemes)

week_to_date <- read_csv("week to date.csv") %>%
  mutate(start_date = dmy(start_date),
         end_date = dmy(end_date))

data <- read.delim("hhp/hpstimeseries.txt",sep = "|") %>%
  left_join(week_to_date)


  

colnames(data)

data_state_rate <- data %>%
  group_by(GEO_ID,start_date) %>%
  summarize(LONGCOVID_1_RATE = LONGCOVID_1_RATE)%>%
  ungroup() %>%
  filter(complete.cases(.))

data_CO_rate_MOE <- data %>%
  filter(GEO_ID == "0400000US08") %>%
  select(start_date,LONGCOVID_1_RATE,LONGCOVID_1_RT_MOE) %>%
  filter(complete.cases(.))

data_US_rate_MOE <- data %>%
  filter(GEO_ID == "0100000US")  %>%
  select(start_date,LONGCOVID_1_RATE,LONGCOVID_1_RT_MOE) %>%
  filter(complete.cases(.))

ggplot() +
  geom_line(data = data_state_rate, aes(y=LONGCOVID_1_RATE,x=start_date,group=GEO_ID),
            alpha = 0.2) +
  geom_ribbon(data = data_US_rate_MOE, aes(y=LONGCOVID_1_RATE,x=start_date,
                                           ymax = LONGCOVID_1_RATE+LONGCOVID_1_RT_MOE,
                                           ymin = LONGCOVID_1_RATE-LONGCOVID_1_RT_MOE),
              fill= "lightblue",
              alpha = 0.4
            )+
  geom_line(data = data_US_rate_MOE, aes(y=LONGCOVID_1_RATE,x=start_date,
                                           ),
            color = "blue"
  )+
  geom_ribbon(data = data_CO_rate_MOE, aes(y=LONGCOVID_1_RATE,x=start_date,
                                           ymax = LONGCOVID_1_RATE+LONGCOVID_1_RT_MOE,
                                           ymin = LONGCOVID_1_RATE-LONGCOVID_1_RT_MOE),
              fill= "pink",
              alpha = 0.4
  )+
  geom_line(data = data_CO_rate_MOE, aes(y=LONGCOVID_1_RATE,x=start_date,
  ),
  color = "red"
  ) + 
  theme_clean()+
  labs(title = "Percent of Individuals Answering YES to: \n      Did you ever have any symptoms lasting 3 months or longer \n      that you did not have prior to having coronavirus or COVID-19?",
       subtitle = "Data shown by state: Colorado highlighted in Red, US aggregate in Blue",
       caption = "Data from US Census Househuld Pulse Survey Set \n
       https://www.census.gov/programs-surveys/household-pulse-survey.html
       ") +
  ylab("Percent Reporting Long COVID Symptoms") +
  xlab("")

ggsave("Percent of Individuals Answering YES.png")
  

# 
# # Data tables -------------------------------------------------------------
# 
# data_files <- list.files(path = "weekly data tables") %>%
#   data.frame() %>%
#   `colnames<-`("filenames")
# 
# excel_fixer <- function(df) {
#   df %>%
# 
#     filter(`...1` %in% 
#              c("Total",
#            "18 - 24",
#            "25 - 39",
#            "40 - 54",
#            "55 - 64",
#            "65 and above",
#            "Male",
#            "Female")) %>%
#     `colnames<-`(c("Total Surveyed",
#                    "Current_Sx_Y",
#                    "Current_Sx_N",
#                    "Current_Sx_Unk",
#                    "Long_Sx_Y",
#                    "Long_Sx_N",
#                    "Long_Sx_Unk",
#                    "No LC",
#                    ""))
# }
# 
# CO_data_extract <- function(filename = "health10_week52.xlsx"){
#   CO_data <- readxl::read_excel(paste0(
#     "weekly data tables/",
#     filename
#   ),
#   sheet = "CO",
#   skip = 7) %>%
#   excel_fixer() %>%
#   mutate(file = as.character(..filename))
# }
# 
# data_files %>%
#   reframe(CO_data_extract(data_files$filenames),bind_rows())
