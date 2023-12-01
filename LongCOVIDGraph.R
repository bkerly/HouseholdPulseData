library(tidyverse)
library(lubridate)

data <- read.delim("hhp/hpstimeseries.txt",sep = "|")
 colnames(data)
 
data %>%
  filter(GEO_ID == "0100000US") %>%
  ggplot(aes(x=`X.WEEK`,y=LONGCOVID_1_RATE)) +
  geom_line()
