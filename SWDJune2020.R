rm(list=ls())

library(tidyverse)
library(curl)
library(googledrive)
library(readxl)
library(forcats)
library(paletteer)
library(hrbrthemes)

#Download from Google Drive
temp <- tempfile()
drive_download(as_id("1SkVFues0SJ5JzMJt1a_XFmRJudrmb34x"), path=temp, overwrite=TRUE)
data <- read_excel(temp, range="K5:Z16")
colnames(data)[names(data)=="...1"] <- "Quarter"

#Lengthen
data_long <- gather(data, dealership, sales, c(2:15))

#Sort out capitalisation of dealership names
data_long$dealership <- paste0(substr(data_long$dealership,1,1), 
                               tolower(substr(data_long$dealership,2,99)))

data_long <- data_long %>%
  group_by(dealership) %>%
  summarise(total=sum(sales), avgtotal=sum(`REGIONAL AVG`))

#Calculate sales vs. average
data_long$salesvsavg <- data_long$total-data_long$avgtotal

ggplot(data_long)+
  geom_col(aes(x=salesvsavg, y=fct_reorder(dealership, salesvsavg), fill=salesvsavg),
           show.legend=FALSE)+
  geom_segment(aes(x=0, xend=0, y=0,yend=15))+
  scale_fill_paletteer_c("pals::kovesi.diverging_gwr_55_95_c38", direction=-1, 
                         limit=c(-1,1)*max(abs(data_long$salesvsavg)))+
  scale_x_continuous(name="Total sales vs. regional average")+
  scale_y_discrete(name="Dealership")+
  theme_classic()+
  labs(title="Over- and under-achievers",
       subtitle="Total car sales between Q1 2017 and Q3 2019 compared to regional average",
       caption="Data from @storywithdata | Plot by @VictimOfMaths")
