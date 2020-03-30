rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(geofacet)

#import tt data
#brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
#beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
#brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

#import US state populations from US Census Bureau
temp <- tempfile()
source <- "http://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
US1019pop <- read.csv(temp)[,c(5,8:17)]
colnames(US1019pop) <- c("State", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")

temp <- tempfile()
source <- "https://www2.census.gov/programs-surveys/popest/tables/2000-2010/intercensal/state/st-est00int-01.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
US0010pop <- read_excel(temp, range="A4:N61", col_names=TRUE)[,-c(2, 13,14)]
colnames(US0010pop)[c(1)] <- c("State")
US0010pop$State <- gsub('^\\.|\\.$', '', US0010pop$State)

#merge populations
USpop <- merge(US0010pop, US1019pop)
USpop <- gather(USpop, year, Pop, c(2:21))
USpop$state <- state.abb[match(USpop$State, state.name)]
USpop$state <- ifelse(USpop$State=="District of Columbia", "DC", USpop$state)

#merge population into production data
beer_states <- merge(beer_states, USpop, by=c("state", "year"))

#generate total production
temp <- beer_states %>%
  group_by(state, year) %>%
  summarise(barrels=sum(barrels), Pop=sum(Pop))

temp$type <- "Total"

beer_states <- bind_rows(beer_states, temp)

#calculate per capita production
beer_states$percap <- beer_states$barrels/beer_states$Pop

tiff("Outputs/USBeer.tiff", units="in", res=300, width=14, height=9)
ggplot(subset(beer_states, type=="Total"), aes(x=year, y=percap))+
  geom_bar(stat="identity", fill="#f28e1c")+
  scale_x_continuous(name="", breaks=c(2008, 2013, 2019))+
  scale_y_continuous(name="Beer production per capita (barrels/year)")+
  facet_geo(~state, grid = "us_state_grid2", label = "name")+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(0.7)),
        axis.text=element_text(size=rel(0.6), face="bold"))+
  labs(title="Who makes all the beer?", subtitle="US beer production by state 2008-2019",
       caption="Data from US TTB & US Census Bureau | Plot by @VictimOfMaths")
dev.off()
