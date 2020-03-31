rm(list=ls())

library(tidyverse)
library(paletteer)

#import data
brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')

#tidy up brewery sizes as categories vary from year-to-year
brewer_size$brewer_size2 <- case_when(
  brewer_size$brewer_size=="Zero Barrels" ~ "<1,000 Barrels",
  brewer_size$brewer_size=="Under 1 Barrel" ~ "<1,000 Barrels",
  brewer_size$brewer_size=="1 to 1,000 Barrels" ~ "<1,000 Barrels",
  brewer_size$brewer_size=="1,001 to 7,500 Barrels" ~ "1,000-7,500 Barrels",
  brewer_size$brewer_size=="7,501 to 15,000 Barrels" ~ "7,500-15,000 Barrels",
  brewer_size$brewer_size=="15,001 to 30,000 Barrels" ~ "15,000-30,000 Barrels",
  brewer_size$brewer_size=="30,001 to 60,000 Barrels" ~ "30,000-60,000 Barrels",
  brewer_size$brewer_size=="60,001 to 100,000 Barrels" ~ "60,000-100,000 Barrels",
  brewer_size$brewer_size=="100,001 to 500,000 Barrels" ~ "100,000-500,000 Barrels",
  brewer_size$brewer_size=="500,001 to 1,000,000 Barrels" ~ "500,000-1m Barrels",
  brewer_size$brewer_size=="1,000,001 to 6,000,000 Barrels" ~ "1m-6m Barrels",
  brewer_size$brewer_size=="1,000,000 to 6,000,000 Barrels" ~ "1m-6m Barrels",
  brewer_size$brewer_size=="1,000,001 to 1,999,999 Barrels" ~ "1m-6m Barrels",
  brewer_size$brewer_size=="2,000,000 to 6,000,000 Barrels" ~ "1m-6m Barrels",
  brewer_size$brewer_size=="6,000,001 Barrels and Over" ~ "6m+ Barrels"
)

#set factor levels
brewer_size$brewer_size2 <- factor(brewer_size$brewer_size2, levels=c("<1,000 Barrels", "1,000-7,500 Barrels",
                                                                      "7,500-15,000 Barrels", "15,000-30,000 Barrels",
                                                                      "30,000-60,000 Barrels", "60,000-100,000 Barrels",
                                                                      "100,000-500,000 Barrels", "500,000-1m Barrels",
                                                                      "1m-6m Barrels", "6m+ Barrels"))

#collapse data onto new size variable
brewer_size2 <- brewer_size %>%
  filter(!is.na(brewer_size2)) %>%
  group_by(year, brewer_size2) %>%
  summarise(n_of_brewers=sum(n_of_brewers, na.rm = TRUE), total_barrels=sum(total_barrels, na.rm = TRUE))

#plot brewery numbers against size
tiff("Outputs/USBeerProd.tiff", units="in", res=300, width=10, height=9)
ggplot(brewer_size2, aes(x=year, y=n_of_brewers, fill=brewer_size2))+
  geom_bar(stat="identity", position="stack")+
  scale_fill_paletteer_d("Redmonder::dPBIPuGn", name="Annual production")+
  theme_classic()+
  scale_x_continuous(breaks=c(2009:2019), name="Year")+
  scale_y_continuous(name="Number of breweries")+
  labs(title="There's been an explosion in US craft beer producers...", subtitle="Number of breweries by size 2009-2019",
       caption="Data from US TTB | Plot by @VictimOfMaths")+
  theme(plot.title.position = "plot")
dev.off()

#plot production against size
tiff("Outputs/USBeerProd2.tiff", units="in", res=300, width=10, height=9)
ggplot(brewer_size2, aes(x=year, y=total_barrels/1000000, fill=brewer_size2))+
  geom_bar(stat="identity", position="stack")+
  scale_fill_paletteer_d("Redmonder::dPBIPuGn", name="Annual production")+
  theme_classic()+
  scale_x_continuous(breaks=c(2009:2019), name="Year")+
  scale_y_continuous(name="Total beer production (million barrels)")+
  labs(title="...but the US beer market is still dominated by the big players", 
       subtitle="Total beer production by brewery 2009-2019",
       caption="Data from US TTB | Plot by @VictimOfMaths")+
  theme(plot.title.position = "plot")
dev.off()
