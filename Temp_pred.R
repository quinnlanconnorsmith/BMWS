getwd()
library(tidyverse)
library(ggplot2)
library(dplyr)
read.csv(hobo_tempsum.csv)
#I guess you need to import the dataset the manual way?

#Some exploratory analysis 

ggplot(data = hobo_tempsum) + 
  geom_point(mapping = aes(x = yday, y =mean.temp))
#This is big and ugly, but that's ALL of the data 
ggplot(data = hobo_tempsum) + 
  geom_point(mapping = aes(x = yday, y =mean.temp, color=depth.m))
#Here's one way to break it down - still a lot of data 
#Further breaking things down 

#Getting into the goodies - here's both lakes broken into temp/day per meter measured 
ggplot(data = hobo_tempsum) + 
  geom_point(mapping = aes(x = yday, y =mean.temp)) +
  facet_wrap(~ depth.m)

#Separated out by lakes 
ggplot(data = hobo_tempsum) + 
  geom_point(mapping = aes(x = yday, y =mean.temp)) +
  facet_grid(lake ~ depth.m)

#Lets filter this down to just McD and head forward with that 
hobo_mcd <-hobo_tempsum[hobo_tempsum$lake =="MCD",]

#Now heading back to temp/day per meter 

ggplot(data = hobo_mcd) + 
  geom_point(mapping = aes(x = yday, y =mean.temp)) +
  facet_wrap(~ depth.m)

#Extra filters, let's focus on 0.5m and 1m using dplyr 

hobo_mcd_up<-filter(hobo_mcd, depth.m =='0.5'| depth.m =='1')
ggplot(data = hobo_mcd_up) + 
  geom_point(mapping = aes(x = yday, y =mean.temp)) +
  facet_wrap(~ depth.m)

#Let's trey getting even more complex, diel cycles 
#Also, we still have to separate out littoral and pelagic 

ggplot(data = hobo_mcd_up) + 
  geom_point(mapping = aes(x = yday, y =mean.temp, color =location)) +
  facet_wrap(hour ~ depth.m)

#This is pretty ugly, and it looks like there's only littoral temp for 1m 
#Let's continue filtering - don't actually do this for the regression model 
#hobo_mcd_up<-filter(hobo_mcd,depth.m =='1')
#Now this is just data from 1m 
ggplot(data = hobo_mcd_up) + 
  geom_point(mapping = aes(x = yday, y =mean.temp, color =location)) +
  facet_wrap(~hour)

#Still a little ugly due to overlap, but now it's only 1m! 
#Maybe we start with just one hour, and then look around from there. 
#It may make sense to do an early morning, noon, and late hour when all is said and done. 

hobo_mcd_12<-filter(hobo_mcd_up,hour =='12')
ggplot(data = hobo_mcd_12) + 
  geom_point(mapping = aes(x = yday, y =mean.temp)) +
  facet_wrap(~location)

#Let's see what kind of bogus we can get up to with filtering
hobo_mcd_4x<-filter(hobo_mcd_up, hour =='0'| hour =='6'|hour =='12'|hour =='18')
ggplot(data = hobo_mcd_4x) + 
  geom_point(mapping = aes(x = yday, y =log(mean.temp))) +
  facet_wrap(hour~location, nrow=4)
#Based on looking at these data, the littoral does seem warmer than the pelagic
#Maybe we can bind this from days 125-260?
#What happened around day 175? Looks like pelagic was warmer than littoral

write.csv(hobo_mcd_up,"C:\\Users\\Quinn\\Documents\\R\\BMWS", row.names = FALSE)

#Trying simple way

mcd_lit <- filter(hobo_mcd_up,location =='littoral')
mcd_pel <- filter(hobo_mcd_up,location =='pelagic')

mod1 <- lm(mcd_lit$mean.temp ~ mcd_pel$mean.temp)

#Differing lengths here

#gotta work with this data 
#Lets go back to hobo_mcd_up and make some more columns, let's turn littoral and 0.5m pelagic into a column

hobo_mcd_up = as_tibble(hobo_mcd_up)
hobo_mcd_up

#For MDY things 
library(lubridate)

wide_hobo_mcd_up = hobo_mcd_up %>%
  pivot_wider(names_from = 'location', values_from = mean.temp) %>%
  rename(littoral_meantemp = littoral, 
         pelagic_meantemp = pelagic)
wide_hobo_mcd_up

hobo_mcd_up_w = wide_hobo_mcd_up %>%
  pivot_wider(names_from = 'depth.m', values_from = pelagic_meantemp)
  

lm <- lm(1~littoral_meantemp, hobo_mcd_up_w)

write.csv(hobo_mcd_up_w, "C:\\Users\\Quinn\\Documents\\R\\BMWS")
