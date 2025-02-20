
# read in data----------
beaches <- read_csv(here("data","sydneybeaches.csv"))


#exploring the data-------

glimpse(beaches)
summary(beaches)
head(beaches)

# there are two problem with the data the data is signed as chart and there are (29)missing values-----

library(lubridate)
beaches$new_date <- dmy(beaches$Date)
clean<- beaches %>%
  clean_names()%>%
  rename(beachbugs=enterococci_cfu_100ml)
#which beach has the most extreme level of bugs-------


worstbugs <- clean %>% arrange(-beachbugs)

# compare max bug values across different beaches 

coogee_bondai <- clean %>%
  filter(site %in% c("Coogee Beach","Bondi Beach")) %>%
  arrange(-beachbugs) 

# conduct summery statistics for all beaches-----

clean %>%
group_by(site)%>%
summarise(maxbug=max(beachbugs, na.rm = TRUE),
          minbug=mean(beachbugs, na.rm = TRUE),
          sdbug=sd(beachbugs, na.rm = TRUE),
          medianbug=median(beachbugs, na.rm=TRUE))

# compare councils-----
councils_by_site <- clean %>%
  group_by(council,site) %>%
  summarise(maxbug=max(beachbugs, na.rm = TRUE),
            meanbug=mean(beachbugs, na.rm = TRUE),
            medianbug=median(beachbugs, na.rm=TRUE))


# separate date into day,month,year-----
testdate <- clean%>%
  separate(new_date,c("year","month","day"))
write.csv(testdate,here("data","testdate.csv"))
 
  
  
  