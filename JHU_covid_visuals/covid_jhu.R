library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)

jhu <- X04_18_2020
jhu <- jhu %>%
  select(-FIPS,-Admin2,-Combined_Key) %>% 
  rename(country = Country_Region) 

jhu$country <- gsub("US", "United States of America", jhu$country)

jhu2 <-  left_join(jhu, UN_regions) %>% 
  select(-UNm49_code, -iso_alpha3)

missing_region <- jhu2[is.na(jhu2$region),]

jhu2$region[jhu2$country == "Bolivia"]<- "South America"
jhu2$region[jhu2$country == "Brunei"]<- "South-eastern Asia"
jhu2$region[jhu2$country == "Burma"]<- "South-eastern Asia"
jhu2$region[jhu2$country == "Congo (Brazzaville)"]<- "Middle Africa"
jhu2$region[jhu2$country == "Congo (Kinshasa)"]<- "Middle Africa"
jhu2$region[jhu2$country == "Cote d'Ivoire"]<- "Southern Africa"
jhu2$region[jhu2$country == "Iran"]<- "Southern Asia"
jhu2$region[jhu2$country == "Korea, South"]<- "Eastern Asia"
jhu2$region[jhu2$country == "Kosovo"]<- "Southern Europe"
jhu2$region[jhu2$country == "Laos"]<- "South-eastern Asia"
jhu2$region[jhu2$country == "Moldova"]<- "Eastern Europe"
jhu2$region[jhu2$country == "Russia"]<- "Eastern Europe"
jhu2$region[jhu2$country == "Syria"]<- "Western Asia"
jhu2$region[jhu2$country == "Taiwan*"]<- "Eastern Asia"
jhu2$region[jhu2$country == "Tanzania"]<- "Eastern Africa"
jhu2$region[jhu2$country == "Venezuela"]<- "South America"
jhu2$region[jhu2$country == "Vietnam"]<- "South-eastern Asia"
jhu2$region[jhu2$country == "West Bank and Gaza"]<- "Western Asia"
jhu2$region[is.na(jhu2$region)]<- "cruise_ship"

region_summaries <- jhu2 %>% 
  group_by(region) %>% 
  summarize(region_cases = sum(Confirmed), 
            region_deaths = sum(Deaths), 
            region_recovered = sum(Recovered), 
            region_active =sum(Active))

highest_deaths <- top_n(region_summaries, 5, wt=region_deaths)
highest_cases <-top_n(region_summaries, 5, wt = region_cases)
highest_recovered <- top_n(region_summaries, 5, wt = region_recovered)

jhu3 <- jhu2 %>% 
  group_by(country) %>% 
  mutate(cases = sum(Confirmed), deaths = sum(Deaths),
         recovered = sum(Recovered), active = sum(Active)) %>% 
  select(-Province_State, 
         -Confirmed, 
         -Deaths, 
         -Recovered,
         -Active) %>% 
  distinct(country, .keep_all = TRUE) %>% 
  ungroup()

ggplot(jhu3, aes(cases, deaths)) +
  geom_point()+
  facet_wrap(~region)

top6 <- top_n(jhu3, n = 6, wt = cases)

world_map <- map_data("world")

jhu4<- jhu3 %>% 
  select(-region, -Last_Update, 
         -Lat, -Long_) %>% 
  rename(region = country)

jhu4$region <- gsub("United States of America", "USA", jhu4$region)
jhu4$region <- gsub("United Kingdom", "UK", jhu4$region)
jhu4$region <- gsub("Antigua and Barbuda", "Antigua", jhu4$region)
jhu4$region <- gsub("Burma", "Myanmar", jhu4$region)
jhu4$region <- gsub("Cabo Verde", "Cape Verde", jhu4$region)
jhu4$region <- gsub("Congo (Brazzaville)", "Republic of Congo", jhu4$region, fixed = TRUE)
jhu4$region <- gsub("Congo (Kinshasa)", "Democratic Republic of the Congo", jhu4$region, fixed = TRUE)
jhu4$region <- gsub("Cote d'Ivoire", "Ivory Coast", jhu4$region, fixed = TRUE)
jhu4$region <- gsub("Czechia", "Czech Republic", jhu4$region, fixed = TRUE)
jhu4$region <- gsub("Eswatini", "Swaziland", jhu4$region, fixed = TRUE)
jhu4$region <- gsub("Holy See", "Democratic Republic of the Congo", jhu4$region, fixed = TRUE)
jhu4$region <- gsub("Korea, South", "South Korea", jhu4$region, fixed = TRUE)
jhu4$region <- gsub("North Macedonia", "Macedonia", jhu4$region, fixed = TRUE)
jhu4$region <- gsub("Saint Kitts and Nevis", "Saint Kitts", jhu4$region, fixed = TRUE)
jhu4$region <- gsub("Saint Vincent and the Grenadines", "Saint Vincent", jhu4$region, fixed = TRUE)
jhu4$region <- gsub("Taiwan*", "Taiwan", jhu4$region, fixed = TRUE)
jhu4$region <- gsub("Trinidad and Tobago", "Trinidad", jhu4$region, fixed = TRUE)
jhu4$region <- gsub("West Bank and Gaza", "Palestine", jhu4$region, fixed = TRUE)


  

jhu.map <- left_join(jhu4, world_map, by = "region")
jhu.map2 <- jhu.map%>% 
  filter(region != "Democratic Republic of the Congo") %>% 
  bind_rows(drc2)


missing_cordinates <- jhu.map %>% 
  filter(is.na(long))

ggplot(jhu.map2, aes(long, lat, group = group))+
  geom_polygon(aes(fill = cases/1000 ), color = "white")+
  scale_fill_viridis_c(option = "D") +
  theme_classic()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())+
  labs(title ="Number of cases(thousands of people) on 18 April", fill = "")


ggplot(jhu.map2, aes(long, lat, group = group))+
  geom_polygon(aes(fill = deaths ), color = "white")+
  scale_fill_viridis_c(option = "D") +
  theme_classic()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())+
  labs(title ="Number of people who have died from Covid19 (22-Jan to 18-Apr)", fill = "")
 


ggplot(jhu.map2, aes(long, lat, group = group))+
  geom_polygon(aes(fill = recovered ), color = "white")+
  scale_fill_viridis_c(option = "D") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())+
  labs(title ="Number of people who have recovered  22-Jan to 18-Apr", fill = "")

congo<- jhu.map[grep("Congo", jhu.map$region), ]
drc <- congo[grep("Democratic", congo$region),]

ggplot(drc2, aes(long, lat, group = group))+
  geom_polygon(aes(fill= cases), color = "black")+
  scale_fill_viridis_c(option = "B")

drc2<- drc %>% 
  distinct(order, .keep_all = TRUE)

cases_jhu_time <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
deaths_jhu_time <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

ts_jhu<- cases_jhu_time %>% 
  select(-Lat, -Long, -Province.State) %>% 
  pivot_longer(cols = 2:90, names_to =  "date")
ts_deaths <-deaths_jhu_time %>% 
  select(-Lat, -Long, -Province.State) %>% 
  pivot_longer(cols = 2:90, names_to = "date")

ts_jhu$date <- gsub("x", "", ts_jhu$date, ignore.case = TRUE)
ts_deaths$date <- gsub("x", "", ts_deaths$date, ignore.case = TRUE)

ts_jhu$date <- mdy(ts_jhu$date) 
ts_deaths$date <- mdy(ts_deaths$date) 

ts_jhu_top5 <- ts_jhu %>% 
  filter(date >= "2020-04-19") %>% 
  top_n(n = 5)

ts_top5 <- ts_jhu %>% 
  filter(Country.Region %in% ts_jhu_top5$Country.Region) %>% 
  mutate(thousands_of_cases = value/1000)

deaths_top5 <- ts_deaths %>% 
  filter(Country.Region %in% ts_jhu_top5$Country.Region) %>% 
  rename(deaths = value)

ggplot(ts_top5, aes(x= date, y = thousands_of_cases))+ 
  geom_line(size = 1.25, color = "#00688B")+
  facet_grid(Country.Region~.) +
  theme_bw()+
  ggtitle("Number of cases from January 22 to April 18")


ggplot(deaths_top5, aes(x= date, y = deaths))+ 
  geom_line(size = 1.25, color = "#00688B")+
  facet_grid(Country.Region~.) +
  theme_bw()+
  ggtitle("Number of deaths from January 22 to April 18")


