install.packages("tidyr")
install.packages("sqldf")
library(tidyr)
texas_cities_data<- read.csv("texas-cities.csv", header = T)
View(texas_cities_data)
cities_to_country<-read.csv("cities-to-county.csv", header = T)

prediction_dat<- merge(texas_cities_data, cities_to_country, by = "CampaignId", all = TRUE)
#View(texas_cities_data)
dataframe1<-data.frame(texas_cities_data)


#View(dataframe1)
colnames(dataframe1) <- c("Name", "2010","2011","2012","2013","2014","2015","2016","2017")
View(dataframe1)

cities_to_country<- read.csv("cities-to-county.csv", header = T)
dataframe_country<-data.frame(cities_to_country)
View(dataframe_country)
colnames(dataframe_country)[2] <- "Name"


#Texas_Cities_Country<- merge(dataframe1, dataframe_country, by = "Category", all. = TRUE)

levels(dataframe_country$Name) <- tolower(levels(dataframe_country$Name))
levels(dataframe_cities$Name) <- tolower(levels(dataframe_cities$Name))

View(dataframe_cities)
Texas_Cities_Country <- merge(dataframe_cities,dataframe_country,by.dataframe_cities="Name",by.dataframe_country="Name")
View(Texas_Cities_Country)
dataframe_cities_country<-data.frame(Texas_Cities_Country)
View(dataframe_cities_country)

newdata <-  sqldf('select * from dataframe_cities_country where Y_2011 != na AND Y_2011 != na')
View(newdata)

total <- merge(dataframe_cities,dataframe_country,by="Name")
View(total)

companies1 = merge(dataframe_cities, dataframe_country, by.x=c("Name"), by.y=c("Name"))
View(companies1)

dataframe2<-data.frame(separate(data = dataframe1, col = Name, into = c("City_town_village", "State"), sep = ","))
View(dataframe2)

dataframe3<-data.frame(separate(data = dataframe2, col = City_town_village, into = c("City" ,"town","village","v1"), sep = " "))
View(dataframe3)

library(sqldf)
colnames(dataframe3)[1] <- "city_name"
colnames(dataframe3)[2] <- "t_1"
colnames(dataframe3)[3] <- "v_1"


newdata <-  sqldf('select * from dataframe3 where t_1 = city')

newdata <- dataframe3[dataframe3$t_1 = "city" ]

library(stringr)
library(dplyr)

dataframe4<-data.frame(dataframe3 %>%
  filter(str_detect(t_1, "city")))
View(dataframe4)

dataframe4<-data.frame(dataframe3 %>%
                         filter(str_detect(t_1, "city")))


dataframe4[3:4] <- NULL 
dataframe5[4] <- NULL 

dataframe5<-data.frame(dataframe3 %>%
                         filter(str_detect(v_1, "city")))
View(dataframe5)

dataframe6<-data.frame(dataframe3 %>%
                         filter(str_detect(v1, "city")))
View(dataframe6)

dataframe_cities <- rbind(dataframe4, dataframe5, dataframe6)
View(dataframe_cities)
View(dataframe3)

dataframe7<-data.frame(dataframe3 %>%
                         filter(str_detect(t_1, "town")))
dataframe10<-data.frame(dataframe3 %>%
                         filter(str_detect(v_1, "town")))
View(dataframe7)
dataframe10[4] <- NULL

dataframe10$city_name <- paste(dataframe10$city_name," ",dataframe10$t_1)
dataframe10[2] <- NULL

colnames(dataframe10)[2] <- "t_1"

dataframe_town <- rbind(dataframe7, dataframe10)
View(dataframe_town)

View(dataframe8)
dataframe7[3:4] <- NULL

dataframe8<-data.frame(dataframe3 %>%
                         filter(str_detect(v_1, "village")))
dataframe8[4] <- NULL

dataframe11<-data.frame(dataframe3 %>%
                          filter(str_detect(t_1, "village")))
View(dataframe8)
dataframe11[3] <- NULL

View(dataframe11)

dataframe8$city_name <- paste(dataframe8$city_name," ",dataframe8$t_1)
dataframe8[2] <- NULL

colnames(dataframe8)[2] <- "t_1"

dataframe_village <- rbind(dataframe8, dataframe11)
View(dataframe_village)

dataframe9$city_name<-data.frame(within(dataframe5, city <- paste(city_name, t_1, sep=' ')))
View(dataframe9)
#dataframe5$city<-c("city_name","t_1")

dataframe5$city_name <- paste(dataframe5$city_name," ",dataframe5$t_1)
View(dataframe5)
dataframe5[2] <- NULL 
dataframe5[13] <- NULL 

dataframe6$city_name <- paste(dataframe6$city_name," ",dataframe6$t_1," ", dataframe6$v_1)
View(dataframe6)
dataframe6[2] <- NULL
dataframe6[2] <- NULL

colnames(dataframe4)[2] <- "v1"
colnames(dataframe5)[2] <- "v1"
#dataframe5[12] <- NULL
dataframe_cities <- rbind(dataframe4, dataframe5, dataframe6)
View(dataframe_cities)
colnames(dataframe_cities) <- c("Name","Category","State","Y_2010","Y_2011","Y_2012","Y_2013","Y_2014","Y_2015","Y_2016","Y_2017")
colnames(dataframe_town) <- c("Name","Category","State","Y_2010","Y_2011","Y_2012","Y_2013","Y_2014","Y_2015","Y_2016","Y_2017")
colnames(dataframe_village) <- c("Name","Category","State","Y_2010","Y_2011","Y_2012","Y_2013","Y_2014","Y_2015","Y_2016","Y_2017")

threecities<-  data.frame(head(dataframe_cities))
View(threecities)
newdata <-  sqldf('select city_name from dataframe3 where t_1 = city')

newdata <- dataframe_cities[order(-Y_2010, -Y_2011, -Y_2012, -Y_2013, -Y_2014, -Y_2015, -Y_2016,-Y_2017)] 


d1_cities<-data.frame(dataframe_cities[
  with(dataframe_cities, order(-Y_2011)),
  ])
View(d1_cities)

View(head(d1_cities, n = 1L))

d2_town<-data.frame(dataframe_town[
  with(dataframe_town, order(-Y_2011)),
  ])
View(d2_town)

View(head(d2_town, n = 1L))

d2_village<-data.frame(dataframe_village[
  with(dataframe_village, order(-Y_2011)),
  ])
View(d2_village)

View(head(d2_village, n = 1L))


d1_cities<-data.frame(dataframe_cities[
  with(dataframe_cities, order(-Y_2011)),
  ])


threecities<-data.frame((head(d1_cities, n = 3L)))
View(threecities)

threecities_2012_2016 <- data.frame(threecities[c(1:3,6:10)])
View(threecities_2012_2016)

data[is.na(data)] <- 0

sum_2017_cities<-sum(dataframe_cities$Y_2017)

sum_2017_towns<-sum(dataframe_town$Y_2017)

sum_2017_villages<-sum(dataframe_village$Y_2017)

Total_2017_population<- sum_2017_cities+sum_2017_towns+sum_2017_villages

Percentage_of_texans_in_towns<-(sum_2017_towns/Total_2017_population)*100
View(Percentage_of_texans_in_towns)

dataframe_village <- rbind(dataframe8, dataframe11)
Texas_total_population<-rbind(dataframe_cities,dataframe_town, dataframe_village )
df_texas<-data.frame(Texas_total_population)
View(df_texas)

all_sums<-data.frame(colSums(Filter(is.numeric, df_texas)))
View(all_sums)
colnames(all_sums)<-c("Total_population")
View(apply( all_sums , 2 , diff ))

all_sums[ , diff := value - shift(value), by = Total_population]
max_growth<-data.frame(diff(all_sums$Total_population))
colnames(max_growth)<-c("max_difference")
View(max_growth)

max_growth_texas<-data.frame(max_growth[
  with(max_growth, order(-max_difference)),
  ])
View(head(max_growth_texas, n = 1L))

