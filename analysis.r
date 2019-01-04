library(tidyr)
library(ggplot2)
library(ggthemes)
library(rvest)
library(stringr)
library(reshape2)
#Base URL for National Jeweler State of the Majors Report

baseurl = ("https://www.nationaljeweler.com/media/stateofthemajors/2018/inc/html/")


#Get data for top 50 specialty jewelry stores

specialtyURL = c("34","35","36","38","39","40","42","43","44","46")
specialtyData = data.frame()

for (items in specialtyURL) { 
  url = paste(baseurl,items,".html",sep = "")
  site = read_html(url)
  job = site %>%
  html_nodes("div") %>%
  html_text()
  job = job[2]
  outcome = str_split(job,"[:digit:]{1}\\.\\s|\\(tie\\)", simplify = TRUE)
  specialtyData = rbind(specialtyData,t(outcome))
}

#supersellers data -- retailers that sold $100M+ in Jewelry and Watches in North America in latest FY

supersellersURL = c("20","21","22","24","26","27","28","30","31","32")
supersellerData = data.frame()

for (items in supersellersURL) { 
  url = paste(baseurl,items,".html",sep = "")
  site = read_html(url)
  job = site %>%
    html_nodes("div") %>%
    html_text()
  job = job[2]
  outcome = str_split(job,"[:digit:]{1}\\.\\s|\\(tie\\)", simplify = TRUE)
  supersellerData = rbind(supersellerData,t(outcome))
}

# Get company names and Watches sold separated out

#supersellers

supersellerList = list()
supersellerWatches = list()

for (stores in supersellerData$V1) {
supersellerList = paste(supersellerList,str_split(stores,"[:digit:]")[[1]][1],sep = ",")
supersellerWatches = paste(supersellerWatches, str_split(stores,".*Watches:\\s*|\\. WEB.*")[[1]][2], sep = ";")
}

supersellerList = as.list(str_split(supersellerList,","))[[1]]
supersellerWatches = as.list(str_split(supersellerWatches,";"))[[1]]
supersellerWatches[27] = ""

supersellerList = supersellerList[-c(1,2,5,8:32,36,41,47,48,53,57,60,63,64,65,69,72,75,81,84,85)]
supersellerWatches = supersellerWatches[!supersellerWatches=="NA"]
supersellerWatches = supersellerWatches[-c(1)]

#Specialty

specialtyList = list()
specialtyWatches = list()

for (stores in specialtyData$V1) {
  specialtyList = paste(specialtyList,str_split(stores,"[:digit:]")[[1]][1],sep = ",")
  specialtyWatches = paste(specialtyWatches, str_split(stores,".*Watches:\\s*|\\. WEB.*")[[1]][2], sep = ";")
}

specialtyList = as.list(str_split(specialtyList,","))[[1]]
specialtyWatches = as.list(str_split(specialtyWatches,";"))[[1]]

specialtyList = specialtyList[-c(1,2,6,12,18,25,29,31:33,35,38,42,50,53,58,60,67)]
specialtyWatches = specialtyWatches[-c(1)]
specialtyWatches = specialtyWatches[!specialtyWatches=="NA"]


# Now merge the two lists

supersellers = as.data.frame(cbind(supersellerList,supersellerWatches),stringsAsFactors = FALSE)
supersellers$supersellerWatches = as.list(sapply(supersellers$supersellerWatches, str_split,","))
supersellers = unnest(supersellers)
supersellers[which(supersellers$supersellerWatches == "IWC"),2] = "IWC Schaffhausen"
supersellers$supersellerWatches = trimws(supersellers$supersellerWatches)
supersellers = rbind(supersellers, c("AMAZON.COM INC.","Casio"))
supersellers[87,2] = "Bulova"
supersellers$supersellerWatches = trimws(supersellers$supersellerWatches)

specialty = as.data.frame(cbind(specialtyList, specialtyWatches),stringsAsFactors = FALSE)
specialty$specialtyWatches = as.list(sapply(specialty$specialtyWatches, str_split,","))
specialty = unnest(specialty)
specialty[which(specialty$specialtyWatches == "IWC"),2] = "IWC Schaffhausen"
specialty$specialtyWatches = trimws(specialty$specialtyWatches)
specialty[54,2] = "Tissot"
specialty$specialtyWatches = trimws(specialty$specialtyWatches)
specialty[which(specialty$specialtyWatches == "IWC Schauffhausen"),]$specialtyWatches = "IWC Schaffhausen"
specialty[which(specialty$specialtyWatches == "IWC"),]$specialtyWatches = "IWC Schaffhausen"
specialty[which(specialty$specialtyWatches == "Jaeger LeCoultre"),]$specialtyWatches = "Jaeger-LeCoultre"
specialty[which(specialty$specialtyWatches == "A. Lange and Sohne"),]$specialtyWatches = "A. Lange & Söhne"
specialty[which(specialty$specialtyWatches == "Tag Heuer"),]$specialtyWatches = "TAG Heuer"
specialty[which(specialty$specialtyWatches == "Frederique Constant"),]$specialtyWatches = "Frédérique Constant"
specialty[which(specialty$specialtyWatches == "Baume and Mercier"),]$specialtyWatches = "Baume & Mercier"
specialty[which(specialty$specialtyWatches == "Dior Watches"),]$specialtyWatches = "Dior"
specialty[which(specialty$specialtyWatches == "G-Shock by Casio"),]$specialtyWatches = "G-Shock"
specialty[which(specialty$specialtyWatches == "Baby G"),]$specialtyWatches = "Baby-G"

topWatch1 = table(supersellers$supersellerWatches)
topWatch1 = as.data.frame(topWatch1[-c(1)])
topWatch1 = topWatch1[order(-topWatch1$Freq),]
names(topWatch1) = c("Brand","Freq")

p1 = ggplot(data = topWatch1[1:30,], aes(x = reorder(Brand,Freq), y = Freq)) + 
  geom_bar(stat = "identity") + ggtitle("2018: Top 30 Watch Brands\nCarried by Retailers\nSelling more than $100M") +
  ylab("Number of Retailers Carrying Brand") + 
  xlab("Brand") 
p1 + coord_flip() + theme_wsj()

topWatch2 = table(specialty$specialtyWatches)
topWatch2 = as.data.frame(topWatch2[-c(1)])
topWatch2 = topWatch2[order(-topWatch2$Freq),]
names(topWatch2) = c("Brand","Freq")

p2 = ggplot(data = topWatch2[1:30,], aes(x = reorder(Brand,Freq), y = Freq)) + 
  geom_bar(stat = "identity") + ggtitle("2018: Top 30 Watch Brands\nCarried by Top 50\nSpecialty Jewelry Chains") +
  ylab("Number of Retailers Carrying Brand") + 
  xlab("Brand") 
p2 + coord_flip() + theme_wsj()


# Historical Data

# 2017 Specialty Data
baseurl = ("https://www.nationaljeweler.com/media/stateofthemajors/2017/inc/html/")

specialtyURL2017 = c("32","33","34","36","38","40","41","42","44","46")
specialtyData2017 = data.frame()

for (items in specialtyURL2017) { 
  url = paste(baseurl,items,".html",sep = "")
  site = read_html(url)
  job = site %>%
    html_nodes("div") %>%
    html_text()
  job = job[2]
  outcome = str_split(job,"[:digit:]{1}\\.\\s|\\(TIE\\)", simplify = TRUE)
  specialtyData2017 = rbind(specialtyData2017,t(outcome))
}

#Specialty 2017

specialtyList2017 = list()
specialtyWatches2017 = list()

for (stores in specialtyData2017$V1) {
  specialtyList2017 = paste(specialtyList2017,str_split(stores,"[:digit:]")[[1]][1],sep = ",")
  specialtyWatches2017 = paste(specialtyWatches2017, str_split(stores,".*Watches:\\s*|\\. WEB.*")[[1]][2], sep = ";")
}

specialtyList2017 = as.list(str_split(specialtyList2017,","))[[1]]
specialtyWatches2017 = as.list(str_split(specialtyWatches2017,";"))[[1]]

specialtyList2017 = specialtyList2017[-c(1,2,6,12,19,23,27,29,34,41,45,49,53,55)]
specialtyWatches2017 = specialtyWatches2017[-c(1)]
specialtyWatches2017 = specialtyWatches2017[!specialtyWatches2017=="NA"]

specialty2017 = as.data.frame(cbind(specialtyList2017, specialtyWatches2017),stringsAsFactors = FALSE)
specialty2017$specialtyWatches2017 = as.list(sapply(specialty2017$specialtyWatches2017, str_split,","))
specialty2017 = unnest(specialty2017)
specialty2017$specialtyWatches2017 = trimws(specialty2017$specialtyWatches2017)
specialty2017[which(specialty2017$specialtyWatches2017 == "IWC Schauffhausen"),]$specialtyWatches2017 = "IWC Schaffhausen"
specialty2017[which(specialty2017$specialtyWatches2017 == "IWC"),]$specialtyWatches2017 = "IWC Schaffhausen"
specialty2017[which(specialty2017$specialtyWatches2017 == "Jaeger LeCoultre"),]$specialtyWatches2017 = "Jaeger-LeCoultre"
specialty2017[which(specialty2017$specialtyWatches2017 == "A. Lange and Sohne"),]$specialtyWatches2017 = "A. Lange & Söhne"
specialty2017[which(specialty2017$specialtyWatches2017 == "Tag Heuer"),]$specialtyWatches2017 = "TAG Heuer"
specialty2017[which(specialty2017$specialtyWatches2017 == "Bell& Ross"),]$specialtyWatches2017 = "Bell & Ross"
specialty2017[which(specialty2017$specialtyWatches2017 == "Frederique Constant"),]$specialtyWatches2017 = "Frédérique Constant"
specialty2017[which(specialty2017$specialtyWatches2017 == "Baume and Mercier"),]$specialtyWatches2017 = "Baume & Mercier"
specialty2017[which(specialty2017$specialtyWatches2017 == "Dior Watches"),]$specialtyWatches2017 = "Dior"
specialty2017[which(specialty2017$specialtyWatches2017 == "G-Shock by Casio"),]$specialtyWatches2017 = "G-Shock"
specialty2017[which(specialty2017$specialtyWatches2017 == "Tissot. Retail Store Brands: Tourbillon Boutique"),]$specialtyWatches2017 = "Tissot"

topWatch2017 = table(specialty2017$specialtyWatches2017)
topWatch2017 = as.data.frame(topWatch2017[-c(1)])
topWatch2017 = topWatch2017[order(-topWatch2017$Freq),]
names(topWatch2017) = c("Brand","Freq")

p3 = ggplot(data = topWatch2017[1:30,], aes(x = reorder(Brand,Freq), y = Freq)) + 
  geom_bar(stat = "identity") + ggtitle("2017: Top 30 Watch Brands\nCarried by Top 50\nSpecialty Jewelry Chains") +
  ylab("Number of Retailers Carrying Brand") + 
  xlab("Brand") 
p3 + coord_flip() + theme_wsj()

#Specialty 2016

#Storing test code
# Data drawn from: testurl = ("http://www.nationaljeweler.com/media/stateofthemajors/2016/?page=30")
# Using the below code and then re-ingested as dedicated data file
# site = read_html(testurl)
# job = site %>%
# html_nodes("script") %>%
# html_text()
# write.csv2(job,"2016Data.txt")

specialty2016 = read.csv2("2016Specialty.txt")

names(specialty2016) = c("Combined")
specialty2016 = specialty2016 %>% separate(Combined, c("Stores","Brand"), ",",fill = "right", extra = "merge")
specialty2016$Brand = as.list(sapply(specialty2016$Brand, str_split,","))
specialty2016 = unnest(specialty2016)
specialty2016[which(specialty2016$Brand == " Company\\u2019s own brand."),]$Brand = "Company's own brand."
specialty2016$Brand = trimws(specialty2016$Brand)
specialty2016[which(specialty2016$Brand == "Tag Heuer"),]$Brand = "TAG Heuer"
specialty2016[which(specialty2016$Brand == "Victorinox Swiss Army"),]$Brand = "Victorinox"
specialty2016[which(specialty2016$Brand == "Victornox Swiss Army"),]$Brand = "Victorinox"
specialty2016[which(specialty2016$Brand == "A. Lange and Sohne"),]$Brand = "A. Lange & Söhne"
specialty2016[which(specialty2016$Brand == "A. Lange & S\\u00f6hne"),]$Brand = "A. Lange & Söhne"
specialty2016[which(specialty2016$Brand == "Baume and Mercier"),]$Brand = "Baume & Mercier"
specialty2016[which(specialty2016$Brand == "ESQ"),]$Brand = "ESQ by Movado"
specialty2016[which(specialty2016$Brand == "ESQ Movado"),]$Brand = "ESQ by Movado"
specialty2016[which(specialty2016$Brand == "Frederique Constant"),]$Brand = "Frédérique Constant"
specialty2016[which(specialty2016$Brand == "Fr\\u00e9d\\u00e9rique Constant"),]$Brand = "Frédérique Constant"
specialty2016[which(specialty2016$Brand == "IWC Schauffhausen"),]$Brand = "IWC Schaffhausen"
specialty2016[which(specialty2016$Brand == "IWC"),]$Brand = "IWC Schaffhausen"
specialty2016[which(specialty2016$Brand == "Jaeger LeCoultre"),]$Brand = "Jaeger-LeCoultre"
specialty2016[which(specialty2016$Brand == "Dior Watches"),]$Brand = "Dior"

topWatch2016 = table(specialty2016$Brand)
topWatch2016 = as.data.frame(topWatch2016[-c(1)])
topWatch2016 = topWatch2016[order(-topWatch2016$Freq),]
names(topWatch2016) = c("Brand","Freq")

#Merge to plot
timeseries = merge(topWatch2,topWatch2017, by.x = "Brand", by.y = "Brand")
timeseries = merge(timeseries,topWatch2016, by.x = "Brand", by.y = "Brand")
names(timeseries) = c("Brand","2018","2017","2016")

#Find which brands have been picked up by the most retailers
timeseries$Growth = (timeseries$"2018" - timeseries$"2016")
timeseries[which(timeseries$Growth > 2),]


#Only plot top30 from 2018
top30 = as.list(as.character(topWatch2$Brand[1:10]))
timeseries = timeseries[timeseries$Brand %in% top30,]

timeseriesPlot = melt(timeseries, id = "Brand")
timeseriesPlot$variable = as.numeric(as.character(timeseriesPlot$variable))


ggplot(timeseriesPlot, aes(x = variable, y = value, label = Brand)) + 
  geom_line(aes(color = Brand), size = 1) +
  ggtitle("3 Year Trend of Brands Being Carried\nBy Top 50 Specialty Retailers\n") +
  labs(caption = "Only top 10 carried brands of 2018 shown")+
  ylab("Number of Retailers Carrying Brand")+
  geom_label()+ 
  theme_wsj()+
  scale_x_continuous(breaks=seq(2016,2018,1))
