
# clean memory ------------------------------------------------------------
rm(list = ls())


# read in data ------------------------------------------------------------
#set working directory

filename="eduwa.csv"
mydata=read.csv(filename)


# see data ----------------------------------------------------------
view(mydata)

head(mydata)


# see data types ----------------------------------------------------------




str(mydata)
table(mydata$LocaleType)
suburbEduwa=mydata[mydata$LocaleType=='Suburb',]
table(suburbEduwa$LocaleSub)
library(ggplot2)
library(scales)




absoluteT = table(suburbEduwa$LocaleSub, exclude = 'nothing')
names(absoluteT)[4] = 'Unknown'
propT = prop.table(absoluteT) * 100

tableFreq = as.data.frame(absoluteT)
names(tableFreq) = c("Size", "Count")
tableFreq$Percent = as.vector(propT)
tableFreq

# deliverable 1 ----------------------------------------------------------
library(ggplot2)


base=ggplot(data = tableFreq, 
            aes(x = reorder(Size, Percent), y = Percent))
base = base + theme_classic()
del1Draft = base + geom_bar(fill = "grey", stat = 'identity') +
  labs(title= "Distribution of Washington Public Schools by Suburb Size (2019)",
       x =NULL, 
       y = NULL,
       caption = 'Source: US Department of Education') +
  geom_text(vjust = 0, size = 5, aes(y = Percent, label = paste0(round(Percent, 2), '%'))) +
  scale_y_continuous(breaks=c(0,20, 40,60),
                     limits = c(0, 70), 
                     labels=unit_format(suffix = '%'))
del1Draft


# save del1Draft ----------------------------------------------------------
saveRDS(del1Draft, file = "del1Draft.rds")


# deliverable 2 ----------------------------------------------------------
linkMass="https://github.com/DACSS-Visual/tabular_bivar_catcat/raw/refs/heads/main/data/MSP%20DFS%20Arrests%2019-20Q1.xlsx"

library(rio)
arrests=rio::import(linkMass,which = 1)
head(arrests)
names(arrests)
(Coderace=table(arrests$`Arrest Offense by UCR Code`,arrests$Race))
library(magrittr) # for %>%
(mgCol=prop.table(Coderace,
                            margin = 2)%>%round(.,3))
mgCol

Racedf=as.data.frame(Coderace)
names(Racedf)=c("ucr","race","counts")
Racedf$pctCol=100*as.data.frame(mgCol)[,3]
Racedf
## 

del2Draft= base + geom_histogram(aes(x=Student.Teacher.Ratio))
del2Draft
library(ggplot2)
baseRE  = ggplot(Racedf, 
                 aes(x = reorder(ucr, pctCol), #here
                     y = pctCol ) ) + theme_minimal()

barsRE = baseRE + geom_bar( stat = "identity" ) 
Base = barsRE + facet_grid( ~ race) 
Base = Base + coord_flip() 

del2Draft = Base + theme(axis.text.y = element_text(size=7,angle = 20)) + 
  geom_text(aes(label=ifelse(pctCol>5,# condition to annotate
                             pctCol,"")),
            nudge_y = 4) +
  labs(title= "Frequency of MA State Police Arrest Offense UCR Codes by Race",
       subtitle = "From January 2019 to March 2020",
  labs(title= "MV : Most Common UCR Code among all Races",
       subtitle = "Frequency of MA State Police Arrest Offense UCR Codes by Race from January 2019 to March 2020",
       caption = "Source : Mass.gov",
       x="",y="Percent (%)")

del2Draft
readRDS("del2Draft.rds")

# save del2Draft ----------------------------------------------------------
saveRDS(del2Draft, file = "del2Draft.rds")


# deliverable 3 ----------------------------------------------------------

del3Draft= base + geom_point(aes(x=Student.Teacher.Ratio,
                                 y=Free.Lunch))
del3Draft 
##DATA
LinkBoston = "https://github.com/DACSS-Visual/SpatialData/raw/refs/heads/main/data/BostonContrib.xlsx"
bostonCont=rio::import(LinkBoston)
head(bostonCont)
names(bostonCont)
str(bostonCont,width = 60, strict.width = 'cut')




##ZIPCODE DATA
library(sf)
linkZips='https://raw.githubusercontent.com/DACSS-Visual/SpatialData/refs/heads/main/data/zip_codes.json'
BostonZips=sf::read_sf(linkZips)
head(BostonZips)
plot = plot(BostonZips[2])
names(BostonZips)


table(bostonCont$'Tender Type Description')
filtered_data <- bostonCont %>%
  filter(`Tender Type Description` %in% c("Cash", "Credit Card"))
head(filtered_data)
table(filtered_data$'Tender Type Description')
names(filtered_data)


aggdata2= filtered_data %>%
  group_by(Zip, `Tender Type Description`) %>%
  summarise_at(vars(Amount),
               list(counts=length,
                    amountPerCap=mean))


length(setdiff(aggdata2$Zip, BostonZips$ZIP5))
length(setdiff(BostonZips$ZIP5,aggdata2$Zip))

contrib_zipMap2=merge(BostonZips,aggdata2,
                     by.x='ZIP5', # 
                     by.y='Zip')
head(contrib_zipMap2)

base=ggplot() + theme_void() 
del3Draft= base + geom_sf(data=contrib_zipMap2,
               aes(fill=amountPerCap)) + 
  scale_fill_viridis_c(direction = -1,
                       na.value = 'red') + # missing in red?
  facet_grid(~`Tender Type Description`) +
  labs(fill='Contribution of Tender Type \nby Zip Code',
       title='Credit Card Contributions More Popular than Cash in Boston',
       subtitle='Boston MA Zip Codes')





del3Draft


# save del3Draft ----------------------------------------------------------
saveRDS(del3Draft, file = "del3Draft.rds")


# deliverable 4  ----------------------------------------------------------

library(sf)
county_map=sf::read_sf("WA_County_Boundaries.geojson")
head(county_map)
head(mydata)

# merge data into map ----------------------------------------------------------
mydataCounty=aggregate(data=mydata,Free.Lunch~County,FUN = mean)
myMapLunch=merge(county_map,mydataCounty,by.x='JURISDIC_2',"County")

# prepare plot

base=ggplot(myMapLunch)
del4Draft=base + geom_sf(aes(fill=Free.Lunch))
del4Draft

# save del4Draft ----------------------------------------------------------
saveRDS(del4Draft, file = "del4Draft.rds")