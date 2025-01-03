
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

del2Draft= base + geom_histogram(aes(x=Student.Teacher.Ratio))
del2Draft


# save del2Draft ----------------------------------------------------------
saveRDS(del2Draft, file = "del2Draft.rds")


# deliverable 3 ----------------------------------------------------------

del3Draft= base + geom_point(aes(x=Student.Teacher.Ratio,
                                 y=Free.Lunch))
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