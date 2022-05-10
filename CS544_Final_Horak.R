####Final Project
####CS 544
####Nathan Horak

###Functions
differentiateMeteor <- function (x) {
  ifelse (x %in% undiff.list,'Undifferentiated','Differentiated')
}

undifferentiateType <- function (x) {
  if (x %in% carbon.list)
    return ('Carbonaceous')
  if (x %in% ordinary.list)
    return ('Ordinary')
  if (x %in% rumuruti.list)
    return ('Rumuruti (R)')
  if (x %in% enstatite.list)
    return ('Enstatite')
  else
    return (NA)
}

ordinaryType <- function (x) {
  if (x %in% h.list)
    return ('H Meteorite')
  if (x %in% l.list)
    return ('L Meteorite')
  if (x %in% ll.list)
    return ('LL Meteorite')
  else
    return (NA)
}

lowestClassCheck <- function(x) {
  if (x %in% carbon.list)
    return ('Carbonaceous')
  if (x %in% rumuruti.list)
    return ('Rumuruti (R)')
  if (x %in% enstatite.list)
    return ('Enstatite')
  if (x %in% h.list)
    return ('H Meteorite')
  if (x %in% l.list)
    return ('L Meteorite')
  if (x %in% ll.list)
    return ('LL Meteorite')
  else
    return ('Differentiated')
  
}

###Setup
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1Ijoia2FsYXRodXIiLCJhIjoiY2twenk4YmNjMDhydjJ2cmthM3B4Nnh0MiJ9.q-S1vQKIJu33aSKWP7tu8w')
options(scipen= 7)
set.seed(101)
library(stringr)
library(tidyverse)
library(plotly)
library(ggplot2)

###Data Preparation
old_meteor <- as_tibble(read.csv("C:\\Users\\Nathan\\Documents\\Grad School\\R\\MET CS 544\\project\\Meteorite-Landings.csv"))
meteor <- old_meteor
undiff.list <- unique(c(meteor[str_detect(as.matrix(meteor$recclass),c('^C|^E|^H|^L|[0-9]|OC')),]$recclass))
undiff.list <- undiff.list[str_detect(undiff.list,c('ite|Lunar|achon'),negate=TRUE)]
carbon.list <- undiff.list[str_detect(undiff.list,c('^C'))]
ordinary.list <- undiff.list[str_detect(undiff.list,c('^H|^L'))]
rumuruti.list <- undiff.list[str_detect(undiff.list,c('^R'))]
enstatite.list <- undiff.list[str_detect(undiff.list,c('^E'))]
h.list <- ordinary.list[str_detect(ordinary.list,c('^H'))]
ll.list <- ordinary.list[str_detect(ordinary.list,c('^LL'))]
l.list <- ordinary.list[str_detect(ordinary.list,c('^L'))]
l.list <- l.list[str_detect(l.list,c('^LL'),negate=TRUE)]
meteor <- meteor %>%
  mutate(Differentiated=sapply(meteor$recclass, differentiateMeteor),
    Undiff_Type=sapply(meteor$recclass, undifferentiateType),
    Ordinary_Type=sapply(meteor$recclass, ordinaryType),
    Lowest_Class=sapply(meteor$recclass, lowestClassCheck))

meteor <- meteor %>%
  mutate(year=format(as.Date(meteor$year, '%m/%d/%Y'), '%Y'))
meteor <- select(meteor, Name=name, Year=year, Fell_Status=fall, Mass=mass..g., Rec_Class=recclass, Differentiated, Undiff_Type, Ordinary_Type, Lowest_Class, Rec_Lat= reclat, Rec_Long= reclong)
meteor <- meteor %>% filter(!is.na(Mass))

###Analysis
##Barplots of Frequency by Year
year.freq <- data.frame(table(meteor$Year))
plot_ly(x=year.freq$Var1, y=year.freq$Freq, name='Frequency of Meteorites by Year', type='bar') %>%
  layout(yaxis= list(title='Frequency',type='log'), xaxis=list(title='Year'),
         title='Logarithmic Scaling of Meteorites Per Year') -> year.barp
year.barp

d.year.freq <- data.frame(table(meteor$Year[meteor$Differentiated == 'Differentiated']))
c.year.freq <- data.frame(table(meteor$Year[meteor$Undiff_Type == 'Carbonaceous']))
o.year.freq <- data.frame(table(meteor$Year[meteor$Undiff_Type == 'Ordinary']))
r.year.freq <- data.frame(table(meteor$Year[meteor$Undiff_Type == 'Rumuruti (R)']))
e.year.freq <- data.frame(table(meteor$Year[meteor$Undiff_Type == 'Enstatite']))
plot_ly(x=d.year.freq$Var1, y=d.year.freq$Freq, name = 'Differentiated', type="bar") %>%
  add_trace(x=c.year.freq$Var1, y=c.year.freq$Freq, name = 'Carbonaceous') %>%
  add_trace(x=o.year.freq$Var1, y=o.year.freq$Freq, name = 'Ordinary') %>%
  add_trace(x=r.year.freq$Var1, y=r.year.freq$Freq, name = 'Rumuruti (R)') %>%
  add_trace(x=e.year.freq$Var1, y=e.year.freq$Freq, name = 'Enstatite') %>%
  layout(yaxis = list(title='Frequency', type='log'), xaxis=list(categoryorder ='ascending',
         categoryarray =year.freq$Var1,title='Year'),
         title='Logarithmic Scaling of Meteorite Type By Year',
         barmode='stack') -> year.sbarp
year.sbarp

h.year.freq <- data.frame(table(meteor$Year[meteor$Ordinary_Type == 'H Meteorite']))
l.year.freq <- data.frame(table(meteor$Year[meteor$Ordinary_Type == 'L Meteorite']))
ll.year.freq <- data.frame(table(meteor$Year[meteor$Ordinary_Type == 'LL Meteorite']))
plot_ly(x=h.year.freq$Var1, y=h.year.freq$Freq, name = 'H Meteorite', type="bar") %>%
  add_trace(x=l.year.freq$Var1, y=l.year.freq$Freq, name = 'L Meteorite') %>%
  add_trace(x=ll.year.freq$Var1, y=ll.year.freq$Freq, name = 'LL Meteorite') %>%
  layout(yaxis = list(title='Frequency', type='log'), xaxis=list(categoryorder ='ascending',
         categoryarray =year.freq$Var1,title='Year'),
         title='Logarithmic Scaling of Meteorite Type By Year',
         barmode='stack') -> oyear.sbarp
oyear.sbarp

##Histograms of Probability Fell vs Found
plot_ly(meteor, x=~Fell_Status, color=~Differentiated, histnorm='probability') %>%
  layout(title='Probability Breakdown of Fell Status For All Meteorites',
         yaxis=list(title='Probability')) %>%
  add_histogram()

plot_ly(meteor, x=~Fell_Status, color=~Undiff_Type, histnorm='probability') %>%
  layout(title='Probability Breakdown of Fell Status For Undifferentiated Meteorites',
         yaxis=list(title='Probability')) %>%
  add_histogram()

plot_ly(meteor, x=~Fell_Status, color=~Ordinary_Type, histnorm='probability') %>%
  layout(title='Probability Breakdown of Fell Status For Ordinary Meteorites',
         yaxis=list(title='Probability')) %>%
  add_histogram()

##Pie Charts of Type of Fell vs Found
meteor.fell <- filter(meteor, Fell_Status=='Fell')
dcount.fell <- meteor.fell %>% count(Differentiated)
udcount.fell <- meteor.fell %>% count(Undiff_Type)
ocount.fell <- meteor.fell %>% count(Ordinary_Type)
pie.values.fell <- as.numeric(c(dcount.fell[1,2],udcount.fell[1,2],udcount.fell[2,2],udcount.fell[4,2],
  ocount.fell[1,2],ocount.fell[2,2],ocount.fell[3,2]))
plot_ly(meteor.fell, labels= c('Differentiated','Carbonaceous','Enstatite','Rumuruti (R)',
        'H Meteorite','L Meteorite','LL Meteorite'), values= pie.values.fell, type='pie') %>%
  layout(title='Pie Chart of Meteorite Type Breakdown By Having Fell') -> fell.pie
fell.pie

meteor.found <- filter(meteor, Fell_Status=='Found')
dcount.found <- meteor.found %>% count(Differentiated)
udcount.found <- meteor.found %>% count(Undiff_Type)
ocount.found <- meteor.found %>% count(Ordinary_Type)
pie.values.found <- as.numeric(c(dcount.found[1,2],udcount.found[1,2],udcount.found[2,2],udcount.found[4,2],
  ocount.found[1,2],ocount.found[2,2],ocount.found[3,2]))
plot_ly(meteor.found, labels= c('Differentiated','Carbonaceous','Enstatite','Rumuruti (R)',
  'H Meteorite','L Meteorite','LL Meteorite'), values= pie.values.found, type='pie') %>%
  layout(title='Pie Chart of Meteorite Type Breakdown By Having Been Found') -> found.pie
found.pie

##Boxplots of Mass by Meteorite Type
plot_ly(y = meteor$Mass, type="box", name = 'All Mass') %>%
  add_trace(y = meteor$Mass[meteor$Differentiated == 'Undifferentiated'], name = 'Undifferentiated Mass') %>%
  add_trace(y = meteor$Mass[meteor$Differentiated == 'Differentiated'], name = 'Differentiated Mass') %>%
  add_trace(y = meteor$Mass[meteor$Undiff_Type == 'Carbonaceous'], name = 'Carbonaceous Mass') %>%
  add_trace(y = meteor$Mass[meteor$Undiff_Type == 'Ordinary'], name = 'Ordinary Mass') %>%
  add_trace(y = meteor$Mass[meteor$Undiff_Type == 'Rumuruti (R)'], name = 'Rumuruti (R) Mass') %>%
  add_trace(y = meteor$Mass[meteor$Undiff_Type == 'Enstatite'], name = 'Enstatite Mass') %>%
  layout(yaxis = list(title='Mass (g)',type = 'log'), title='Logarithmic Scaling of Meteorite Mass') -> mass.boxp
mass.boxp

plot_ly(y = meteor$Mass[meteor$Undiff_Type == 'Ordinary'], type="box", name = 'Ordinary Mass') %>%
  add_trace(y = meteor$Mass[meteor$Ordinary_Type == 'H Meteorite'], name = 'H Meteorite Mass') %>%
  add_trace(y = meteor$Mass[meteor$Ordinary_Type == 'L Meteorite'], name = 'L Meteorite Mass') %>%
  add_trace(y = meteor$Mass[meteor$Ordinary_Type == 'LL Meteorite'], name = 'LL Meteorite Mass') %>%
  layout(yaxis = list(title='Mass (g)',type = 'log'), title='Logarithmic Scaling of Meteorite Mass') -> omass.boxp
omass.boxp

##Scatterplots of Mass and Year by Meteorite Type
plot_ly(data=meteor, x=~Year, y=~Mass, color=~Lowest_Class, type='scatter', mode='markers') %>%
  layout(yaxis=list(type='log'), title= 'Mass of Meteorites Per Year By Type')-> mass.plot
mass.plot

maxmass=aggregate(x=meteor[c('Mass')], by=meteor[c('Year','Lowest_Class')], FUN=max, na.rm= TRUE)
plot_ly(data=maxmass, x=~Year, y=~Mass, color=~Lowest_Class, type='scatter', mode='markers') %>%
  layout(yaxis=list(type='log'), title= 'Maximum Meteorite Mass Per Year By Type')-> maxmass.plot
maxmass.plot

minmass=aggregate(x=meteor[c('Mass')], by=meteor[c('Year','Lowest_Class')], FUN=min, na.rm= TRUE)
plot_ly(data=minmass, x=~Year, y=~Mass, color=~Lowest_Class, type='scatter', mode='markers') %>%
  layout(yaxis=list(type='log'), title= 'Minimum Meteorite Mass Per Year By Type')-> minmass.plot
minmass.plot

avgmass=aggregate(x=meteor[c('Mass')], by=meteor[c('Year','Lowest_Class')], FUN=mean, na.rm= TRUE)
plot_ly(data=avgmass, x=~Year, y=~Mass, color=~Lowest_Class, type='scatter', mode='markers') %>%
  layout(yaxis=list(type='log'), title= 'Average Meteorite Mass Per Year By Type')-> avgmass.plot
avgmass.plot

stddevmass=aggregate(x=meteor[c('Mass')], by=meteor[c('Year','Lowest_Class')], FUN=sd, na.rm= TRUE)
plot_ly(data=stddevmass, x=~Year, y=~Mass, color=~Lowest_Class, type='scatter', mode='markers') %>%
  layout(yaxis=list(type='log'), title= 'Standard Deviation of Meteorite Mass Per Year By Type')-> stddevmass.plot
stddevmass.plot

##Bar Chart of Mean and Median Mass by Meteorite Type
meanmass.type=aggregate(x=meteor[c('Mass')], by=meteor[c('Lowest_Class')], FUN=mean, na.rm=TRUE)
medianmass.type=aggregate(x=meteor[c('Mass')], by=meteor[c('Lowest_Class')], FUN=median, na.rm=TRUE)
plot_ly(x=meanmass.type$Lowest_Class, y =meanmass.type$Mass, type = 'bar', name = 'Mean') %>%
  add_trace(x=meanmass.type$Lowest_Class, y =medianmass.type$Mass, name= 'Median') %>%
  layout(yaxis = list(title = 'Mass (g)', type= 'log'), barmode = 'group') -> meanmedian.bar
meanmedian.bar

##Density Curve of Mass by Meteorite Type
mmasslc <- meteor[c('Mass','Lowest_Class')]
ggplot(mmasslc, aes(Mass, fill= Lowest_Class)) + geom_density(alpha=0.25) -> masscurve.scatter
masscurve.scatter <- ggplotly(masscurve.scatter+scale_x_continuous(trans= 'log10')+ylab('Density')
    +xlab('Mass (g)')+ggtitle('Density Curve of Mass With Logarithmic Scale Per Type')
    +theme(plot.title = element_text(hjust = 0.5)))
masscurve.scatter

##Map of Meteorite Coordinates by Type
plot_mapbox(meteor) %>%
  add_markers(x=~Rec_Long, y=~Rec_Lat, size=~Mass, color=~Lowest_Class, text=~paste('Name:',Name,'- Mass:',Mass,'grams - Year:',Year),
  hoverinfo='text', marker=list(sizeref=0.25))

##Distribution of Mass
plot_ly(x=meteor$Mass, type='histogram', histnorm='probability') %>%
  layout(title='Distribution of Meteoric Mass', yaxis=list(type='log',title='Probability'),
  xaxis=list(title='Mass (g)')) -> massdist.hist
massdist.hist

ggplot(data.frame(meteor), aes(x=meteor$Mass)) + geom_density(alpha=0.5) -> massdistlog.hist
massdistlog.hist <- ggplotly(massdistlog.hist+scale_x_continuous(trans= 'log10')+ylab('Density')
    +xlab('Mass (g)')+ggtitle('Density Curve of Mass With Logarithmic Scale')
    +theme(plot.title = element_text(hjust = 0.5)))
massdistlog.hist

##Central Limit Theorem
mmass <- meteor$Mass
samples <- 1000
xbar <- numeric(samples)
for (size in c(25,50,75,100,125,150)) {
  for (i in 1:samples) {
    xbar[i] <- mean(sample(mmass, size, replace = FALSE), na.rm=TRUE)
  }
  
  ggplot(data.frame(xbar), aes(x=xbar)) + geom_density(alpha=0.5) -> loop.hist
  loop.hist <- ggplotly(loop.hist+scale_x_continuous(trans= 'log10')+ylab('Density')
                               +xlab('Average Mass (g)')+ggtitle(paste("Sample Size =",size,
                                'With Logarithmic Scale'))
                               +theme(plot.title = element_text(hjust = 0.5)))
  print(loop.hist)
  
  cat("Sample Size = ", size, " Mean = ", mean(xbar),
      " SD = ", sd(xbar), "\n")
  
}
mean(mmass, na.rm=TRUE)
sd(mmass, na.rm=TRUE)
sd(mmass, na.rm=TRUE)/sqrt(c(25,50,75,100,125,150))

##Sampling Methods
sample.size=125
round(table(meteor$Lowest_Class)/nrow(meteor),2)
#Simple Random Sampling Without Replacement
mass.srswor <- srswor(sample.size,nrow(meteor))
df.msrswor <- meteor[mass.srswor !=0,]
table(df.msrswor$Lowest_Class)/sample.size
#Systematic Sampling
meteor.nrow <- nrow(meteor)
meteor.ss <- sample.size
meteor.ceiling <- ceiling(meteor.nrow/meteor.ss)
meteor.r <- sample(meteor.ceiling,1)
mass.ss <- seq(meteor.r, by=meteor.ceiling, length=meteor.ss)
df.mss <- meteor[mass.ss,]
table(df.mss$Lowest_Class)/sample.size
#Systematic Sampling With Unequal Probabilities
meteor.pik <- inclusionprobabilities(meteor$Mass, sample.size)
mass.upss <- UPsystematic(meteor.pik)
df.mupss <- meteor[mass.upss!=0,]
table(df.mupss$Lowest_Class)/sample.size
#Stratified Sampling
meteor.nonone <- meteor %>% filter(Lowest_Class != 'Rumuruti (R)')
meteor.nonone <- #meteor.nonone %>% filter(Lowest_Class != 'Enstatite')
meteor.strat <- meteor.nonone[order(meteor.nonone$Lowest_Class),]
mass.strat <- strata(meteor.strat, stratanames=c('Lowest_Class'),size=c(table(meteor.nonone$Lowest_Class)/nrow(meteor.nonone)*sample.size), method='srswor')
df.mstrat <- getdata(meteor.strat, mass.strat)
table(df.mstrat$Lowest_Class)

mean(df.msrswor$Mass, na.rm=TRUE)
mean(df.mss$Mass, na.rm=TRUE)
mean(df.mupss$Mass, na.rm=TRUE)
mean(df.mstrat$Mass, na.rm=TRUE)
mean(meteor$Mass, na.rm=TRUE)