library(ggplot2)
library(reshape2)
library(naturalsort)
#library(wesanderson)  # colour palettes based on Wes Anderson movies
library(RColorBrewer)
Sys.setlocale("LC_ALL","fr_CA.UTF-8")  # set locale to french, for french date axis ?



raw.cases <- read.csv("cases.csv")
raw.cases$date <- as.Date(raw.cases$date)


# before March 14, I only have counts of new cases
# calculate a cumulative total for each day
preMarch14 <- raw.cases[raw.cases$date < as.Date("2020-03-14"), ]
preMarch14.wide <- dcast(preMarch14,date ~ region,fun.aggregate=length,value.var="new")

cases.wide <- preMarch14.wide[preMarch14.wide$date == as.Date("2020-02-28"),]
for(date in as.list(seq(as.Date("2020-02-29"), as.Date("2020-03-13"), by="1 day"))){

  if(any(preMarch14.wide$date == date)){
    new.date <- preMarch14.wide[preMarch14.wide$date == date,-c(1)] + cases.wide[cases.wide$date == date-1,-c(1)]
  }else {
    new.date <-cases.wide[cases.wide$date == date-1,]
  }    
  new.date$date <- date
  cases.wide <- rbind(cases.wide,new.date)
}
cases <- melt(cases.wide,id.vars="date",value.name ="cumulative",variable.name="region" )


# add data from after March 13, from https://www.quebec.ca/sante/problemes-de-sante/a-z/coronavirus-2019/
cases <- rbind(cases,raw.cases[raw.cases$date > as.Date("2020-03-13"),c("date","region","cumulative")])
#cases <- merge(cases,raw.cases[,c("date","region","new")])  # add new cases 

# change region order for barplot
cases <- cases[naturalorder(cases$region),]
cases$region <- factor(as.character(cases$region))

nRegions <- length(levels(cases$region))
blues_fun <- colorRampPalette(brewer.pal(9,"Blues")) # https://stackoverflow.com/questions/16922988/interpolating-a-sequential-brewer-palette-as-a-legend-for-ggplot2


p <- ggplot(cases,aes(x=date,y=cumulative,fill=region)) + geom_bar(stat="identity") + ggtitle("COVID-19 au Québec") 
p <- p + ylab("Cas confirmés") + xlab("Date")
#p <- p + scale_fill_manual(values = wes_palette("Darjeeling1", nRegions, type = "continuous"))
p <- p + scale_fill_manual("Région",values=blues_fun(nRegions))



p
ggsave("covid_19_au_quebec.png",p,height=3.5,width=7)
