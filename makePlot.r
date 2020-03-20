library(ggplot2)
library(reshape2)
library(naturalsort)
library(wesanderson)  # colour palettes based on Wes Anderson movies
library(RColorBrewer)
Sys.setlocale("LC_ALL","fr_CA.UTF-8")  # set locale to french, for french date axis ?



raw.cases <- read.csv("cases.csv")
raw.cases$date <- as.Date(raw.cases$date)

# for the data from the Montreal Gazette (pre-March 14), in some cases I couldn't figure out the location
#   for these cases, in the .csv, I indicated the region as 'non indique'
#   By contrast, when Sante Quebec is unsure about the location of a case, they say it's "À déterminé"
#   The plot is more succinct if it only has one label that indicates uncertainty about location,
#    so I'm going to collapse these two together and use "À déterminé" in the Gazette data as well
raw.cases[raw.cases$region == 'non indiqué','region'] <- 'À déterminer'


# before March 14 (data from the Montreal Gazette), I only have counts of new cases.
# Calculate a cumulative total for each day
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


# add data from after March 13 (from https://www.quebec.ca/sante/problemes-de-sante/a-z/coronavirus-2019/)
cases <- rbind(cases,raw.cases[raw.cases$date > as.Date("2020-03-13"),c("date","region","cumulative")])

# change region order for barplot
cases <- cases[naturalorder(cases$region),]
cases$region <- factor(as.character(cases$region))

nRegions <- length(levels(cases$region))
blues_fun <- colorRampPalette(brewer.pal(9,"Blues")) # https://stackoverflow.com/questions/16922988/interpolating-a-sequential-brewer-palette-as-a-legend-for-ggplot2



# plot cumulative cases
p <- ggplot(cases,aes(x=date,y=cumulative,fill=region)) + geom_bar(stat="identity") + ggtitle("COVID-19 au Québec") 
p <- p + ylab("Cas confirmés") + xlab("Date")
#p <- p + scale_fill_manual(values = wes_palette("Darjeeling1", nRegions, type = "continuous"))
p <- p + scale_fill_manual("Région",values=blues_fun(nRegions))
p
ggsave("covid_19_au_quebec_cumulative.png",p,height=4,width=7)



# from cumulativate cases dataframe, get new cases for each day
## I'm trying to do this while still keeping track of what regions the cases are from. Can't get this to work.
#new.cases <- cases
#new.cases.wide <- dcast(new.cases,date ~ region,fun.aggregate=sum,value.var="cumulative")
#for(date in as.list(seq( max(cases$date),  min(cases$date)+1, by="-1 day"))){
#  print(date)
#  new.cases.wide[new.cases.wide$date == date,-c(1)] <- new.cases.wide[new.cases.wide$date == date,-c(1)] - new.cases.wide[new.cases.wide$date == date-1,-c(1)]
#}
#new.cases <- melt(new.cases.wide,id.vars="date",value.name ="new",variable.name="region" )

# plot new cases each day
#p <- ggplot(new.cases,aes(x=date,y=new,fill=region)) + geom_bar(stat="identity") + ggtitle("COVID-19 au Québec") 
#p <- p + ylab("Nouveau cas confirmés") + xlab("Date")
#p <- p + scale_fill_manual("Région",values=blues_fun(nRegions))
#p
#ggsave("covid_19_au_quebec_nouveau.png",p,height=3.5,width=7)



# from cumulativate cases dataframe (cases), get new cases for each day by taking the derivative
new.cases <- aggregate(cumulative ~ date,cases,FUN=sum)   # get sum over regions
new.cases$new <- c(new.cases$cumulative[1],diff(new.cases$cumulative, lag=1))   # take derivative

# moving average function - from https://stackoverflow.com/a/4862334/765287
ma <- function(x, n = 7){filter(x, rep(1 / n, n), sides = 1,method="convolution")}

# calculate 7-day moving average, like in these New York Times plots (https://www.nytimes.com/interactive/2020/03/19/world/coronavirus-flatten-the-curve-countries.html)
new.cases$ma <- ma(new.cases$new)
new.cases[is.na(new.cases$ma),"ma"] <- 0    # filter function puts NAs at start of vector, replace these with zeros
new.cases$ma
new.cases$ma_type <- "7-day average"


# plot new cases each day
p <- ggplot(new.cases,aes(x=date,y=new)) + geom_bar(stat="identity",aes(alpha=I(0.3))) + ggtitle("COVID-19 au Québec")
p <- p + geom_line(aes(y=ma,colour=ma_type)) + scale_colour_manual(values=c("#2171B5"))
p <- p + ylab("Nouveau cas confirmés") + xlab("Date") + labs(colour = "")
p
ggsave("covid_19_au_quebec_nouveau.png",p,height=4,width=7)

