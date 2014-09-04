## read the accepted returned transaction and plot them
# utilize the zipcode package to plot on location
library(zipcode)
library(lattice)
library(maps)
library(ggplot2)

#load the transaction data
setwd('/home/eric/Documents/Coursea')  ## use this line when run on linux
#setwd('C:/Users/exguan/Desktop/Return')  ## use this line when run on Win
mydata <- read.csv('accepted_trans.csv')
summary(mydata)

#png("distTranAmt.png")
hist(mydata$TranAmount, breaks = 20, main="Distribution of Transaction Amount",xlab="Transaction Amount ($)")
rug(mydata$TranAmount)
#dev.off()

dev.new()
#png("distCategory.png")
boxplot(mydata$TranAmount ~ mydata$TranCategory, main="Distribution by Tran Category")
#dev.off()

data(zipcode)
moreData = merge(mydata, zipcode, by.x='ZIP', by.y='zip')
#g = ggplot(data) + geom_point(aes(x=longitude, y=latitude, color=TranCategory))

dev.new()
map("state")
par(new=TRUE)
with(moreData, xyplot(latitude ~ longitude, group=TranCategory))

#plot all states with ggplot
#load us map data
all_states <- map_data("state")
p <- ggplot()
p <- p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white" )
p <- p + geom_point( data=moreData, aes(x=longitude, y=latitude, size = TranAmount), color="coral1") + scale_size(name="Total Tran")
#p <- p + geom_text( data=moreData, hjust=0.5, vjust=-0.5, aes(x=longitude, y=latitude, label="Label"), colour="gold2", size=4 )
p

#plot just MN with ggplot
MNdata <- subset(moreData, state == 'MN', select=c(state, TranAmount, TranCategory, latitude, longitude))
states <- subset(all_states, region %in% c( "minnesota"))
p <- ggplot()
p <- p + geom_polygon( data=states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
p <- p + geom_point( data=MNdata, aes(x=longitude, y=latitude, size=TranAmount), color="coral1") + scale_size(name="Total Tran")
p

