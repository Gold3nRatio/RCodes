library(ggmap)
library(zipcode)
library(ggplot2)

mydir <- '/home/eric/Documents/Coursea/workPro/AP_NY_Labor/'

## read in the files
#nyarea <- paste0(mydir, 'ny_area.csv')
#nybank <- paste0(mydir, 'ny_bank.csv')
#nyprefix <- paste0(mydir, 'ny_prefix.csv')

nyfee <- paste0(mydir, 'ny_fee.csv')
nyzip <- paste0(mydir, 'ny_zip.csv')

#nyarea <- read.csv(nyarea)
#nybank <- read.csv(nybank)
#nyprefix <- read.csv(nyprefix)

nyfee <- read.csv(nyfee)
## take out the OTHER fee_type from fee
nyfee <- subset(nyfee, !fee_type=='OTHER')
nyfee <- nyfee[order(nyfee$state, nyfee$fee_type),]
table(nyfee$state, nyfee$fee_type)

## analyze the fee by state level
feebar <- ggplot(nyfee, aes(x=state, y=fee_amt, fill=factor(fee_type)))
feebar <- feebar + geom_bar(stat='identity') + scale_fill_grey()
feebar <- feebar + labs(title='Annual Fee Collected by State')
feebar <- feebar + labs(x='State', y='Fee Amount', fill='Fee type')
png(paste0(mydir, 'feebystate.png'), 500, 500)
feebar
dev.off()

## analyze the fee by zip code
nyzip <- read.csv(nyzip, colClasses = 'character')
nyzip$cardcount <- as.numeric(nyzip$cardcount)

data(zipcode)
nyzip <- merge(nyzip, zipcode, by.x = 'zip', by.y = 'zip')
nyzip <- subset(nyzip, state %in% c('NY', 'PA', 'NJ', 'CT', 'MA', 'VT'))

## get the map of the NY area
nyc <- 'New York, New York'
nyMap <- qmap(nyc, zoom = 6, maptype = 'toner', source = 'google')

png(paste0(mydir, 'chlocation_bystate.png'), 500, 500)
nyMap + 
    geom_point(aes(x = longitude, y = latitude, color = state, size = cardcount),
               data = nyzip, alpha = 0.5) 
dev.off()
