library(ggplot2)

mypath <- 'C:/Users/exguan/Desktop/pos/'
myfile <- 'pos.csv'

## read the file
k <- read.csv(paste0(mypath, myfile))


## clean up the data's format
k$TRANSACTION_DATE <- as.Date(as.character(k$TRANSACTION_DATE), format='%m/%d/%Y')
k$TERMINAL_DESCRIPTION <- as.character(k$TERMINAL_DESCRIPTION)
k$TERMINAL_STATE <- as.character(k$TERMINAL_STATE)
k$PURCH_DESC <- as.character(k$PURCH_DESC)
k$PURCH_STATE <- as.character(k$PURCH_STATE)
k$TERM_OWNER_NAME_AND_ADDRESS <- NULL
k$wkday <- weekdays(k$TRANSACTION_DATE)
k$wkday <- factor(k$wkday, 
	levels=c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))


## replace punctuation characters with white space
k$TERMINAL_DESCRIPTION <- gsub('[[:punct:]]', ' ', k$TERMINAL_DESCRIPTION)
k$PURCH_DESC <- gsub('[[:punct:]]', ' ', k$PURCH_DESC)


## identify the add fund address by creating new fields
## remove double space, space at the beginning or the end, and periods
k$af_addr <- gsub('  |^ | $', '', substr(k$TERMINAL_DESCRIPTION, 1, 24))
k$af_city <- gsub(' ', '', substr(k$TERMINAL_DESCRIPTION, 25, 37))
k$af_state <- gsub(' ', '', substr(k$TERMINAL_DESCRIPTION, 38, 40))


## identify the spend address by createing new fields
k$pc_addr <- gsub('  |^ | $', '', substr(k$PURCH_DESC, 1, 24))
k$pc_city <- gsub(' ', '', substr(k$PURCH_DESC, 25, 37))
k$pc_state <- gsub(' ', '', substr(k$PURCH_DESC, 38, 40))


## create a merchant location where the money was spend
k$pc_merchant <- NA
k[k$pc_addr != '', 19] <- 'Other'
k[grep('WM|WAL MART|WALMART|SAMS', k$pc_addr, ignore.case=TRUE), 19] 	<- 'WALMART'
k[grep('FIRST TECH', k$pc_addr, ignore.case=TRUE), 19] 		<- 'FIRST TECH'
k[grep('FRED.+MEYER', k$pc_addr, ignore.case=TRUE), 19] 		<- 'FRED MEYER'
k[grep('BANK.+AMERICA', k$pc_addr, ignore.case=TRUE), 19] 		<- 'BANK OF AMERICA'
k[grep('WELL.+FARGO', k$pc_addr, ignore.case=TRUE), 19] 		<- 'WELLS FARGO'
k[grep('APPL', k$pc_addr, ignore.case=TRUE), 19] 			<- 'APPLE'
k[grep('KROG', k$pc_addr, ignore.case=TRUE), 19] 			<- 'KROGER'
k[grep('APPL', k$pc_addr, ignore.case=TRUE), 19] 			<- 'APPLE'
k[grep('AMAZ', k$pc_addr, ignore.case=TRUE), 19] 			<- 'AMAZON'
k[grep('PAYPAL', k$pc_addr, ignore.case=TRUE), 19] 			<- 'PAYPAL'
k[grep('FRYS FOOD DRUG', k$pc_addr, ignore.case=TRUE), 19] 			<- 'FRYS FOOD DRUG'
k[grep('WINN DIXI', k$pc_addr, ignore.case=TRUE), 19] 			<- 'WINN DIXI'
k[grep('TGT|TARGET', k$pc_addr, ignore.case=TRUE), 19] 			<- 'TARGET'
k[grep('HOME DEPOT|HD', k$pc_addr, ignore.case=TRUE), 19] 			<- 'HOME DEPOT'
k[grep('SMITHS', k$pc_addr, ignore.case=TRUE), 19] 			<- 'SMITHS'
k[grep('PUBLIX', k$pc_addr, ignore.case=TRUE), 19] 			<- 'PUBLIX'
k[grep('HEB', k$pc_addr, ignore.case=TRUE), 19] 			<- 'HEB'
k[grep('BEST', k$pc_addr, ignore.case=TRUE), 19] 			<- 'BEST BUY'
k[grep('KING SOOPER', k$pc_addr, ignore.case=TRUE), 19] 			<- 'KING SOOPERS'


## summarize how many cards were sold and the amount loaded
table(k$TRANSACTION_AMOUNT)

png(paste0(mypath, 'sales.png'), width=300, height=200)
s <- qplot(TRANSACTION_DATE, data=k, geom='histogram')
s <- s + geom_abline(intercept = nrow(k) / 17, slope = 0, color = 'red')
s <- s + labs(title='Cards sold October', x='Trans Date', y='Cards Sold')
s <- s + theme(text = element_text(size=10), axis.text.x = element_text(angle=90, vjust=1))
s
dev.off()

png(paste0(mypath, 'weekday.png'), width=300, height=200)
w <- qplot(wkday, data=k, geom='histogram', xlab=NULL)
w <- w + labs(title='Card Sold per Weekday', y='Cards Sold')
w <- w + theme(text = element_text(size=10), axis.text.x = element_text(angle=90, vjust=1))
w
dev.off()


## what percentage of the cards had a purchase of the same amount
round(100 * ( nrow(k)-sum(is.na(k$PURCH_TRAN)) ) / nrow(k), 1)

## where are the cards being sold (merchant and states)
table(k$af_state)

## where are the cards being spend (merchant and states)
m <- as.data.frame(table(k$pc_merchant))
png(paste0(mypath, 'merchant.png'), width=600, 300)
mp <- ggplot(m, aes(x=reorder(Var1, -Freq), y=Freq))
mp <- mp + geom_bar(stat = 'identity')
mp <- mp + theme(text = element_text(size=10), axis.text.x = element_text(angle=90, vjust=1))
mp <- mp + labs(title = 'Spend by Merchant'
	, x = 'Merchants'
	, y = 'Frequency'
	, size = 7)
mp
dev.off()

p <- as.data.frame(table(k$pc_state[k$pc_state!='']))
png(paste0(mypath, 'states.png'), width=800, 300)
pp <- ggplot(p, aes(x=reorder(Var1, -Freq), y=Freq))
pp <- pp + geom_bar(stat = 'identity')
pp <- pp + theme(text = element_text(size=10), axis.text.x = element_text(angle=90, vjust=1))
pp <- pp + labs(title = 'Spend by Location'
	, x = 'States'
	, y = 'Frequency'
	, size = 7)
pp
dev.off()






