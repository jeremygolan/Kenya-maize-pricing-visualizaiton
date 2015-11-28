rm(list = ls())

# kenya maize price visuals
# written by: jeremy golan (jeremy.golan@oneacrefund.org)
# written for: PI project
# last edited: 9 november 2015 (jg)

# directories
setwd("~/drive/kenya_maize_price")
wd <- ("~/drive/kenya_maize_price")
dd <- "~/drive/kenya_maize_price/data"
od <- "~/drive/kenya_maize_price/output"

#libraries
library(lubridate)
library(plyr)
library(zoo)
library(reshape2)
library(ggplot2)
library(scales)
library(tidyr)

# data input
# gather files to read in
files <- list.files(dd, pattern = ".csv")

# read in each dataset and put into variable
for(i in 1:length(files)) {
	tmp <- read.csv(paste(dd, files[i], sep = "/"), header = TRUE, 
  		sep = ",", stringsAsFactors = FALSE)

	# now assign data that reads in to an r object 
	assign(substr(files[i], 1, (nchar(files[i]) - 4)), tmp)
}


#start with FAO data
#reshape
f <- fao_nairobi
f1 <- melt(f, id="Country")
f1 <- f1[, c(2:3)]

names(f1) <- c("date", "price")
#get date how we want it 
f1$date <- gsub("X", "", f1$date)
f1$date <-gsub(".", "/", f1$date, fixed = T)

f1$date <- as.Date(f1$date, "%m/%d/%Y")

#remove Nas
f1 <- na.omit(f1)

#creating year variable
f1$year <- as.numeric(substring(f1$date, 1, 4))

#creating month variable
f1$month <- as.numeric(substring(f1$date, 6, 7))

#create date variable -- will be set for the first of each month
f1$date <- as.Date(paste(f1$month, 1, f1$year, 
	sep = "-"), format = "%m-%d-%Y")

f2 <- subset(f1, select = (c(date, price)))

#turn price into KES at 100 to 1
f2$price <- f2$price * 100

#per kg

f2$location <- "Nairobi Wholesale"

#and market survey
#clean to date, location, price 

m <- market_survey
m1 <- subset(m, select = (c(Year, Date.of.Survey, Interior.or.Town, X90.kg.Bag.Maize,X1.Goro.Goro.Maize)))

names(m1) <- c("year", "month.day", "location", "bag", "goro")
m1$location <- gsub(" ", "", m1$location)
m1$location <- gsub("interior", "Interior", m1$location)
m1$location <- gsub("town", "Town", m1$location)
m1$location <- ifelse(!m1$location == "Interior" & !m1$location == "Town", NA, m1$location)

m1$bag <- ifelse(m1$bag == "N/A", NA, m1$bag)
m1$bag <- as.numeric(gsub(",", "", m1$bag))

m1$goro <-ifelse(m1$goro == "N/A", "0", m1$goro)
m1$goro <-gsub("7500", "75", m1$goro)
m1$goro <-gsub(" ", "", m1$goro)
m1$goro <- ifelse(is.na(m1$goro), "0", m1$goro)

m1$goro <- as.numeric(as.character(m1$goro))

#date
m1$month <-   substr(m1$month.day, nchar(m1$month.day)-3, nchar(m1$month.day))
m1$month <- gsub("-se", "-sep", m1$month)
m1$month <- gsub("-", "", m1$month)
m1$month <- ifelse(m1$month == "N/A"| m1$month == "ated", NA, m1$month)
m1$month <- gsub("0sep", "sep", m1$month)
m1$month <- match(m1$month,month.abb)
m1$date <- as.Date(paste(m1$month, 1, m1$year, 
	sep = "-"), format = "%m-%d-%Y")

m1$kg.bag <- m1$bag/90
m1$kg.goro<- m1$goro/2
m1$kg.goro22<- m1$goro/2.2


#lets use bag since we do not know whether goro is 2 or 2.2
m1$price <- m1$kg.bag

m2 <- subset(m1, select = (c(date, price, location)))

m2$price <- ifelse(m2$price == 0, NA, m2$price)

m2 <- na.omit(m2)

int <- subset(m2, location =="Interior")

town <- subset(m2, location =="Town")

nairobi.wholesale <- f2

#test <- rbind(m2, f2)


#test2 <- do.call(rbind, lapply(split(test, list(test$location, test$date), drop=T), function(x) {
  #  z <- mean(x$price, na.rm = T)
    
 #   x$price <- z
      
#  output <- data.frame(date = unique(x$date), price = unique(x$price), location = unique(x$location))

#    return(output)
#}
#))


  #   test2 <- split(int, int$date)
#    y <- test2[[1]]
date.price <- function(x) {
  y <- do.call(rbind, lapply(split(x, x$date),
  function(y) {
    z <- mean(y$price, na.rm = T)
    
    y$price <- z
      
  output <- data.frame(date = unique(y$date), price = unique(y$price), location = unique(y$location))

    return(output)
}
))
}

mp <- rbind(date.price(int), date.price(town), date.price(nairobi.wholesale))

#9/1/13 to 9/1/2015
mp1 <- subset(mp, date < as.Date("2015-9-1") & date > as.Date("2013-03-30"))

p1 <- ggplot(mp1, aes(x=date, y=price, colour=location)) + 
 geom_smooth(se=FALSE) +
 xlab('Month') +
  ylab('KES per kg') +
  scale_fill_continuous(guide = guide_legend()) +
  labs(title = element_blank()) +
  theme_bw() +
    theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
expand_limits(y=c(0,50)) + 
scale_x_date(labels = date_format("%b-%y"), breaks = date_breaks("3 months"))
#  scale_x_date(labels = date_format("%b%y"), breaks = date_breaks("month")) +
p1

nairobi.wholesale$month <- as.numeric(substring(nairobi.wholesale$date, 6, 7))

min.mean.max <- function(x) {
  y <- do.call(rbind, lapply(split(x, x$month),
  function(y) {
    mean <- mean(y$price, na.rm = T)
    min <- min(y$price, na.rm = T)
    max <- max(y$price, na.rm = T)
    
    y$mean <- mean
     y$min <- min
     y$max <- max
    
  output <- data.frame(month = unique(y$month), mean = unique(y$mean),min = unique(y$min), max = unique(y$max))

    return(output)
}
))
}

mp2 <- min.mean.max(nairobi.wholesale)
mp2$date <- as.Date(paste(mp2$month,1,2015, sep = "-"),format = "%m-%d-%Y")


mp3 <- gather(mp2, type, price, mean:max)
mp3$date <- as.Date(paste(mp3$month,1,2015, sep = "-"),format = "%m-%d-%Y")


p2 <- ggplot(mp2, aes(x=date, y=mean)) + 
geom_line(aes(y=min), colour="grey25", linetype="dotted") +
geom_line(aes(y=max), colour="grey25", linetype="dotted") +
   geom_smooth(se=FALSE) +
 xlab('Month') +
  ylab('KES per kg') +
  scale_fill_continuous(guide = guide_legend()) +
  labs(title = element_blank()) +
  theme_bw() +
    theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
expand_limits(y=c(0,50)) +
  scale_x_date(labels = date_format("%b"), breaks = date_breaks("month"))

p2

p3 <- ggplot(mp2, aes(x=date, y=mean)) + 
geom_ribbon(aes(ymin=min, ymax=max),
alpha=0.1) +
  geom_line(aes(y=min), colour="grey25", linetype="dotted") +
geom_line(aes(y=max), colour="grey25", linetype="dotted") +
   geom_smooth(se=FALSE) +
 xlab('Month') +
  ylab('KES per kg') +
  scale_fill_continuous(guide = guide_legend()) +
  labs(title = element_blank()) +
  theme_bw() +
    theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
expand_limits(y=c(0,50)) +
  scale_x_date(labels = date_format("%b"), breaks = date_breaks("month"))

p3



#average FAO data 
#5 year average
# high
#low

