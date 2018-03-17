---
title: "C5"
author: "Mohsin"
date: "March 16, 2018"
output: html_document
---



library(ggplot2)
library(knitr) 
library(markdown)
library(Hmisc)
library(lubridate)


datac<-read.csv("./Course5/activity.csv") 
str(datac)
names(datac)


#Total Steps Taken in a Day

```r
steps <- aggregate(datac$steps, by = list(datac$date), sum, na.rm=TRUE) 
plot(steps,col="green",pch=16,main="Daily Steps")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)
#Calculate Mean and Median of the Total Number of Steps Taken Daily

```r
steps_mean <- mean(steps$steps) 
```

```
## Warning in mean.default(steps$steps): argument is not numeric or logical:
## returning NA
```

```r
steps_median <- median(steps$steps)
```

```
## Warning in is.na(x): is.na() applied to non-(list or vector) of type 'NULL'
```

```r
print(steps_mean)
```

```
## [1] NA
```

```r
print(steps_median)
```

```
## NULL
```

#Average Daily Pattern of Activity

```r
stepavg <- aggregate(steps ~ interval, data = datac, mean, na.rm = TRUE)

plot(stepavg$interval, stepavg$steps, type = "l", lwd = 2.5, col = "blue",
     main = "Average/Mean of Steps Taken", xlab = "Five Minute interval", ylab = "Steps(average)")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)
# Inputting Missing Values

```r
missingval <- length(which(is.na(datac$steps)))
imputed <- datac
imputed$steps <- impute(datac$steps, fun=mean)
imputedstepsd <- tapply(imputed$steps, imputed$date, sum)
par(mfrow=c(1,2))
boxplot(imputedstepsd, xlab='Imputed Total Steps', ylab='Frequency(Binwidth=350)', binwidth=350)
hist(imputedstepsd,main="Histogram of Imputed Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)
#Weekday and Weekend Steps


```r
ds<-datac$date
dss<-as.Date(ds)
dayz<-weekdays(dss)
datac$dayz<-dayz
dayz<-as.character(dayz)
wkday<-subset(datac,dayz==c("Monday","Tuesday","Wednesday","Thursday","Friday"))
```

```
## Warning in dayz == c("Monday", "Tuesday", "Wednesday", "Thursday",
## "Friday"): longer object length is not a multiple of shorter object length
```

```r
wkend<-subset(datac,dayz==c("Satuday","Sunday"))
par(mfrow=c(2,2))
hist(wkday$steps,col="beige", main="Frequency of Week-day Steps")
plot(wkday$steps,col="red", main="Week-day Steps")
hist(wkend$steps,col="green",main="Frequency of Week-end Steps")
plot(wkend$steps,col="pink",main="Week-end Steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)
