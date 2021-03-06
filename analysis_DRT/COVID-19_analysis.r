# Analysis of COVID-19 data
# Darren Tyson
# 2020-03-22
# Data from https://github.com/CSSEGISandData/COVID-19 (forked)

filepath <- "../csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-XXX.csv"
# filepath <- "../csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
# filepath <- "../csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
confirmed_path <- sub("XXX","Confirmed",filepath)
deaths_path <- sub("XXX","Deaths",filepath)
recovered_path <- sub("XXX","Deaths",filepath)

# make function to convert date format
fixDate <- function(x) as.Date(x, format="%m_%d_%y")


infected <- read.csv(confirmed_path, as.is=TRUE)
colnames(infected) <- gsub("/", "_", unlist(strsplit(readLines(confirmed_path,1),",")), fixed=TRUE)

# focus on US cases only
infected.us <- infected[infected$Country_Region == "US",]

# make a subset of the infection data; this should include TN
ss <- infected.us[infected.us$'3_10_20' > 0,]
ss <- ss[!grepl("Princess", ss$Province_State),]

# transform case data
ss.t <- t(ss[,c(5:ncol(ss))])
colnames(ss.t) <- ss$Province_State

# reformat to long form 
r <- data.frame(state=rep(colnames(ss.t), each=nrow(ss.t)), 
                date=fixDate(rownames(ss.t)), 
                cases=as.integer(ss.t), stringsAsFactors=FALSE)

# remove data before 2020-03-09 (no cases reported)
r <- r[r$date > "2020-03-09",]
rownames(r) <- NULL
r$days <- floor(difftime(r$date, "2020-03-09", units="days"))

# reference states
refstates <- c("Washington","Louisiana","Tennessee","New York")

statecol <- rainbow(n=length(unique(r$state)))
names(statecol) <- sort(unique(r$state))

###########################
# Plot multiple statewide cases
###########################
dev.new(width=9, height=4)
par(mfrow=c(1,3))

plot(log2(cases) ~ days, data=r, type="n", main="Cases by State")
invisible(lapply(unique(r$state), function(state) 
    lines(log2(cases) ~ days, 
          data=r[r$state==state,],
          col=statecol[state],
          lwd=ifelse(state %in% refstates,4,.5)
          )))
legend("topleft", refstates, col=statecol[refstates], lwd=4)

m <- lm(log2(cases) ~ date * state, data=r)

rates <- coef(m)[grep("date",names(coef(m)))]
rates[-1] <- rates[-1]+rates[1]
names(rates)[1] <- "Arizona"
names(rates) <- gsub("date:state","",names(rates))
stdev <- summary(m)$coefficients[grep("date",rownames(summary(m)$coefficients)),"Std. Error"]
names(stdev) <- names(rates)


DT <- 1/sort(rates, decreasing=TRUE)

print("days for number of infected people to double")
DT[refstates]

# tr <- read.table("TN_Cases_-_Bar_data.csv", as.is=TRUE, 
#                  fileEncoding="UTF-16", header=TRUE)
# tr$days <- ceiling(difftime(as.Date(tr$Date, format="%m/%d/%Y"), "2020-03-10", units="days"))
# # write.csv(tr[order(tr$days, decreasing=TRUE),], file="TN_cases_DRT.csv", row.names=FALSE)

###########################
# TN cases
###########################

tr <- read.csv("TN_cases_DRT.csv")
tr$Date <- strptime(tr$Date, "%m/%d/%Y")
nc <- read.table("TN_new_cases_DRT.txt",skip=1, header=TRUE)
tr <- merge(tr, nc, all=TRUE)
tr$days <- ceiling(difftime(tr$Date, "2020-03-05", units="days"))
m <- lm(log2(Cases) ~ days, data=tr[tr$days <= 14,])

# Plot all TN cases with trendline from first 14 days
plot(log2(Cases) ~ days, data=tr, type='l', ylim=c(0,22), main="TN cases")
abline(m, col='green', lwd=3)

# plot TN cases after first month with post-30d trendline
plot(log2(Cases) ~ days, data=tr[tr$days>30,], type='l', 
     xlab="Days after first case",
     ylim=c(0,22), main="TN cases")
tm_30to60 <- lm(log2(Cases) ~ days, data=tr[tr$days >= 30 & tr$days < 60,])
tm_post60 <- lm(log2(Cases) ~ days, data=tr[tr$days >= 60,])
abline(tm_30to60, col='orange', lwd=2)
abline(tm_post60, col='red', lwd=2)
text(30,5,labels=paste("DT, days30-60 =",round(1/coef(tm_30to60)['days'],2),"days"), pos=4)
text(30,4.25,labels=paste("DT, days>60 =",round(1/coef(tm_post60)['days'],2),"days"), pos=4)

###########################
# Davidson county cases
###########################
dev.new(width=9, height=4)
par(mfrow=c(1,3))

dc <- read.table("Davidson_cases_DRT.txt", header=TRUE, as.is=TRUE)
dc$Date <- as.Date(dc$Date)
dc$days <- as.integer(difftime(dc$Date,tail(dc$Date,1), units="days"))
plot(log2(Cases) ~ Date, dc, type='l', main="Davidson cases")



plot(log2(Cases) ~ Date, dc[dc$Date > as.Date("2020-04-01"),], type='l', 
          main="Davidson cases", ylim=c(9.5,14))
m2 <- lm(log2(Cases) ~ Date, data=dc[dc$Date > as.Date("2020-04-01") & dc$Date < as.Date("2020-04-10"),])
m3 <- lm(log2(Cases) ~ Date, data=dc[dc$Date > as.Date("2020-04-08")& dc$Date < as.Date("2020-05-05"),])
m4 <- lm(log2(Cases) ~ Date, data=dc[dc$Date > as.Date("2020-05-05"),])
abline(m2, col='green', lwd=2)
abline(m3, col='orange', lwd=2)
abline(m4, col='red', lwd=2)
legend("topleft", legend=c("Doubling time",
                           round(1/coef(m2)['Date'],1), 
                           round(1/coef(m3)['Date'],1), 
                           round(1/coef(m4)['Date'],1)),
       col=c("white","green","orange","red"), lwd=2)
abline(v=as.Date("2020-04-08"),lty=2, col=grey(.5))
abline(v=as.Date("2020-05-05"),lty=2, col=grey(.5))
text(as.Date("2020-04-08"),9.5,"Apr-08", pos=4)
text(as.Date("2020-05-05"),9.5,"May-05", pos=4)

dctf <- dc[dc$days>10 & !is.na(dc$Cases),]

# Fit a logistic model to the data to predict the maximum number of cases
logisticModelSS <- nls(Cases ~ SSlogis(days, Asym, xmid, scal), dctf)

print(paste("Expected maximum number of cases =", ceiling(coef(logisticModelSS)['Asym'])))

plot(dctf$Date, log2(predict(logisticModelSS)), type="l",
     col="blue", lwd=3, main="Davidson cases")
lines(log2(Cases) ~ Date, data=dctf)
