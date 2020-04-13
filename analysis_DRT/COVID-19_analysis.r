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

dev.new(width=8, height=4)
par(mfrow=c(1,2))

plot(log2(cases) ~ days, data=r, type="n")
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

tr <- read.csv("TN_cases_DRT.csv")
tr$Date <- strptime(tr$Date, "%m/%d/%Y")
nc <- read.table("TN_new_cases_DRT.txt",skip=1, header=TRUE)
tr <- merge(tr, nc, all=TRUE)
tr$days <- ceiling(difftime(tr$Date, "2020-03-05", units="days"))
m <- lm(log2(Cases) ~ days, data=tr[tr$days <= 14,])


plot(log2(Cases) ~ days, data=tr, type='l', xlim=c(0,45), ylim=c(0,20))
abline(m, col='green', lwd=3)


# Davidson county only cases
dc <- read.table("Davidson_cases_DRT.txt", header=TRUE, as.is=TRUE)
dc$Date <- as.Date(dc$Date)
plot(log2(Cases) ~ Date, dc, type='l')


m2 <- lm(log2(Cases) ~ Date, data=dc[dc$Date > as.Date("2020-04-01"),])
plot(log2(Cases) ~ Date, dc[dc$Date > as.Date("2020-04-01"),], type='l')
abline(m2, col='green', lwd=3)


