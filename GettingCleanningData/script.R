# working directory
setwd("~/Desktop/playground/r")

# download file from internet

if (!file.exists("BaltimoreData")) {
  dir.create("BaltimoreData")
}
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.xlsx?accessType=DOWNLOAD"
download.file(fileUrl, destfile = "cameras.xlsx", method = "curl")
dateDownloaded <- date()

# read csv file to data
cameraData <- read.table("./cameras.csv", sep = ",", header = TRUE)

# read xlsx file to data
# sudo apt-get install r-cran-rjava
install.packages("xlsx")
cameraData <- read.xlsx("./cameras.xlsx", sheetIndex=1, header = TRUE)

# Subset by rows and columns
colIndex <- 2:3
rowIndex <- 1:4
cameraDataSubset <- read.xlsx("./cameras.xlsx", sheetIndex = 1, colIndex = colIndex, rowIndex = rowIndex)

# data.table
DF = data.frame(x = rnorm(9), y = rep(c("a","b","c"), each=3), z = rnorm(9))
DT = data.table(x = rnorm(9), y = rep(c("a","b","c"), each=3), z = rnorm(9))
# Choose only a type
DT[DT$ y=="a",]

# Summarize data
DT[,list(mean(x), sum(z))]
# New column w = z column al 2
DT[,w:=z^2]
# If duplicate, use copy
DT[,m:= { tmp <- (x + z); log2(tmp + 5) }]
DT[,a:= x > 0]
DT[,b:= mean(x + w), by = a]
# Count
DT[, .N, by = x]
# Count if
communityTable[communityTable$ VAL==24, .N, by = VAL]

# Connections
#MySQL
install.packages("RMySQL")
ucscDb <- dbConnect(MySQL(), user = "genome", host = "genome-mysql.cse.ucsc.edu")
result <- dbGetQuery(ucscDb, "show databases;"); dbDisconnect(ucscDb);

hg19 <- dbConnect(MySQL(), user = "genome", db = "hg19", host = "genome-mysql.cse.ucsc.edu")
allTables <- dbListTables(hg19)
length(allTables)
allTables[1:5]

query <- dbSendQuery(hg19, "SELECT * FROM affyU133Plus2 WHERE misMatches BETWEEN 1 AND 3")
affyMis <- fetch(query, n = 10); dbClearResult(query)
affyMis
dim(affyMis)

# HDF5
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
library(rhdf5)
rhdf5File <- "example.h5"
created <- h5createFile(rhdf5File)
created <- h5createGroup(rhdf5File, "foo")
created <- h5createGroup(rhdf5File, "bar")
created <- h5createGroup(rhdf5File, "foo/foobaa")

A <- matrix(1:10, nr = 5, nc = 2)
h5write(A, rhdf5File, "foo/A")
B <- array(seq(0.1, 0.2, by = 0.1), dim = c(5, 2, 2))
attr(B, "scale") <- "liter"
h5write(B, rhdf5File, "foo/foobaa/B")
h5ls(rhdf5File)

columns <- c("ab", "cde", "fghi", "a", "s")
df <- data.frame(1L:5L, seq(0, 1, length.out = 5), columns, stringsAsFactors = FALSE)
h5write(df, rhdf5File, "df")
h5ls(rhdf5File)

readA <- h5read(rhdf5File, "foo/A")
readB <- h5read(rhdf5File, "foo/foobaa/B")
readdf <- h5read(rhdf5File, "df")

h5write(c(12, 13, 14), rhdf5File, "foo/A", index = list(1:3, 1))
h5read(rhdf5File, "foo/A")

# Web
con <- url("http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en")
htmlCode <- readLines(con)

library(XML)
url <- "http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en"
html <- htmlTreeParse(url, useInternalNodes = TRUE)
xpathSApply(html, "//title", xmlValue)

library(httr)
html2 <- GET(url)
content2 <- content(html2, as = "text")
parsedHtml <- htmlParse(content2, asText = TRUE)
xpathSApply(parsedHtml, "//title", xmlValue)

auth <- authenticate("user", "passwd")
pg <- GET("http://httpbin.org/basic-auth/user/passwd", auth)
names(pg)

google <- handle("http://google.com")
pg1 <- GET(handle = google, path = "/")
pg2 <- GET(handle = google, path = "search")

# APIs
library(httr)
install.packages("httpuv")
library(httpuv)
library(jsonlite)
appName <- "github"
oauth_endpoints(appName)
key <- "7467746be28aefc9f9ce"
secret <- "f2706b0f90020c6ce10ac8b1e1d19bbbd795891d"
myapp <- oauth_app(appName, key, secret)

github_token <- oauth2.0_token(oauth_endpoints(appName), myapp)
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(req)
json <- content(req)
json <- fromJSON(toJSON(json))
json[json$ name=="datasharing",] $created_at

install.packages("sqldf")
library(sqldf)
library(RMySQL)
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv", "communitySurvey.csv")
DT <- read.table("communitySurvey.csv", header = TRUE)
head(DT)
query <- "select * from DT where AGEP < 50 and pwgtp1"
sqldf(query)

con <- url("http://biostat.jhsph.edu/~jleek/contact.html")
htmlCode <- readLines(con)
nchar(htmlCode[10])
nchar(htmlCode[20])
nchar(htmlCode[30])
nchar(htmlCode[100])

# .for file
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"
download.file(fileUrl, destfile = "./data/ac_survey.csv")
destName <- "dataFor.for"
# Using method = "curl" on OS X works
download.file(fileUrl, destName)
list.files(getwd())
q5_df <- read.fwf(destName, widths = c(15, 4, 1, 3, 5, 4), header = FALSE, sep = "\t", skip = 4)
head(q5_df)
# Need to sum up the V6 column
sum(q5_df$V6)

# subsetting, sorting and ordering
set.seed(13435)
X <- data.frame("var1" = sample(1:5), "var2" = sample(6:10), "var3" = sample(11:15))
X <- X[sample(1:5), ]
X $var2[c(1, 3)] = NA
X[1:2, "var2"]
X[which(X $var2 > 8),]
sort(X $var2, na.last = TRUE)
X[order(X $var2, X $var3, na.last = TRUE),]

install.packages("plyr")
library(plyr)
arrange(X, desc(var1))
X $var4 <- rnorm(5)
Y <- cbind(X, rnorm(5))

# Summarizing data
quantile(X, na.rm = TRUE)
table(X $var1, X $var3)
table(X $var3 %in% c(11, 12))
X[X $var3 %in% c(11, 12),]
data("UCBAdmissions")
DF <- as.data.frame(UCBAdmissions)
summary(DF)
str(DF)
# Cross tabs
xt <- xtabs(Freq ~ Gender + Admit, data = DF)
xt
warpbreaks $replicate <- rep(1:9, len = 54)
xt <- xtabs(breaks ~., data = warpbreaks) # . = all variables
xt
ftable(xt)
# Creating new variables
fileUrl <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile = "restaurants.csv")
restdata <- read.csv("restaurants.csv")
restdata $nearMe <- restdata $neighborhood %in% c("Roland Park", "Homeland")
restdata $zipWrong <- ifelse(restdata $zipCode < 0, TRUE, FALSE)
table(restdata $zipWrong, restdata $zipCode < 0)
restdata $zipGroups <- cut(restdata $zipCode, breaks = quantile(restdata $zipCode))
table(restdata $zipGroups)
# OR
install.packages("Hmisc")
library(Hmisc)
restdata $zipGroups = cut2(restdata $zipCode, g = 4)
table(restdata $zipGroups)

# Reshaping data
library(reshape2)
mtcars $carname <- rownames(mtcars)
carMelt <- melt(mtcars, id = c("carname", "gear", "cyl"), measure.vars = c("mpg", "hp"))
head(carMelt, n = 3)
cylData <- dcast(carMelt, cyl ~ variable)

