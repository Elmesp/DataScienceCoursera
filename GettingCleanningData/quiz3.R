url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
f <- file.path(getwd(), "ss06hid.csv")
download.file(url, f)
dt <- data.table(read.csv(f))
agricultureLogical <- dt$ACR == 3 & dt$AGS == 6
which(agricultureLogical)[1:3]