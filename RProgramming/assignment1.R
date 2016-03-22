dataZip <- "data.zip"
download.file("https://d396qusza40orc.cloudfront.net/rprog/data/specdata.zip", dataZip)
# local unzip

pollutantmean <- function(directory, pollutant, id = 1:332) {
  setwd(directory)
  total = 0
  observations = 0

  for (i in id) {
    if (i < 10) {
      fileNumber <- "00"
    } else if (i > 9 & i < 100) {
      fileNumber <- "0"
    } else {
      fileNumber <- ""
    }
    
    fileName <- paste(fileNumber, as.character(i), ".csv", sep = "")

    data <- read.csv(fileName, header = TRUE, na.strings = c("NA", "NaN", " "))
    data <- na.omit(data)
    observations <- observations + nrow(data)

    if (pollutant == "sulfate") {
      total <- total + sum(data$sulfate)
    } else {
      total <- total + sum(data$nitrate)
    }
  }
  
  ## returning the mean of the pollutant values
  return (total/observations)
}

workDir <- "C:/Users/Elmer/Documents/DataScienceCoursera/RProgramming/specdata"

#faq

# see also
# https://github.com/rdpeng/practice_assignment/blob/master/practice_assignment.rmd

# run this file 
# R -f  pollutantmean.R
# or source('~/project/R-Programming/week2/pollutantmean.R')

# Example:
# pollutantmean('/Users/win/project/R-Programming/week2/specdata', 'sulfate', 1:10)

pollutantmean <- function(directory, pollutant, id = 1:332) {
  # set dictory to your specdata folder
  # 交作業時把這行 comment
  # setwd(directory)
  
  # 把 specdata 資料夾底下的檔案都列出來
  # full.names 的參數可以取得絕對路徑
  
  # 320.csv -->  "/Users/win/project/R-Programming/week2/specdata/320.csv"
  fules_full <- list.files(directory, full.names = TRUE)
  
  # 不可以用 data.frame   一定要呼叫()
  # init empty date.frame
  data <- data.frame()
  
  for (i in id)
  {
    # 每隻 csv 開始搜，rbind 是搜每一行
    # rbind 合併 row, 一直把抓到的資料 bind 到 data 去
    data <- rbind(data, read.csv(fules_full[i]))
  }
  
  # 計算平均值，至於要計算哪一欄，可能是 (sulfate or nitrate 硫酸鹽或硝酸鹽) 吧
  # 參數 na.rm 表示是否要移除 NA 的值，預設是 false
  # 但因題目條件，必須將 NA 刪掉
  # 有另一方式是 colmean，但我不會 XD
  mean(data[, pollutant], na.rm = TRUE)
}

complete <- function(directory, id = 1:332) {
  
  data <- data.frame()
  
  files_full <- list.files(directory, full.names = TRUE)
  
  for (i in id)
  {
    # 每隻 csv 開始搜，rbind 是搜每一行
    # rbind 合併 row, 一直把抓到的資料 bind 到 data 去
    #data <- rbind(data, read.csv(files_full[i]))
    thisrow <- read.csv(files_full[i])
    
    # 取單一一個 row 總和
    # ‘is.na’, ‘na.omit’, ‘na.fail’, 'complete.cases' 是差不多的意思
    nobs <- sum(complete.cases(thisrow))
    # print(nobs)
    
    tmp <- data.frame(i, nobs)
    
    data <- rbind(data, tmp)
  }
  
  # 改變 column name
  colnames(data) <- c("id", "nobs")
  
  # 類似 return，這行一定要有
  data
  
  # 測試可先印前六行
  # print(data)
}

corr <- function(directory, threshold = 0) {
  
  files_full <- list.files(directory, full.names = TRUE)
  
  data <- vector(mode = "numeric", length = 0)
  
  for (i in 1:length(files_full)) {
    
    thisrow <- read.csv(files_full[i])
    
    # thisrow$XXX 取某個 column 欄位 
    # 取總和，如果 (sulfate or nitrate 硫酸鹽或硝酸鹽) 都不是 NA
    csum <- sum((!is.na(thisrow$sulfate)) & (!is.na(thisrow$nitrate)))
    
    if (csum > threshold) {
      tmp <- thisrow[which(!is.na(thisrow$sulfate)), ]
      submoni_i <- tmp[which(!is.na(tmp$nitrate)), ]
      data <- c(data, cor(submoni_i$sulfate, submoni_i$nitrate))
    }
  }
  
  data
}

#end faq

pollutantmean(workDir, "sulfate", 1:10)
pollutantmean(workDir, "nitrate", 70:72)
pollutantmean(workDir, "sulfate", 34)
pollutantmean(workDir, "nitrate")
cc <- complete(workDir, c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete(workDir, 54)
print(cc$nobs)

set.seed(42)
cc <- complete(workDir, 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

cr <- corr(workDir)
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr(workDir, 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr(workDir, 2000)                
n <- length(cr)                
cr <- corr(workDir, 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
