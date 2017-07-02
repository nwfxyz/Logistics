# Logs Tracking
# rm(list =ls())
options(stringsAsFactors = F)
options(scipen = 999)
# setwd("C:/Users/userpv/Documents/Rwd/SMS/V1")
source('functions.R')
# source('ui.R')
# source('ShinyFunctions.R')
PriceListDef <- ReadPriceList()
if(!exists('PriceListDef')){print("Price List not found. type ManualSelectPriceList()") }
#check if need to backup(weekly) / create new log file (monthly)
LogFileLocation<- ("LogsInput.csv")
# 
# if(file.copy("LogsInput.csv",format.Date(Sys.Date(),'%d'),".csv"),overwrite = TRUE ){
# cat("Backup Successful ! "  )
# }