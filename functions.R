



ReadInvoiceLogFile <- function(Date1 = Sys.Date()){
  # Date1 <- as.Date(Date1)
  
  LogFile <- read.csv('InvoiceLog.csv', header = T )
  LogFile[grepl("/",LogFile$Date),]$Date <- as.character(as.Date(LogFile[grepl("/",LogFile$Date),]$Date,"%d/%m/%Y"))
  
  LogFile$Date <- as.Date(LogFile$Date)
  return(LogFile)
}


ReadPriceList <- function(){
  
  PriceList <- read.xlsx2('ProductList.xlsx',1, colIndex = c(1:3))
  return(PriceList)
}




ReadLogFile <- function(Date1 = Sys.Date()){
  # Date1 <- as.Date(Date1)
  
  LogFile <- read.csv('LogsInput.csv', header = T )
  LogFile[grepl("/",LogFile$Date),]$Date <- as.character(as.Date(LogFile[grepl("/",LogFile$Date),]$Date,"%d/%m/%Y"))
  
  LogFile$Date <- as.Date(LogFile$Date)
  return(LogFile)
}


ReadCustFile <- function(FolderLocation = FolderLocationDef){
  
  CustFile <- read.csv('CustomerList.csv', header = T )
  return(CustFile)
}


GetSeriesID <- function(date,LogFile = LogIn){
  
  date2 <- as.Date(date)
  AB <- format.Date(date2, '%y') #first 2 character is year
  CD <- format.Date(date2, '%m') #second 2 character is month
  if(!format.Date(date2,"%Y-%m") %in% substr(LogFile$Date,1,7)){ 
    EFGHIJ <- "00001" #last 6 characters is number
  } else{
    EFGHIJ <-  sprintf("%05d",as.numeric(substr(LogFile[nrow(LogFile),]$SID,5,10))+1)
  }
  SID <- paste0(AB,CD,EFGHIJ)
  return(SID)
}


Stock <- function(PriceList = PriceListDef, Directory = FolderLocation, Price = F, Date = Sys.Date()){
  
  #UserInput   
  LogIn <- ReadLogFile(Directory) 
  CustIn <- ReadCustFile(Directory)
  
  ProductID <- readline("Product ID?   ")
  Quantity <- readline("Quantity?    ")
  Location <- readline("Stored at?    ")
  
  SID <- GetSeriesID(Date,LogIn)
  
  RowFrame <- data.frame(
    SID = SID,
    Date = Date,
    ProductID = ProductID, 
    Quantity = Quantity, 
    Location = Location,
    CustID = "B",
    CostPrice = ''
  )
  
  
  colnames(PriceList) <- c('ProductID','ProductName','Price')
  InputRow <- merge(RowFrame,PriceList, all.x = T , all.y = F)
  
  InputRow <- merge(InputRow,CustIn, all.x = T , all.y = F)
  
  if(is.na(InputRow$ProductName)){
    
    stop(paste0("ProductID: [",ProductID,"] not found. Update Product List or Check Product ID."))
  }
  
  InputRow <- InputRow[c( "SID",
                          "Date",
                          "ProductID" ,  
                          "ProductName", 
                          "Quantity"  ,  
                          "Location",    
                          "Price",
                          "CustID",
                          "Customer",
                          'CostPrice')]
  
  if(Price == T){
    #Userinput 2
    CostPrice <- readline("Price?   ")
    InputRow$CostPrice <- as.numeric(CostPrice)
  } else {
    InputRow$CostPrice <- as.numeric(InputRow$Price)
  }
  
  
  #Ask user to confirm 
  Answer1 <- readline(paste0(
    "
Did you just stock: [Y/N]

ProductID:  ",ProductID,"
Product Name:  ",InputRow$ProductName[1],"
Quantity:  ",Quantity,"
Location:  ",Location,"
Price:  ",InputRow$CostPrice)
  )
  
  if(sum(Answer1 == c("Y","y"))){
    LogFileUpdate <- rbind(LogIn,InputRow)
    write.csv(LogFileUpdate,paste0(Directory,"/LogsInput.csv"), row.names = F)
    print(paste("Successfully updated as SID: ",SID))
    return(InputRow)
  }
  
}

Sell <- function(CustID = '',PriceList = PriceListDef, 
                 Directory = FolderLocation, 
                 Price = T, 
                 Date = Sys.Date()){
  
  #UserInput   
  LogIn <- ReadLogFile(Directory)  
  CustIn <- ReadCustFile(Directory)
  
  ProductID <- readline("Product ID?   ")
  Quantity <- readline("Quantity?    ")
  Location <- readline("Stored at?    ")
  
  if(CustID == ''){
    CustID <- readline("Customer ID?   ")
  }
  
  
  SID <- GetSeriesID(Date,LogIn)
  
  #To add details, add to column here and INPUTROW
  RowFrame <- data.frame(
    SID = SID,
    Date = Date,
    ProductID = ProductID, 
    Quantity = as.numeric(Quantity), 
    Location = Location,
    CustID = CustID
  )
  
  
  colnames(PriceList) <- c('ProductID','ProductName','CostPrice')
  InputRow <- merge(RowFrame,PriceList, all.x = T , all.y = F)
  InputRow <- merge(InputRow,CustIn, all.x = T , all.y = F)
  
  if(is.na(InputRow$ProductName)){
    
    stop(paste0("ProductID: [",ProductID,"] not found. Update Product List or Check Product ID."))
    
  }
  
  InputRow <- InputRow[c( "SID",
                          "Date",
                          "ProductID" ,  
                          "ProductName", 
                          "Quantity"  ,  
                          "Location",    
                          "CostPrice",
                          "CustID",
                          "Customer")]
  
  if(Price == T){
    #Userinput 2
    Price2 <- readline("Price sold? ")
    #print if Price != numerical error and break
    
    InputRow$Price <- as.numeric(Price2)
    
  }
  
  InputRow$Quantity <- -as.numeric(InputRow$Quantity)
  
  #Ask user to confirm 
  Answer1 <- readline(paste0(
    "
Did you just Sell: [Y/N]

ProductID:  ",ProductID,"
Product Name:  ",InputRow$ProductName[1],"
Quantity:  ",-as.numeric(Quantity),"
Location:  ",Location,"
Price:  ", Price2,"
Customer: ",InputRow$Customer[1])
  )
  
  if(sum(Answer1 == c("Y","y"))){
    LogFileUpdate <- rbind(LogIn,InputRow)
    write.csv(LogFileUpdate,paste0(Directory,"/LogsInput.csv"), row.names = F)
    print(paste("Successfully updated as SID: ",SID))
    return(InputRow)
  }
  
}


CheckQuantity <- function(FindProductID = "Unf",Directory = FolderLocation, Table = F, PriceList = PriceListDef){
  
  if(FindProductID == 'Unf'){
    FindProductID <- readline("Product ID?   ")
  }
  if(FindProductID == 'Unf'){
    break
  }
  
  
  if(nrow(PriceList[PriceList$ProductID == FindProductID,]) == 0){
    
    stop(paste0("ProductID: [",FindProductID,"] not found. Update Product List or Check Product ID."))
  }
  
  PartDes <- PriceList[PriceList$ProductID == FindProductID,]$ProductDes
  LogFile <- ReadLogFile(Directory)
  LogFileP <- LogFile[LogFile[,'ProductID'] == FindProductID,] 
  if(is.numeric(LogFileP$Price)){
    LogFileP$ListPrice <- as.numeric(LogFileP$Price)}
  LogFileP$Quantity <- as.numeric(LogFileP$Quantity)
  LocationSummary <- aggregate(LogFileP$Quantity,by = list(LogFileP$Location),FUN = sum)
  colnames(LocationSummary) <- c('Location','Quantity')
  cat(paste0(
    "
ProductNo:  ",FindProductID,"
Product:  ",PartDes,"
 
"))
  print(LocationSummary)
  if(Table){
    View(LocationSummary)
  }
  cat(" 
Total in Stock:  ",sum(LocationSummary$Quantity))
  
}

DeleteSID <- function(SID2, Show = F){
  
  LogIn <- ReadLogFile(Sys.Date())
  if(!SID2 %in% LogIn$SID){
    return(paste("SID:",SID2,"does not exist
"))
  } else{
    
    if(Show == T){
      
      return(LogIn[LogIn$SID == SID2])
      
    }
    LogFileUpdate <- LogIn[!LogIn$SID == SID2,]
    write.csv(LogFileUpdate,"LogsInput.csv", row.names = F)
    return(paste("SID:",SID2,"successfully removed from data."))
  }
}
# 


BusinessSummary_Y <- function(Month = format.Date(Sys.Date(),"%m"),Year = format.Date(Sys.Date(),'%Y'), Save = T){
  # Add older period log files into here
  if(Year == format.Date(Sys.Date(),'%Y')  ){
    
    LogIn <- ReadLogFile()
    LogIn_m <- LogIn[paste0("20",substr(LogIn$SID,1,2)) == Year,]
  }
  if(nrow(LogIn_m) == 0) {
    cat("No Input Data, Stock or Sell something first ! ")
  } else{
    
    Sales <- LogIn_m[LogIn_m$Price > 0,]
    Revenue <- abs(sum(Sales$Quantity * Sales$Price))
    Sales_T <- nrow(Sales)
    Expenditure <- LogIn_m[LogIn_m$Price < 0,]
    Cost <- -sum(Expenditure$Quantity * Expenditure$Price)
    Exp_T <- nrow(Expenditure)
    TotalTransaction <- Sales_T + Exp_T
    COGS_r <- sum(Sales$CostPrice*Sales$Quantity)
    Inventory <- sum(LogIn$Quantity*LogIn$CostPrice)
    Profit <- Revenue - COGS_r
    
    Summary_df <- t(data.frame(
      Revenue,
      'Cost of Goods Sold' = COGS_r,
      Profit,
      'Purchasing' = Cost,
      'Inventory' = Inventory,
      'Total Sales Transaction' = Sales_T,
      'Total Purchasing Transaction' = Exp_T,
      'Total Transaction' = TotalTransaction
    ))
    Summary_df[1:5] <- paste0("$",Summary_df[1:5])
    colnames(Summary_df) <- 'Summary'
    
    cat("
 Business Summary for:",Year,"

")
    if(Save == T){
      write.xlsx2(Summary_df,paste0("Business_Summary_",Year,".xlsx"))
    }
    return(Summary_df)
    
  }
}


BusinessSummary_M <- function(Month = format.Date(Sys.Date(),"%m"),Year = format.Date(Sys.Date(),'%Y'), Save = T){
  # Add older period log files into here
  if(Year == format.Date(Sys.Date(),'%Y')  ){
    
    LogIn <- ReadLogFile()
    LogIn_m <- LogIn[as.numeric(substr(LogIn$SID,3,4)) == Month,]
  }
  if(nrow(LogIn_m) == 0) {
    cat("No Input Data, Stock or Sell something first ! ")
  } else{
    
    Sales <- LogIn_m[LogIn_m$Price > 0,]
    Revenue <- abs(sum(Sales$Quantity * Sales$Price))
    Sales_T <- nrow(Sales)
    Expenditure <- LogIn_m[LogIn_m$Price < 0,]
    Cost <- -sum(Expenditure$Quantity * Expenditure$Price)
    Exp_T <- nrow(Expenditure)
    TotalTransaction <- Sales_T + Exp_T
    COGS_r <- sum(Sales$CostPrice*Sales$Quantity)
    Inventory <- sum(LogIn$Quantity*LogIn$CostPrice)
    Profit <- Revenue - COGS_r
    
    Summary_df <- t(data.frame(
      Revenue,
      'Cost of Goods Sold' = COGS_r,
      Profit,
      'Purchasing' = Cost,
      'Inventory' = Inventory,
      'Total Sales Transaction' = Sales_T,
      'Total Purchasing Transaction' = Exp_T,
      'Total Transaction' = TotalTransaction
    ))
    Summary_df[1:5] <- paste0("$",Summary_df[1:5])
    colnames(Summary_df) <- 'Summary'
    
    cat("
        Business Summary for:",paste0(Month,"/",Year),"
        
        ")
    if(Save == T){
      write.xlsx2(Summary_df,paste0("Business_Summary_",month.name[Month],"_",Year,".xlsx"))
    }
    return(Summary_df)
    
  }
}



GetLogFileMonth <- function(MonthName,Year){
  Month <- which(month.name == MonthName)
  Month <- sprintf('%02d',Month)
  LogFile <- ReadLogFile()
  LogIndex <- substr(LogFile$SID,1,4)
  Index <- paste0(Year,Month)
  return(LogFile[LogIndex == Index,])
}

AggregateData <- function(LogFile, Product = T){
  LogFileSales <- LogFile[LogFile$Price > 0,]
  LogFileSales$Price <- LogFileSales$Price* LogFileSales$Quantity
  LogFileSales$CostPrice <- LogFileSales$CostPrice* LogFileSales$Quantity
 AggregatedTablePrice <- aggregate(LogFileSales$Price, by = list(ProductID = LogFileSales$ProductID),sum) 
 AggregatedTableCostPrice <- aggregate(LogFileSales$CostPrice, by = list(ProductID = LogFileSales$ProductID),sum)  
Output <- merge(AggregatedTablePrice,AggregatedTableCostPrice,by = 'ProductID')
colnames(Output) <- c('ProductID','Price','CostPrice')
return(Output)
}
