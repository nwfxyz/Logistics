# Invoicing

#NewSell



Shiny_Stock <- function(ProductID, Quantity, Location, CostPrice = " ", Date = Sys.Date(), PriceList = PriceListDef,  
                        Save = F, Invoice = F){
  
  
  #UserInput   
  LogIn <- ReadLogFile(Date) 
  CustIn <- ReadCustFile()
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
  
  if(ProductID == ""){
    
    return(data.frame('New Entry' = 'Insert ProductID'))
  }
  if(Quantity == ""){
    
    return(data.frame('New Entry' = 'Insert Quantity'))
  }
  
  if(Location == ""){
    
    Location <- "Default Location"
    
  }
  
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
  
  
  #Userinput 2
  if(CostPrice == "From Price List"){
    InputRow$CostPrice <- as.numeric(InputRow$Price)
  } else {
    InputRow$CostPrice <- as.numeric(CostPrice)
  }
  print(Save)
  if(Save == T){
    LogFileUpdate <- rbind(LogIn,InputRow)
    write.csv(LogFileUpdate,"LogsInput.csv", row.names = F)
    return(paste("Successfully updated as SID: ",SID))
  }
  return(InputRow)
}



Shiny_Sell <- function(ProductID, Quantity, Location, Price = " ",CostPrice, CustID = 100,Date = Sys.Date(), PriceList = PriceListDef,  
                       Save = F, Invoice = F){
  print(Invoice)
  # print(Date)
  # print(ProductID)
  # print(Quantity)
  # print(Location)
  # print(Price)
  # print(CostPrice)
  # print(CustID)
  # print(Date)
  # print(Save)
  # 
  #UserInput   
  LogIn <- ReadLogFile(Date) 
  CustIn <- ReadCustFile()
  SID <- GetSeriesID(Date,LogIn)
  if(Invoice != F){
    print(SID)
    SID <- as.character(as.numeric(SID) + as.numeric(Invoice) -1)
    print(SID)
  }
  
  if(ProductID == ""){
    
    return(data.frame('New Entry' = 'Insert ProductID'))
  }
  if(Quantity == ""){
    
    return(data.frame('New Entry' = 'Insert Quantity'))
  }
  
  if(Location == ""){
    
    Location <- "Default Location"
    
  }
  
  if(Price == ""){
    
    return(data.frame('New Entry' = 'Insert Price'))
  }
  
  if(CustID == ""){
    
    return(data.frame('New Entry' = 'Insert CustID'))
  }
  
  
  RowFrame <- data.frame(
    SID = SID,
    Date = Date,
    ProductID = ProductID, 
    Quantity = -as.numeric(Quantity), 
    Location = Location,
    CustID = CustID,
    Price = Price
  )
  
  colnames(PriceList) <- c('ProductID','ProductName','CostPrice')
  InputRow <- merge(RowFrame,PriceList, all.x = T , all.y = F)
  
  InputRow <- merge(InputRow,CustIn, all.x = T , all.y = F)
  
  
  if(CostPrice != "From Price List"){
    InputRow$CostPrice <- CostPrice
  } 
  
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

  
  #Userinput 2
  
  

  
  print(Save)
  if(Save == T){
    LogFileUpdate <- rbind(LogIn,InputRow)
    write.csv(LogFileUpdate,"LogsInput.csv", row.names = F)
   
    return(paste("Successfully updated as SID: ",SID))
  }
  if(Invoice != F){
    assign(paste0("InvoiceRow",Invoice),value = InputRow,envir = Invoice.env)
    return(paste0("Added Product ",Invoice))
  }
  print(InputRow)
  return(InputRow)
  
  
  
}

# Shiny_Sell(102220,100,'Desk',1000,1000,100,,,Save = T, Invoice = 1)

NewInvoice <- function(){
  
  for(i in 1:5){
    
    assign(paste0('InvoiceRow',i),value = data.frame(),envir = Invoice.env)
  }
  return("Invoice Data Cleared")
}

InvoicePrinting <- function(CustID,InvoiceNo, CustomerPO = "")
{
  TableInvoicePrint <- data.frame()
  for(i in 1:5){
    
    tmp <- get(paste0('InvoiceRow',i),envir = Invoice.env)
    TableInvoicePrint <- rbind(TableInvoicePrint,tmp)
  }
  
  LogFile <- ReadLogFile()
  LogFileUpdate <- rbind(LogFile,TableInvoicePrint)
  write.csv(LogFileUpdate,"LogsInput.csv", row.names = F)
  
  
  
  
  CustFile <- ReadCustFile()
  
  Address <- CustFile[CustFile$CustID == CustID,'Address']
  
  TableInvoicePrint <- cbind(TableInvoicePrint,Address,InvoiceNo,CustomerPO)
  print(TableInvoicePrint$Price)
  print(213123)
  TableInvoicePrint$Price <- as.character(format(as.numeric(TableInvoicePrint$Price), nsmall =2, big.mark = ","))
  print(TableInvoicePrint$Price)
print(TableInvoicePrint$Price)
  print(TableInvoicePrint)
  InvoiceFile <- ReadInvoiceLogFile()
  InvoiceFileUpdate <- rbind(InvoiceFile,TableInvoicePrint)
  write.csv(LogFileUpdate,"LogsInput.csv", row.names = F)
  
  
  write.xlsx2(TableInvoicePrint,file = "InvoicePrinting.xlsx")
  shell.exec(paste0(getwd(),"/","InvoicePrint.xlsx"))
  return('Success')
}

DeleteSID_Shiny <- function(SID2, Show = F){
  LogIn <- ReadLogFile(Sys.Date())
  print(SID2)
  if(SID2 == "SID"){
    
    return(LogIn[(nrow(LogIn)-10):nrow(LogIn),])
  }
  

  if(!SID2 %in% LogIn$SID){
    return(paste("SID:",SID2,"does not exist
                 "))
  } else{
    
    if(Show == T){
      
      return(LogIn[LogIn$SID == SID2,])
      
    }
    LogFileUpdate <- LogIn[!LogIn$SID == SID2,]
    write.csv(LogFileUpdate,"LogsInput.csv", row.names = F)
    return(paste("SID:",SID2,"successfully removed from data."))
  }
}
#

AddCustomer <- function(CustID,Customer,Address,Save = F){
  
  CustFile <- ReadCustFile()
  
  InputRow <- data.frame(Customer = Customer,CustID = CustID,Address = Address)
  
  CustFileNew <- rbind(CustFile,InputRow)
  

  if(Save == F){
  return(InputRow)
  } else {
    
    if(CustID %in% CustFile$CustID){return("Cust ID already exist, choose another")} else{
    write.csv(CustFileNew,"CustomerList.csv",row.names = F)
    return("Customer added.")
    }
  }
}
  
  
# Shiny_Sell('102220','100','102',"103","From Price List",100,Date =Sys.Date(),PriceListDef,F,2)
