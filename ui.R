#ui
library(shiny)
library(DT)
library(xlsx)
# setwd("C:/Users/userpv/Documents/Rwd/SMS")
# rm(list =ls())
setwd("C:/Users/userpv/Documents/Rwd/SMS/V2")
# source('ui.R')
source('ShinyFunctions.R')
source('Call file.R')
#source(ui.R)
Invoice.env <- new.env()
InvoiceTable <- ReadLogFile(Sys.Date())[0,]
NewInvoice()


## TO DO :
## add customer name and address in invoice page
## PO
## Add InvoiceLogging



ui <- shinyUI(
  fluidPage(
    titlePanel("Logistics SMS"),
    
    tabsetPanel(
      "Add Items",
      
      ### STOCK 
      
      
      tabPanel("Stock",
               
               sidebarLayout(
                 sidebarPanel(
                   
                   hr(),
                   textInput("ProductID", label = ("Product ID"), 
                             value = "") , 
                   
                   textInput("Quantity", label = ("Quantity"), 
                             value = "")  ,
                   hr(),
                   
                   textInput("Location", label = ("Location"), 
                             value = "")   ,
                   hr(),
                   
                   textInput("Date", label = ("Entry Date"), 
                             value = Sys.Date())   ,
                   hr(),
                   
                   textInput("CostPrice", label = ("Cost Price"), 
                             value = "From Price List"),   
                   actionButton("SaveButton", "Save")
                 ),mainPanel(
                   tableOutput("stock1"),
                   textOutput("StockSuccess")
                 )
               )),
      tabPanel("Sell",
               
               
               sidebarLayout(
                 sidebarPanel(
                   
                   textInput("CustID", label = ("Customer ID"), 
                             value = ""),
                   
                   textInput("Sell_ProductID", label = ("Product ID"), 
                             value = "") , 
                   
                   textInput("Sell_Quantity", label = ("Quantity"), 
                             value = "")  ,
                   hr(),
                   
                   textInput("Sell_Location", label = ("Location"), 
                             value = "")   ,
                   hr(),
                   
                   textInput("Sell_Date", label = ("Entry Date"), 
                             value = Sys.Date())   ,
                   hr(),
                   
                   textInput("Sell_Price", label = ("Price"), 
                             value = ""),   
                   textInput("Sell_CostPrice", label = ("Cost Price"), 
                             value = "From Price List"),  
                   actionButton("Sell_SaveButton", "Sell")
                 ),mainPanel(
                   tableOutput("sell1"),
                   textOutput("SellSuccess")
                 )
               )),
      tabPanel("Invoice",
               
               sidebarLayout(
                 sidebarPanel(
                   
                   textInput("ISell_Date", label = ("Entry Date"), 
                             value = Sys.Date())   ,
                   hr(),
                   
                   textInput("ICustID", label = ("Customer ID"), 
                             value = ""),
                   
                   textInput("InvoiceNo", label = "Invoice No: ", 
                             value = "") , 
                   
                   textInput("PO_No", label = "Customer Purchasing Order No: ", 
                             value = "") , 
                   
                   radioButtons("InvoiceRow", "Invoice Product:", list('Product 1' = 1,'Product 2' = 2, 'Product 3' = 3,"Product 4" = 4, "Product 5" = 5),
                                selected = 1) ,
                   
                   textInput("ISell_ProductID", label = ("Product ID"), 
                             value = "") , 
                   
                   textInput("ISell_Quantity", label = ("Quantity"), 
                             value = "")  ,
                   hr(),
                   
                   textInput("ISell_Location", label = ("Location"), 
                             value = "")   ,
                   hr(),
                   
                   
                   textInput("ISell_Price", label = ("Price"), 
                             value = ""),   
                   textInput("ISell_CostPrice", label = ("Cost Price"), 
                             value = "From Price List"),  
                   
                   actionButton("ISell_SaveButton", "Add Product"),
                   
                   hr(),
                   
                   actionButton("I_PrintInvoice", "Print Invoice"),
                   
                   hr(),
                   
                   actionButton("I_NewInvoice", "New Invoice")
                 ),mainPanel(
                   tableOutput("Invoice1"),
                   textOutput('Invoice1Success'),
                   tableOutput("InvoiceTable"),
                   
                   textOutput('InvoicePrint'),
                   textOutput('InvoiceNew1')
                   
                 )
                 
                 
                 
               )),
      "Business Statistics",
      tabPanel("Check Product"),
      tabPanel("Business Summary"),
      "-----",
      tabPanel("Delete SID",
               sidebarLayout(
                 sidebarPanel(
                   
                   hr(),
                   textInput("DeleteSID_Input", "SID", value = "SID"),
                   
                   hr(),
                   
                   actionButton("DeleteSID_Button", "Delete Entry")
                 ),
                 mainPanel(
                   tableOutput("DeleteSID_Show"),
                   textOutput("DeleteSID_Result")
                 )
               )
               
      ),
      tabPanel("Add Customer",  
               sidebarLayout(
                 sidebarPanel(
                   
                   hr(),
                   textInput("AddCust_CustID", "Customer ID", value = ""),
                   
                   hr(),
                   textInput("AddCust_CustName", "Customer Name", value = ""),
                   
                   hr(),
                   textInput("AddCust_Address1", "Customer Address Line 1", value = ""),
                   textInput("AddCust_Address2", "Customer Address Line 2", value = ""),
                   textInput("AddCust_Address3", "Customer Address Line 3", value = ""),
                   
                   hr(),
                   
                   actionButton("AddCust_Button", "Add Customer")
                 ),
                 mainPanel(
                   tableOutput("AddCust_Show"),
                   textOutput("AddCust_Result")
                 )          
               )
      ),
      tabPanel("Entry Log",  
               DT::dataTableOutput('LogFile')
                 )          
               
      
      # tabPanel("",  
      #          sidebarLayout(
      #            sidebarPanel(
      #              
      #            ),
      #            mainPanel(
      #              
      #            )          
      #          )
      # ),
      
      
      
    )
  )
  
)  




server <- shinyServer(function(input,output){
  
  ##Stock 
  
  
  stock1 <- reactive({   
    Shiny_Stock(input$ProductID,input$Quantity,input$Location,Date = input$Date, CostPrice = input$CostPrice, Save = F)
  })
  re <- eventReactive(input$SaveButton,{
    Shiny_Stock(input$ProductID,input$Quantity,input$Location,Date = input$Date, CostPrice = input$CostPrice, Save = T)
  })
  
  
  output$stock1 <- renderTable({stock1()})
  output$StockSuccess <- renderText({re()})
  
  
  #######Stock End######
  
  #####SELL####
  
  
  
  sell1 <- reactive({   
    Shiny_Sell(input$Sell_ProductID,input$Sell_Quantity,input$Sell_Location,Date = input$Sell_Date,CustID = input$CustID, Price = input$Sell_Price, PriceList = PriceListDef,CostPrice = input$Sell_CostPrice,Save = F)
  })
  save_sell <- eventReactive(input$Sell_SaveButton,{
    Shiny_Sell(input$Sell_ProductID,input$Sell_Quantity,input$Sell_Location,Date = input$Sell_Date,CustID = input$CustID, Price = input$Sell_Price,PriceList = PriceListDef,CostPrice = input$Sell_CostPrice, Save = T)
  })
  
  
  output$sell1 <- renderTable({sell1()})
  output$SellSuccess <- renderText({save_sell()})
  
  ####Sell End########
  
  
  
  ##invoicing - send output to external environment, call from external environemtn
  
  
  InvoiceRow <- reactive({   
    Shiny_Sell(input$ISell_ProductID,input$ISell_Quantity,input$ISell_Location,Date = input$ISell_Date,CustID = input$ICustID, Price = input$ISell_Price, PriceList = PriceListDef,CostPrice = input$ISell_CostPrice,Save = F, Invoice = F)
  })
  
  Save_InvoiceRow <- eventReactive(input$ISell_SaveButton,{
    print('nebby')
    Shiny_Sell(input$ISell_ProductID,input$ISell_Quantity,input$ISell_Location,Date = input$ISell_Date,CustID = input$ICustID, Price = input$ISell_Price, PriceList = PriceListDef,CostPrice = input$ISell_CostPrice,Save = F, Invoice = input$InvoiceRow)
  })
  
  
  InvoiceNew1 <- eventReactive(input$I_NewInvoice,{NewInvoice()
  })
  
  InvoiceTable_Re <- eventReactive(list(input$ISell_SaveButton,input$I_NewInvoice),{InvoiceTable <- rbind(Invoice.env$InvoiceRow1,Invoice.env$InvoiceRow2,Invoice.env$InvoiceRow3,Invoice.env$InvoiceRow4,Invoice.env$InvoiceRow5)
  print(InvoiceTable)})
  
  
  InvoicePrint_Re <- eventReactive(input$I_PrintInvoice,{InvoicePrinting(input$ICustID,input$InvoiceNo,input$PO_No)
  })
  
  
  output$Invoice1 <- renderTable({InvoiceRow()})
  output$Invoice1Success <- renderText({Save_InvoiceRow()})
  output$InvoiceTable <- renderTable({InvoiceTable_Re()})
  output$InvoiceNew1 <- renderText({InvoiceNew1()})
  output$InvoicePrint <- renderText({InvoicePrint_Re()})
  # output$SellSuccess <- renderText({save_sell()})
  
  ####Sell End########
  
  
  ###Delete SID Start
  
  DeleteSID_Show <- reactive({ DeleteSID_Shiny(input$DeleteSID_Input,Show = T)})
  
  output$DeleteSID_Show <- renderTable({DeleteSID_Show()})
  
  DeleteSID_Result <- eventReactive(input$DeleteSID_Button,{ DeleteSID_Shiny(input$DeleteSID_Input)})
  
  output$DeleteSID_Result <- renderText({DeleteSID_Result()})
  
  ###Delete SID End
  
  
  ### Add Cust Start 
  
  
  AddCust_Show <- reactive({ AddCustomer(input$AddCust_CustID, input$AddCust_CustName,
                                         paste(input$AddCust_CustName,input$AddCust_Address1,input$AddCust_Address2,input$AddCust_Address3, sep = "\n")
                                         ,Save = F)})
  
  output$AddCust_Show <- renderTable({AddCust_Show()})
  
  AddCust_Result <- eventReactive(input$AddCust_Button,{ AddCustomer(input$AddCust_CustID, input$AddCust_CustName,
                                                                       paste(input$AddCust_CustName,input$AddCust_Address1,input$AddCust_Address2,input$AddCust_Address3, sep = "\n")
                                                                       ,Save = T)})
  
  output$AddCust_Result <- renderText({AddCust_Result()})
  
  
  
  
  ### Add Cust End

  
  ### View Log file
  
  output$LogFile <- DT:: renderDataTable(DT::datatable(ReadLogFile(),options  = list(pageLength = 20)))
  
  
  }



)

shinyApp(ui = ui , server = server)
