#加载包并进行一些配置------------------------------
library(shiny)
library(shinydashboard)
library(shinyjs)
library(glue)
library(dplyr)
library(readxl)
library(RCurl)
library(tidyr)
library(prophet)
library(shinythemes)
library(quantmod)

options("getSymbols.warning4.0"=FALSE)
`%then%` <- shiny:::`%OR%`

#导入代码

fund_id <- read_excel("data/fund_id.xlsx")
stock_id_cn <- read_excel("data/stock_id.xlsx",sheet = "CN")
stock_id_us <- read_excel("data/stock_id.xlsx",sheet = "US")
stock_id_glob <- read_excel("data/stock_id.xlsx",sheet = "GLOB")
future_id_cn<-read_excel("data/futures_id.xlsx",sheet = "CN")
future_id_fn<-read_excel("data/futures_id.xlsx",sheet = "FN")




#导入中文---------
Chinese<- read_excel("data/Chinese.xlsx")

choices1 = list("Past 30 days" = 30,"Past 90 days" = 90,"Past 180 days" = 180,
               "Past 30 days" = 360)
choices2 = list("1 day" = 1,"3 days" = 3,
               "10 days"= 10)
choices3 = c("White" = "white",
             "White Mono" = "white.mono",
             "Black" = "black",
             "Black Mono" = "black.mono",
             "Beige" = "beige",
             "WSJ"= "wsj"
)
choices4 = c("Directional Movement Index" = "addADX()",
              "Average True Range" = "addATR()",
              "Bollenger Bands" = "addBBands()",
              "Commodity Channel Index" = "addCCI()",
              "Chaiken Money Flow" = "addCMF()",
              "Chande Momentum Oscillator" = "addCMO()",
              "Contract Expiration Bars" = "addExpiry()",
              "De-trended Price Oscillator" = "addDPO()",
              "Simple Moving Average 5 days" = "addSMA(n=5,col=\"green\")",
              "Simple Moving Average 10 days" = "addSMA(n=10,col=\"blue\")",
              "Simple Moving Average 30 days" = "addSMA(n=30,col=\"red\")",
              "Expotential Moving Average" = "addEMA()",
              "Weighted Moving Average" = "addWMA()",
              "Double Expotential Moving Average" = "addDEMA()",
              "Expotential Volume Weighted Moving Average" = "addEVWMA()",
              "ZLEMA" = "addZLEMA()",
              "Moving Average Convergence Divergence" = "addMACD()",
              "Price Envelope" = "addEnvelope()",
              "Relative Strength Index" = "addRSI()",
              "Parabolic Stop and Reversal Indicator" = "addSAR()",
              "Rate of Change" = "addROC()",
              "Stochastic Momemtum Indicator" = "addSMI()"
  )
names(choices1)<-c(Chinese$Chinese[48:51])
names(choices2)<-c(Chinese$Chinese[52:54])
names(choices3)<-c(Chinese$Chinese[60:65])
names(choices4)<-c(Chinese$Chinese[67:88])

#设置一些函数---------------
{
  
  
  id_choice_list<-function(id_df){
    output<-as.list(t(as.character(id_df$code)))
    names(output)<-id_df$id
    output
  }
  #洗数小程序---------------
strrd<-function(txt,sep,num){
  if (missing(num)){
    htm1<-strsplit(txt,sep)
    htm2<-unlist(htm1)}
  else
  {htm1<-strsplit(txt,sep)
  htm2<-unlist(htm1)
  htm2[num]}
}


  #爬虫程序------------------
dayDayFund<-function(i1,i2){
  
  i<-ifelse(as.numeric(i1<0),i2,i1)
  #写URL
  url<-paste("http://fund.eastmoney.com/pingzhongdata/",i,".js",sep="")
  #节选出有用的部分
  html<-getURL(url,.encoding ="utf-8")
  html1<-strrd(html,"Data_netWorthTrend =",2)
  html1<-strrd(html1,"];",1)
  html1<-strrd(html1,"\\},\\{")
  html9<-as.data.frame(html1)
  #将有用部分进行拆分
  html10<-separate(html9,col = html1,into=c("else1","time"),sep ="\"x\":") %>%
    separate(col=time,into = c("time","net_value"),sep=",\"y\":") %>%
    separate(col=net_value,into = c("net_value","equityReturn"),sep=",\"equityReturn\":") %>%
    separate(col=equityReturn,into= c("equityReturn","unitMoney"),sep = ",\"unitMoney\":\"") %>%
    separate(col=unitMoney,into=c("unitMoney","else2"),sep="\"") %>%
    subset(select=-c(else1,else2))
  #
  html10$code = i
  html10<-html10%>%select("code","time","net_value","equityReturn","unitMoney")
  html10$time<-as.character(as.POSIXct(as.numeric(html10$time)/1000, origin="1970-01-01"))
  html10$net_value<-as.numeric(html10$net_value)
  html10$equityReturn<-as.numeric(html10$equityReturn)
  html10
}#爬天天基金的净值

futures_xts<-function(name,type,days){
  if (type=="INF"){
    url<-paste("https://stock2.finance.sina.com.cn/futures/api/jsonp.php=/InnerFuturesNewService.getDailyKLine?symbol=",name,sep="")
    seplist<-c("\"d\":\"","\",\"o\":\"","\",\"h\":\"","\",\"l\":\"","\",\"c\":\"","\",\"v\":\"","\",\"p\":\"")
  }else {
    url<-paste("https://stock2.finance.sina.com.cn/futures/api/jsonp.php=/GlobalFuturesService.getGlobalFuturesDailyKLine?symbol=",name,sep="")
    seplist<-c("\"date\":\"","\",\"open\":\"","\",\"high\":\"","\",\"low\":\"","\",\"close\":\"","\",\"volume\":\"","\"")
  }
  html<-getURL(url,.encoding ="utf-8")
  html1<-strrd(html,"\\},\\{") %>%
    as.data.frame()
  names(html1)<-"start"
  html2<-separate(html1,col = start,into=c("else1","time"),sep =seplist[1]) %>%
    separate(col = time,into=c("time","open"),sep =seplist[2]) %>%
    separate(col = open,into=c("open","high"),sep =seplist[3]) %>%
    separate(col = high,into=c("high","low"),sep =seplist[4]) %>%
    separate(col = low,into=c("low","close"),sep =seplist[5]) %>%
    separate(col = close,into=c("close","volume"),sep =seplist[6]) %>%
    separate(col = volume,into=c("volume","else2"),sep =seplist[7])
  rows<-nrow(html2)
  html_final <- html2[rows-days+1:rows,2:7]
  html_final <- html_final[1:days,]
  dates<-as.Date(html_final[,1])
  html_final$add<-html_final$close
  datas<-html_final[,2:7]
  namexts<-c("RD.Open","RD.High","RD.Low","RD.Close","RD.Volume","RD.Adjusted")
  names(datas)<-namexts
  for (n in namexts){
    datas[,n]<-as.numeric(datas[,n])
  }
  RDxts <- xts(x = datas, order.by = dates)
  RDxts
}#爬期货并转为xts数据

dayDayFund_Name<-function(i1,i2){
  i<-ifelse(as.numeric(i1<0),i2,i1)
  url<-paste("http://fund.eastmoney.com/pingzhongdata/",i,".js",sep="")
  #find the useful part
  html<-getURL(url,.encoding ="utf-8")
  html1<-strrd(html,"fS_name = \"",2)
  html2<-strrd(html1,"\";var fS_code",1)
  html2
}#爬天天基金的名字

dayDayFund_realTime<-function(i){
  url<-paste("http://fundgz.1234567.com.cn/js/",i,".js",sep="")
  html<-getURL(url,.encoding ="utf-8")
  html1<-strrd(html,"gsz\":\"",2)
  html2<-as.numeric(strrd(html1,"\",\"gszzl",1))
  html2
}#爬取实时净值

  #基于30天/90天/180天建立模型，并预测未来k天-------------

forecast_rd<-function(data,d,k){
  df<-data[(nrow(data)-d+1) : nrow(data),2:3]
  names(df)<-c("ds","y")
  m<-prophet(df,growth = "linear",yearly.seasonality = FALSE,weekly.seasonality = FALSE,daily.seasonality=TRUE)
  output<-make_future_dataframe(m,k,freq='day',include_history = TRUE)
  fst<-predict(m,output)
  
  fst
}#出建模结果表


forecast_rd_plot<-function(data,d,k){
  df<-data[(nrow(data)-d+1) : nrow(data),2:3]
  names(df)<-c("ds","y")
  m<-prophet(df,growth = "linear",yearly.seasonality = FALSE,weekly.seasonality = FALSE,daily.seasonality=TRUE)
  output<-make_future_dataframe(m,k,freq='day',include_history = TRUE)
  fst<-predict(m,output)
  plot (m,fst,uncertainty =TRUE, plot_cap = TRUE, xlabel = 'ds', ylabel = 'y')
}#出建模图

forecast_increase<-function(outdata,k,i1,i2){
  i<-ifelse(as.numeric(i1<0),i2,i1)
  est_value = round(outdata$yhat[nrow(outdata)-k+1],4)
  est_value_lst=round(outdata$yhat[nrow(outdata)-k],4)
  realTime_value = round(dayDayFund_realTime(i),4)
  error<-paste(round((realTime_value/est_value-1)*100,4),"%",sep="")
  error_numic<-realTime_value/est_value-1
  trend_numic<- est_value/est_value_lst-1
  
  
  if(error_numic<(-0.02)){
    advice<-Chinese$Chinese[43]
  } else if(error_numic<(-0.01)){
    advice<-Chinese$Chinese[44]
  } else if(error_numic<0.01){
    advice<-Chinese$Chinese[45]
  } else if(error_numic<0.02){
    advice<-Chinese$Chinese[46]
  } else {
    advice<-Chinese$Chinese[47]
  }

  output<-c(est_value,realTime_value,error,advice)
  output
}#出建议
}





#ui--------------
{ui <- tagList(
  #第一页-----------
  navbarPage(
    theme = shinytheme("cosmo"),  
    Chinese$Chinese[1],
    #模型一----------------
    tabPanel(Chinese$Chinese[2],
               tabsetPanel(
                 tabPanel(Chinese$Chinese[11],
                          sidebarPanel(
                            width = 3,
                            div(h3(Chinese$Chinese[3])),
                            selectInput("Fund_id_1", label = h4(Chinese$Chinese[4]),
                                        choices = id_choice_list(fund_id)),
                            tags$hr(),
                            div(h3(Chinese$Chinese[7])),
                            selectInput("days_1", label = h4(Chinese$Chinese[8]),
                                        choices = choices1),
                            selectInput("days_2", label = h4(Chinese$Chinese[9]),
                                        choices = choices2),
                            tags$hr()
                          ),
                          mainPanel(
                          uiOutput("testUI"),
                          plotOutput(outputId = "mychart")
                          )
                 ),
    #模型二----------------
                 tabPanel(Chinese$Chinese[12], 
                          fluidPage(
                            
                            # 应用程序标题
                            #titlePanel("R quantmod Demonstration"),
                            
                            # 为 quantmod 绘图功能提供参数入口的输入栏
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("stock_type", label = h4(Chinese$Chinese[4]),
                                            choices = list("中国股市" = 1,"美股" = 2,"指数"=3)),
                                uiOutput("stock_choice"),
                                br(),
                                dateRangeInput("dtRange", Chinese$Chinese[58], start = Sys.Date()-90, end = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en", separator = " to ", width = NULL),
                                hr(),
                                flowLayout(checkboxGroupInput("ta", Chinese$Chinese[66],
                                                              choices4
                                                              )
                                           
                                )
                              ),
                              # 作图
                              mainPanel(
                                plotOutput("distPlot", height="885px")
                                #      ,textOutput("dispPrint")
                              )
                            )
                          )
                          # div(h1(Chinese$Chinese[14]))
                          ),
    #模型三----------------
    tabPanel(Chinese$Chinese[13], 
             fluidPage(
               
               # 应用程序标题
               #titlePanel("R quantmod Demonstration"),
               
               # 为 quantmod 绘图功能提供参数入口的输入栏
               sidebarLayout(
                 sidebarPanel(
                   selectInput("futures_type", label = h4(Chinese$Chinese[4]),
                               choices = list("国外" = "GLF","国内" = "INF")),
                   uiOutput("future_choice"),
                   sliderInput("f_days", label = h3("需要分析的天数"), min = 0, 
                               max = 100, value = 30),
                   # submitButton(text="Get Stock Quote"),
                   br(),
  
                   hr(),
                   flowLayout(checkboxGroupInput("ta_f", Chinese$Chinese[66],
                                                 choices4
                   )
                   
                   )
                 ),
                 # 作图
                 mainPanel(
                   plotOutput("distPlot_f", height="885px")
                        # ,textOutput("dispPrint")
                 )
               )
             )
             # div(h1(Chinese$Chinese[14]))
    )
    )),
    
  #第二页-----------    
    tabPanel(Chinese$Chinese[15], 
               tabsetPanel(
                 tabPanel(Chinese$Chinese[11],
                          h2(strong(Chinese$Chinese[16])),
                          h3(Chinese$Chinese[17]),
                          h4(Chinese$Chinese[18]),
                          h4(Chinese$Chinese[19]),
                          h4(Chinese$Chinese[20]),
                          br(),
                          br(),
                          h2(strong(Chinese$Chinese[21])),
                          h3(Chinese$Chinese[22]),
                          img(src = "demo1.png", height = 500, width = 1000),
                          h3(Chinese$Chinese[23]),
                          img(src = "demo2.png", height = 500, width = 1000),
                          h3(Chinese$Chinese[24]),
                          img(src = "demo3.png", height = 500, width = 1000),
                          h3(Chinese$Chinese[25]),
                          img(src = "demo4.png", height = 500, width = 1000)
                 ),
                 tabPanel(Chinese$Chinese[12], 
                          h2(strong(Chinese$Chinese[16])),
                          h3(Chinese$Chinese[90]),
                          h3(Chinese$Chinese[91]),
                          h4(Chinese$Chinese[92]),
                          h4(Chinese$Chinese[93]),
                          h4(Chinese$Chinese[94]),
                          h4(Chinese$Chinese[95]),
                          h4(Chinese$Chinese[96]),
                          h4(Chinese$Chinese[97]),
                          h4(Chinese$Chinese[98]),
                          h4(Chinese$Chinese[99]),
                          h4(Chinese$Chinese[100]),
                          h4(Chinese$Chinese[101]),
                          h4(Chinese$Chinese[102]),
                          h4(Chinese$Chinese[103]),
                          h4(Chinese$Chinese[104]),
                          h4(Chinese$Chinese[105]),
                          h4(Chinese$Chinese[106]),
                          h4(Chinese$Chinese[107]),
                          h4(Chinese$Chinese[108]),
                          h4(Chinese$Chinese[109]),
                          h4(Chinese$Chinese[110]),
                          h4(Chinese$Chinese[111]),
                          h4(Chinese$Chinese[112]),
                          h4(Chinese$Chinese[113])),
                          
                 tabPanel(Chinese$Chinese[13], 
                          h2(strong(Chinese$Chinese[16])),
                          div(h3(Chinese$Chinese[114])))
             )),
    
  #第三页-----------    
    tabPanel(Chinese$Chinese[26],
                 h1(Chinese$Chinese[27]),
                 p(Chinese$Chinese[115]),
                 p(Chinese$Chinese[89]),
                 p(Chinese$Chinese[28]),
                 p(Chinese$Chinese[29]),
                 p(Chinese$Chinese[30]),
                 br(),
                 h1(Chinese$Chinese[31]),
                 p(Chinese$Chinese[32]),
                 p(Chinese$Chinese[33]),
                 p(strong(Chinese$Chinese[34])),
                 br(),
                 h1(Chinese$Chinese[35]),
                 p(Chinese$Chinese[36]),
                 p(Chinese$Chinese[37]),
                 img(src = "wechat_qrcode.png", height = 300, width = 300)
               )
  )
)

}
#server--------------
{server <- function(input, output, session) {
  #模型一-------------------
  {
  #导入数据--------------------------------
  data<- reactive({
    dayDayFund(input$Fund_id_1,0)
  })
  
  data_name<-reactive({
    dayDayFund_Name(input$Fund_id_1,0)
  })
  
  #点击后运行程序，并设计反应按钮-------------------
  fund_result_data<-reactive({
    forecast_rd(data(),as.numeric(input$days_1),as.numeric(input$days_2))
  })
  
  fund_forecast<-reactive({
    forecast_increase(fund_result_data(),as.numeric(input$days_2),input$Fund_id_1,0)
  })
  
  Plot_fund<-reactive({
    forecast_rd_plot(data(),as.numeric(input$days_1),as.numeric(input$days_2))
  })
  
  
  
  #最终导出的主页面内容和UI-------------------------------
  output$testUI <- renderUI({
    withProgress(message = Chinese$Chinese[55],
                 detail = Chinese$Chinese[56], value = 0,{
                   for (i in 1:5) {
                     incProgress(1/5)
                     Sys.sleep(0.25)
                   }
    
                   fluidRow(
                     column(
                       width = 12,
                       downloadButton("downloadData", Chinese$Chinese[38]),
                       tags$h2(glue("{Chinese$Chinese[39]}
                     {data_name()}{Chinese$Chinese[40]}
                     {fund_forecast()[2]}{Chinese$Chinese[41]}
                     {fund_forecast()[1]}{Chinese$Chinese[42]}
                     {fund_forecast()[3]}
                     {fund_forecast()[4]} "))
                     )
                   )
                 })
                 })

  
  output$mychart<-renderPlot({
    
        Plot_fund()

  })
 
  #下载数据-------------------------------------
  output$downloadData <- downloadHandler( # 下载处理器
    filename = function() { paste(Sys.Date()[1], '.csv', sep='') },
    # 将文件写入到临时文件file
    content = function(file) {write.csv(fund_result_data(), file)}
  )
}
  
  #模型二----------
  {
    output$stock_choice <- renderUI({
      if(input$stock_type==1){
        selectInput("stock", label = h4(Chinese$Chinese[4]),
                    choices = id_choice_list(stock_id_cn))
      } else if(input$stock_type==2){
        selectInput("stock", label = h4(Chinese$Chinese[4]),
                    choices = id_choice_list(stock_id_us))
      } else{
        selectInput("stock", label = h4(Chinese$Chinese[4]),
                    choices = id_choice_list(stock_id_glob))
      }
                   })
    
    sSymbol <- reactive({
      tryCatch({
        suppressWarnings(getSymbols(input$stock, from=input$dtRange[1], to=input$dtRange[2],
                                    auto.assign = FALSE))
      }, error = function(err) {
        return(NULL)
      })
    })
    
    output$distPlot <- renderPlot({
      taStr<-"addVo()"
      if (!is.null(input$ta)) {
        for (ta in input$ta) {
          taStr<-paste(taStr, paste(";", ta))
        }
      }
      
      if(!is.null(sSymbol())) {
        chartSeries(sSymbol(), name=input$stock, TA=taStr,theme = chartTheme("white"),up.col='red',dn.col='green')
      }
    })
    
    output$dispPrint <- renderPrint({
      print(sSymbol())
    })
  }
  
  #模型三----------
  {
    
    output$future_choice <- renderUI({
      if(input$futures_type=="INF"){
        selectInput("futures", label = h4(Chinese$Chinese[4]),
                    choices = id_choice_list(future_id_cn))
      } else {
        selectInput("futures", label = h4(Chinese$Chinese[4]),
                    choices = id_choice_list(future_id_fn))
      } 
    })
    sSymbol_f <- reactive({
      tryCatch({
        futures_xts(input$futures,input$futures_type,input$f_days)
      }, error = function(err) {
        return(NULL)
      })
    })

    output$distPlot_f<- renderPlot({
      taStr<-"addVo()"
      if (!is.null(input$ta_f)) {
        for (ta in input$ta_f) {
          taStr<-paste(taStr, paste(";", ta))
        }
      }

      if(!is.null(sSymbol())) {
        chartSeries(sSymbol_f(), name=input$futures, TA=taStr,theme = chartTheme("white"),up.col='red',dn.col='green')
      }
    })
    
    # output$dispPrint <- renderPrint({
    #   print(sSymbol())
    # })
  }
  
  
  
  
}}
#运行-----------------------------
shiny::shinyApp(ui, server)