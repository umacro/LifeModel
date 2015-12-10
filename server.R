
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output,session) {
  pFrechet <- function(t,scaleF,locationF) exp(-exp(-(log(t)-locationF)/scaleF))
  # quantile
  qFrechet<- function(p,scaleF,locationF) exp(locationF-scaleF*log(-log(p)))                                                                                                                                                                                                                                                                                                                                                                
  # pdf
  dFrechet <- function(s,scaleF,locationF) exp(-exp(-(log(s)-locationF)/scaleF))*exp(-(log(s)-locationF)/scaleF)*(1/(s*scaleF))
  # failure rate
  fFrechet <- function(t,scaleF,locationF) (exp(-exp(-(log(t)-locationF)/scaleF))*exp(-(log(t)-locationF)/scaleF)*(1/(t*scaleF)))/(1-exp(-exp(-(log(t)-locationF)/scaleF))) 
  modeldata <- read.table(file="http://52.193.24.228/data/modeldata.csv",sep=",",header=T)
  # modeldata <- read.table(file="c:\\tmp\\modeldata.csv",sep=",",header=T,stringsAsFactors = F) 
  formulas <- read.table(file="http://52.193.24.228/data/modelformula.csv",sep=",",header=T,stringsAsFactors = F)
  # 动态生成参数输入框
  output$numpara = renderUI({
    # 选择模型类型
    selectInput('Distribution','Distribution',choices = levels(modeldata$Distribution),selected =modeldata$Distribution[1])
    
    # Distribution <- modeldata[modeldata$Distribution==input$Distribution,]$Distribution
    mdata <- modeldata[modeldata$Distribution==input$Distribution,]
    # 模型参数名
    Parameters <-mdata[mdata$Object==input$Object,]$Parameter
    # 模型参数值
    Estimate <- mdata[mdata$Object==input$Object,]$Estimate
    # 动态生成数值输入框
    lapply(1:length(Parameters),function(i){
      numericInput(Parameters[i], Parameters[i],Estimate[i])
    })
    
  })
  # 根据选择分布输出公式
  output$sformula=renderUI({
    if(input$Showmodel){
      withMathJax(h2(
        formulas[formulas$Distribution==input$Distribution[1],]$CDF
      )) 
    }
    
    
  }) 
  
  output$myChart=renderChart2({
    Parameters <-modeldata[modeldata$Object==input$Object,]$Parameter
    
    locationF<- input[[as.character(Parameters[1])]]          #8.8061476
    scaleF <-input[[as.character(Parameters[2])]]         #0.9033256
    
    # 计算时间最大值
    tmax <- floor(qFrechet(0.99,scaleF ,locationF))
    pdf <- function (t) dFrechet(t,scaleF,locationF)
    xmax <- optimize(pdf,c(0,tmax),maximum = TRUE)
    # 计算FR最大值产生时间
    fr <- function (t) fFrechet(t,scaleF,locationF)
    frxmax <- optimize(fr,c(0,tmax),maximum = TRUE)
    maxt <- max(xmax$maximum,frxmax$maximum)
    Xrange <- 3*max(xmax$maximum,frxmax$maximum)
    t <- seq(0,Xrange,length.out =800)
    dataframe <- data.frame(days=t
                            ,CDF=pFrechet(t,scaleF,locationF)
                            ,PDF=dFrechet(t,scaleF,locationF)
                            ,FR =fFrechet(t,scaleF,locationF)
                            # ,Q=qFrechet(pFrechet(t,scaleF,locationF),scaleF,locationF)
                            
    )
    # 绘图
    h1 <- Highcharts$new()
    h1$chart(type = "line")
    # 绘制CDF，PDF和FR
    h1$series(data = toJSONArray2(data.frame(dataframe$days,dataframe$CDF),json = F, names = F), name = 'CDF')
    h1$series(data = toJSONArray2(data.frame(dataframe$days,dataframe$PDF),json = F, names = F), name = 'PDF',yAxis=1)
    h1$series(data = toJSONArray2(data.frame(dataframe$days,dataframe$FR),json = F, names = F),name = 'FR',yAxis=2)
    # 绘制标线
    h1$xAxis(crosshair=TRUE,units='days',plotLines = list(list(value = 365,color = '#ff0000',width = 1,label=list(text='365days',textAlign= 'right',y=50)),list(value = 1155,color = '#ff0000',width = 1,label=list(text='0.0009',textAlign= 'right',y=50))))
    
    h1$yAxis(
      list(
        list(height = "33%",labels=list(align="left"),title="CDF")
        ,list(height = "33%",top="33%",labels=list(align="left"),title="PDF",offset=50)
        ,list(height = "33%",top="66%",labels=list(align="left"),title="CDF",offset=50)
      )
    )
    h1$chart(zoomType='x',panning=TRUE,panKey='shift')
    h1$subtitle(text='Click and drag to zoom in. Hold down shift key to pan.')
    h1$params$width=1000
    h1$params$height=input$hheight
    h1$legend(verticalAlign="bottom")
    h1$series(data=list(list(x=maxt,y=pFrechet(maxt,scaleF,locationF))),type="scatter",name="CDF@MAXFR")
    h1$series(data=list(list(x=maxt,y=dFrechet(maxt,scaleF,locationF))),type="scatter",name="PDF@MAXFR",yAxis=1)
    h1$series(data=list(list(x=maxt,y=fFrechet(maxt,scaleF,locationF))),type="scatter",name="PDF@MAXFR",yAxis=2)
    h1$series(data=list(list(x=365,y=pFrechet(365,scaleF,locationF))),type="scatter",name="CDF@365")
    h1$series(data=list(list(x=365,y=dFrechet(365,scaleF,locationF))),type="scatter",name="PDF@365",yAxis=1)
    h1$series(data=list(list(x=365,y=fFrechet(365,scaleF,locationF))),type="scatter",name="PDF@365",yAxis=2)
    # 计算上一代产品的数据
    locationFOld<- 17.333760#17.333760 
    scaleFOld <-5.905551#5.905551
    dataframeOld <- data.frame(days=t
                               ,CDF=pFrechet(t,scaleFOld,locationFOld)
                               ,PDF=dFrechet(t,scaleFOld,locationFOld)
                               ,FR =fFrechet(t,scaleFOld,locationFOld)
                               # ,Q=qFrechet(pFrechet(t,scaleF,locationF),scaleF,locationF)
                               
    )
    h1$series(data = toJSONArray2(data.frame(dataframeOld$days,dataframeOld$CDF),json = F, names = F), name = 'OldCDF',visible=input$Oldmodel)
    h1$series(data = toJSONArray2(data.frame(dataframeOld$days,dataframeOld$PDF),json = F, names = F), name = 'OldPDF',yAxis=1,visible=input$Oldmodel)
    h1$series(data = toJSONArray2(data.frame(dataframeOld$days,dataframeOld$FR),json = F, names = F),name = 'OldFR',yAxis=2,visible=input$Oldmodel)
    h1$exporting(enabled = T)
    h1$tooltip(formatter = "#!function () {
               var s = '<b>' + this.x + '</b>';
               
               $.each(this.points, function () {
               s += '<br/>'+this.series.name +'='+ this.point.y;
               });
               
               return s;
  }!#",shared=T)
    return(h1)
})
output$hheight=renderUI({
  sliderInput("hheight","Height",min=200,max=1000,value=600,step = 1)
})
eventReactive(input$file1, {
  modeldata<<-read.table(inFile$datapath,sep=",",header=T,fileEncoding = "GBK")
  updateSelectInput(session, 'Object',choices = levels(modeldata$Object),selected =levels(modeldata$Object)[2])
  updateSelectInput(session, "Distribution", choices = levels(modeldata$Distribution),selected =modeldata$Distribution[2])
  
})
})

