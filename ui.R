
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
require(rCharts)
require(shiny)
require(shinyjs)
modeldata <- read.table(file="http://52.193.24.228/data/modeldata.csv",sep=",",header=T)
formulas <- read.table(file="http://52.193.24.228/data/modelformula.csv",sep=",",header=T,stringsAsFactors = F)

shinyUI(fluidPage(
  
  # Application title
  titlePanel(
    a()
  ),
  
  sidebarPanel(
    # 测试输出变量值
    uiOutput('text'),
    # 选择建模对象
    selectInput('Object','Choose Object',choices = levels(modeldata$Object)),
    # minAICc <-  min(modeldata[modeldata$Object=="PCBA",]$AICc,na.rm = TRUE),
    # 选择模型类型
    selectInput('Distribution','Choose Distribution',choices = levels(modeldata$Distribution),selected =modeldata$Distribution[1]),
    checkboxInput("Showmodel", "Show Formula(显示模型公式)", TRUE),
    # 动态生成输入框
    uiOutput('numpara'),
    checkboxInput("Oldmodel", "Show Old(显示老产品曲线)", FALSE),
    helpText("The old YGFC Life model is Frechet Distribution
             With Parameters: scale=5.905551; 
             location=17.333760."),
    
    
    # 调节图像高度
    uiOutput('hheight'),
    # 上传数据
    #     fileInput('file1', "import data file(导入数据)",
    #               accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    width =2
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    useShinyjs(),
    uiOutput('sformula'),
    showOutput("myChart", "highcharts")
  )
  
))