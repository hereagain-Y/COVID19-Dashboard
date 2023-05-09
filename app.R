#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(tsModel)
library(sandwich)
library(readxl)
library(stargazer)
library(dplyr)
library(tsModel)
library(sandwich)
library(lubridate)
library(ggthemes)
library(cowplot)
library(MASS)
library(shinythemes)
library(shinydashboard)


Get_figure <- function(dat1,dat2,variable){
  figure=dat2%>%
    ggplot(aes(x=month,y=values,color=Models,linetype=Models))+
    #geom_line(color = "steelblue")+#linetype="dotdash")+
    geom_point(aes(x=month,y=rep(dat1[,variable],3),group=1),color = "grey")+#"#FC4E07" red
    geom_line(size=1)+
    theme_bw()+
    scale_color_manual(
      values =c( "steelblue","darkorange","darkgreen"))+
    scale_linetype_manual(values = c(2,3,1))+
    
    
    xlab("Calendar time")+
    
    scale_x_continuous(breaks=c(as.numeric(dat1$month[1]),as.numeric(dat1$month[6]),as.numeric(dat1$month[11]),as.numeric(dat1$month[24]),as.numeric(dat1$month[30])), label=c("Oct-2019", "Mar-2020","Aug-2020","Sep-2021","Jun-2022"))+
    #geom_vline(xintercept = as.numeric(dat1$month[10]),color="darkblue",linetype=2)+
    #geom_vline(xintercept = as.numeric(dat1$month[15]),color="darkblue",linetype=2)+
    geom_vline(xintercept = as.numeric(dat1$month[28]),color='darkgrey',linetype='dashed')+
    theme_stata()+
    theme(legend.position="none")
  # chan
  return(figure)
}
Get_figure_2 <- function(dat1,dat2,variable){
  figure=dat2%>%
    ggplot(aes(x=month,y=values,color=Models,linetype=Models))+
    #geom_line(color = "steelblue")+#linetype="dotdash")+
    geom_point(aes(x=month,y=rep(dat1[,variable],3),group=1),color = "grey")+#"#FC4E07" red
    geom_line(size=1)+
    theme_bw()+
    scale_color_manual(
      values =c( "steelblue","darkorange","darkgreen"))+
    scale_linetype_manual(values = c(2,3,1))+
    
    
    #xlab("Calendar time")+
    
    scale_x_continuous(breaks=c(as.numeric(dat1$month[1]),as.numeric(dat1$month[27]),as.numeric(dat1$month[32]),as.numeric(dat1$month[45]),as.numeric(dat1$month[51])), label=c("Jan-2018", "Mar-2020","Aug-2020","Sep-2021","Mar-2022"))+
    #geom_vline(xintercept = as.numeric(dat1$month[27]),color="darkblue",linetype=2)+
    #geom_vline(xintercept = as.numeric(dat1$month[32]),color="darkblue",linetype=2)+
    geom_vline(xintercept = as.numeric(dat1$month[45]),color='grey',linetype='dashed')+
    
    theme_stata()+
    theme(legend.position="none",axis.title.y = element_blank() )
  # chan
  return(figure)
}
# Define UI for application that draws a histogram
ui <-dashboardPage(
  dashboardHeader(
    title = "COVID-19 ITS Project Dashboard"
  ),
  dashboardSidebar(selectInput("dataset", "Choose a dataset:",
                             choices = c("CDC_MSM", "CDC_FSW"),selected = "CDC_MSM"),
                 selectInput("Indicator","Choose a Indicator variable:",
                             choices = c("HTS_TST_TOTAL","HTS_TST_POS_TOTAL","TX_NEW_TOTAL","PREP_NEW_TOTAL"),selected = "HTS_TST_TOTAL")),
  dashboardBody(
      tableOutput("table"),
      plotOutput("Figure"),
      downloadButton(outputId = "downloadFigure",
                     label = "Download Figure")
      
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {

  data_full_file_name <-reactive(
  paste(input$dataset,"/",input$Indicator,"/data.csv", sep = ""))
  model_full_file_name <-reactive(
    paste(input$dataset,"/",input$Indicator,"/model.csv", sep = ""))
    output$table <- renderTable({
      data = read.csv(data_full_file_name(),sep=',')
      head(data)})
    
  Get_graph = reactive({
    d1 = read.csv(data_full_file_name(),sep=',')
    d1$month=as.Date(d1$month)
    d2 = read.csv(model_full_file_name(),sep=',')
    d2$month =as.Date(d2$month)
    if (input$dataset=='CDC_MSM'){
      p<- Get_figure(d1,d2,input$Indicator)
      if(input$Indicator=="HTS_TST_TOTAL"){
        p + annotate("rect",xmin=as.Date("2020-03-01"),xmax=as.Date("2020-08-01"),ymin = 0,ymax=Inf,alpha=.4,fill="#E7B800")+
          ggtitle("HIV TOTAL TESTS Among MSM Oct 2019-Mar 2022 (CDC)")+
          annotate("rect",xmin=as.Date("2020-08-01"),xmax=as.Date("2022-06-01"),ymin = 0,ymax=Inf,alpha=.2,fill="#E7B800")+
          theme(plot.title = element_text(hjust=0.5,face="bold"),
                axis.title.y = element_text(size = rel(1.5), angle = 90,margin = margin(t = 0, r = 20, b = 0, l = 0)),
                axis.title.x = element_text(size = rel(1.5), angle = 00,margin = margin(t = 20, r = 0, b = 0, l = 0)))+
          #annotate("text",x=as.Date("2020-06-01"),y=3500,label="High Level",size=5)+
          #annotate("text",x=as.Date("2021-10-01"),y=3500,label="After High level",size=5)+
          #scale_y_continuous(limits = c(0,10000))+
          ylab("HIV Tests per Month (n) ")+
          #geom_hline(yintercept = 9496,color="darkblue",size=0.5,linetype="dashed")+
          #geom_text(aes( as.Date("2019-06-01"),9496, label = 9500, vjust = -1), size = 3,color="black")+
          theme(axis.title.x = element_blank())
      }else if(input$Indicator=="HTS_TST_POS_TOTAL"){
        p + annotate("rect",xmin=as.Date("2020-03-01"),xmax=as.Date("2020-08-01"),ymin = 0,ymax=Inf,alpha=.3,fill="darkorange")+
          ggtitle("HIV TOTAL POSITIVE TESTS Among MSM Oct 2019-Mar 2022 (CDC)")+
          annotate("rect",xmin=as.Date("2020-08-01"),xmax=as.Date("2022-06-01"),ymin = 0,ymax=Inf,alpha=.1,fill="darkorange")+
          theme(plot.title = element_text(hjust=0.5,face="bold"),
                axis.title.y = element_text(size = rel(1.5), angle = 90,margin = margin(t = 0, r = 20, b = 0, l = 0)),
                axis.title.x = element_text(size = rel(1.5), angle = 00,margin = margin(t = 20, r = 0, b = 0, l = 0)))+
          #annotate("text",x=as.Date("2020-06-01"),y=600,label="High Level",size=5)+
          #annotate("text",x=as.Date("2021-10-01"),y=600,label="After High level",size=5)+
          #geom_hline(yintercept = 764,color="red",size=0.5,linetype="dashed")+
          ylab("HIV Case Finding")+
          #scale_y_continuous(limits = c(0,800))+
          #geom_hline(yintercept = 764,color="darkblue",size=0.5,linetype="dashed")+
          #geom_text(aes( as.Date("2019-06-01"),764, label = 770, vjust = -1), size = 3,color="black")+#+
          theme(axis.title.x = element_blank())
      }else if(input$Indicator=="TX_NEW_TOTAL"){
        p+annotate("rect",xmin=as.Date("2020-03-01"),xmax=as.Date("2020-08-01"),ymin = 0,ymax=Inf,alpha=.3,fill="purple")+
          ggtitle("TX NEW Among MSM Oct 2019-Mar 2022 (CDC)")+
          annotate("rect",xmin=as.Date("2020-08-01"),xmax=as.Date("2022-06-01"),ymin = 0,ymax=Inf,alpha=.1,fill="purple")+
          theme(plot.title = element_text(hjust=0.5,face="bold"),
                axis.title.y = element_text(size = rel(1.5), angle = 90,margin = margin(t = 0, r = 20, b = 0, l = 0)),
                axis.title.x = element_text(size = rel(1.5), angle = 00,margin = margin(t = 20, r = 0, b = 0, l = 0)))+
          #annotate("text",x=as.Date("2020-06-01"),y=300,label="High Level",size=5)+
          #annotate("text",x=as.Date("2021-10-01"),y=300,label="After High level",size=5)+
          geom_hline(yintercept = 480,color="darkblue",size=0.5,linetype="dashed")+
          ylab("ART Newly Initiated")+
          scale_y_continuous(limits = c(0,500),breaks = seq(0,500,by=50))  +
          geom_text(aes( as.Date("2019-07-01"),485,label = 485, vjust = -1), size = 3,color="black")+ #+
          theme(axis.title.x = element_blank())
      }else if(input$Indicator=="PREP_NEW_TOTAL"){
        p+annotate("rect",xmin=as.Date("2020-03-01"),xmax=as.Date("2020-08-01"),ymin = 0,ymax=Inf,alpha=.3,fill="lightgreen")+
          ggtitle("PREP NEW Among MSM Oct 2019-Mar 2022 (CDC)")+
          annotate("rect",xmin=as.Date("2020-08-01"),xmax=as.Date("2022-06-01"),ymin = 0,ymax=Inf,alpha=.1,,fill="lightgreen")+
          theme(plot.title = element_text(hjust=0.5,face="bold"))+
          #annotate("text",x=as.Date("2020-06-15"),y=1200,label="High Level",size=5)+
          #annotate("text",x=as.Date("2020-11-15"),y=1200,label="After High level",size=5)+
          
          scale_y_continuous(limits = c(0,2500))+
          geom_hline(yintercept = 2265,color="darkblue",size=0.5,linetype="dashed")+
          geom_text(aes( as.Date("2019-06-01"),2265,label = 2200, vjust = -1), size = 3,color="black")+
          theme(axis.title.x = element_blank())#+#+
        
      }else{
        print('not well developed')
      }
      
      
    }else if(input$dataset=='CDC_FSW'){
      p<- Get_figure_2(d1,d2,input$Indicator)
      if(input$Indicator=="HTS_TST_TOTAL"){
        p+annotate("rect",xmin=as.Date("2020-03-01"),xmax=as.Date("2020-08-01"),ymin = 0,ymax=Inf,alpha=.4,fill="#E7B800")+
          
          annotate("rect",xmin=as.Date("2020-08-01"),xmax=as.Date("2022-03-28"),ymin = 0,ymax=Inf,alpha=.2,fill="#E7B800")+
          ggtitle("HIV TOTAL TESTS Among FSW Jan 2018-Mar 2022 (CDC)")+
          theme(plot.title = element_text(hjust=0.5,face="bold"),
                axis.title.y = element_text(size = rel(1.5), angle = 90,margin = margin(t = 0, r = 20, b = 0, l = 0)),
                axis.title.x = element_text(size = rel(1.5), angle = 00,margin = margin(t = 20, r = 0, b = 0, l = 0)))+
          theme(axis.title.y = element_blank(),axis.title.x = element_blank())#
        
        
      }else if(input$Indicator=="HTS_TST_POS_TOTAL"){
        p+annotate("rect",xmin=as.Date("2020-03-01"),xmax=as.Date("2020-08-01"),ymin = 0,ymax=Inf,alpha=.3,fill="darkorange")+
          
          annotate("rect",xmin=as.Date("2020-08-01"),xmax=as.Date("2022-03-28"),ymin = 0,ymax=Inf,alpha=.1,fill="darkorange")+
          ggtitle("HIV Positive cases Among FSW Jan 2018-Mar 2022 (CDC)")+
          theme(plot.title = element_text(hjust=0.5,face="bold"),
                axis.title.y = element_text(size = rel(1.5), angle = 90,margin = margin(t = 0, r = 20, b = 0, l = 0)),
                axis.title.x = element_text(size = rel(1.5), angle = 00,margin = margin(t = 20, r = 0, b = 0, l = 0)))+
          #annotate("text",x=as.Date("2020-05-15"),y=300,label="High Level",size=5)+
          #annotate("text",x=as.Date("2021-09-15"),y=300,label="After High level",size=5)+
          ylab("HIV Case Finding")+
          scale_y_continuous(limits = c(0,400))+
          theme(axis.title.x = element_blank(),axis.title.y = element_blank())
        
      }else if(input$Indicator=="TX_NEW_TOTAL"){
        p+annotate("rect",xmin=as.Date("2020-03-01"),xmax=as.Date("2020-08-01"),ymin = 0,ymax=Inf,alpha=.3,fill="purple")+
          
          annotate("rect",xmin=as.Date("2020-08-01"),xmax=as.Date("2022-03-28"),ymin =0,ymax=Inf,alpha=.1,fill="purple")+
          ggtitle("ART NEW Among FSW Jan 2018-Mar 2022 (CDC)")+
          theme(plot.title = element_text(hjust=0.5,face="bold"),
                axis.title.y = element_text(size = rel(1.5), angle = 90,margin = margin(t = 0, r = 20, b = 0, l = 0)),
                axis.title.x = element_text(size = rel(1.5), angle = 00,margin = margin(t = 20, r = 0, b = 0, l = 0)))+
          #annotate("text",x=as.Date("2020-05-15"),y=450,label="High Level",size=5)+
          #annotate("text",x=as.Date("2021-09-15"),y=450,label="After High level",size=5)+
          #ylab("ART NEW Numbers")+
          #geom_text(aes( as.Date("2018-01-01"),1050, label = 1050, vjust = -1), size = 3,color="black")+#+
          theme(axis.title.x = element_blank(),axis.title.y = element_blank())
      }else if(input$Indicator=="PREP_NEW_TOTAL"){
        p+ annotate("rect",xmin=as.Date("2020-03-01"),xmax=as.Date("2020-08-01"),ymin = 0,ymax=Inf,alpha=.3,fill="lightgreen")+
          
          annotate("rect",xmin=as.Date("2020-08-01"),xmax=as.Date("2022-03-28"),ymin = -Inf,ymax=0,alpha=.1,fill="lightgreen")+
          ggtitle("PREP New Among FSW Jan 2018-Mar 2022 (CDC)")+
          theme(plot.title = element_text(hjust=0.5,face="bold"),
                axis.title.y = element_text(size = rel(1.5), angle = 90,margin = margin(t = 0, r = 20, b = 0, l = 0)),
                axis.title.x = element_text(size = rel(1.5), angle = 00,margin = margin(t = 20, r = 0, b = 0, l = 0)))+
          # annotate("text",x=as.Date("2020-05-15"),y=450,label="High Level",size=5)+
          #  annotate("text",x=as.Date("2021-09-15"),y=450,label="After High level",size=5)+
          ylab("PREP NEW TOTAL")+geom_hline(yintercept = 1239,color="darkblue",size=0.5,linetype="dashed")+
          scale_y_continuous(limits = c(0,1250))+
          geom_text(aes( as.Date("2018-01-01"),1239, label = 1239, vjust = -1), size = 3,color="black")+
          theme(axis.title.x = element_blank(),axis.title.y = element_blank())#+#+
        
      }else{
        print('model waiting to be built')
      }}})
    
    output$Figure <- renderPlot({
      Get_graph()
    })
    output$downloadFigure <- 
      downloadHandler(
        
        filename = function(){paste(input$dataset,'_',input$Indicator,'.png',sep='')},
        content = function(file){
          ggsave(file,plot=Get_graph(),height=5,width=8)
        } )
    
      
  
}

# Run the application 
shinyApp(ui = ui, server = server)
