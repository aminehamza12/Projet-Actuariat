#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(gridExtra)
library(shinydashboardPlus)
library(plotly)
library(lifecontingencies)
library(reliaR)
library(forecast)
library(zoo)
library(demography)
library(MASS)
library(tidyr)
library(gnm)
library(StMoMo)
# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "red",
                    
                    dashboardHeader(title = "Dashboard"),
                    dashboardSidebar(
                        sidebarMenu(id = "tabs",
                                    menuItem("Sujet", tabName = "Sujet", icon = icon("file")),
                                    menuItem("Tables", tabName = "Tables", icon = icon("table")),
                                    menuItem("Analyse", icon=icon("calculator"),
                                             menuSubItem("Taux de moratalité", tabName = "Mortality"),
                                             menuSubItem("Espérance de vie", tabName = "Esperance") ),
                                    menuItem("Lee-Carter", icon = icon("analytics") ,
                                             menuSubItem("Estimation", tabName = "Estimation"),
                                             menuSubItem("Projection", tabName = "Projection"))  ,
                                    menuItem("VAP", icon = icon("analytics") ,
                                             menuSubItem("Contrat de retraite", tabName = "retraite"),
                                             menuSubItem("Capital au décès", tabName = "capital"))  
                        )
                    ), 
                    
                    
                    
                    
                    dashboardBody(
                        
                        
                        tabItems(
                            tabItem("Sujet",
                                    fluidRow(
                                        column(width = 12,align = "center", 
                                               flipBox(
                                                   id = 1,
                                                   main_img = "https://www.flaticon.com/premium-icon/icons/svg/913/913361.svg",
                                                   header_img = NULL,
                                                   front_title = tags$b("Assurance vie") ,
                                                   back_title = "Ce dashboard est dédié à l'estimation et la projection de la mortalité d’une cohorte d’assurés danois afin d’étudier deux types de contrats :",
                                                   "Ceci est un dashboard interactif permettant de :",
                                                   hr(),
                                                   fluidRow( column(width = 12, align = "center","Visualiser les indicateurs de survie") ),
                                                   hr(),
                                                   fluidRow( column(width = 12,align = "center","Visualiser les projection de mortalité")),
                                                   hr(),
                                                   fluidRow( column(width = 12, align = "center","Visualiser les VAP et provisions" )),
                                                   back_content = list( column(width = 12,align = "center",
                                                                               infoBox(width = 12,icon = shiny::icon("fal fa-file-contract"),title  = h3("Contrat de retraite", align="center") ),
                                                                               
                                                                               infoBox(width = 12, icon = shiny::icon("fal fa-file-contract"),title= h3("Contrat capital décès" , align="center"))
                                                                               
                                                   )
                                                   ) 
                                                   
                                                   
                                               )) )),
                            
                            
                            tabItem("Tables",
                                    fluidPage( 
                                        
                                        
                                        tabsetPanel(
                                            
                                            tabPanel("Male", DT::dataTableOutput("mytable1")),
                                            tabPanel("Female", DT::dataTableOutput("mytable2")),
                                            tabPanel("Total", DT::dataTableOutput("mytable3"))
                                        ) 
                                        
                                    )
                            ),
                            
                            
                            
                            
                            tabItem( "Mortality",
                                     fluidRow(
                                         column(6,h1("Taux de mortalité")),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         column(12,
                                                tabsetPanel(
                                                    
                                                    tabPanel("Male", box( splitLayout(plotOutput("plot1"),plotOutput("plot11")),width = 12    )),
                                                    tabPanel("Female", box( splitLayout(plotOutput("plot2"),plotOutput("plot22")),width = 12    )  ),
                                                    tabPanel("Total", box( splitLayout(plotOutput("plot3"),plotOutput("plot33")),width = 12    )  )
                                                ) 
                                                
                                         ),
                                         box(width = 12, p("- Les chiffres suivants présentent pour la population danoise le schéma du logarithme des taux de mortalité en fonction de lâge et du temps.
                                                      On remarque plusieurs comportements, respectivement pour les hommes, les femmes et la population totale."),
                                             p("- Les données danoises confirment que la mortalité diminue à tous les âges avec un comportement différent selon les âges.")
                                         )
                                         
                                     )
                                     
                                     
                            ),
                            
                            tabItem("Esperance",
                                    
                                    column(6, h1("Espérance de vie")),
                                    box( width=12,
                                         box(width = 8,sliderInput("age","Choisir l'age", 0, 110, value=60 , step=1 ,sep = "") ,solidHeader = TRUE, status = "primary", title = "Age"  ),
                                         box( radioButtons("input1","choisir le sexe" ,c(Male = "male", Female = "female")  )   , width = 4 ,solidHeader = TRUE, status = "primary", title = "Sexe" )
                                    ),
                                    box( width = 12,
                                         textOutput("esp"),
                                         br(),
                                         textOutput("survie")
                                         
                                    )
                                    
                                    
                            ),
                            
                            
                            tabItem("Estimation",
                                    
                                    column(width = 12, align="center",
                                           fluidRow(
                                               column(width = 12,align = "center",
                                                      flipBox(
                                                          id = 2,
                                                          main_img = "https://image.flaticon.com/icons/svg/2362/2362384.svg",
                                                          header_img = NULL,
                                                          front_title = "Estimation des paramétres du modèle Lee-Carter",
                                                          back_title = "Interprétation",
                                                          hr(),
                                                          fluidRow(column(width = 12,align = "center" ,  plotOutput("estimate", height = 300) ) ),
                                                          back_content = tagList( column(width = 12 , 
                                                                                         box(width = 12, p("En observant le paramètre ax(moyenne temporelle du logarithme du taux de mortalité par âge), on remarque un pic de mortalité infantile à moins de 4 ans puis une augmentation de cette mortalité de 10 à 20 ans. On peut voir qu’à partir de l’âge 45 à 80, son comportement est presque linéaire sur une échelle logarithmique. Enfin, jusqu’à l’âge de 90 ans, le taux de mortalité a augmenté de façon exponentielle.") )  ,                                                      
                                                                                         box(width = 12,  p("K_t représente la diminution des taux de mortalité au fil des années avec une tendance presque linéaire adaptée à une projection future. Cette tendance à la décroissance du paramètre k_t, qui devient négatif au cours de la période, associée à la positivité moyenne du paramètre β implique d’après la formule de Lee-Carter, une diminution des taux de mortalité. En conséquence, on assiste à un rallongement de la durée de vie sur la période observée.") ),                                                         
                                                                                         box(width = 12,  p("Le paramètre b_x représente l’interaction âge-temps qui traduit l’effet des années sur la sensibilité du taux de mortalité à l’âge x par rapport au changement temporel de k_t. Cet effet est toujours positif mais la valeur ne cesse de diminuer avec l’âge. Autrement dit, l’effet des années calendaires agit majoritairement avant 50 ans et de moins en moins au delà. On constate que les âges les plus sensibles à l’évolution temporelle de la mortalité sont ceux entre 20 et 35 ans, et un peu autour de 80 ans. On atteint en effet des pics sur ces tranches d’âges qui peuvent être expliquées respectivement par un fort taux de mortalité chez les jeunes pendant une époque de guerre et les progrès scientifiques et médicaux qui ont permis de réduire le taux de mortalité chez les personnes âgées pendant les années récentes. Pour des âges plus élevés, l’effet est quasi-inexistant puisque b_x est très proche de 0."))
                                                          )  )
                                                          
                                                      )
                                               )
                                           )
                                           
                                    )       
                            ),
                            
                            
                            tabItem("Projection", h1("Projection des taux de mortalité"),column(width = 12,fluidRow(height=450,width=12,plotOutput("forec")  ) )) ,
                            
                            tabItem(  "retraite", h1("Contrat de retraite"),
                                      box(
                                          width="12",
                                          title = "Valeurs actuelles probables",
                                          numericInput("rente", "Rente :", value = 1000, min=200, max=10000 ) ,
                                          
                                          sliderInput("slidervap", "Age:", 1, 110, 1),
                                          #sliderInput("slidervapA", "Année:", 1900, 2019, 1, sep=" "),
                                          sliderInput("nn", "Nombre de terme de la rente :", 1,90,1 , sep = " "),
                                          
                                          box(width = 12,textOutput("valRetraite"))
                                          
                                          
                                      )
                                      
                                      
                                      
                            ),
                            
                            tabItem(  "capital", h1("Capital au décès") ,
                                      
                                      box(
                                          width="12",
                                          title = "Valeurs actuelles probables",
                                          numericInput("k", "capital au décès :", value = 100000, min=200, max=900000 ) ,
                                          
                                          sliderInput("slidervap2", "Age:", 1, 110, 1),
                                          #sliderInput("slidervapA", "Année:", 1900, 2019, 1, sep=" "),
                                          #sliderInput("nn", "Nombre de terme de la rente :", 1,90,1 , sep = " "),
                                          
                                          box(width = 12,textOutput("valcap"))
                                          
                                          
                                      ),
                                      
                                      
                                      
                                      
                            )
                            
                            
                            
                            
                            
                            
                            
                        ))
                    
)

# Define server logic required to draw a histogram
server <- function(input,output) {
    
    
    
    output$mytable1<-DT::renderDataTable({
        DT::datatable(dataM,options = list(lengthMenu = c(10, 30, 50), pageLength = 10))
    })
    
    output$mytable2<-DT::renderDataTable({
        DT::datatable(dataF,options = list(lengthMenu = c(10, 30, 50), pageLength = 10))
    })
    
    output$mytable3<-DT::renderDataTable({
        DT::datatable(dataT,options = list(lengthMenu = c(10, 30, 50), pageLength = 10))
    })
    
    output$downloadData <- downloadHandler(
        filename = "rapport_v0.pdf",
        content = function(file) {
            file.copy("www/rapport_v0.pdf", file)
        }
    )
    
    
    output$plot1 <- renderPlot({
        
        plot(Denmark,series="male",datatype="rate", main="Male rates")
        #plot(Denmark,series="male",datatype="rate", plot.type="time", main="Male rates",xlab="Years")
        
    })
    
    output$plot11 <- renderPlot({
        
        #plot(Denmark,series="male",datatype="rate", main="Male rates")
        plot(Denmark,series="male",datatype="rate", plot.type="time", main="Male rates",xlab="Years")
        
    })
    
    
    output$plot2 <- renderPlot({
        
        plot(Denmark,series="female",datatype="rate", main="Female rates")
        #plot(Denmark,series="male",datatype="rate", plot.type="time", main="Male rates",xlab="Years")
        
    })
    
    output$plot22 <- renderPlot({
        
        #plot(Denmark,series="male",datatype="rate", main="Male rates")
        plot(Denmark,series="female",datatype="rate", plot.type="time", main="Female rates",xlab="Years")
        
    })
    
    output$plot3 <- renderPlot({
        
        plot(Denmark,series="total",datatype="rate", main="Total rates")
        #plot(Denmark,series="male",datatype="rate", plot.type="time", main="Male rates",xlab="Years")
        
    })
    
    output$plot33 <- renderPlot({
        
        #plot(Denmark,series="male",datatype="rate", main="Male rates")
        plot(Denmark,series="total",datatype="rate", plot.type="time", main="Total rates",xlab="Years")
        
    })
    
    output$esp<-renderText(
        
        {
            if(input$input1=="male") { out<-paste("- L'espérance de vie d'un homme agé de ", input$age , "est", exn(ltd1,x=input$age)  )}
            if (input$input1=="female")  {  out<- paste("- L'espérance de vie d'une femme agé de ", input$age , "est", exn(ltd2,x=input$age)   )}
            
            out
            
            
        }
    )
    
    
    output$survie<-renderText(
        
        {
            if(input$input1=="male") { out<-paste("- La probabilité de survie d'un homme agé de ", input$age , "est", pxt(ltd1,x=input$age, 1 )  )}
            if (input$input1=="female")  {  out<- paste("- La probabilité de survie d'une femme agé de ", input$age , "est", pxt(ltd2,x=input$age, 1 )   )}
            
            out
            
            
        }
    )
    
    
    output$estimate<-renderPlot( { 
        par(mfcol=c(12,12), oma=c(1,1,0,0), mar=c(1,1,1,0),tcl=-0.1, mgp=c(0,0,0) )
        
        
        plot(LCfit, cex=0.5) 
        
        
    } )
    
    
    
    output$valRetraite<-renderText(
        
        
        {  
            out2<-paste("- La VAP pour un client, âgé de" , input$slidervap, "est :",VAP_retraite = input$rente * axn(ltd3, x=input$slidervap , m=input$nn)  )
            
            out2
            
            
            
            
        }
    )
    
    
    output$pltVap<-renderPlot(
        
        {
            chosen_cohort=2016-input$slidervap
            
            lc_historical_rates <- extractCohort(fitted(LCfit, type = "rates"), cohort = chosen_cohort)
            
            lc_forecasted_rates <- extractCohort(LCfor$rates,cohort = chosen_cohort)
            
            lc_rates_1976 <- c(lc_historical_rates,lc_forecasted_rates)
            
            lc_qx_1976<-mx2qx(lc_rates_1976)
            
            lc_lifetable_1976<-probs2lifetable(probs=lc_qx_1976,type = "qx", name = paste("LC","chosen cohort","lt",sep="_"))
            
            lc_acttbl_1976<-new("actuarialtable",x=lc_lifetable_1976@x,lx=lc_lifetable_1976@lx,interest =0.03)
            
            VAP_retraite <- Vectorize(function(x){ input$rente * axn(lc_acttbl_1976, x=input$slidervap , m=input$nn)})
            
            VAP_retraite_tbl2019 <- Vectorize(function(x){ input$rente * axn(ltd3, x=input$slidervap, m=input$nn)})
            
            #puisque l'annuité est differée de 25, on s'arrête à l'age max - 25: 100-25
            o= 100-input$slidervap
            o1=o-1
            
            plot(0:o,c(VAP_retraite_tbl2019(0:o1),0),type="l", ylab="VAP",xlab="Ages x",
                 main="VAP contrat de retraite (rente differée de 25 ans)")
            
            lines(0:o,c(VAP_retraite(0:o1),0),type="l", lty=2, col="blue")
            
            legend("topright",legend = c("Taux observés", "Taux projetés"),
                   lty = 1:2, cex=0.6)
            
            
        }
        
        
    )
    
    output$valcap<-renderText(
        
        
        {  
            out3<-paste("- La VAP pour un client, âgé de" , input$slidervap2, "est :",VAP_cap = input$k * Axn(ltd3, x=input$slidervap2 )  )
            
            out3
            
        }
    )
    
    
    
    
    
    
    
    output$res<-renderPlot(
        
        
        {
            par(mfrow=c(1,3))
            
            #total
            plot(denmarkLcaT$residuals, main="total")
            #male
            plot(denmarkLcaM$residuals, main="male")
            #female
            plot(denmarkLcaF$residuals, main="female")
            
            
            
        }
    )
    
    output$forec<-renderPlot(
        
        {
            
            rates<-cbind(Denmark$rate$total[0:98,],LCfor$rate[0:98,])
            chosen_cohort=2016-40      
            plot(0:43, extractCohort(fitted(LCfit, type = "rates"), cohort = chosen_cohort), type = "l", log = "y", xlab = "age", ylab = "q(x)",
                 main = paste(c("Cohort",toString(chosen_cohort),"mortality rates"), collapse = " "),xlim = c(0,103))            
            lines(44:98, extractCohort(LCfor$rates, cohort = chosen_cohort),lty = 2, lwd=2, col="red")
            
            
            #plot(seq(min(Denmark$year),max(Denmark$year)+110),ratesF[input$age1,],col="red",xlab="Years",ylab="Death Rates",type="l")
            #lines(seq(min(Denmark$year),max(Denmark$year)+110),ratesM[input$age1,],col="blue",xlab="Years",ylab="Death Rates")
            #lines(seq(min(Denmark$year),max(Denmark$year)+110),ratesT[input$age1,],col="black",xlab="Years",ylab="Death Rates")
            #legend("topright" , c("Male","Female","Total"),cex=0.8,col=c("blue","red","black"),lty=1)
            
            
            
        }
        
    )
    
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
