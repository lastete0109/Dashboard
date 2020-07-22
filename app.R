# PROYECTO DASHBOARDS
library(shiny)
library(DT)
library(htmltools)
library(dplyr)
library(plotly)
library(reshape)

dist_cant_sin = c("Poisson","Binomial Negativa")
dist_mon_sin = c("Exponencial","Log-normal","Weibull")

# Creamos una interfaz del usuario
#####################################################################################
ui = fluidPage(
  titlePanel("Panel de simulacion"),
    sidebarLayout(
    sidebarPanel(
      tabsetPanel(id="inputs",

      tabPanel("Simulacion",
      hr(),
      # Seleccionar el numero de simulaciones deseadas
      sliderInput("n_sim","# de Simulaciones",min=1000,max=100000,step=5000,value=1000),

      # Seleccionando la distribucion de las cantidades
      selectizeInput("dist_num","Dist.(Num. de siniestros)",choices=dist_cant_sin,multiple=F,
                    options = list( placeholder = 'Seleccione alguna opcion',
                                    onInitialize = I('function() { this.setValue(""); }'))),
      # Panel condicional de la dist. Poisson
      conditionalPanel("input.dist_num=='Poisson'",
                       numericInput("lambda_pois",HTML("&lambda;"),value=0,min=0,max=Inf)),

      # Panel condicional de la dist. BN
      conditionalPanel("input.dist_num=='Binomial Negativa'",
                       numericInput("k_bn",HTML("<em>k</em>"),value=0,min=0,max=Inf),
                       numericInput("p",HTML("<em>p</em>"),value=0,min=0,max=1,step=0.01)),

      # Seleccionando la distribucion de los montos
      selectizeInput("dist_montos","Dist.(Montos de siniestros)",choices=dist_mon_sin,multiple=F,
                    options = list( placeholder = 'Seleccione alguna opcion',
                                    onInitialize = I('function() { this.setValue(""); }'))),

      # Panel condicional de la dist. Exponencial
      conditionalPanel("input.dist_montos=='Exponencial'",
                       numericInput("lambda_exp",HTML("&lambda;"),value=0,min=0,max=Inf)),

      # Panel condicional de la dist. Log Normal
      conditionalPanel("input.dist_montos=='Log-normal'",
                       numericInput("mu",HTML("&mu;"),value=0,min=-Inf,max=Inf),
                       numericInput("sigma",HTML("&sigma;"),value=0,min=-Inf,max=Inf)),

      # Panel condicional de la dist. Weibull
      conditionalPanel("input.dist_montos=='Weibull'",
                       numericInput("lambda_weib",HTML("&lambda;"),value=0,min=0,max=Inf),
                       numericInput("k_weib",HTML("<em>k</em>"),value=0,min=0,max=Inf)),
      hr(),
      actionButton("simular","Simular"),
      hr(),
      downloadButton("downloadData","Descargar .csv"))
      ,
      tabPanel("Opciones",
               hr(),
               sliderInput("bins","# de agrupaciones",min=2,max=50,value=30),
               textInput("quantiles","Cuantiles a graficar",placeholder = "Ejm: 0.1,0.9")
               )),width=2),

    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Data simulada",dataTableOutput("resumen")),
                  tabPanel("Graficas",
                           fluidRow(
                           splitLayout(cellWidths=c("70%","30%"),
                                       plotlyOutput("graficas"), dataTableOutput("estad1"))),
                           fluidRow(
                           splitLayout(cellWidths=c("70%","30%"),
                                       plotlyOutput("graficas2"), dataTableOutput("estad2"))))

    )

  )
  ))

#####################################################################################3
server = function(session,input,output){

cant_sim = reactiveValues(q=NULL)
mon_sin = reactiveValues(montos=NULL)

observeEvent(input$simular,{
  req(input$n_sim)
  req(input$dist_num)

  num_simulaciones = input$n_sim
  dist_num = input$dist_num

  if(dist_num=="Poisson"){
    aux = rpois(n=num_simulaciones,lambda=input$lambda_pois)
    cant_sim$q = ifelse(is.na(aux),0,aux)}
  else{
    aux = rnbinom(n=num_simulaciones,size=input$k_bn,prob=input$p)
    cant_sim$q = ifelse(is.na(aux),0,aux)}

})

observeEvent(input$simular,{
  req(input$n_sim)
  req(input$dist_montos)

  num_simulaciones = input$n_sim
  dist_montos = input$dist_montos
  vector_sim = cant_sim$q
  suma=c()
  if(dist_montos=="Exponencial"){
    for(i in 1:num_simulaciones){
      suma=c(suma,sum(rexp(n=vector_sim[i],rate=input$lambda_exp))) }}
  if(dist_montos=="Log-normal"){
    for(i in 1:num_simulaciones){
      suma=c(suma,sum(rlnorm(n=vector_sim[i],meanlog=input$mu,sdlog=input$sigma))) }}
  if(dist_montos=="Weibull"){
    for(i in 1:num_simulaciones){
      suma=c(suma,sum(rweibull(n=vector_sim[i],shape=input$lambda_weib,scale=input$k_weib))) }}
  mon_sin$montos = suma
  })

output$resumen = DT::renderDataTable({
  req(cant_sim$q)
  req(mon_sin$montos)
  resumen = data.frame(Anio=seq(1,input$n_sim),
                       Cantidad_sin=cant_sim$q,
                       Montos_totales=round(mon_sin$montos,4))
  resumen},filter="none",options = list(searching = F))

output$downloadData =  downloadHandler(
    filename = function(){paste("Simulacion_creada_",Sys.Date(),".csv",sep="")},
    content = function(file){
      resumen = data.frame(Cantidad_sin=cant_sim$q,
                           Montos_totales=round(mon_sin$montos,4))
      write.csv(resumen,file,row.names=F)} )

output$graficas = renderPlotly({
  req(cant_sim$q)
  req(mon_sin$montos)
  req(input$bins)
  resumen = data.frame( Montos_totales=round(mon_sin$montos,4))
  p1 =  ggplot(data=resumen,
             aes(x=Montos_totales),show.legend=FALSE) +
        geom_histogram(bins=input$bins,fill="#003399")+
        ylab("Cantidad")+
        labs(title="Distribucion de la suma de montos")+
        theme(legend.position='none')

  if(input$quantiles!=""){
    cuantiles = strsplit(input$quantiles,",") %>% do.call(c,.) %>% as.double()
    rectas1 = quantile(resumen$Montos_totales,probs=cuantiles) %>% as.numeric()
    p1 = p1 + geom_vline(xintercept = rectas1,colour="#999999",linetype="dashed")
    p = ggplotly(p1)
  }else{
    p = ggplotly(p1)}

  p})


output$graficas2 = renderPlotly({
  req(cant_sim$q)
  resumen = data.frame(Cantidad_sin=cant_sim$q)
  p2 =  ggplot(data=resumen,aes(x=Cantidad_sin),show.legend=FALSE) +
        geom_bar(fill="#990000")+
        ylab("Cantidad")+
        labs(title="Distribucion de la cantidad de siniestros")+
        scale_x_continuous(labels = resumen$Cantidad_sin, breaks = resumen$Cantidad_sin)+
        theme(legend.position='none')
  p2 = ggplotly(p2)
  p2})

output$estad1 = DT::renderDataTable({
  req(mon_sin$montos)
  x= mon_sin$montos
  estad = data.frame(n = length(x),
                     Prom = mean(x),
                     Mediana = median(x),
                     Min = min(x),
                     Max = max(x),
                     Rango = max(x) - min(x),
                     `Desv. Est.`=sd(x),
                     CV = sd(x)/mean(x)*100,
                     Q25 = quantile(x,0.25),
                     Q75 = quantile(x,0.75))
  estad = round(estad,3) %>% melt(.)
  estad
}, options = list(dom = 't') )

output$estad2 = DT::renderDataTable({
  req(cant_sim$q)
  x= cant_sim$q
  estad = data.frame(n = length(x),
                     Prom = mean(x),
                     Mediana = median(x),
                     Min = min(x),
                     Max = max(x),
                     Rango = max(x) - min(x),
                     `Desv. Est.`=sd(x),
                     CV = sd(x)/mean(x)*100,
                     Q25 = quantile(x,0.25),
                     Q75 = quantile(x,0.75))
  estad = round(estad,3) %>% melt(.)
  estad
}, options = list(dom = 't') )

  }

shinyApp(ui,server)

