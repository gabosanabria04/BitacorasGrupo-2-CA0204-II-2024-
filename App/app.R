
library(shinydashboard)
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(palmerpenguins)
library(ggthemes)
library(cowplot)
library(tidyr)
library(purrr)
library(ggExtra)
library(ggridges)
library(bslib)

#Datos a formato tidy, y a español
original <- read.csv("data/complete_renewable_energy_dataset.csv")
data <- original|> mutate(Country = case_when(
  Country == "Brazil"~"Brasil", Country == "Canada" ~ "Canadá", 
  Country == "France"~"Francia", Country == "Germany"~"Alemania", 
  Country == "Japan"~"Japón", Country == "Russia"~"Rusia", 
  Country == "USA"~"E.E.U.U.", Country == "Australia"~"Australia",
  Country == "China"~"China", Country == "India"~"India",
  TRUE~"Other")) %>% mutate(Energy.Type = case_when(
    Energy.Type == "Solar"~"Solar", Energy.Type == "Biomass" ~ "Biomasa", 
    Energy.Type == "Wind"~"Eólica", Energy.Type == "Hydro"~"Hidráulica", 
    Energy.Type == "Geothermal"~"Geotérmica", TRUE~"Other")) %>% 
  group_by(Country, Year, Energy.Type) |> 
  summarise(Total.Production = sum(Production..GWh.), Electricity.Prices = mean(Electricity.Prices), R.D.Expenditure = sum(R.D.Expenditure), CO2.Emissions = sum(CO2.Emissions), Investments..USD. = sum(Investments..USD.), Public.Awareness = mean(Public.Awareness), Exportaciones = sum(Energy.Exports), Importaciones = sum(Energy.Imports), Renewable.Energy.Jobs = sum(Renewable.Energy.Jobs), Renewable.Energy.Patents = sum(Renewable.Energy.Patents), Renewable.Energy.Education.Programs = max(Renewable.Energy.Education.Programs), Government.Policies= max(Government.Policies)) %>% 
  pivot_longer(cols = c("Importaciones", "Exportaciones"),
               names_to = "Energy.Flow.Type",
               values_to = "Energy.Flow")
data <- data %>% mutate(Government.Policies = case_when(Government.Policies == 1~"Con politicas de gobierno",
                                                        TRUE~"Sin políticas de gobierno")) %>% 
  mutate(Renewable.Energy.Education.Programs = case_when(Renewable.Energy.Education.Programs == 1~"Con programas educativos",
                                                         TRUE~"Sin programas educativos"))

#Precios anualizados

annual <- data %>% group_by(Year, Country) %>% summarise(avg_electricity_price = mean(Electricity.Prices))

energy.type <- c("Biomasa", "Geotérmica", "Hidráulica", "Solar")



#Interfaz
?navset_card_pill
ui <- fluidPage(
  titlePanel("Energía del Futuro: Análisis Global de Tendencias e Indicadores en Energías Renovables"),
  navset_bar(
    nav_panel("Contexto", 
              textOutput(outputId = "variables"),
              tableOutput(outputId = "table")),
    
    nav_panel("Impacto Ambiental", 
              fluidRow(
                column(6,
                       checkboxGroupInput("group", "Tipos de Energía:", energy.type, selected = c("Solar")),
                       plotOutput(outputId = "emissions")
                ),
                column(6,
                       textOutput(outputId = "CO2"),
                       plotOutput(outputId = "wind")
                )
              )),
    
    nav_panel("Impacto Económico", 
              fluidRow(
                column(6,
                       textOutput(outputId = "precios"),
                       radioButtons("facetp", "Separar por:",  c("Políticas de Gobierno", "País", "Ninguno"), selected = "Ninguno"),
                       sliderInput("heightprice", "Seleccione la altura de la figura", 250, 600, value = 300),
                       plotOutput(outputId = "distprices")
                ),
                column(6,
                       textOutput(outputId = "precios2"),
                       plotOutput(outputId = "prices")
                )
              )),
    
    nav_panel(
      "Impacto Social",
      sidebarLayout(
        sidebarPanel(
          textOutput(outputId = "conciencia"),
          selectInput("facets", "Separar por:", c("Año", "Programas Educativos", "Ambos"), selected = "Programas Educativos"),
          sliderInput("height", "Seleccione la altura de la figura", 250, 2500, value = 300)
        ),
        mainPanel(
          plotOutput(outputId = "social")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #Introductorio  
  variables <- data.frame(
    Variable = c(
      "Precios de la electricidad",
      "Producción",
      "Programas Educativos",
      "Conciencia pública",
      "Emisiones de CO2"
    ),
    Importancia = c(
      "La electricidad es un aspecto fundamental para determinar la calidad de vida de las personas, pues permite realizar varias actividades cotidianas, como cocinar o mantener el calor.",
      "Permite visualizar la evolución de los indicadores de bienestar con respecto a los cambios en producción energética; de modo que se pueda concluir el impacto que tiene la energía renovable en la sociedad.",
      "La existencia de programas educativos sobre energías renovables es un buen indicador del compromiso de los países con el uso de energías más limpias. Además, resulta esencial para crear conciencia, especialmente entre los jóvenes, sobre la importancia de reducir la huella ecológica.",
      "La aceptación pública es un factor importante en la cantidad de inversión destinada a la producción o desarrollo de energías renovables. La aceptación pública depende de la información disponible al público general, como los programas educativos y su nivel educativo.",
      "Aunque la cantidad de emisiones de dióxido de carbono generadas al producir energía eléctrica es relevante para todas las fuentes, cobra especial importancia en el caso de las energías renovables, ya que estas se consideran una medida esencial para reducir el impacto ambiental y combatir el cambio climático."
    )
  )
  output$variables <- renderText("Con el fin de responder la pregunta de investigación ¿Qué cambios ambientales y socioeconómicos produce la instauración de la energía renovable en los países?, se analizaron varias variables, entre las que destacan:")
  output$table <- renderTable(variables)
  #Ambientales  
  output$CO2 <- renderText("En cuanto al impacto ambiental, se encontraron patrones similares entre los diversos tipos de energía renovable, excepto para la energía eólica. \n La relación directa entre la producción de energías renovables y las emisiones de CO2 pueden explicarse por la dificultad de la transición desde fuentes  tradicionales a fuentes renovables, junto a las emisiones indirectas relacionadas a dicho proceso.")
  
  output$emissions <- renderPlot({
    data %>% filter(Energy.Type == input$group) %>% 
      ggplot(aes(x = Total.Production, y = CO2.Emissions, color = Energy.Type)) +
      geom_point()+
      scale_color_brewer(type = "qual", palette = 2)  +
      scale_x_continuous(labels = label_number(scale = 1/1000))+
      scale_y_continuous(labels = label_number(scale = 1/1000000))+
      facet_wrap(~Energy.Type, ncol = 2)+
      labs(
        title = "Producción y Emisiones de CO2",
        x = "Producción total (miles de GWh)",
        y = "Emisiones de CO2 (millones)",
        caption= "Información de \n https://www.kaggle.com/datasets/anishvijay/global-renewable-energy-\nand-indicators-dataset")+
      theme_cowplot()+
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 30, vjust=0.5))
  })
  
  output$wind <- renderPlot({
    data %>% filter(Energy.Type == "Eólica") %>% 
      ggplot(aes(x = Total.Production, y = CO2.Emissions, color = Energy.Type)) +
      geom_point(color = "#66a61e")+
      scale_x_continuous(labels = label_number(scale = 1/1000))+
      scale_y_continuous(labels = label_number(scale = 1/1000000))+
      facet_wrap(~Energy.Type, ncol = 2)+
      labs(
        title = "Producción y Emisiones de CO2",
        subtitle = "energía eólica",
        x = "Producción total (miles de GWh)",
        y = "Emisiones de CO2 (millones)",
        caption= "Información de \n https://www.kaggle.com/datasets/anishvijay/global-renewable-energy-\nand-indicators-dataset")+
      theme_cowplot()+
      theme(legend.position = "none")
  })
  
  #Económico
  output$precios <- renderText("Los precios de la electricidad, en general, tienden a concentrarse alrededor de 0,3. También se observa que una mayor producción de energías renovables contribuye a estabilizarlos en ese rango. Además, la existencia de políticas gubernamentales parece favorecer una mayor concentración de precios en niveles cercanos a 0,3.")
  output$precios2 <- renderText("Esto sugiere que una mayor estabilidad en los precios podría lograrse mediante el impulso a la producción de energías renovables y la implementación de políticas adecuadas.")
  
  output$distprices <- renderPlot({
    ggplot(data, aes(x = Total.Production, y = Electricity.Prices))+
      geom_point()+
      geom_hline(yintercept=0.29, linetype="dashed", color = "red")+
      labs(title = "Producción y Precios de Energia",
           x = "Producción",
           y = "Precios (dólares)",
           caption= "Información de \n https://www.kaggle.com/datasets/anishvijay/global-renewable-energy-\nand-indicators-dataset")+
      theme_cowplot()+
      theme(legend.position = "none")
  })
  
  pricesplot <- data %>% 
    ggplot(aes(x = Electricity.Prices))+
    geom_density() +
    labs(title = "Concentración Precios Energía",
         x = "Precios (dólares)",
         y = "Densidad",
         caption= "Información de \n https://www.kaggle.com/datasets/anishvijay/global-renewable-energy-\nand-indicators-dataset")+
    theme_cowplot()+
    theme( 
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none")
  
  
  output$prices <- renderPlot(
    if(input$facetp == "Políticas de Gobierno"){
      pricesplot + 
        geom_density(aes(fill = Government.Policies))+
        facet_wrap(~Government.Policies)+
        labs(subtitle = "en relación a políticas de gobierno")+
        scale_fill_manual(name="", 
                          labels = c("Con", "Sin"), 
                          values = c("Con politicas de gobierno"="#b8d8be", "Sin políticas de gobierno"="#ae5a41"))
    }
    else if(input$facetp == "País"){
      pricesplot + 
        geom_density(aes(fill = Country))+
        facet_wrap(~Country, ncol = 2)+
        labs(subtitle = "por país")+
        scale_fill_brewer(type = "div", palette = 1)
    }
    else{
      pricesplot}, height = function() {input$heightprice})
  
  #Social
  output$conciencia <- renderText("Aunque la segmentación por años permite observar los cambios en la conciencia pública global a lo largo del tiempo y evidenciar su aumento, resulta interesante analizar si los programas educativos contribuyen realmente a este crecimiento. De esta manera, es posible determinar que la conciencia pública, tanto en términos generales como por segmentos de edad, se ve impulsada por la presencia de programas educativos.")
  
  public <- data %>% ggplot(aes(Public.Awareness))+
    geom_density()+
    labs(title = "Conciencia pública",
         x = "Conciencia pública",
         y = "Densidad",
         caption= "Información de \n https://www.kaggle.com/datasets/anishvijay/global-renewable-energy-\nand-indicators-dataset")+
    theme_cowplot()+
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none")
  
  output$social <- renderPlot({
    if(input$facets == "Programas Educativos"){
      public + geom_density(aes(fill = Renewable.Energy.Education.Programs))+
        facet_wrap(~Renewable.Energy.Education.Programs, ncol = 2)+
        scale_fill_manual(name="", 
                          labels = c("Con", "Sin"), 
                          values = c("Con programas educativos"="#b8d8be", "Sin programas educativos"="#ae5a41"))+
        labs(subtitle = "Por existencia de programas educativos")
    }
    else if(input$facets == "Año"){
      public + facet_wrap(~Year, ncol = 2)+
        labs(subtitle = "Por año")
    }
    else if(input$facets == "Ambos"){
      public + geom_density(aes(fill = Renewable.Energy.Education.Programs))+
        facet_wrap(Year~Renewable.Energy.Education.Programs, ncol = 2)+
        scale_fill_manual(name="", 
                          labels = c("Con", "Sin"), 
                          values = c("Con programas educativos"="#b8d8be", "Sin programas educativos"="#ae5a41"))+
        labs(subtitle = "Por existencia de programas educativos y año")
    }
    else{
      public
    }
  }, height = function() {input$height})
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
