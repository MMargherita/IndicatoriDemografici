# Libraries ---------------------------------------------------------------
library(readr)
library(readxl)
library(shiny)
library(ggplot2)
library(dplyr)
library(shiny)
library(shinythemes)
library(stringr)
library(forcats)
library(magrittr)
library(ggdark)
library(hrbrthemes)

# Data manipulation ------------------------------------------------------------
# read data
data <- read_delim("DCIS_INDDEMOG1_21072021152359210.csv",
                   ";",
                   escape_double = FALSE,
                   col_types = cols(TIPO_DATO15 = col_skip(),
                                    `Seleziona periodo` = col_skip(),
                                    `Flag Codes` = col_skip(),
                                    Flags = col_skip()),
                   trim_ws = TRUE)


# filter what we want and adjust levels
data_reg <- data %>% 
  arrange(`Tipo indicatore`) %>% 
  filter(str_length(ITTER107)<5,
         TIME<2020,
         !Territorio %in% c("Nord","Centro","Sud", "Isole",
                            "Italia",
                            "Nord-ovest","Nord-est","Mezzogiorno",
                            "Provincia Autonoma Trento",
                            "Provincia Autonoma Bolzano / Bozen"),
         !`Tipo indicatore`%in% c("Saldo migratorio per altro motivo (per mille abitanti)",
                                  "Saldo migratorio interno (per mille abitanti)",
                                  "Saldo migratorio totale (per mille abitanti)")) %>% 
  mutate(Territorio = as.factor(Territorio),
         Ripartizione = as.factor(case_when(Territorio %in% c("Piemonte","Valle d'Aosta / Vallée d'Aoste","Liguria", "Lombardia","Trentino Alto Adige / Südtirol","Veneto","Friuli-Venezia Giulia", "Emilia-Romagna") ~ "Nord",
                                            Territorio %in% c("Toscana","Umbria","Marche","Lazio") ~ "Centro",
                                            Territorio %in% c("Abruzzo","Molise","Campania","Puglia","Basilicata","Calabria","Sicilia","Sardegna") ~ "Sud e Isole")))

levels(data_reg$Territorio)[levels(data_reg$Territorio) == "Valle d'Aosta / Vallée d'Aoste"] <- "Valle d'Aosta"
levels(data_reg$Territorio)[levels(data_reg$Territorio) == "Trentino Alto Adige / Südtirol"] <- "Trentino Alto Adige"
data_reg$Ripartizione <- factor(data_reg$Ripartizione,levels(data_reg$Ripartizione)[c(3,4,1,5,2)])

anno <- unique(data_reg$TIME)

indicatore <- as.factor(unique(data_reg$`Tipo indicatore`))
names(indicatore) <- unique(data_reg$`Tipo indicatore`)
levels(indicatore)
indicatore <- sort(factor(as.factor(indicatore), levels(as.factor(indicatore))[c(17,15,16,14,12,13,19,21,20,7,2,3,8,9,10,18,1,11,4,5,6)]))


# captions
captions <- read_excel("captions.xlsx") %>%
  arrange(indicatori)%>% 
  filter(!indicatori%in% c("Saldo migratorio per altro motivo (per mille abitanti)",
                           "Saldo migratorio interno (per mille abitanti)",
                           "Saldo migratorio totale (per mille abitanti)")) %>% 
  select(`descrizione indicatori`)

capt <- captions$`descrizione indicatori`

capt <- capt[c(15,15,15,14,14,14,19,21,20,7,2,3,8,9,10,18,1,11,4,5,6)]
names(capt) <- names(indicatore)



# UI and Server -----------------------------------------------------------
# Define UI for application that draws a plot


ui <- navbarPage(
  theme = shinytheme("cyborg"),
  title = "Indicatori demografici",
  tabPanel(
    "Descrizione",
    h1("Metti le regioni in classifica ... demograficamente!"),
    h4("Con questa applicazione puoi visualizzare la classifica delle regioni italiane secondo alcuni dei principali indicatori demografici, dal 2002 al 2019."),
    h4("Scopri di più cliccando in alto < Visualizzazione interattiva > ! All'interno della visualizzazione interattiva, potrai scegliere l'indicatore demografico e l'anno di interesse e ottenere la classifica delle regioni italiane. Sotto al grafico della classifica è anche riportata la spiegazione di ogni indicatore"),
    h3(""),
    h3(""),
    h5("Realizzato per il festival della statistica e della demografia StatisticAll 2021, a cura di:"),
    h6("Margherita Moretti,",tags$sup(1),", Cosmo Strozza",tags$sup(2),", Cecilia Fortunato",tags$sup(1),", Gruppo Demografica...Mente!"),
    h6(tags$sup(1),"Sapienza Università di Roma,",tags$sup(2)," Interdisciplinary Centre on Population Dynamics"),
    
    # prove per inserire l'immagine:
    
    # includeMarkdown("introduction.Rmd"),
    img(src = "logo-sdu-cpop.png", height = 40, width = 100) 
    
    
    ), 
  tabPanel(
    "Visualizzazione interattiva",
    # Application title
    titlePanel(
      h3("Visualizza i principali indicatori demografici")),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(width = 4,
                   selectizeInput( # choose the indicatore
                     "indicatore",
                     label     = "Scegli l'indicatore",
                     choices   = indicatore, 
                     multiple = FALSE,
                     selected  = "Speranza di vita alla nascita - totale",
                     options = list(create = TRUE, maxItems = 1)
                   ),
                   selectizeInput( # choose the year
                     "anno",
                     label     = "Scegli l'anno",
                     choices   = anno, 
                     multiple = FALSE,
                     selected  = 2019,
                     options = list(create = TRUE, maxItems = 1)
                   ),
      ),
      # Show plot 
      mainPanel(
        plotOutput("plot", height = "500px", width = "100%"),
        textOutput("caption")
        #, width = "100%")
      )
    )
  )
)


# server to draw the plot
server <- function(input, output) {
  
# Reactive UI widgets
  output$plot <- renderPlot({
    
    
    indic = input$indicatore
    an = input$anno
    
    # plot
    data_reg %>% 
      filter(`Tipo indicatore` == indic,
             TIME == an) %>%
      ggplot(aes(x = Value, y = reorder(Territorio,Value),
                 color = fct_rev(Ripartizione))) +
      geom_point(size = 3.5)+
      labs(x = element_blank(),
           y = element_blank(),
           color = "Ripartizione geografica",
           title = str_wrap(element_text(paste(indic, "nelle regioni italiane. Anno",
                                               an)), 80),
           caption = "Fonte dati: Istat\nA cura di: M. Moretti, C. Strozza, C. Fortunato (Demografica...Mente!)")+
      dark_theme_minimal()+
      theme(text = element_text(size = 13),
            axis.text.y = element_text(size = 10),
            legend.position = "bottom")+
      scale_color_viridis_d(option = "B",
                            begin = 0.3,
                            end = 0.8)
    
  }, height = 500, res = 100)
  
  
  
  
  output$caption <- renderText({
    capt[input$indicatore]
  })
  
  
}


# RUN THE APP -------------------------------------------------------------
shinyApp(ui = ui, server = server)


  
    
    