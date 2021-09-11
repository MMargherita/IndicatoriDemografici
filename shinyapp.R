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

# Data manipulation -------------------------------------------------------
# read data
data <- read_delim("DCIS_INDDEMOG1_21072021152359210.csv",
                   ";",
                   escape_double = FALSE,
                   col_types = cols(TIPO_DATO15 = col_skip(),
                                    `Seleziona periodo` = col_skip(),
                                    `Flag Codes` = col_skip(),
                                    Flags = col_skip()),
                   trim_ws = TRUE)


# data manipulation
data_reg <- data %>% 
  filter(str_length(ITTER107)<5,
         TIME<2020,
         !Territorio %in% c("Nord","Centro","Sud", "Isole",
                            "Italia",
                            "Nord-ovest","Nord-est","Mezzogiorno",
                            "Provincia Autonoma Trento",
                            "Provincia Autonoma Bolzano / Bozen"),
         !`Tipo indicatore`%in% c("saldo migratorio per altro motivo (per mille abitanti)",
         "saldo migratorio interno (per mille abitanti)",
         "saldo migratorio totale (per mille abitanti)")) %>% 
  mutate(Territorio = as.factor(Territorio),
         Ripartizione = as.factor(case_when(Territorio %in% c("Piemonte","Valle d'Aosta / Vallée d'Aoste",
                                                              "Liguria", "Lombardia") ~ "Nord-ovest",
                                            Territorio %in% c("Trentino Alto Adige / Südtirol","Veneto",
                                                              "Friuli-Venezia Giulia", "Emilia-Romagna") ~ "Nord-est",
                                            Territorio %in% c("Toscana","Umbria","Marche","Lazio") ~ "Centro",
                                            Territorio %in% c("Abruzzo","Molise","Campania","Puglia",
                                                              "Basilicata","Calabria") ~ "Sud",
                                            Territorio %in% c("Sicilia","Sardegna") ~ "Isole")))

levels(data_reg$Territorio)[levels(data_reg$Territorio) == "Valle d'Aosta / Vallée d'Aoste"] <- "Valle d'Aosta"
levels(data_reg$Territorio)[levels(data_reg$Territorio) == "Trentino Alto Adige / Südtirol"] <- "Trentino Alto Adige"
data_reg$Ripartizione <- factor(data_reg$Ripartizione,levels(data_reg$Ripartizione)[c(3,4,1,5,2)])

indicatore <- as.factor(unique(data_reg$`Tipo indicatore`))
names(indicatore) <- unique(data_reg$`Tipo indicatore`)
levels(indicatore)
indicatore <- factor(as.factor(indicatore), levels(as.factor(indicatore))[c(20,18,19,17,15,16,22,24,23,2,7,1,21,8,9,10,3,4,5,6,11,12,13,14)])
anno <- unique(data_reg$TIME)

# caption
captions <- read_excel("captions2.xlsx") %>% 
  #filter(!indicatori%in% c("Saldo migratorio per altro motivo (per mille abitanti)",
  #                                "Saldo migratorio interno (per mille abitanti)",
  #                                "Saldo migratorio totale (per mille abitanti)")) %>% 
  select(`descrizione indicatori`)
capt <- captions$`descrizione indicatori`
names(capt) <- names(indicatore)
# caption <- "prova"


# UI and Server -----------------------------------------------------------
# Define UI for application that draws a plot
ui <- fluidPage(theme = shinytheme("slate"),
                
                # Application title
                titlePanel("Indicatori demografici"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(selectizeInput( # choose the indicatore
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
                    plotOutput("plot", height = "300px"),
                    textOutput("caption")
                  )
                )
)

# server to draw the plot
server <- function(input, output) {
  
  output$plot <- renderPlot({
    
      indic = input$indicatore
      an = input$anno
      
      # plot
      data_reg %>% 
        filter(`Tipo indicatore` == indic,
               TIME == an) %>%
        #mutate(`Tipo indicatore` %>% as.factor() = factor(`Tipo indicatore`, levels(`Tipo indicatore`)[23,14,12,24,15,13,2,3,1,10,11,8,9,16,17,18,19,20,21,22,4,5,6,7])) %>% 
        # arrange(desc(Value)) %>% 
        ggplot(aes(x = Value, y = reorder(Territorio,Value),
                   color = Ripartizione)) +
        geom_point(size = 3)+
        labs(x = element_blank(),
             y = element_blank(),
             color = "Ripartizione\ngeografica",
             title = element_text(paste(indic, "nelle regioni italiane. Anno", an)),
             caption = "Fonte: Istat")+
        theme_minimal()+
        theme(text = element_text(size = 15),
              legend.position = "bottom")

  })#, width = 800, height = 315, res = 100)
  
  
  
  
  output$caption <- renderText({
    capt[input$indicatore]
  })
  
  
}


# RUN THE APP -------------------------------------------------------------
shinyApp(ui = ui, server = server)
