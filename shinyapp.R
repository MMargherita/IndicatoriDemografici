# packages
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

# read data
data <- read_delim("DCIS_INDDEMOG1_21072021152359210.csv",
                   ";",
                   escape_double = FALSE,
                   col_types = cols(TIPO_DATO15 = col_skip(),
                                    `Seleziona periodo` = col_skip(),
                                    `Flag Codes` = col_skip(),
                                    Flags = col_skip()),
                   trim_ws = TRUE)


# data
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
         "saldo migratorio totale (per mille abitanti)"))

indicatore <- unique(data_reg$`Tipo indicatore`)
names(indicatore) <- unique(data_reg$`Tipo indicatore`)
anno <- unique(data_reg$TIME)


# caption
captions <- read_excel("captions.xlsx") %>% 
  filter(!indicatori%in% c("saldo migratorio per altro motivo (per mille abitanti)",
                                  "saldo migratorio interno (per mille abitanti)",
                                  "saldo migratorio totale (per mille abitanti)")) %>% 
  select(`descrizione indicatori`)
capt <- captions$`descrizione indicatori`
names(capt) <- names(indicatore)
# caption <- "prova"



# ui and server -----------------------------------------------------------


# Define UI for application that draws a plot
ui <- fluidPage(#theme = shinytheme("slate"),
                
                # Application title
                titlePanel(NULL),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(selectizeInput( # choose the indicatore
                                 "indicatore",
                                 label     = "Scegli l'indicatore",
                                 choices   = indicatore, 
                                 multiple = FALSE,
                                 selected  = "numero medio di figli per donna",
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
        # arrange(desc(Value)) %>% 
        ggplot(aes(x = Value, y = reorder(Territorio,Value),
                   color = Territorio)) +
        geom_point(size = 1.5)+
        labs(x = element_text("Valore"), y = element_text("Regione"))+
        theme_minimal()+
        theme(text = element_text(size = 15),
              legend.position ="none")+
        guides(fill=guide_legend(nrow=5,byrow=TRUE))

  })#, width = 800, height = 315, res = 100)
  
  
  
  
  output$caption <- renderText({
    capt[input$indicatore]
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
