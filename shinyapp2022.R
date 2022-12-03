# Libraries -----
library(tidyverse)
library(dplyr)
library(shiny)
library(shinythemes)
library(bslib)
library(markdown)
library(readxl)
library(readr)
library(ggplot2)
library(ggdark)


# N.B. perchè alcuni intervalli non sono centrati?? es. indice di vecchiaia...


data <- read_csv("data/DCIS_INDDEMOG1_25072022184237822.csv", 
                 col_types = cols(TIPO_DATO15 = col_skip(), 
                                  `Seleziona periodo` = col_skip(), 
                                  `Flag Codes` = col_skip(), Flags = col_skip())) %>% 
  mutate(`Intervallo di previsione` = "mediana") %>% 
  select(ITTER107,Territorio,`Tipo indicatore`,TIME,Value,`Intervallo di previsione`)

previsioni <- read_csv("data/DCIS_PREVDEM1_23112022151530397.csv", 
                       col_types = cols(TIPO_DATO15 = col_skip(), 
                                        INT_PREV = col_skip(),`Seleziona periodo` = col_skip(),
                                        `Flag Codes` = col_skip(), 
                                        Flags = col_skip()))%>% 
  rename(`Tipo indicatore`=`Tipo di indicatore demografico`)%>% 
  dplyr::filter(!`Territorio`%in% c("Provincia Autonoma Bolzano / Bozen",
                                 "Provincia Autonoma Trento")) %>% 
  select(ITTER107,Territorio,`Tipo indicatore`,TIME,Value,`Intervallo di previsione`)




# merge, filter what we want and adjust levels
data_prev <- rbind(data,previsioni)

data_prev_reg <- data_prev %>% 
  arrange(`Tipo indicatore`) %>% 
  filter(str_length(ITTER107)<5,
         !`Tipo indicatore`%in% c("Saldo migratorio per altro motivo (per mille abitanti)",
                                  "Saldo migratorio interno (per mille abitanti)",
                                  "Saldo migratorio totale (per mille abitanti)")) %>% 
  mutate(Territorio = as.factor(Territorio),
         Ripartizione = as.factor(case_when(Territorio %in% c("Piemonte","Valle d'Aosta / Vallée d'Aoste","Liguria", "Lombardia","Trentino Alto Adige / Südtirol","Veneto","Friuli-Venezia Giulia", "Emilia-Romagna") ~ "Nord",
                                            Territorio %in% c("Toscana","Umbria","Marche","Lazio") ~ "Centro",
                                            Territorio %in% c("Abruzzo","Molise","Campania","Puglia","Basilicata","Calabria","Sicilia","Sardegna") ~ "Sud e Isole")))


# to wide format to add the CIs variable
data_prev_reg_wide <- data_prev_reg %>% 
  pivot_wider(names_from = `Intervallo di previsione`,
              values_from = `Value`)


levels(data_prev_reg_wide$Territorio)[levels(data_prev_reg_wide$Territorio) == "Valle d'Aosta / Vallée d'Aoste"] <- "Valle d'Aosta"
levels(data_prev_reg_wide$Territorio)[levels(data_prev_reg_wide$Territorio) == "Trentino Alto Adige / Südtirol"] <- "Trentino Alto Adige"
data_prev_reg_wide$Ripartizione <- factor(data_prev_reg_wide$Ripartizione,levels(data_prev_reg_wide$Ripartizione)[c(3,4,1,5,2)])

anno <- unique(data_prev_reg_wide$TIME)

indicatore <- as.factor(unique(data_prev_reg_wide$`Tipo indicatore`))
names(indicatore) <- unique(data_prev_reg_wide$`Tipo indicatore`)
levels(indicatore)
indicatore <- sort(factor(as.factor(indicatore), levels(as.factor(indicatore))[c(17,15,16,14,12,13,19,21,20,7,2,3,8,9,10,18,1,11,4,5,6)]))





# read captions
captions <- read_excel("data/captions.xlsx") %>%
  arrange(indicatori)%>% 
  filter(!indicatori%in% c("Saldo migratorio per altro motivo (per mille abitanti)",
                           "Saldo migratorio interno (per mille abitanti)",
                           "Saldo migratorio totale (per mille abitanti)")) %>% 
  select(`descrizione indicatori`)

# name and order captions
capt <- captions$`descrizione indicatori`

capt <- capt[c(15,15,15,14,14,14,19,21,20,7,2,3,8,9,10,18,1,11,4,5,6)]
names(capt) <- names(indicatore)





# UI and Server -----------------------------------------------------------

# set-up for light/dark mode
light <- bs_theme(bootswatch = "flatly")
# dark <- bs_theme(bg = "black", fg = "white", primary = "purple")
# dark <- bs_theme(bootswatch = "darkly")


# ui
ui <- navbarPage(
  title = "INDICATORI DEMOGRAFICI",
  
  # theme = shinytheme("cyborg"),
  theme = light, 
  # checkboxInput("dark_mode", "Dark mode"),
  
  tabPanel(
    "Descrizione", # cover page
    withMathJax((includeMarkdown("Cover_Page.Rmd")))
  ), 
  
  
  tabPanel(
    "Visualizzazione interattiva", # page with interactive plot
    withMathJax((includeMarkdown("Figure.Rmd"))),
    sidebarLayout(
      sidebarPanel(width = 4,
                   selectizeInput( # select indicator
                     "indicatore",
                     label     = "Scegli l'indicatore",
                     choices   = indicatore, 
                     multiple = FALSE,
                     selected  = "Speranza di vita alla nascita - totale",
                     options = list(create = TRUE, maxItems = 1)
                   ),
                   selectizeInput( # select the year
                     "anno",
                     label     = "Scegli l'anno",
                     choices   = anno, 
                     multiple = FALSE,
                     selected  = 2019,
                     options = list(create = TRUE, maxItems = 1)
                   ),
      ),
      mainPanel(
        plotOutput("plot", height = "500px", width = "100%"), # Show the plot 
        textOutput("caption") # display the caption under the plot
      )
    )
  )
)



# server 
server <- function(input, output){#, session) {
  
  # observe(session$setCurrentTheme(
  #   if (isTRUE(input$dark_mode)) dark else light
  # ))
  
  output$plot <- renderPlot({ # plot
    indic = input$indicatore
    an = input$anno

      data_prev_reg_wide %>% 
      filter(`Tipo indicatore` == indic,
             TIME == an) %>%
      ggplot(aes(x = mediana, y = reorder(Territorio,mediana),
                 color = fct_rev(Ripartizione))) +
      geom_point(size = 3.5)+
      geom_errorbarh(aes(xmin=`limite inferiore 90%`,
                     xmax=`limite superiore 90%`,
                     color = fct_rev(Ripartizione)))+
      labs(x = element_blank(),
           y = element_blank(),
           color = "Ripartizione geografica",
           title = str_wrap(element_text(paste(indic, "nelle regioni italiane. Anno",an)), 80),
           caption = "Fonte dati: Istat\nA cura di: M. Moretti, C. Strozza, C. Fortunato (Demografica...Mente!)")+
      # dark_theme_minimal()+
      theme_minimal()+
      theme(text = element_text(size = 13),
            axis.text.y = element_text(size = 10),
            legend.position = "bottom")+
      scale_color_viridis_d(option = "B",
                            begin = 0.3,
                            end = 0.8)
  }, height = 500, res = 100)
  
  output$caption <- renderText({
    capt[input$indicatore] # captions
  })
  
  
}  


# Run the app ------------------------------------------------------------------
shinyApp(ui = ui, server = server)

