
library(shiny)
library(ggplot2)
library(dplyr)
library(shiny)
library(shinythemes)
library(stringr)
library(forcats)
library(readr)

data <- read_delim("C:/Users/margh/LA MIA CARTELLA/DOTTORATO - Copia/Attività/Conferenze/StatisticAll 2021/DCIS_INDDEMOG1_21072021152359210.csv",
                                               ";", escape_double = FALSE, col_types = cols(TIPO_DATO15 = col_skip(),
                                                                                            `Seleziona periodo` = col_skip(),
                                                                                            `Flag Codes` = col_skip(), Flags = col_skip()),
                                               trim_ws = TRUE)





data_reg <- data %>% 
  filter(str_length(ITTER107)<5,
         !Territorio %in% c("Nord","Centro","Sud", "Isole",
                            "Italia",
                            "Nord-ovest","Nord-est","Mezzogiorno",
                            "Trentino Alto Adige / Südtirol")) #ci sono separate


# data %>% 
#   filter(`Tipo indicatore` == "tasso di natalità (per mille abitanti)",
#          Territorio %in% c("Nord","Centro","Sud", "Isole")) %>% 
#   ggplot(aes(x = TIME, y = Value,
#              color = Territorio)) +
#   geom_line(size = 1.5)+
#   # scale_y_continuous(limits = c(min(Value)+1, max(Value)+1),
#   #                    breaks = seq(min(Value)+1, max(Value)+1,
#   #                                 by = 2))+
#   # scale_y_continuous(limits = c(6, 12), breaks = seq(6, 12, by = 2))+
#   labs(x = element_text("Anni"), y = element_text("Valore"))+
#   theme_minimal()+
#   theme(text = element_text(size = 26))

# asse y breaks ? 
# ordine factors legenda ?


data_reg %>% 
  filter(`Tipo indicatore` == "speranza di vita a 65 anni - femmine",
         TIME == 2019) %>% 
  # arrange(desc(Value)) %>% 
  ggplot(aes(x = Value, y = reorder(Territorio,Value),
             color = Territorio)) +
  geom_point(size = 1.5)+
  labs(x = element_text("Valore"), y = element_text("Regione"))+
  theme_minimal()+
  theme(text = element_text(size = 26),
        legend.position ="none")+
  guides(fill=guide_legend(nrow=5,byrow=TRUE))

