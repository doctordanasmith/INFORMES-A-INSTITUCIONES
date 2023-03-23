library(tidyverse)

library(lubridate)

library(readxl)


base_12_5_2021 <- read_csv("base llena 12-5-2021.csv")


base_12_5_2021 <- base_12_5_2021 %>% mutate(`Fecha Diagnós.` = dmy(`Fecha Diagnós.`))

TOPcancerHIGA_2018_2019 <- base_12_5_2021 %>% filter(`Fecha Diagnós.` > "2017-12-31") %>%
  mutate(clasificacion = if_else(str_detect(`Morfología (desc)`, pattern = "[Mm]elanoma"), "Melanoma", `Topografía (cat)`))
TOPcancerHIGA_2018_2019 %>%  ggplot(aes(x = clasificacion))+
  geom_bar(position = "stack")


library(scales)

library(ggrepel)

TOPcancerHIGA_2018_2019 %>% count(`Sexo (desc)`) %>% 
  mutate(porc = n/sum(n)*100) 

## grafico circular SEXO

TOPcancerHIGA_2018_2019 %>% 
  # filter(`Sexo (desc)` != "Desc.") %>%               
  group_by(`Sexo (desc)`) %>%                    
  summarise(Cantidad = length(`Sexo (desc)`)) %>% 
  mutate(fraccion = Cantidad/sum(Cantidad),
         ymax = cumsum(fraccion),
         ymin = c(0, head(ymax, n=-1)), 
         etiqueta = number(fraccion,accuracy = 0.1, scale = 100, decimal.mark = ",")) %>% 
  ggplot(aes(fill=`Sexo (desc)`, ymax=ymax, ymin=ymin,  
             xmax=4, xmin=2.5)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  annotate("text", x = 0, y = 0, label = "Sexo") +
  scale_fill_brewer(palette = "Accent", 
                    name="", direction = 1) +
  labs(title="",x="",y="", caption = "N = 1151") +
  geom_text_repel(aes(x = 3.9, y = ymax-((ymax-ymin)/2),
                      label = paste0(etiqueta, "%")), point.padding = 0.5, nudge_x = 0.5, nudge_y = 0.0005)


  geom_text_repel(aes(x = 4, y = ymax-((ymax-ymin)/2),
                      label = paste0(etiqueta, "%"))) 

  
 

TOPcancerHIGA_2018_2019 %>% 
  # filter(`Sexo (desc)` != "Desc.") %>%               
  group_by(`Sexo (desc)`) %>%                    
  summarise(Cantidad = length(`Sexo (desc)`)) %>% 
  mutate(fraccion = Cantidad/sum(Cantidad),
         ymax = cumsum(fraccion),
         ymin = c(0, head(ymax, n=-1)), 
         etiqueta = number(fraccion,accuracy = 0.1, scale = 100, decimal.mark = ",")) %>% 
  ggplot(aes(fill=`Sexo (desc)`, ymax=ymax, ymin=ymin,  
             xmax=4, xmin=3)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  annotate("text", x = 0, y = 0, label = "Sexo") +
  scale_fill_manual(values = c("sandybrown", "paleturquoise3")) +
  labs(title="",x="",y="", caption = "N = 2783") +
  geom_text_repel(aes(x = 4, y = ymax-((ymax-ymin)/2),
                      label = paste0(etiqueta, "%")))

## grafico piramide 

# intervalos de edad cada 20 años

TOPcancerHIGA_2018_2019 %>% 
  mutate(grupo_edad = cut(Edad, breaks = seq(0,120, by =20))) %>% 
  count(grupo_edad, `Sexo (desc)`, name = "Poblacion") %>% 
  filter(!is.na(grupo_edad)) %>%
  mutate(Poblacion = as.numeric(Poblacion),
         Poblacion = if_else(`Sexo (desc)` == "Femenino", Poblacion*-1, Poblacion)) %>% 
  ggplot(aes(x = grupo_edad, fill = `Sexo (desc)`,
             y = Poblacion)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(breaks = seq(-700, 700, 50), 
                     labels = paste0(as.character(c(seq(-700, 0, 50)*-1, seq(50, 700, 50)))),
                     limits = c(-700, 700)) + 
  coord_flip() + 
  scale_fill_brewer(palette = "Set1",direction = -1) + 
  theme_bw() + 
  labs(y = "Población", title = "Titulo")		  


# intervalos de edad cada 10 años

TOPcancerHIGA_2018_2019 %>% 
  mutate(grupo_edad = cut(Edad, breaks = seq(0,120, by =10))) %>% 
  count(grupo_edad, `Sexo (desc)`, name = "Poblacion") %>% 
  filter(!is.na(grupo_edad)) %>%
  mutate(Poblacion = as.numeric(Poblacion),
         Poblacion = if_else(`Sexo (desc)` == "Femenino", Poblacion*-1, Poblacion)) %>% 
  ggplot(aes(x = grupo_edad, fill = `Sexo (desc)`,
             y = Poblacion)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(breaks = seq(-200, 200, 50), 
                     labels = paste0(as.character(c(seq(-200, 0, 50)*-1, seq(50, 200, 50)))),
                     limits = c(-200, 200)) + 
  coord_flip() + 
  scale_fill_brewer(palette = "Set1",direction = 1) + 
  theme_bw() + 
  labs(y = "Población", title = "Distribución según Sexo y Edad - Pacientes HIGA \n Año 2018-2019",
       caption = "N = 1151")		

ggsave(filename = "piramide.jpg", dpi = 600, device = "jpeg")	

ggsave(filename = "piramide.svg", dpi = 600, device = "svg")

# grafico de barras - top 10

TOPcancerHIGA_2018_2019 %>% 
  mutate(clasificacion = factor(clasificacion),
         clasificacion = fct_collapse(clasificacion,
                                      Mama = "Mama",
                                      Piel = "Piel",
                                      `Cuello utero` = "Cuello utero",
                                      Pulmon = "Bronquios & Pulmón",
                                      Colon = "Colon",
                                      Hematopo = "Hematopo. y Reticul. sys",
                                      Lymph = "Lymph nodes",
                                      Tiroides = "Glándula Tiroides",
                                      Recto = "Recto",
                                      `Estómago` = "Estómago",
                                      other_level = "Otros")) %>% 
  count(clasificacion, sort = T) %>% 
  mutate(porc = round(100*n/sum(n),1)) %>% 
  filter(clasificacion != "Otros") %>% 
  mutate(clasificacion = fct_reorder(clasificacion, porc, .desc = TRUE)) %>% 
  ggplot(aes(x =clasificacion, y = porc, fill = clasificacion)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = porc)) +
  scale_y_continuous(name = "%", breaks = seq(0,20, by = 2)) +
  scale_fill_brewer(palette = "Set3")
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=0.6))	+ 
    labs(title = "Distribución de los 10 sitios más frecuentes - Pacientes HIGA \n Año 2018-2019",
         caption = "N = 1151") 
  


