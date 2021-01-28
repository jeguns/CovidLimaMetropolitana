
# -------- #
# Paquetes #
# -------- #

library(ggplot2)
library(padr)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(reshape)
library(plotrix)
library(tidyquant)

# -------------- #
# Carga de datos #
# -------------- #

npos = paste0("positivos_covid_",as.character(today()),".csv")
nfal = paste0("fallecidos_covid_",as.character(today()),".csv")

download.file("https://cloud.minsa.gob.pe/s/Y8w3wHsEdYQSZRp/download",destfile=npos)
download.file("https://cloud.minsa.gob.pe/s/Md37cjXmjT9qYSa/download",destfile=nfal)

datos.abiertos.p = read.csv(npos,sep=";")
datos.abiertos.f = read.csv(nfal,sep=";")
demografia       = read.csv("Demografia.csv", sep = ";")

# ----------------- #
# Limpieza de datos #
# ----------------- #

datos.abiertos.p$FECHA_RESULTADO = as.Date(as.character(datos.abiertos.p$FECHA_RESULTADO), format = c("%Y%m%d"))
datos.abiertos.p = datos.abiertos.p %>% 
  dplyr::filter(!is.na(FECHA_RESULTADO))

datos.abiertos.f$FECHA_FALLECIMIENTO = as.Date(as.character(datos.abiertos.f$FECHA_FALLECIMIENTO), format = c("%Y%m%d"))
datos.abiertos.f = datos.abiertos.f %>% 
  dplyr::filter(!is.na(FECHA_FALLECIMIENTO))

print(paste0("Datos abiertos POSITIVOS COVID actualizados disponibles del ",
             min(datos.abiertos.p$FECHA_RESULTADO), " al ",
             max(datos.abiertos.p$FECHA_RESULTADO)))
print(paste0("Datos abiertos FALLECIDOS COVID actualizados disponibles del ",
             min(datos.abiertos.f$FECHA_FALLECIMIENTO), " al ",
             max(datos.abiertos.f$FECHA_FALLECIMIENTO)))

datos.abiertos.p$SEXO = as.factor(datos.abiertos.p$SEXO)
datos.abiertos.p$SEXO %>% table()
datos.abiertos.f$SEXO = as.factor(datos.abiertos.f$SEXO)
datos.abiertos.f$SEXO %>% table()

# ---------------------------------------------------------- #
# Número de casos y fallecidos por distrito (Lima Provincia) #
# ---------------------------------------------------------- #

casos.distritos = datos.abiertos.p %>% 
  dplyr::filter(DEPARTAMENTO == "LIMA" & PROVINCIA == "LIMA" | DEPARTAMENTO =="CALLAO") %>% 
  dplyr::mutate(DISTRITO = ifelse(DISTRITO=="CARMEN DE LA LEGUA-REYNOSO","CARMEN DE LA LEGUA REYNOSO",DISTRITO)) %>% 
  dplyr::group_by(DISTRITO) %>% 
  dplyr::count() %>% 
  dplyr::rename(NCASOS = n) %>% 
  dplyr::select(DISTRITO, NCASOS)

fallecidos.distritos = datos.abiertos.f %>% 
  filter(DEPARTAMENTO == "LIMA" & PROVINCIA == "LIMA"| DEPARTAMENTO =="CALLAO") %>% 
  filter(!DISTRITO %in% c("MORROPON","PUEBLO NUEVO","")) %>% 
  dplyr::mutate(DISTRITO = ifelse(DISTRITO=="CARMEN DE LA LEGUA-REYNOSO","CARMEN DE LA LEGUA REYNOSO",DISTRITO)) %>% 
  group_by(DISTRITO) %>% 
  dplyr::count() %>% 
  dplyr::rename(NFALLECIDOS = n)  

reporte = casos.distritos %>% 
  full_join(fallecidos.distritos) %>% 
  left_join(demografia) %>% 
  mutate(NCASOS10MIL = NCASOS/TOTALPOB*10^4,
         NCASOSKM2   = NCASOS/SUPERFICIE,
         NFALL10MIL  = NFALLECIDOS/TOTALPOB*10^4,
         NFALLKM2    = NFALLECIDOS/SUPERFICIE,
         LETALIDAD   = round(NFALLECIDOS/NCASOS*100,2)) %>% 
  dplyr::select(PROVINCIA,DISTRITO,
                NCASOS,NCASOS10MIL,NCASOSKM2,
                NFALLECIDOS,NFALL10MIL,NFALLKM2,
                LETALIDAD,TOTALPOB,SUPERFICIE)

nombre_archivo = paste0("Reporte_LimaMetropolitana_",
                        as.character(max(max(datos.abiertos.p$FECHA_RESULTADO),
                                         max(datos.abiertos.f$FECHA_FALLECIMIENTO))),
                        ".csv")

write.csv(reporte,nombre_archivo)

reporte %>% arrange(desc(NCASOS)) %>% select(DISTRITO,NCASOS)
reporte %>% arrange(NCASOS) %>% select(DISTRITO,NCASOS)
reporte %>% arrange(desc(NCASOS10MIL)) %>% select(DISTRITO,NCASOS10MIL)
reporte %>% arrange(NCASOS10MIL) %>% select(DISTRITO,NCASOS10MIL)
reporte %>% arrange(desc(NCASOSKM2)) %>% select(DISTRITO,NCASOSKM2)
reporte %>% arrange(NCASOSKM2) %>% select(DISTRITO,NCASOSKM2)

reporte %>% arrange(desc(NFALLECIDOS)) %>% select(DISTRITO,NFALLECIDOS)
reporte %>% arrange(NFALLECIDOS) %>% select(DISTRITO,NFALLECIDOS)
reporte %>% arrange(desc(NFALL10MIL)) %>% select(DISTRITO,NFALL10MIL)
reporte %>% arrange(NFALL10MIL) %>% select(DISTRITO,NFALL10MIL)
reporte %>% arrange(desc(NFALLKM2)) %>% select(DISTRITO,NFALLKM2)
reporte %>% arrange(NFALLKM2) %>% select(DISTRITO,NFALLKM2)
reporte %>% arrange(desc(LETALIDAD)) %>% select(DISTRITO,LETALIDAD)
reporte %>% arrange(LETALIDAD) %>% select(DISTRITO,LETALIDAD)


# ---------------------------------------------------- #
# GRÁFICAS DE CASOS DIARIOS Y ACUMULADOS POR DISTRITOS #
# ---------------------------------------------------- #

tema1 =   theme(axis.text.x      = element_text(angle = 45, hjust = 1),
                axis.text        = element_text(size = 8),
                axis.title       = element_text(size = 8),
                legend.position  = "bottom",
                legend.key.size  = unit(0.5, "cm"),
                legend.key.width = unit(0.5,"cm"),
                legend.text      = element_text(size = 10),
                plot.title       = element_text(size = 12, face = "bold",hjust = 0.5),
                plot.caption     = element_text(size = 8),
                panel.background = element_rect(fill = "white",
                                                colour = "white",
                                                size = 0.5, linetype = "solid"),
                panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                colour = "gray80"), 
                panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                colour = "gray80"))

fechas.p = seq.Date(from = min(datos.abiertos.p$FECHA_RESULTADO),
                    to   = max(datos.abiertos.p$FECHA_RESULTADO)+1,
                    by   = 1)

fechas.f = seq.Date(from = min(datos.abiertos.f$FECHA_FALLECIMIENTO),
                    to   = max(datos.abiertos.f$FECHA_FALLECIMIENTO)+1,
                    by   = 1)

distritos = reporte$DISTRITO[reporte$DISTRITO!="EN INVESTIGACIÓN"] 

for(i in 1:length(distritos)){
  
  
  datos1.p = datos.abiertos.p %>% 
    filter(DEPARTAMENTO == "LIMA" & PROVINCIA == "LIMA"| DEPARTAMENTO =="CALLAO") %>% 
    filter(!DISTRITO %in% c("MORROPON","PUEBLO NUEVO","","EN INVESTIGACIÓN")) %>%  
    filter(DISTRITO == distritos[i]) %>% 
    group_by(FECHA_RESULTADO) %>% 
    dplyr::count() %>% 
    right_join(data.frame(fechas.p),by=c("FECHA_RESULTADO"="fechas.p")) %>% 
    dplyr::mutate(n=replace_na(n,0)) %>% 
    arrange(FECHA_RESULTADO)
  
  datos1.p = cbind(datos1.p, nacum = cumsum(datos1.p$n))
  
  tit1 = paste0("Casos diarios - ",distritos[i]," ",max(fechas.p)-1)
  
  gra1 = datos1.p %>%    
    ggplot(aes(x = FECHA_RESULTADO, y = n)) +
    geom_bar(stat = "identity", colour = "royalblue1", fill = "royalblue1") +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    geom_ma(aes(color = "Media móvil 7 días"),ma_fun = SMA, n = 7, linetype = 1, size = 1.25) +
    #scale_fill_manual(values = c("lightpink2","lightsteelblue2")) +
    ggtitle(tit1) +
    labs(y = "Número de personas", 
         x = "Fecha",
         caption = "Fuente: MINSA, Elaboración: Jesús Gamboa @jesuseduardog")+
    scale_x_date(limits = c(as.Date('2020-03-06'), max(datos.abiertos.p$FECHA_RESULTADO+1)),
                 expand = c(0,0),
                 breaks = function(x) seq.Date(from = as.Date('2020-03-06'), to = max(x), by = "30 days"))+ 
    scale_color_manual(name = '', 
                       guide = 'legend',
                       values = c('Media móvil 7 días' = 'darkblue'))+
    tema1
  
  
  tit2 = paste0("Casos acumulados - ",distritos[i]," ",max(fechas.p)-1)
  
  gra2 = datos1.p %>%    ggplot(aes(x = FECHA_RESULTADO, y = nacum)) +
    geom_bar(stat = "identity", colour = "royalblue1", fill = "royalblue3") +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    #scale_fill_manual(values = c("lightpink2","lightsteelblue2")) +
    geom_ma(aes(color = "Media móvil 7 días"),ma_fun = SMA, n = 7, linetype = 1, size = 1.25) +
    ggtitle(tit2) +
    labs(y = "Número de personas", 
         x = "Fecha",
         caption = "Fuente: MINSA, Elaboración: Jesús Gamboa @jesuseduardog")+
    scale_x_date(limits = c( as.Date('2020-03-06'), max(datos.abiertos.p$FECHA_RESULTADO+1)),
                 expand = c(0,0),
                 breaks = function(x) seq.Date(from = as.Date('2020-03-06'), to = max(x), by = "30 days"))+ 
    scale_color_manual(name = '', 
                       guide = 'legend',
                       values = c('Media móvil 7 días' = 'darkblue')) +
    tema1
  
  
  
  datos1.f = datos.abiertos.f %>% 
    filter(DEPARTAMENTO == "LIMA" & PROVINCIA == "LIMA"| DEPARTAMENTO =="CALLAO") %>% 
    filter(!DISTRITO %in% c("MORROPON","PUEBLO NUEVO","","EN INVESTIGACIÓN")) %>%  
    filter(DISTRITO == distritos[i]) %>% 
    group_by(FECHA_FALLECIMIENTO) %>% 
    dplyr::count() %>% 
    right_join(data.frame(fechas.f),by=c("FECHA_FALLECIMIENTO"="fechas.f")) %>% 
    dplyr::mutate(n=replace_na(n,0)) %>% 
    arrange(FECHA_FALLECIMIENTO)
  
  datos1.f = cbind(datos1.f, nacum = cumsum(datos1.f$n))
  
  tit3 = paste0("Fallecimientos diarios - ",distritos[i]," ",max(fechas.f)-1)
  
  gra3 = datos1.f %>%    
    ggplot(aes(x = FECHA_FALLECIMIENTO, y = n)) +
    geom_bar(stat = "identity", colour = "darkorange1", fill = "darkorange1") +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    geom_ma(aes(color = "Media móvil 7 días"),ma_fun = SMA, n = 7, linetype = 1, size = 1.25) +
    ggtitle(tit3) +
    labs(y = "Número de personas fallecidas", 
         x = "Fecha",
         caption = "Fuente: MINSA, Elaboración: Jesús Gamboa @jesuseduardog")+
    scale_x_date(limits = c( as.Date('2020-03-06'), max(datos.abiertos.f$FECHA_FALLECIMIENTO+1)),
                 expand = c(0,0),
                 breaks = function(x) seq.Date(from = as.Date('2020-03-06'), to = max(x), by = "30 days"))+ 
    scale_color_manual(name = '', 
                       guide = 'legend',
                       values = c('Media móvil 7 días' = 'coral4')) + 
    tema1
  
  tit4 = paste0("Fallecimientos acumulados - ",distritos[i]," ",max(fechas.f)-1)
  
  gra4 = datos1.f %>%  
    ggplot(aes(x = FECHA_FALLECIMIENTO, y = nacum)) +
    geom_bar(stat = "identity", colour = "darkorange1", fill = "darkorange1") +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    geom_ma(aes(color = "Media móvil 7 días"),ma_fun = SMA, n = 7, linetype = 1, size = 1.25) +
    ggtitle(tit4) +
    labs(y = "Número de personas fallecidas", 
         x = "Fecha",
         caption = "Fuente: MINSA, Elaboración: Jesús Gamboa @jesuseduardog")+
    scale_x_date(limits = c( as.Date('2020-03-06'), max(datos.abiertos.f$FECHA_FALLECIMIENTO+1)),
                 expand = c(0,0),
                 breaks = function(x) seq.Date(from = as.Date('2020-03-06'), to = max(x), by = "30 days"))+ 
    scale_color_manual(name = '', 
                       guide = 'legend',
                       values = c('Media móvil 7 días' = 'coral4')) +
    tema1
  
  
  gra5 = grid.arrange(gra1,gra2,gra3,gra4,ncol=2)
  
  nombre = paste0(distritos[i],".png")
  png(filename=nombre, width=4500,height=4500,units="px",
      pointsize=12,bg="white",res=400)
  plot(gra5)
  dev.off()
}

