library(readr)
library(tidyverse)
library(highcharter)
library(ggplot2)

df<- read_delim("tabla_completa.csv",
                ",", escape_double = FALSE, trim_ws = TRUE)

read.table(text=df$CLIENTE, sep="|", fill=TRUE, header=FALSE)



splitdf<-cSplit(df, "CLIENTE", "|")

view(splitdf)

### ver dataset
Tabla<- (splitdf %>% 
           select(-12,-13,-15,-16,-17))

view(Tabla)
###cambiar nombre de columna
names(Tabla)[11]<- "CLIENTE"
names(Tabla)[12]<- "MOTIVO"
names(Tabla)

### dplyr
glimpse(Tabla)

### convertir columnas a factores
df <-mutate_if(Tabla, is.character, as.factor)
df$CLIENTE <- iconv(Tabla$CLIENTE, to = "UTF-8")
df$PILOTO <- iconv(Tabla$PILOTO, to = "UTF-8")
df$UNIDAD <- iconv(Tabla$UNIDAD, to = "UTF-8")
#glimpse(df)


### Numero de Pilotos

Tabla %>% 
  group_by(PILOTO) %>% 
  summarise(Pilotos_unicos=n_distinct(PILOTO))

### Cuantos viajes hay por mes
Viajes_mes <- (Tabla %>% 
  group_by(MES) %>%
  summarise(viajes=n()))

#hist(Viajes_mes$viajes, main = "Viajes por mes", xlab = "Mes", ylab = "Numero de viejes")

### Viajes por unidad
viajes_unidad <- (Tabla %>% 
                    group_by(UNIDAD) %>% 
                    summarise(viajes_u=n()))
view(viajes_unidad)

### Viajes por piloto
vp <- (Tabla %>% 
                    group_by(PILOTO) %>% 
                    summarise(viajes_p=n()))

view(vp)
### Viajes por piloto
viajes_piloto <- (Tabla %>% 
                    group_by(MES,PILOTO) %>% 
                    summarise(viajes_p=n()))

view(viajes_piloto)

### Clientes
viajes_cliente <- (Tabla %>% 
                    group_by(MES, CLIENTE) %>%
                    summarise(viajes_p=n()))

view(viajes_cliente)

### Cantidad por cliente
Cant_Cliente<-(Tabla %>%
                 group_by(CLIENTE) %>%
                 summarise_at(vars(CANTIDAD),
                              list(CANTIDAD = sum)))
  
Cant_Cliente$ACUMULADO<-cumsum(Cant_Cliente$CANTIDAD)

view(Cant_Cliente)


ggplot(Cant_Cliente, aes(x=Cant_Cliente$CLIENTE)) +
  geom_bar(aes(y=Cant_Cliente$CANTIDAD), fill='blue', stat="identity") +
  geom_point(aes(y=Cant_Cliente$ACUMULADO)) +
  geom_path(aes(y=Cant_Cliente$ACUMULADO, group=1)) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
  labs(title = "Pareto Plot", subtitle = "Clientes", x = 'CLIENTES', y =
         'CANTIDAD')

