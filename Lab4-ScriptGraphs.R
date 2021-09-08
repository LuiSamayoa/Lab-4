library(readr)
library(tidyverse)
library(highcharter)
library(splitstackshape)
library(dplyr)
library(ggplot2)
library(formattable)
library(qcc)
library(hrbrthemes)

df<- read_delim("tabla_completa.csv",
                ",", escape_double = FALSE, trim_ws = TRUE)

view(df)

df$CLIENTE <- str_replace(df$CLIENTE, '/', '|||')

splitdf <- cSplit(df, 'CLIENTE', '|')

view(splitdf)

### ver dataset
Tabla<- (splitdf %>% 
           select(-12,-13,-15,-16,))

view(Tabla)

###cambiar nombre de columna
names(Tabla)[11]<- "CLIENTE"
names(Tabla)[12]<- "MOTIVO"
names(Tabla)[13]<- "MOTIVO2"

### dplyr
glimpse(Tabla)

### convertir columnas a factores
df <-mutate_if(Tabla, is.character, as.factor)
df$CLIENTE <- iconv(Tabla$CLIENTE, to = "UTF-8")
df$PILOTO <- iconv(Tabla$PILOTO, to = "UTF-8")
df$UNIDAD <- iconv(Tabla$UNIDAD, to = "UTF-8")
#glimpse(df)

Tabla <-mutate_if(Tabla, is.character, as.factor)
Tabla$CLIENTE <- iconv(Tabla$CLIENTE, to = "UTF-8")
Tabla$PILOTO <- iconv(Tabla$PILOTO, to = "UTF-8")
Tabla$UNIDAD <- iconv(Tabla$UNIDAD, to = "UTF-8")
#glimpse(df)

Tabla$MOTIVO<-Tabla$MOTIVO %>%
  replace_na('Despacho a cliente')
Tabla$MOTIVO <- str_replace(Tabla$MOTIVO, 'FALTANTE', 'Faltante')

### Numero de Pilotos

Tabla %>% 
  group_by(PILOTO) %>% 
  summarise(Pilotos_unicos=n_distinct(PILOTO))

### Cuantos viajes hay por mes
Viajes_mes <- (Tabla %>% 
                 group_by(MES) %>%
                 summarise(viajes=n()))


Ingreso_mes <- (Tabla %>% 
                  group_by(MES) %>%
                  filter(MOTIVO != "DEVOLUCION") %>% 
                  summarise_at(vars(Q),
                               list(Q = sum)))
#Ingreso por mes plot
Ingreso_mes %>%
  tail(11) %>%
  ggplot( aes(x=MES, y=Q)) +
  geom_line( color="black") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  theme_ipsum() +
  ggtitle("Ingresos Mensuales")



### Viajes por unidad
viajes_unidad <- (Tabla %>% 
                    group_by(UNIDAD) %>% 
                    summarise(viajes_u=n()))
view(viajes_unidad)

### Viajes por piloto
viajep <- (Tabla %>% 
         group_by(MES, PILOTO) %>% 
         summarise(viajes_p=n()))

ggplot(viajep, aes( y=viajep$PILOTO, x=viajep$viajes_p)+
         geom_bar(fill="#f68060",stat="identity")+
         xlab("Numero de Viajes")+
         ylab("Piloto")+
         ggtitle("Viajes por Piloto")+
         theme(axis.text.x = element_text(angle = 90)))



view(vp)
### Viajes por piloto
viajes_piloto <- (Tabla %>% 
                    group_by(PILOTO) %>% 
                    summarise(viajes_p=n()) %>%
                    arrange(-viajes_p))

view(viajes_piloto)

ggplot(viajes_piloto, aes( y=viajes_piloto$PILOTO, x=viajes_piloto$viajes_p)+
  geom_bar(fill="#f68060",stat="identity")+
  xlab("Numero de Viajes")+
  ylab("Piloto")+
  ggtitle("Viajes por Piloto")+
  theme(axis.text.x = element_text(angle = 90)))
  


### Clientes
viajes_cliente <- (Tabla %>% 
                     group_by(MES, CLIENTE) %>%
                     summarise(viajes_p=n()))

view(viajes_cliente)

### Ingreso
Ingreso_cliente<-(Tabla %>%
                    group_by(CLIENTE) %>%
                    filter(MOTIVO != "DEVOLUCION") %>% 
                    summarise_at(vars(Q),
                                 list(Q = sum)) %>%
                    arrange(-Q))
Ingreso_cliente$ACUMULADO<-cumsum(Ingreso_cliente$Q)

Ingreso_cliente$Porcen_ingreso <- (Ingreso_cliente$ACUMULADO/max(Ingreso_cliente$ACUMULADO))

Ingreso_cliente$Porcen_ingreso <- formattable::percent(Ingreso_cliente$Porcen_ingreso, 2)

Ingreso_cliente$CLIENTE <- str_sub(Ingreso_cliente$CLIENTE, 1, 9)

view(Ingreso_cliente)

ggplot(Ingreso_cliente, aes(y=Ingreso_cliente$Q, x=Ingreso_cliente$CLIENTE)) + 
  geom_bar(stat="identity", fill="#42A7E3")+
  xlab("Cliente")+
  ylab("Ingreso")+
  ggtitle("Ingreso por Cliente")+
  theme(axis.text.x = element_text(angle = 90))

Clientes<- (Tabla %>% 
  group_by(CLIENTE) %>%
  summarise(clt=n()))

view(Clientes)

##Grafica Pareto
ggplot(Ingreso_cliente)+
  geom_bar(aes(x=reorder(Ingreso_cliente$CLIENTE, desc(Q)),y=Q), fill="#70C594", stat="identity") +
  geom_line(aes(x=Ingreso_cliente$CLIENTE,y=Ingreso_cliente$Porcen_ingreso*max(Ingreso_cliente$Q)), stat ="identity") +
  geom_text(aes(label=Ingreso_cliente$Porcen_ingreso, x=Ingreso_cliente$CLIENTE,y=Ingreso_cliente$Porcen_ingreso*max(Ingreso_cliente$Q)), colour="#4285E3") +
  geom_text(aes(label=Ingreso_cliente$Q, x=Ingreso_cliente$CLIENTE,y=0.7*Ingreso_cliente$Q), colour="black") +
  theme(axis.text.x = element_text(angle = 90))+
  xlab("Cliente")+
  scale_y_continuous(sec.axis = sec_axis(~./max(Ingreso_cliente$Q)))+ 
  ggtitle("Pareto Plot")
  



### Tipo de Viaje

Tipo_viaje <- (Tabla %>%
                 group_by(CLIENTE, MOTIVO) %>% 
                 summarise(viaje=n()))
Tipo_viaje$CLIENTE <- str_sub(Tipo_viaje$CLIENTE, 1, 9)

view(Tipo_viaje)

#tipo de viaje por cliente plot

ggplot(Tipo_viaje, aes(fill= MOTIVO, y=Tipo_viaje$viaje, x=Tipo_viaje$CLIENTE)) + 
  geom_bar(stat="identity")+
  xlab("Cliente")+
  ylab("Numero de viajes")+
  ggtitle("Tipo de Viaje por Cliente")+
  theme(axis.text.x = element_text(angle = 90))

### Tipo de Viaje

Mot_uni <- (Tabla %>%
              group_by(UNIDAD, MOTIVO) %>% 
              summarise(motun=n()))

## Tipo de viaje por unidad
ggplot(Mot_uni, aes(fill=MOTIVO, y=Mot_uni$motun, x=Mot_uni$UNIDAD)) + 
  geom_bar(position="stack", stat="identity")+
  xlab("Unidad")+
  ylab("Numero de viajes")+
  ggtitle("Tipo de viaje por Unidad")


#Credito Por cliente

Credito_cliente <- (Tabla %>%
                      group_by(CLIENTE)  %>% 
                 summarise(credi=sum(CREDITO)))
Credito_cliente$CLIENTE <- str_sub(Credito_cliente$CLIENTE, 1, 9)

view(Credito_cliente)

ggplot(Credito_cliente, aes(y=credi, x=CLIENTE)) + 
  geom_bar(position="stack", stat="identity", fill="#8DC570")+
  xlab("Cliente")+
  ylab("total credito")+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Credito por cliente")


## Creditos Mensuales

Credito_mes <- (Tabla %>%
                  group_by(MES)  %>% 
                  summarise(credimes=sum(CREDITO)))


ggplot(Credito_mes, aes(y=credimes, x=MES)) + 
  geom_line( )+
  xlab("Mes")+
  ylab("Credito Otorgado")+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Credito Mensual")



