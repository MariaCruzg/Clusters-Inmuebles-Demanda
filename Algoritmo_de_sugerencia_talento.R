
library(odbc)

con <- dbConnect(odbc(),
                 Driver = "SQLServer",
                 Server = "54.245.36.29",
                 Database = "ocesa_dwh",
                 UID = "ocesa_da",
                 PWD = "P4ssw0rd3",
                 Port = 1433)
library(glue)
library(sqldf)
library(dplyr)
library(ggplot2)
library(RWeka)
library(zoo)


x1<-dbListTables(con)

Fac_audreps <- tbl(con,"fact_audreps") #dia a dia
#Fac_completa<-tbl(con,"fact_ocerep_ut") #acumulados 
con %>% tbl("fact_audreps")

completa1z<-tbl(con,"fact_completa")
#completa , transacciones, fecha de transacción,  #pep 

dim_precio<-tbl(con,"dim_nivel_precio")
dim_evento<-tbl(con,"dim_evento")
dim_evento_cat<-tbl(con,"dim_evento_cat")
dim_metodo<-tbl(con,"dim_metodo_pago") 
dim_inmueble<-tbl(con,"dim_inmueble")
dim_inmueble_desz<-tbl(con,"dim_inmueble_des")
dim_evento_des<-tbl(con,"dim_evento_des")
dim_registo<-tbl(con,"dim_registro_tipo")
#ljsp <- left_join(completa1z, dim_evento)
 ljsp <- left_join(Fac_audreps, dim_evento)

com3<-left_join(ljsp,dim_metodo)
boleto_tipo<-tbl(con,"dim_boleto_tipo")

com4<-left_join(com3,boleto_tipo)
dim_zona<-tbl(con,"dim_zona")
dim_evento_propiedad<-tbl(con,"dim_evento_propiedad")

dim_canal<-tbl(con,"dim_canal")
#evento<-inner_join(dim_evento_cat,dim_evento)
com5<-left_join(com4,dim_inmueble_desz)
com6<-left_join(com5,dim_evento_cat)
com7<-left_join(com6,dim_canal)
dim_acredor<-tbl(con,"dim_acreedor")

dim_origen_outlet<-tbl(con,"dim_origen_outlet")
com8<-left_join(com7,dim_origen_outlet)



com9<-left_join(com8,dim_evento_propiedad)
com10<-left_join(com9,dim_evento_des)
com11<-left_join(com10,dim_evento)
#dim_evento_propiedad<-tbl(con,"dim_evento_propiedad")
dim_canal_suz<-tbl(con,"dim_canal_sub")
#com12<-left_join(com11,dim_evento_propiedad)
#completa<-inner_join(completa1z,com9)
com12<-left_join(com11,dim_canal_suz)

com13<-left_join(com12,dim_zona)

com14<-left_join(com12,dim_registo)
a<- com14 %>% select(registro_tipo_des,canal_sub_des,canal_des,zona_pk,canal_sub_pk,evento_cve_pk,evento_des_pk,evento_cat_ind,evento_des_ind, evento_cat_sub_ind,inmueble_pk,evento_fecha,boleto_tipo_cve,registro_tipo_pk,transaccion_diaria_importe_comision_outlet,transaccion_diaria_importe_costo_tc,transaccion_diaria_importe_costo_boleto,transaccion_diaria_importe_cargo_interno,transaccion_diaria_importe_ingreso_tc,inmueble_des_pk,transaccion_diaria_importe_total,transaccion_diaria_fecha,boleto_tipo_des,evento_pk,transaccion_diaria_importe_ingreso_tc,transaccion_diaria_importe_bruto,transaccion_diaria_cantidad_devolucion,evento_cat_sub,registro_tipo_pk,inmueble_des, evento_cat, origen_outlet_des,evento_cat_pk,evento_fecha,boleto_tipo_pk,transaccion_diaria_cantidad_devolucion,inmueble_des_pk,inmueble_des,transaccion_diaria_cantidad_bruto,evento_cat_ind,nivel_precio_pk,transaccion_diaria_importe_neto,canal_des,transaccion_diaria_cantidad_neto)
a2<- a%>% group_by(zona_pk,evento_fecha,evento_des_ind,evento_cat_ind,evento_des_pk,inmueble_des,canal_sub_pk,canal_des, canal_sub_des) %>% summarise(Boletos=sum(transaccion_diaria_cantidad_neto),Boleto.Tran.com_outlet=sum(transaccion_diaria_importe_comision_outlet),Trans.importe_total=sum(transaccion_diaria_importe_neto)) 

################################################## 
a22<- a2%>% group_by(evento_fecha,evento_des_ind,inmueble_des,canal_des) %>% summarise(Boletos_Talento=sum(Boletos),Importe=sum(Trans.importe_total)) 



a22<-as.data.frame(a22)




###### WORLD CLOUND 
 teatroMetr<- subset( a22, inmueble_des =="TEATRO METROPOLITAN")
 library("tm")
 library("SnowballC")
 library("wordcloud")
 library("RColorBrewer")
 set.seed(1234)
 
 
 library(odbc)
 
 con <- dbConnect(odbc(),
                  Driver = "SQLServer",
                  Server = "54.245.36.29",
                  Database = "ocesa_dwh",
                  UID = "ocesa_da",
                  PWD = "P4ssw0rd",
                  Port = 1433)
 library(glue)
 library(sqldf)
 library(dplyr)
 library(ggplot2)
 library(RWeka)
 library(zoo)
 
 
 x1<-dbListTables(con)
 
 Fac_audreps <- tbl(con,"fact_audreps") #dia a dia
 #Fac_completa<-tbl(con,"fact_ocerep_ut") #acumulados 
 con %>% tbl("fact_audreps")
 
 completa1z<-tbl(con,"fact_completa")
 #completa , transacciones, fecha de transacción,  #pep 
 
 dim_precio<-tbl(con,"dim_nivel_precio")
 dim_evento<-tbl(con,"dim_evento")
 dim_evento_cat<-tbl(con,"dim_evento_cat")
 dim_metodo<-tbl(con,"dim_metodo_pago") 
 dim_inmueble<-tbl(con,"dim_inmueble")
 dim_inmueble_desz<-tbl(con,"dim_inmueble_des")
 dim_evento_des<-tbl(con,"dim_evento_des")
 ljsp <- left_join(completa1z, dim_evento)
 com3<-left_join(ljsp,dim_metodo)
 boleto_tipo<-tbl(con,"dim_boleto_tipo")
 
 com4<-left_join(com3,boleto_tipo)
 dim_zona<-tbl(con,"dim_zona")
 dim_evento_propiedad<-tbl(con,"dim_evento_propiedad")
 
 dim_canal<-tbl(con,"dim_canal")
 #evento<-inner_join(dim_evento_cat,dim_evento)
 com5<-left_join(com4,dim_inmueble_desz)
 com6<-left_join(com5,dim_evento_cat)
 com7<-left_join(com6,dim_canal)
 dim_acredor<-tbl(con,"dim_acreedor")
 
 dim_origen_outlet<-tbl(con,"dim_origen_outlet")
 com8<-left_join(com7,dim_origen_outlet)
 
 
 
 com9<-left_join(com8,dim_evento_propiedad)
 com10<-left_join(com9,dim_evento_des)
 com11<-left_join(com10,dim_evento)
 #dim_evento_propiedad<-tbl(con,"dim_evento_propiedad")
 dim_canal_suz<-tbl(con,"dim_canal_sub")
 #com12<-left_join(com11,dim_evento_propiedad)
 #completa<-inner_join(completa1z,com9)
 com12<-left_join(com11,dim_canal_suz)
 
 com13<-left_join(com12,dim_zona)
 a<- com12 %>% select(zona_pk,canal_sub_pk,evento_cve_pk,evento_des_pk,evento_cat_ind,evento_des_ind, evento_cat_sub_ind,inmueble_pk,evento_fecha,boleto_tipo_cve,registro_tipo_pk,transaccion_diaria_importe_comision_outlet,transaccion_diaria_importe_costo_tc,transaccion_diaria_importe_costo_boleto,transaccion_diaria_importe_cargo_interno,transaccion_diaria_importe_ingreso_tc,inmueble_des_pk,transaccion_diaria_importe_total,transaccion_diaria_fecha,boleto_tipo_des,evento_pk,transaccion_diaria_importe_ingreso_tc,transaccion_diaria_importe_bruto,transaccion_diaria_cantidad_devolucion,evento_cat_sub,registro_tipo_pk,inmueble_des, evento_cat, origen_outlet_des,evento_cat_pk,evento_fecha,boleto_tipo_pk,transaccion_diaria_cantidad_devolucion,inmueble_des_pk,inmueble_des,transaccion_diaria_cantidad_bruto,evento_cat_ind,nivel_precio_pk,transaccion_diaria_importe_neto,canal_des,transaccion_diaria_cantidad_neto)
 a2<- a%>% group_by(zona_pk,evento_fecha,evento_des_ind,evento_cat_ind,evento_des_pk,inmueble_des) %>% summarise(Boletos=sum(transaccion_diaria_cantidad_neto),Boleto.Tran.com_outlet=sum(transaccion_diaria_importe_comision_outlet),Trans.importe_total=sum(transaccion_diaria_importe_total)) 
 
 
 #### Lista de Talentos ##### 
 
 a3<- a%>%filter(evento_des_ind=="TIMBIRICHE")
 
 negativos<-a3 %>% filter(transaccion_diaria_cantidad_devolucion>1)
 curva<-NULL
 curva1<-NULL
 curva<-data.frame(a3)
 
 ########## plot Curva para dividir ####################### 
 curva1<-curva %>%group_by(evento_fecha,transaccion_diaria_fecha) %>% summarise(total_boletos=sum(transaccion_diaria_cantidad_neto))
 
 c<-hist(curva1$total_boletos,labels = TRUE)
 
 #### Negativos ######## 
 
 
 wordcloud(words = teatroMetr$evento_des_ind, freq = teatroMetr$Importe, min.freq = 1,
           max.words=200, random.order=FALSE, rot.per=0.35, 
           colors=brewer.pal(8, "Dark2"))

 
 set.seed(1234)
 wordcloud(words = teatroMetr$evento_des_ind, freq = teatroMetr$Boletos_Talento, min.freq = 1,
           max.words=200, random.order=FALSE, rot.per=0.35, 
           colors=brewer.pal(8, "Dark2"))
 
 

Canales<-as.data.frame(a22)

Canales$year <- as.numeric(format(as.Date(Canales$evento_fecha),"%Y"))

canales1<-a %>% group_by(registro_tipo_des,inmueble_des)%>% summarise(Totales=sum(Boletos_Talento))


############################################



a3<- a2 %>% filter(registro_tipo_pk == 3) %>% group_by(evento_cat_ind,canal_des,evento_fecha,registro_tipo_des_ind) 

a4<-as.data.frame(a2)
a4$mes <- as.numeric(format(as.Date(a4$evento_fecha),"%m"))
a4$year <- as.numeric(format(as.Date(a4$evento_fecha),"%Y"))
a4$MesAbre  <- months(as.Date(a4$evento_fecha), abbreviate=TRUE)
a4$month<- factor(format(a4$evento_fecha, format = "%b"), levels = month.abb)
a4$season <- character(length = NROW(a4))
## I'm sure these 4 steps can be simplified but just
## how escapes me at the moment
a4$season[a4$month %in% month.abb[c(12,1,2)]] <- "Winter"
a4$season[a4$month %in% month.abb[c(3,4,5)]] <- "Spring"
a4$season[a4$month %in% month.abb[c(6,7,8)]] <- "Summer"
a4$season[a4$month %in% month.abb[c(9,10,11)]] <- "Autumn"
a4$season <- factor(a4$season, 
                    levels = c("Spring","Summer","Autumn","Winter"))

#a4$year_tran <- as.numeric(format(as.Date(a4$transaccion_diaria_fecha),"%Y"))
a4<- a4 %>% filter(year>2013 &year<2018) 

a5<- a4%>% group_by(evento_des_ind) %>% summarise(Boletos_Talento=sum(Boletos),Total_talento=sum(Trans.importe_total)) 

#a6<- a4%>% group_by(evento_cat_pk) %>% summarise(Boletos_Talento=sum(Boletos),Total_talento=sum(Trans.importe_total)) 

a7<- a4%>% group_by(evento_cat_ind,evento_des_ind) %>% summarise(Boletos_Talento=sum(Boletos),Total_talento=sum(Trans.importe_total)) 

a8<- a4%>% group_by(evento_fecha,evento_des_ind) %>% summarise(Boletos_Talento=sum(Boletos),Total_talento=sum(Trans.importe_total)) 

a9<- a4%>% group_by(evento_des_ind,inmueble_des,zona_pk) %>% summarise(Boletos_Talento=sum(Boletos),Total_talento=sum(Trans.importe_total)) 

a9<-as.data.frame(a9)

a10<- a4%>% group_by(inmueble_des,year) %>% summarise(Boletos_Talento=sum(Boletos),Total_talento=sum(Trans.importe_total)) 

#a11<- a4%>% group_by(canal_des,inmueble_des) %>% summarise(Boletos_Talento=sum(Boletos),Total_talento=sum(Trans.importe_total)) 

a12<- a4%>% group_by(zona_pk,year) %>% summarise(Boletos_Talento=sum(Boletos),Total_talento=sum(Trans.importe_total)) 

### ALUVIAL ### 
#library(networkD3)


#library(alluvial)
#alluvial(a12[,1:4], freq=a12$Total_talento, 3
 #col = ifelse(a12$zona_pk == "1", "pink", "lightskyblue"),
         #col = ifelse(a12$zona_pk == "2", "red", "lightskyblue"),
         #border = ifelse(a12$year == "2017", "green", "gray"),
        #  hide = a12$Boletos_Talento == 0,
         #cex = 0.7
#)


a13<- a4%>% group_by(evento_cat_ind) %>% summarise(Boletos_Talento=sum(Boletos),Total_talento=sum(Trans.importe_total)) 


a222<- a%>% group_by(registro_tipo_des,evento_fecha) %>% summarise(Boletos_Talento=sum(transaccion_diaria_cantidad_neto)) 

Canales<-as.data.frame(a222)

Canales$year <- as.numeric(format(as.Date(Canales$evento_fecha),"%Y"))

canales1<-Canales %>% group_by(canal_sub_des,year)%>% summarise(Totales=sum(Boletos_Talento))
a14<- a4%>% group_by(inmueble_des,evento_des_ind,year) %>% summarise(Boletos_Talento=max(Boletos),Total_talento=max(Trans.importe_total)) 
#a14$year <- as.numeric(format(as.Date(a14$evento_fecha),"%Y"))

immueble_table<-as.data.frame(table(a14$inmueble_des))
cross_table<- count(a14,c("inmueble_des"))
a15<- a4%>% group_by(evento_des_ind,year,inmueble_des) %>% summarise(Boletos_Talento=max(Boletos),Total_talento=max(Trans.importe_total)) 


Exito_por_inmueble<- a4%>% group_by(inmueble_des) %>% summarise(Total_inmueble=quantile(Boletos,.70),Ganancia=quantile(Trans.importe_total,.70)/quantile(Boletos,.70)) 

Exito_por_inmuebl1<- a4%>% group_by(inmueble_des,year) %>% summarise(Total_inmueble=quantile(Boletos,.70),Ganancia=quantile(Trans.importe_total,.70)/quantile(Boletos,.70)) 

exito_by_year<-as.data.frame(Exito_por_inmuebl1)
compendio<-left_join(a4,Exito_por_inmueble)

a21<-compendio %>% group_by(zona_pk,evento_des_ind,inmueble_des,evento_fecha,evento_cat_ind,Total_inmueble,Ganancia) %>% summarise(BoletosEvento=sum(Boletos), BoletoPrecioMean=sum(Trans.importe_total)/sum(Boletos))

is.na(a21) <- do.call(cbind,lapply(a21, is.infinite))
#a16<- a4%>% group_by(evento_des_ind,year,season) %>% summarise(Boletos_Talento=mean(Boletos))

exito<-as.numeric(((a21$BoletosEvento- a21$Total_inmueble )/a21$Total_inmueble)*100)

a21$exito<-exito

is.na(a21) <- do.call(cbind,lapply(a21, is.infinite))
clasification<- as.data.frame(a21)
### limpiando de inf los datos ### 


iris<-data.frame(clasification$evento_cat_ind ,clasification$exito,clasification$BoletoPrecioMean,clasification$BoletosEvento,clasification$Total_inmueble)

newdata <- na.omit(iris)



irisCluster <- kmeans(newdata[, 2:5], 6, nstart = 5)
newdata$cluster <- as.factor(irisCluster$cluster)
ggplot(newdata, aes(clasification.Total_inmueble,clasification.BoletosEvento,  color =cluster)) + geom_point()

ggplot(newdata,aes(clasification.evento_cat_ind,clasification.Total_inmueble,color=cluster))+geom_point()

inmueble<-data.frame(a21$evento_cat_ind,a21$inmueble_des,a21$Ganancia,a21$Total_inmueble,a21$evento_fecha,exito,a21$zona_pk)

inmueble1<-inmueble %>% group_by(a21.evento_cat_ind,exito,a21.inmueble_des)%>%summarise(Boleto.mean=mean(a21.Ganancia))


inmueble2<-inmueble %>% group_by(a21.inmueble_des,a21.zona_pk)%>%summarise(Total.Inmueble=mean(a21.Total_inmueble), Precio.mean=mean(a21.Ganancia))

 
Entretenimiento<-data.frame(a21$evento_cat_ind,a21$evento_des_ind,a21$BoletosEvento,a21$BoletoPrecioMean,a21$evento_fecha,exito,a21$inmueble_des)

names(direcciones_inmuebles)<-c("1","lat","log","a21.inmueble_des")
Entretenimiento1<-left_join(Entretenimiento,direcciones_inmuebles )

a18 <- a4%>% count(inmueble_des,year, sort = TRUE)
#library(matchingR)

Entrenimiento_Conciertos<-subset(Entretenimiento, a21.evento_cat_ind=="CONCIERTOS")
Inmuebles_Conciertos<-subset(inmueble1, a21.evento_cat_ind=="CONCIERTOS")

eventos.frecuent<-as.data.frame(table(Entretenimiento$a21.evento_des_ind))

StudentData<-subset(eventos.frecuent, Freq>5)  


####  INMUEBLE #### 

# KNN # 
library(fastknn)
library(caTools)
library(class)
prc_train <- inmueble2[1:65,]
prc_test <- inmueble2[66:100,]

inmueble3<-as.data.frame(inmueble2)
imueble_zona_1<-subset(inmueble3,a21.zona_pk==1)

## Demo of the plot function

#svp <- ksvm(inmueble3$Total.Inmueble,inmueble3$Precio.mean,type="C-svc")
plot(svp,data=x)
#is.na(inmueble3) <- do.call(cbind,lapply(inmueble3, is.infinite))
inmueble3[is.na(inmueble3)] <- 0
inmuebleCluster <- kmeans(inmueble3[, 2:4], 10, nstart = 20)
inmueble3$cluster <- as.factor(inmuebleCluster$cluster)
 
ggplot(inmueble3,aes(inmueble3$Total.Inmueble,inmueble3$Precio.mean, color=inmueble3$cluster)) +geom_point()

#matching = galeShapley.marriageMarket(inmueble[,2:3],Entretenimiento[,2:3])
#entregar # var 
subset_test<-subset(Entretenimiento1,a21.evento_des_ind=="LUIS MIGUEL")

##########################################33MAPA #####################################3

################################ waffle ###### 

library(waffle)

# current verison
packageVersion("waffle")

#library(extrafont)
library(extrafont)

parts<-c(maximo=max(subset_test$a21.BoletosEvento), last=subset_test$a21.BoletosEvento(nrow(subset_test)))

waffle(
  parts/10, rows = 3, colors = c("#969696", "#1879bf", "#009bda"),
  use_glyph = "child", size = 8
)











library(leaflet)

puntos_inmuebles<- as.data.frame(table(subset_test$a21.inmueble_des))
names(puntos_inmuebles)  <- c("a21.inmueble_des","Frecuencia")
puntos_totales<-left_join(puntos_inmuebles,direcciones_inmuebles)
m = leaflet() %>% addTiles()
m  # a map with the default OSM tile layer

m = m %>% setView(puntos_totales$lat[1], puntos_totales$log[1], zoom = 10)
m

m %>% addPopups(puntos_totales$lat, puntos_totales$lon, puntos_totales$lugar)


subset_test[order(as.Date(subset_test$a21.evento_fecha, format="%d/%m/%Y")),]
library(forecast)
library(PerformanceAnalytics)
fit <- auto.arima(subset_test$a21.BoletosEvento)
puntos<-forecast(fit,h=4)
autoplot(forecast(fit,h=4))
boletos<-xts(subset_test$a21.BoletosEvento,order.by=as.Date(subset_test$a21.evento_fecha))
ggtsdisplay(boletos)

fit2<-auto.arima(boletos)
plot(forecast(fit2,h=4))
##3 COnvertir a xts 
library(xts)
Serie1<-xts(subset_test$exito,order.by=as.Date(subset_test$a21.evento_fecha))
VaR(Serie1, p=.95, method="historical",modified=TRUE)

data(managers)
chart.VaRSensitivity(Serie1,
                     methods=c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"),
                     colorset=bluefocus, lwd=2)



                     