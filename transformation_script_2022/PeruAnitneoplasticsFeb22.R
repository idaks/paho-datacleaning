#@begin PeruProcessing
#@in Prices_2019_2021_File @URI file:Prices_2019-2021.xlsx
#@in Peru_Scrapped_Files @URI file:Scrappers/Perú/*
#@in Taxonomy_File @URI file:Taxonomy.xlsx
#@out PeruAntineoPlasticsClean_File @URI file:PeruAntineoPlasticsClean.csv

library(dplyr)
library(XLConnect)
library(readxl)
library(stringdist)
library(stringr)
#FILTRAR 19-20-21 (Del identificador)
#Agregar conversión de sol a usd por año

#hacer busqueda con items desiertos
#popular proveedor <- Razon Social
#Region<-en donde dejaron las compras
#Hacer columna<- Institucion Compradora (hasta encontrar mejor nombre)


#******************



#SELECT the relevant years
#READ THE PURCHASE AND THE CURRENCY CONVERSION FILES
anos<-c(2019,2020,2021)
files<-list.files(path="~/RStudio/PAHO/Scrappers/Perú")
toberead<-files[grep("antineoplastics",files)]
SolaUSD<-3.77 #Precio Promedio anual
#@begin read_prices
#@in Prices_2019_2021_File @URI file:Prices_2019-2021.xlsx
#@out Precios
Precios<- read_excel("~/RStudio/PAHO/Prices 2019-2021.xlsx")
#@end read_prices

Precios<-Precios[,c(4,8)]
#@begin read_perumed
#@in Peru_Scrapped_Files @URI file:Scrappers/Perú/*
#@out PeruMed
PeruMed<- read_excel(paste0("~/RStudio/PAHO/Scrappers/Perú/",toberead[1]), skip=1)
for(i in seq(2, length(toberead))){
  temp<-read_excel(paste0("~/RStudio/PAHO/Scrappers/Perú/",toberead[i]), skip=1)
  PeruMed<-rbind(PeruMed,temp)
}
#@end read_perumed

#READ THE CATALOG AND FILTER THE THERAPEUTIC AREA
#@begin read_catalog
#@in Taxonomy_File @URI file:Taxonomy.xlsx
#@out Catalog
Catalog<-read_excel("~/RStudio/PAHO/Scrappers/Taxonomy.xlsx", sheet = "SF Products")
#@end read_catalog

#@begin filter_datos
#@in Catalog
#@out DatosFinal
toconsider<-Catalog[which(Catalog$`Therapeutic Area`=="Antineoplastics"),]
DatosFinal<-as.data.frame(matrix(nrow=nrow(toconsider),ncol=8))
names(DatosFinal)<-c("Nombre","Concentracion","Presentacion","Cuarto","Quinto","Taxonomy","CatalogName","ConcentracionNum")

for(i in seq(1, nrow(toconsider))){
  primer<-unlist(str_split(toconsider$`Product Name`[i], ","))[1]
  DatosFinal$Nombre[i]<-unlist(str_split(primer," " ))[1]
  #largoprimer<-length(unlist(strsplit(primer," ")))
  DatosFinal$Concentracion[i]<-paste(unlist(str_split(primer," " ))[2],unlist(str_split(primer," " ))[3])
  if (startsWith(DatosFinal$Concentracion[i],"(")){
    stringi<-gsub("\\([^()]*\\)", "", primer)
    #DatosFinal$Nombre[i]<-paste(unlist(str_split(primer," " ))[1],unlist(str_split(primer," " ))[2])
    DatosFinal$Concentracion[i]<-paste(unlist(str_split(stringi," " ))[3],unlist(str_split(stringi," " ))[4])
  }
  DatosFinal$Presentacion[i]<-unlist(str_split(toconsider$`Product Name`[i], ","))[2]
  DatosFinal$Cuarto[i]<-unlist(str_split(toconsider$`Product Name`[i], ","))[3]
  DatosFinal$Quinto[i]<-unlist(str_split(toconsider$`Product Name`[i], ","))[4]
  DatosFinal$Taxonomy[i]<-toconsider$Taxonomy[i]
  DatosFinal$CatalogName[i]<-toconsider$`Product Name`[i]
  DatosFinal$ConcentracionNum[i]<-unlist(str_split(DatosFinal$Concentracion[i]," "))[1]
}
#@end read_catalog


#@begin filter_perumed
#@in PeruMed
#@in DatosFinal
#@out PeruMed.1
PeruMed$NomCatalog<-NA
PeruMed$Dist<-NA
PeruMed$NomProducto<-NA
PeruMed$NomConcentracion<-NA
PeruMed$Ano<-NA
PeruMed$Metodos<-NA

for(i in seq(1,nrow(PeruMed))){
  PeruMed$NomProducto[i]<-unlist(str_split(PeruMed$Producto[i]," "))[1]
  #PeruMed$NomConcentracion[i]<-paste(unlist(str_split(PeruMed$Producto[i]," "))[2],unlist(str_split(PeruMed$Producto[i]," "))[3])
  PeruMed$NomConcentracion[i]<-str_extract(unlist(str_split(PeruMed$Producto[i]," "))[2],"[[:digit:]]+\\.*[[:digit:]]*")
  PeruMed$NomCatalog[i]<-DatosFinal$Nombre[which.min(stringdist(PeruMed$NomProducto[i],DatosFinal$Nombre))]
  PeruMed$Dist[i]<-stringdist(PeruMed$NomProducto[i],PeruMed$NomCatalog[i])
  PeruMed$Ano[i]<-unlist(str_split(PeruMed$`Identificación Proceso`[i]," "))[6]
  PeruMed$Metodos[i]<-unlist(str_split(PeruMed$`Identificación Proceso`[i]," "))[1]
  if(PeruMed$Dist[i]>3){
    Empiezan<-DatosFinal$Nombre[startsWith(DatosFinal$Nombre,str_sub(PeruMed$NomProducto[i],1,1))]
    PeruMed$NomCatalog[i]<-Empiezan[which.min(stringdist(PeruMed$NomProducto[i],Empiezan))]
    PeruMed$Dist[i]<-stringdist(PeruMed$NomProducto[i],PeruMed$NomCatalog[i])
  }
  #if(startsWith(PeruMed$NomConcentracion[i],"(")){
  if(is.na(PeruMed$NomConcentracion[i])){
    stringi<-gsub("\\([^()]*\\)", "", PeruMed$Producto[i])
    #PeruMed$NomConcentracion[i]<-paste(unlist(str_split(stringi," "))[3],unlist(str_split(stringi," "))[4])
    PeruMed$NomConcentracion[i]<-str_extract(unlist(str_split(stringi," "))[3],"[[:digit:]]+\\.*[[:digit:]]*")
  }
}
test<-PeruMed%>%
  distinct(Producto, .keep_all = TRUE)%>%
  dplyr::select(Producto,NomProducto, NomConcentracion)
  
PeruMed$Metodos[PeruMed$Metodos=="SIE"]<-"Subasta Inversa Electrónica"
PeruMed$Metodos[PeruMed$Metodos=="LP"]<-"Licitación Pública"
PeruMed$Metodos[PeruMed$Metodos=="CD"]<-"Compra Directa"
PeruMed$Ano[grep("2020",PeruMed$`Identificación Proceso`)]<-2020
PeruMed$Ano[grep("2021",PeruMed$`Identificación Proceso`)]<-2021
PeruMed$Ano[grep("2019",PeruMed$`Identificación Proceso`)]<-2019

PeruMed$ConcetracionNum<-gsub("[^0-9.-]", "", PeruMed$NomConcentracion)
PeruMed$Taxonomy<-NA
PeruMed$CatalogEntry<-NA
PeruMed$ConcetracionNum[which(PeruMed$NomCatalog=="TRASTUZUMAB"& PeruMed$ConcetracionNum=="120")]<-"150"
PeruMed$ConcetracionNum[which(PeruMed$NomCatalog=="TRASTUZUMAB"& PeruMed$ConcetracionNum=="440")]<-"420"

for(i in seq(1,nrow(PeruMed))){
  if(!is.na(PeruMed$NomCatalog[i])){
    if(sum(grepl(PeruMed$NomCatalog[i],DatosFinal$Nombre) & (PeruMed$ConcetracionNum[i]==DatosFinal$ConcentracionNum))>0){
      PeruMed$Taxonomy[i]<-toconsider$Taxonomy[grepl(PeruMed$NomCatalog[i],DatosFinal$Nombre) & 
                                               (PeruMed$ConcetracionNum[i]==DatosFinal$ConcentracionNum)]
      PeruMed$CatalogEntry[i]<-toconsider$`Product Name`[grepl(PeruMed$NomCatalog[i],DatosFinal$Nombre) & 
                                                         (PeruMed$ConcetracionNum[i]==DatosFinal$ConcentracionNum)]
    }
  }
}
#@end filter_perumed

#@begin merge_perumed_precios
#@in PeruMed.1
#@in Precios
#@out PeruMed.2

#PeruMed2<-PeruMed[c(1:4),c(23,14)]
Precios2<-Precios%>%
  distinct(`Product Name`, .keep_all = TRUE)
Precios2<-Precios2[!is.na(Precios2$`Product Name`),]
duplic<-Precios[duplicated(Precios$`Product Name`),1]

PeruMed<-left_join(PeruMed,Precios2, by=c("CatalogEntry"="Product Name"))

Test<-PeruMed%>%
  distinct(Producto, .keep_all = TRUE)%>%
  dplyr::select(Producto, CatalogEntry)
  
#@end merge_perumed_precios


#@begin create_final_data
#@in PeruMed.2
#@out FinalData

#write.csv(PeruMed,"~/RStudio/PAHO/Scrappers/Perú/PeruClean.csv")
namescols<-c("Country Code", "Entity", "Entity Type","Punto de Entrega", "Mecanismo de Compra", "Programa","Region", "Therapeutic Area", 
         "Generic Product Name","Presentación","Genérico","Subunits per Unit","Catalog Name","Supplier","Manufacturer",
         "Purchase Date","Purchase Year","Total Amount","Unit Quantity","Min Unit Price USD", "PAHO Min Unit Price USD",
         "Patent","Observaciones")
FinalData<- as.data.frame(matrix(nrow=nrow(PeruMed),ncol=length(namescols)))
names(FinalData)<-namescols
FinalData$`Country Code`<-"PER"
FinalData$Entity<-paste(PeruMed$Entidad ,PeruMed$Establecimiento,sep="-")
FinalData$`Therapeutic Area`<-"Antineoplastics"
FinalData$`Catalog Name`<-PeruMed$CatalogEntry
FinalData$`Total Amount`<-PeruMed$`Monto Total Ofertado`/SolaUSD
FinalData$`Unit Quantity`<-PeruMed$Cantidad
FinalData$`Min Unit Price USD`<-PeruMed$`Precio Unitario Ofertado`/SolaUSD
FinalData$`PAHO Min Unit Price USD`<-PeruMed$`Min Unit Cost`
FinalData$`Subunits per Unit`<-1
FinalData$`Purchase Year`<-PeruMed$Ano
FinalData$Supplier<-PeruMed$RazonSocial
FinalData$`Generic Product Name`<-PeruMed$Producto
FinalData$`Punto de Entrega`<-PeruMed$`Punto de Entrega`
FinalData$`Mecanismo de Compra`<-PeruMed$Metodos
FinalData<-FinalData%>%
  filter(`Purchase Year` %in% anos)
#@end create_final_data

#@begin write_final_data
#@in FinalData
#@out PeruAntineoPlasticsClean_File @URI file:PeruAntineoPlasticsClean.csv
write.csv(FinalData,"~/RStudio/PAHO/Scrappers/Perú/PeruAntineoPlasticsClean.csv")
#@end write_final_data

#@end PeruProcessing
