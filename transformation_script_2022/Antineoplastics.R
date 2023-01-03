library(dplyr)
library(XLConnect)
library(readxl)
library(stringdist)
library(stringr)
library(readr)
library(lubridate)
#******************
#Read the purchase files, and the Brazilian Real to US dollar per month conversion

#@begin BRA_Antineoplastics_Transformation
#@in antineoplastics_files @URI file:~/RStudio/PAHO/Scrappers/Brasil/Antineoplasticos/*
#@in usd_clean @URI file:USD_BRLClean.csv
#@in catalog_file @URI file:~/RStudio/PAHO/CatalogwPreciosClean.csv 
#@out output_file

TerapeuticArea<-"Antineoplastics"

files<-list.files(path="~/RStudio/PAHO/Scrappers/Brasil/Antineoplasticos/")
toberead<-files[grep(".csv",files)]

#@begin read_usd_brlclean
#@in usd_clean @URI file:USD_BRLClean.csv
#@out Rea_USD
Rea_USD<-read_csv("~/RStudio/PAHO/Scrappers/Brasil/BRL-USD/USD_BRLClean.csv",col_types = cols(X1 = col_skip()) )
#@end read_usd_brlclean


#@begin read_data_file
#@desc read multiple input files in directory and agregate the files into a variable DataProv
#@in ~/RStudio/PAHO/Scrappers/Brasil/Antineoplasticos/* @as antineoplastics_files
#@out DataProv

DataProv<- read_csv(paste0("~/RStudio/PAHO/Scrappers/Brasil/Antineoplasticos/",toberead[1]),  col_types = cols(`Qtd Itens Comprados` = col_character()), skip = 2)
for(i in seq(2, length(toberead))){
  temp<-read_csv(paste0("~/RStudio/PAHO/Scrappers/Brasil/Antineoplasticos/",toberead[i]), col_types = cols(`Qtd Itens Comprados` = col_character()), skip = 2)
  DataProv<-rbind(DataProv,temp)
}

#@end read_data_file

rm(temp)

rm(temp)

#Catalog<-read_excel("~/RStudio/PAHO/Scrappers/Taxonomy.xlsx", sheet = "SF Products")

#Read the master catalog file, and filter for the Therapeutic Area 

#@begin read_catalog_file
#@desc read input catalog file, assign to variable Catalog
#@in ~/RStudio/PAHO/CatalogwPreciosClean.csv @as catalog_file
#@out Catalog
Catalog<- read_csv("~/RStudio/PAHO/CatalogwPreciosClean.csv",col_types = cols(X1 = col_skip()) )
#@end read_catalog_file

#@begin to_consider
#@desc also output catalog to consider which is a filter based on TerapeuticArea
#@in Catalog
#@param Antineoplastics @as TerapeuticArea
#@out toconsider
toconsider<-Catalog[which(Catalog$`Therapeutic Area`==TerapeuticArea),]
toconsider<-toconsider%>%
  distinct(`Product Name`, .keep_all = TRUE)
#@end to_consider


#SELECT THE RELEVANT COLUMNS and FILTER OUT ACCESORIES and NUTRITION entries

#@begin dataprov_filtering_1
#@desc filtering DataProv based on raw data columns and numberings/orders in the file
#@in DataProv
#@param c(2:4,6:10,12,14:22)
#@out DataProvFilter1
DataProv<-DataProv[,c(2:4,6:10,12,14:22)]
#@end dataprov_filtering_1


#@begin dataprov_filtering_2
#@desc additional filtering based onvalue on the column
#@param not(ACESSÓRIO|CANETA|NUTRIÇÃO)
#@in DataProvFilter1
#@out DataProvFilter2
DataProv<-DataProv[!grepl("ACESSÓRIO",DataProv$`Descrição CATMAT`),]
DataProv<-DataProv[!grepl("CANETA",DataProv$`Descrição CATMAT`),]
DataProv<-DataProv[!grepl("NUTRIÇÃO",DataProv$`Descrição CATMAT`),]
DataProv$`Descrição CATMAT`<-str_replace_all(DataProv$`Descrição CATMAT`,"0,","0.")
DataProv$`Descrição CATMAT`<-str_replace_all(DataProv$`Descrição CATMAT`,"1,","1.")
DataProv$`Descrição CATMAT`<-str_replace_all(DataProv$`Descrição CATMAT`,"2,","2.")
DataProv$`Descrição CATMAT`<-str_replace_all(DataProv$`Descrição CATMAT`,"3,","3.")
DataProv$`Descrição CATMAT`<-str_replace_all(DataProv$`Descrição CATMAT`,"4,","4.")
DataProv$`Descrição CATMAT`<-str_replace_all(DataProv$`Descrição CATMAT`,"5,","5.")
DataProv$`Descrição CATMAT`<-str_replace_all(DataProv$`Descrição CATMAT`,"6,","6.")
DataProv$`Descrição CATMAT`<-str_replace_all(DataProv$`Descrição CATMAT`,"7,","7.")
DataProv$`Descrição CATMAT`<-str_replace_all(DataProv$`Descrição CATMAT`,"8,","8.")
DataProv$`Descrição CATMAT`<-str_replace_all(DataProv$`Descrição CATMAT`,"9,","9.")
DataProv$NomCatalog<-NA
DataProv$Dist<-NA
DataProv$NomProducto<-NA
DataProv$NomConcentracion<-NA
DataProv$Ano<-NA
DataProv$Metodos<-NA
#@end dataprov_filtering_2


#SEPARATE SINGLE AND FIXED DOSE (MORE THAN ONE MOLECULE) Entries
#@begin separate_single_fixed_dose
#@desc SEPARATE SINGLE AND FIXED DOSE (MORE THAN ONE MOLECULE) Entries
#@in DataProvFilter2
#@out Single
#@out Falta
#@out FixedDose
Single<-DataProv[grepl("DOSAGEM", DataProv$`Descrição CATMAT`) | grepl("CONCENTRAÇÃO", DataProv$`Descrição CATMAT`),]
Single<-Single[!grepl("\\+",Single$`Descrição CATMAT`),]
Falta<-DataProv[!grepl("DOSAGEM", DataProv$`Descrição CATMAT`) & !grepl("CONCENTRAÇÃO", DataProv$`Descrição CATMAT`),]
FixedDose<-DataProv[grepl("\\+",DataProv$`Descrição CATMAT`) & !grepl("\\%",DataProv$`Descrição CATMAT`),]
#@end separate_single_fixed_dose

#CREATE NEW COLUMNS TO FIND EACH OF THE MOLECULES PRESENT ON THE ENTRIES, PRIMER=FIRST SEGUNDO=SECOND
#@begin create_new_columns
#@desc SEPARATE SINGLE AND FIXED DOSE (MORE THAN ONE MOLECULE) Entries
#@in Single
#@in Falta
#@in FixedDose
#@out Single.1
#@out Falta.1
#@out FixedDose.1
Single$Primer<-NA
Falta$Primer<-NA
FixedDose$Primer<-NA
Single$Segundo<-NA
Falta$Segundo<-NA
FixedDose$Segundo<-NA
Single$Concentracion1<-NA
Single$Concentracion2<-NA
Falta$Concentracion1<-NA
Falta$Concentracion2<-NA
FixedDose$Concentracion1<-NA
FixedDose$Concentracion2<-NA



for(i in seq(1,nrow(Single))){
  Single$Primer[i]<-unlist(str_split(Single$`Descrição CATMAT`[i], ","))[1]
  Single$Concentracion1[i]<-str_extract((unlist(str_split(Single$`Descrição CATMAT`[i],",")))[grepl("DOSAGEM",(unlist(str_split(Single$`Descrição CATMAT`[i],","))))|grepl("CONCENTRAÇÃO",(unlist(str_split(Single$`Descrição CATMAT`[i],","))))],"[[:digit:]]+\\.*[[:digit:]]*")
  Single$Primer[i]<-unlist(str_split(Single$Primer[i]," "))[1]
}
for(i in seq(1,nrow(Falta))){
  Falta$Primer[i]<-unlist(str_split(Falta$`Descrição CATMAT`[i], ","))[1]
}
for(i in seq(1,nrow(FixedDose))){
  FixedDose$Primer[i]<-unlist(str_split(FixedDose$`Descrição CATMAT`[i], ","))[1]
  FixedDose$Primer[i]<-word(FixedDose$Primer[i],1)
  FixedDose$Segundo[i]<-unlist(str_split(sub(".*ASSOCIAD","",FixedDose$`Descrição CATMAT`[i]),","))[1]
  FixedDose$Segundo[i]<-word(FixedDose$Segundo[i],-1)
  FixedDose$Concentracion1[i]<-str_extract(unlist(str_split(unlist(str_split(FixedDose$`Descrição CATMAT`[i],","))[grepl("CONCENTRAÇÃO",unlist(str_split(FixedDose$`Descrição CATMAT`[i],","))) |grepl("DOSAGEM",unlist(str_split(FixedDose$`Descrição CATMAT`[i],","))) ],"\\+"))[1],"[[:digit:]]+\\.*[[:digit:]]*") 
  FixedDose$Concentracion2[i]<-str_extract(unlist(str_split(unlist(str_split(FixedDose$`Descrição CATMAT`[i],","))[grepl("CONCENTRAÇÃO",unlist(str_split(FixedDose$`Descrição CATMAT`[i],","))) |grepl("DOSAGEM",unlist(str_split(FixedDose$`Descrição CATMAT`[i],",")))],"\\+"))[2],"[[:digit:]]+\\.*[[:digit:]]*") 
}
Falta$Concentracion1<-50

FixedDose$Segundo<-str_remove_all(FixedDose$Segundo,"COM")
FixedDose$Segundo<-str_replace_all(FixedDose$Segundo,"_"," ")
FixedDose$Segundo<-str_remove_all(FixedDose$Segundo," ")
#@end create_new_columns

#@@begin create_single_nom
#@desc Single table for Primer and Concentraction1
#@in Single.1
#@out SingleNom
SingleNom<-Single[,c("Primer","Concentracion1")]
SingleNom$Distance<-NA
stringdist(DataProv$NomProducto[i],DataProv$NomCatalog[i])
#@@end create_single_nom


#@begin join_single_fixed_dose
#@desc JOIN SINGLE AND FIXED DOSE ENTRIES
#@in Single.1
#@in FixedDose.1
#@in toconsider
#@out DatosBr

test<-FixedDose%>%
  distinct(`Descrição CATMAT`, .keep_all = TRUE)%>%
  dplyr::select(`Descrição CATMAT`, Primer,Concentracion1, Segundo,Concentracion2)
#JOIN SINGLE AND FIXED DOSE ENTRIES
DatosBr<-rbind(Single,FixedDose)
DatosBr$Catalog1<-NA
DatosBr$Dist1<-NA
DatosBr$Catalog2<-NA
DatosBr$Dist2<-NA
for(i in seq(1,nrow(DatosBr))){
  DatosBr$Catalog1[i]<-toconsider$Primer[which.min(stringdist(DatosBr$Primer[i],toconsider$Primer))]
  DatosBr$Dist1[i]<-stringdist(DatosBr$Primer[i],DatosBr$Catalog1[i])
  if(DatosBr$Dist1[i]>3){
    if(length(toconsider$Primer[startsWith(toconsider$Primer,str_sub(DatosBr$Primer[i],1,1))])>0){
      Empiezan<-toconsider$Primer[startsWith(toconsider$Primer,str_sub(DatosBr$Primer[i],1,1))]
      DatosBr$Catalog1[i]<-Empiezan[which.min(stringdist(DatosBr$Primer[i],Empiezan))]
      DatosBr$Dist1[i]<-stringdist(DatosBr$Catalog1[i],DatosBr$Primer[i])
      if(DatosBr$Dist1[i]>8){
        DatosBr$Catalog1[i]<-NA
      }
    }else{
      DatosBr$Catalog1[i]<-NA
    }
  }
  if(!is.na(DatosBr$Segundo[i])){
    DatosBr$Catalog2[i]<-toconsider$Segundo[which.min(stringdist(DatosBr$Segundo[i],toconsider$Segundo))]
    DatosBr$Dist2[i]<-stringdist(DatosBr$Segundo[i],DatosBr$Catalog2[i])
    if(DatosBr$Dist2[i]>4){
      if(length(toconsider$Segundo[startsWith(toconsider$Segundo,str_sub(DatosBr$Segundo[i],1,1))])>0){
        Empiezan<-toconsider$Segundo[startsWith(toconsider$Segundo,str_sub(DatosBr$Segundo[i],1,1))]
        DatosBr$Catalog2[i]<-Empiezan[which.min(stringdist(DatosBr$Segundo[i],Empiezan))]
        DatosBr$Dist2[i]<-stringdist(DatosBr$Catalog2[i],DatosBr$Segundo[i])
        if(DatosBr$Dist2[i]>8){
          DatosBr$Catalog2[i]<-"Not Found"
        }
      }else{
        DatosBr$Catalog2[i]<-"Not Found"
      }
    }
  }
  a<-toconsider$`Product Name`[grepl(DatosBr$Catalog1[i],toconsider$Primer)&
                                 (DatosBr$Concentracion1[i]==toconsider$Concentracion1)&
                                 (is.na(toconsider$Segundo))]
  b<-toconsider$`Product Name`[grepl(DatosBr$Catalog1[i],toconsider$Primer)&
                                 (DatosBr$Concentracion1[i]==toconsider$Concentracion1)&
                                 grepl(DatosBr$Catalog2[i],toconsider$Segundo)&
                                 (DatosBr$Concentracion2[i]==toconsider$Concentracion2)]
  if(is.na(DatosBr$Segundo[i])){
    if(!identical(a, character(0))){
      DatosBr$NomCatalog[i]<-a
    }
  }else{
    if(!identical(b, character(0))){
      DatosBr$NomCatalog[i]<-b
    }
  }
}
#@end join_single_fixed_dose


#@begin datosbr_exceptions
#@desc ADD EXCEPTIONS
#@in DatosBr
#@out DatosBr.1
#ADD EXCEPTIONS
DatosBr$NomCatalog[which(DatosBr$`Descrição CATMAT`=="CISPLATINA, CONCENTRAÇÃO:1 MG/ML, FORMA FARMACEUTICA:SOLUÇÃO INJETÁVEL")]<-"CISPLATIN 50 MG, POWDER FOR INJECTION, 50 ML VIAL, 1X1"
DatosBr$NomCatalog[which(DatosBr$`Descrição CATMAT`=="RITUXIMABE, DOSAGEM:10MG/ML, INDICAÇÃO:SOLUÇÃO INJETÁVEL"&
                     DatosBr$` Unidade de Fornecimento ` =="FRASCO 50,00 ML")]<-"RITUXIMAB 500 MG, POWDER FOR INJECTION, 50 ML VIAL, 1X1"
DatosBr$NomCatalog[which(DatosBr$`Descrição CATMAT`=="RITUXIMABE, DOSAGEM:10MG/ML, INDICAÇÃO:SOLUÇÃO INJETÁVEL"&
                     DatosBr$` Unidade de Fornecimento ` =="FRASCO 10,00 ML")]<-"RITUXIMAB 100 MG, POWDER FOR INJECTION, 10 ML VIAL, 1X1"
DatosBr$NomCatalog[which(DatosBr$`Descrição CATMAT`=="DOXORRUBICINA CLORIDRATO, CONCENTRAÇÃO:2 MG/ML, FORMA FARMACÊUTICA:SOLUÇÃO INJETÁVEL")]<-"DOXORUBICIN 50 MG (HYDROCHLORIDE), POWDER FOR INJECTION, 25 ML VIAL, 1X1"
DatosBr$NomCatalog[which(DatosBr$`Descrição CATMAT`=="TRASTUZUMABE, CONCENTRAÇÃO:440 MG, FORMA FARMACÊUTICA:PÓ LIOFILO INJETÁVEL")]<-"TRASTUZUMAB 420 MG, POWDER FOR INJECTION, 20 ML VIAL, 1X1"
DatosBr$NomCatalog[which(DatosBr$`Descrição CATMAT`=="TRASTUZUMABE, CONCENTRAÇÃO:120 MG/ML, FORMA FARMACÊUTICA:SOLUÇÃO INJETÁVEL")]<-"TRASTUZUMAB 150 MG, POWDER FOR INJECTION, 7.2 ML VIAL, 1X1"
#@end datosbr_exceptions


#CHECK
test<-DatosBr%>%
  distinct(`Descrição CATMAT`, .keep_all = TRUE)%>%
  select(`Descrição CATMAT`, NomCatalog)



#@begin finalize_datos_br
#@in toconsider
#@in DatosBr.1
#@in Rea_USD
#@out DatosBrFinal
priceconsider<-toconsider[,c(1,2,6,7,9)]

DatosBrFinal<-DatosBr%>%
  left_join(priceconsider, by=c("NomCatalog"="Product Name"))
test<-DatosBrFinal[, c(1,19,36)]
DatosBrFinal$` Data Compra `<-as.Date(DatosBrFinal$` Data Compra `, format = "%d/%m/%Y")
DatosBrFinal$Mes<-month(DatosBrFinal$` Data Compra `)
DatosBrFinal$Ano<-year(DatosBrFinal$` Data Compra `)
DatosBrFinal$`Preço Unitário`<-str_remove_all(DatosBrFinal$`Preço Unitário`,"\\.")
DatosBrFinal$`Preço Unitário`<-as.numeric(str_replace_all(DatosBrFinal$`Preço Unitário`,",","."))
DatosBrFinal$`Qtd Itens Comprados`<-as.numeric(str_replace_all(DatosBrFinal$`Qtd Itens Comprados`,",","."))
DatosBrFinal<-DatosBrFinal%>%
  left_join(Rea_USD, by=c("Mes","Ano"))
DatosBrFinal$`Price USD`<-DatosBrFinal$`Preço Unitário`/DatosBrFinal$Precio
#@end finalize_datos_br


################################################################################
#ADDING TO THE FINAL FORMAT
#@begin finalize_data
#@in DatosBrFinal
#@out FinalData
namescols<-c("Country Code", "Entity", "Entity Type","Punto de Entrega", "Mecanismo de Compra", "Programa","Region", "Therapeutic Area", 
             "Generic Product Name","Presentación","Genérico","Subunits per Unit","Catalog Name","Supplier","Manufacturer",
             "Purchase Date","Purchase Year","Total Amount","Unit Quantity","Min Unit Price USD", "PAHO Min Unit Price USD",
             "Patent","Observaciones")
FinalData<- as.data.frame(matrix(nrow=nrow(DatosBrFinal),ncol=length(namescols)))
names(FinalData)<-namescols
FinalData$`Country Code`<-"BRA"
FinalData$Entity<-DatosBrFinal$`Nome Instituição`
FinalData$`Entity Type`[grepl("Universidade",DatosBrFinal$`Nome Instituição`, ignore.case = TRUE)]<-"Universidad"
FinalData$`Entity Type`[grepl("Estado",DatosBrFinal$`Nome Instituição`, ignore.case = TRUE)]<-"Estado"
FinalData$`Entity Type`[grepl("Estatal",DatosBrFinal$`Nome Instituição`, ignore.case = TRUE)]<-"Estado"
FinalData$`Entity Type`[grepl("Instituto",DatosBrFinal$`Nome Instituição`, ignore.case = TRUE)]<-"Municipio"
FinalData$`Entity Type`[grepl("Secretaria",DatosBrFinal$`Nome Instituição`, ignore.case = TRUE)]<-"Estado"
FinalData$`Entity Type`[grepl("Municip",DatosBrFinal$`Nome Instituição`, ignore.case = TRUE)]<-"Municipio"
FinalData$`Entity Type`[grepl("Consorcio",DatosBrFinal$`Nome Instituição`, ignore.case = TRUE)]<-"Consorcio"
FinalData$`Therapeutic Area`<-TerapeuticArea
FinalData$`Catalog Name`<-DatosBrFinal$NomCatalog
FinalData$`Unit Quantity`<-DatosBrFinal$`Qtd Itens Comprados`
FinalData$`Min Unit Price USD`<-DatosBrFinal$`Price USD`
FinalData$`Total Amount`<-DatosBrFinal$`Qtd Itens Comprados`*DatosBrFinal$`Price USD`
FinalData$`PAHO Min Unit Price USD`<-DatosBrFinal$`Min Unit Cost`
FinalData$`Subunits per Unit`<-1
FinalData$`Purchase Year`<-DatosBrFinal$Ano
FinalData$Supplier<-DatosBrFinal$Fornecedor
FinalData$`Generic Product Name`<-DatosBrFinal$`Descrição CATMAT`
FinalData$`Punto de Entrega`<-DatosBrFinal$`Município Instituição`
FinalData$`Mecanismo de Compra`[grepl("Licit",DatosBrFinal$`Modalidade da Compra`,ignore.case = TRUE)]<-"Contrato Directo"
FinalData$`Mecanismo de Compra`[grepl("Preg",DatosBrFinal$`Modalidade da Compra`,ignore.case = TRUE)]<-"Licitación" 
FinalData$`Mecanismo de Compra`[grepl("Tomada",DatosBrFinal$`Modalidade da Compra`,ignore.case = TRUE)]<-"Licitación" #Pregao, Inexigibilidade de Licitação, Tomada de Preços, Dispensa de Licitação
FinalData$Manufacturer<-DatosBrFinal$Fabricante
FinalData$`Purchase Date`<- DatosBrFinal$` Data Compra `
FinalData$Genérico<-DatosBrFinal$Genérico
FinalData$Presentación<-DatosBrFinal$` Unidade de Fornecimento `
filename<-paste0("~/RStudio/PAHO/Scrappers/Brasil/",TerapeuticArea,".csv")
#@end finalize_data

#@begin write_final_data
#@in FinalData
#@out output_file @URI file:~/RStudio/PAHO/Scrappers/Brasil/\<TerapeuticArea\>.csv
write.csv(FinalData, filename)
#@end write_final_data

#**********************************************************************


#@end BRA_Antineoplastics_Transformation