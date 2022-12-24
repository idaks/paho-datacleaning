library(readxl)
library(stringr)
library(stringdist)
library(dplyr)
library(readr)
#Read the Entry files, the full database and the Taxonomy (the Catalog)
CHL<- read_excel("~/OneDrive - Pan American Health Organization/Price Analysis/CHL/CHL Cenabast Filtered 20-22.xlsx")
CHL<-CHL[which(CHL$Proveedor!="PAN AMERICAN HEALTH ORGANIZATION"),]
PriceDataBase <- read_excel("~/OneDrive - Pan American Health Organization/Price Analysis/PriceDB/PriceDataBase.xlsx")
Prices_2019_2021 <- read_excel("~/OneDrive - Pan American Health Organization/Price Analysis/PriceDB/Prices 2019-2021.xlsx")
Prices_2019_2021<-Prices_2019_2021[!is.na(Prices_2019_2021$`Product Name`) & !is.na(Prices_2019_2021$`Min Unit Cost`),c(4,8)]
Taxonomy <- read_excel("~/RStudio/PAHO/Scrappers/Taxonomy.xlsx",sheet = "SF Products")
CHLFinal<-as.data.frame(matrix(nrow=nrow(CHL),ncol=ncol(PriceDataBase)))
names(CHLFinal)<-names(PriceDataBase)
###############Catalogo
#Generate the catalog
SingleDose<-CHL$`Nombre producto genérico`[grepl("\\/",CHL$`Nombre producto genérico`)]
#Catalog<- read_excel("~/OneDrive - Pan American Health Organization/Price Analysis/PriceDB/Prices 2019-2021.xlsx")
NewEntries <- read_excel("~/OneDrive - Pan American Health Organization/Price Analysis/PriceDB/New Catalog Products.xlsx")
NewEntries2<-as.data.frame(matrix(nrow=nrow(NewEntries),ncol=ncol(Taxonomy)))
names(NewEntries2)<-names(Taxonomy)
NewEntries2$`Product Name`<-NewEntries$`Catalog Name`
NewEntries2$`Therapeutic Area`<-NewEntries$`Therapeutic Area`
Catalog<-rbind(Taxonomy,NewEntries2)
Catalog<-Catalog[(!grepl("Kit",Catalog$`Product Name`,ignore.case = TRUE)) & !is.na(Catalog$`Product Name`),]
rm(Taxonomy, NewEntries, NewEntries2)
Catalog<-Catalog%>%
  left_join(Prices_2019_2021, by="Product Name")
FixedDoseCatalog<-Catalog[grepl("\\+",Catalog$`Product Name`),]
SingleDoseCatalog<-Catalog[!grepl("\\+",Catalog$`Product Name`),]
FixedDoseCatalog$Mol1<-NA
FixedDoseCatalog$Con1<-NA
FixedDoseCatalog$Mol2<-NA
FixedDoseCatalog$Con2<-NA
FixedDoseCatalog$Mol3<-NA
FixedDoseCatalog$Con3<-NA
FixedDoseCatalog$Mol4<-NA
FixedDoseCatalog$Con4<-NA
#Separate the catalog in its most basic molecule and concentration
for(i in seq(1,nrow(FixedDoseCatalog))){
  Product<-unlist(strsplit(FixedDoseCatalog$`Product Name`[i],","))[1]
  FixedDoseCatalog$Mol1[i]<-unlist(strsplit(unlist(strsplit(Product,"\\+"))[1]," "))[1]
  FixedDoseCatalog$Con1[i]<-unlist(strsplit(unlist(strsplit(Product,"\\+"))[1]," "))[2]
  FixedDoseCatalog$Mol2[i]<-unlist(strsplit(unlist(strsplit(Product,"\\+"))[2]," "))[2]
  FixedDoseCatalog$Con2[i]<-unlist(strsplit(unlist(strsplit(Product,"\\+"))[2]," "))[3]
  FixedDoseCatalog$Mol3[i]<-unlist(strsplit(unlist(strsplit(Product,"\\+"))[3]," "))[2]
  FixedDoseCatalog$Con3[i]<-unlist(strsplit(unlist(strsplit(Product,"\\+"))[3]," "))[3]
  FixedDoseCatalog$Mol4[i]<-unlist(strsplit(unlist(strsplit(Product,"\\+"))[4]," "))[2]
  FixedDoseCatalog$Con4[i]<-unlist(strsplit(unlist(strsplit(Product,"\\+"))[4]," "))[3]
}

Catalog$Mol1<-NA
Catalog$Con1<-NA
Catalog$Mol2<-NA
Catalog$Con2<-NA
Catalog$Mol3<-NA
Catalog$Con3<-NA
Catalog$Mol4<-NA
Catalog$Con4<-NA
Catalog$Presentacion<-NA

for(i in seq(1,nrow(Catalog))){ 
  Product<-unlist(strsplit(Catalog$`Product Name`[i],","))[1]
  Catalog$Mol1[i]<-unlist(strsplit(unlist(strsplit(Product,"\\+"))[1]," "))[1]
  Catalog$Con1[i]<-unlist(strsplit(unlist(strsplit(Product,"\\+"))[1]," "))[2]
  j<-2
  while(is.na(as.numeric(Catalog$Con1[i])) & j<5){
    Catalog$Mol1[i]<-paste(Catalog$Mol1[i],unlist(strsplit(unlist(strsplit(Product,"\\+"))[1]," "))[j])
    Catalog$Con1[i]<-unlist(strsplit(unlist(strsplit(Product,"\\+"))[1]," "))[j+1]
    j<-j+1
  }
  Catalog$Mol2[i]<-unlist(strsplit(unlist(strsplit(Product,"\\+"))[2]," "))[2]
  Catalog$Con2[i]<-unlist(strsplit(unlist(strsplit(Product,"\\+"))[2]," "))[3]
  j<-3
  while(is.na(as.numeric(Catalog$Con2[i])) & j<6){
    Catalog$Mol2[i]<-paste(Catalog$Mol2[i],unlist(strsplit(unlist(strsplit(Product,"\\+"))[2]," "))[j])
    Catalog$Con2[i]<-unlist(strsplit(unlist(strsplit(Product,"\\+"))[2]," "))[j+1]
    j<-j+1
  }
  Catalog$Mol3[i]<-unlist(strsplit(unlist(strsplit(Product,"\\+"))[3]," "))[2]
  Catalog$Con3[i]<-unlist(strsplit(unlist(strsplit(Product,"\\+"))[3]," "))[3]
  j<-3
  while(is.na(as.numeric(Catalog$Con3[i])) & j<6){
    Catalog$Mol3[i]<-paste(Catalog$Mol3[i],unlist(strsplit(unlist(strsplit(Product,"\\+"))[3]," "))[j])
    Catalog$Con3[i]<-unlist(strsplit(unlist(strsplit(Product,"\\+"))[3]," "))[j+1]
    j<-j+1
  }
  Catalog$Mol4[i]<-unlist(strsplit(unlist(strsplit(Product,"\\+"))[4]," "))[2]
  Catalog$Con4[i]<-unlist(strsplit(unlist(strsplit(Product,"\\+"))[4]," "))[3]
  j<-3
  while(is.na(as.numeric(Catalog$Con4[i])) & j<6){
    Catalog$Mol4[i]<-paste(Catalog$Mol4[i],unlist(strsplit(unlist(strsplit(Product,"\\+"))[4]," "))[j])
    Catalog$Con4[i]<-unlist(strsplit(unlist(strsplit(Product,"\\+"))[4]," "))[j+1]
    j<-j+1
  }
  Catalog$Presentacion[i]<-unlist(strsplit(Catalog$`Product Name`[i],","))[2]
}
Catalog$Mol2[which(Catalog$Mol2=="NA NA NA NA")]<-NA
Catalog$Mol3[which(Catalog$Mol3=="NA NA NA NA")]<-NA
Catalog$Mol4[which(Catalog$Mol4=="NA NA NA NA")]<-NA
Catalog$Mol1[which(Catalog$Mol1=="TENOFOVIR ALAFENAMIDE")]<-"TENOFOVIR"
Catalog$Mol2[which(Catalog$`Product Name`=="DOCETAXEL 40 MG/ML + SOLVENT 6 ML, SOLUTION FOR INFUSION, 2 ML VIAL, 1X1")]<-NA
Catalog$Con2[which(Catalog$`Product Name`=="DOCETAXEL 40 MG/ML + SOLVENT 6 ML, SOLUTION FOR INFUSION, 2 ML VIAL, 1X1")]<-NA



#############MAPEO
#SELECT THE RELEVANT COLUMNS
CHLMAP<-CHL[,c(8,11)]
CHLMAP$`Nombre producto genérico`<-str_replace_all(CHLMAP$`Nombre producto genérico`,"  "," ")
CHLMAP$Mol1<-NA
CHLMAP$Con1<-NA
CHLMAP$Mol2<-NA
CHLMAP$Con2<-NA
CHLMAP$Mol3<-NA
CHLMAP$Con3<-NA
CHLMAP$Mol4<-NA
CHLMAP$Con4<-NA
CHLMAP$`Nombre producto genérico`<-str_replace_all(CHLMAP$`Nombre producto genérico`,"\\/HIDROCLOROT\\.","/HIDROCLOROTIAZIDA")
CHLMAP$`Nombre producto genérico`<-str_replace_all(CHLMAP$`Nombre producto genérico`,"HIDROCLOROTIAZ\\.","HIDROCLOROTIAZIDA")
CHLMAP$`Nombre producto genérico`<-str_replace_all(CHLMAP$`Nombre producto genérico`,"TEN ALA","TENOFOVIR")
CHLMAP$`Nombre producto genérico`<-str_replace_all(CHLMAP$`Nombre producto genérico`,"IMIPENEM-CILASTATINA","IMIPENEM/CILASTATINA")
CHLMAP$`Nombre producto genérico`<-str_replace_all(CHLMAP$`Nombre producto genérico`,"CICLOSPORINA ORAL","CICLOSPORINA")

#SEPARATE THE CHILE ENTRIES IN ITS SIMPLEST FORM (SINGLE MOLECULE AND CONCENTRATION)

for(i in seq(1,nrow(CHLMAP))){
  Product<-unlist(strsplit(CHLMAP$`Nombre producto genérico`[i]," "))[1]
  Concentracion<-unlist(strsplit(CHLMAP$`Nombre producto genérico`[i]," "))[2]
  CHLMAP$Mol1[i]<-unlist(strsplit(Product,"\\/"))[1]
  CHLMAP$Mol2[i]<-unlist(strsplit(Product,"\\/"))[2]
  CHLMAP$Mol3[i]<-unlist(strsplit(Product,"\\/"))[3]
  CHLMAP$Mol4[i]<-unlist(strsplit(Product,"\\/"))[4]
  CHLMAP$Con1[i]<-unlist(strsplit(Concentracion,"\\/"))[1]
  CHLMAP$Con2[i]<-unlist(strsplit(Concentracion,"\\/"))[2]
  CHLMAP$Con3[i]<-unlist(strsplit(Concentracion,"\\/"))[3]
  CHLMAP$Con4[i]<-unlist(strsplit(Concentracion,"\\/"))[4]
}
CHLMAP$Con1<-str_remove_all(CHLMAP$Con1,"M")
CHLMAP$Con1<-str_remove_all(CHLMAP$Con1,"C")
CHLMAP$Con1<-str_remove_all(CHLMAP$Con1,"G")
CHLMAP$Con1<-str_replace_all(CHLMAP$Con1,",",".")

#EXCEPTIONS

CHLMAP$Con1[grepl("MCG",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-as.numeric(CHLMAP$Con1[grepl("MCG",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )])/1000
CHLMAP$Con1[grepl("1G",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-1000
CHLMAP$Con1[grepl("2G",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-2000
CHLMAP$Con1[grepl("TRASTUZUMAB LIOFILIZADO O SOL. INY. FAM",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-420
CHLMAP$Con1[grepl("TRASTUZUMAB 440 MG LIOFIL P/INF. IV FAM",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-420
CHLMAP$Con1[grepl("100 MG/5 ML",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-20
CHLMAP$Con1[grepl("TENOFOVIR DISOPROXIL 300 MG CM REC",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-300
CHLMAP$Con1[grepl("TRASTUZUMAB 600MG/ 5ML SOL. INY",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-150


CHLMAP$Con1[grepl("MIDAZOLAM 50 MG/10ML SOL.INY. AM/FAM",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-5
CHLMAP$Con1[grepl("MIDAZOLAM 5 MG/5ML SOL.INY. AM",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-1
CHLMAP$Con1[grepl("FENTANILO 0,1 MG/2ML SOL.INY. AM/FAM/JPR",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-0.05
CHLMAP$Con1[grepl("FENTANILO 0,5 MG/10 ML SOL. INY. AM/FAM",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-0.05
CHLMAP$Con1[grepl("METOTREXATO 50 MG/2ML SOL.INYECTABLE FAM",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-25
CHLMAP$Con1[grepl("RITUXIMAB 1400MG/ 11,7ML SOL INYECT FAM",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-120
CHLMAP$Con1[grepl("RITUXIMAB 100 MG/10ML SOL.INY. FAM",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-10
CHLMAP$Con1[grepl("METOTREXATO 500MG/20ML SOL.INYECT. FAM",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-25
CHLMAP$Con1[grepl("DOCETAXEL 20 MG/2ML SOL.ADMIN.IV FAM",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-10
CHLMAP$Con1[grepl("DOCETAXEL 80 MG/2ML SOL.ADMIN.IV FAM",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-40
CHLMAP$Con1[grepl("TOCILIZUMAB 80 MG/4 ML SOL. P/INF. FAM",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-20
CHLMAP$Con1[grepl("TOCILIZUMAB 162 MG/0,9 ML SOL. INY. JRP",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-180
CHLMAP$Con1[grepl("TOCILIZUMAB 400 MG/20 ML SOL. P/INF. FAM",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-20
CHLMAP$Con1[grepl("TOCILIZUMAB 200 MG/10 ML SOL. P/INF. FAM",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-20
CHLMAP$Con1[grepl("FLUCONAZOL 200 MG/100 ML SOL. INY.",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-2
CHLMAP$Con1[grepl("BEVACIZUMAB 100 MG/4 ML SOL. P/INF. FAM",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-25
CHLMAP$Con1[grepl("ZIDOVUDINA 200 MG/20 ML SOL.INY. FAM",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-10
CHLMAP$Con1[grepl("MIDAZOLAM 100 MG/100ML SOL.INY FRASCO",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-1
CHLMAP$Con1[grepl("MIDAZOLAM 50 MG/50ML SOL.INY. FRASCO",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-1
CHLMAP$Con1[grepl("MIDAZOLAM 15 MG/3ML SOL.INY. AM",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-5
CHLMAP$Con1[grepl("MESNA 400 MG/4 ML FAM/AM",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-100
CHLMAP$Con1[grepl("METOTREXATO 500MG/20ML SOL.INYECT. FAM",CHLMAP$`Nombre producto genérico`,ignore.case = TRUE )]<-25




CHLMAP$Con1[CHLMAP$Con1=="10.000"]<-10000
CHLMAP$Con2<-str_replace_all(CHLMAP$Con2,",","\\.")
CHLMAP$Con2<-str_remove_all(CHLMAP$Con2,"MG")
CHLMAP$Con3<-str_remove_all(CHLMAP$Con3,"MG")
CHLMAP$Con4<-str_remove_all(CHLMAP$Con4,"MG")

#Exceptions
CHLMAP$Mol2[which(CHLMAP$Mol2=="EMTR" | CHLMAP$Mol2=="EMTRIC" | CHLMAP$Mol2=="EMTRICIT")]<-"EMTRICITABINA"
CHLMAP$Mol2[which(CHLMAP$Mol2=="EMTR" | CHLMAP$Mol2=="EMTRIC" | CHLMAP$Mol2=="EMTRICIT")]<-"EMTRICITABINA"
CHLMAP$Mol1[which(CHLMAP$Mol1=="TENOF")]<-"TENOFOVIR"
CHLMAP$Mol1[which(CHLMAP$Mol1=="MESNA")]<-"MESNA (2-MERCAPTOETHANE SULFONATE SODIUM)"
CHLMAP$Mol1[which(CHLMAP$Mol1=="MESNA")]<-"MESNA (2-MERCAPTOETHANE SULFONATE SODIUM)"
CHLMAP$Con1[which(CHLMAP$Mol1=="MICOFENOLATO") & grepl("250", CHLMAP$`Nombre producto genérico`)]<-"250"
CHLMAP$Con1[which(CHLMAP$Mol1=="MICOFENOLATO")& grepl("500", CHLMAP$`Nombre producto genérico`)]<-"500"
CHLMAP$Mol1[which(CHLMAP$Mol1=="MICOFENOLATO")]<-"MYCOPHENOLATE MOFETIL"


CHLMAP$Mol3[which(CHLMAP$Mol3=="ELV")]<-"ELVITEGRAVIR"
CHLMAP$Mol4[which(CHLMAP$Mol4=="COB")]<-"COBICISTAT"
CHLMAP$Mol3[which(CHLMAP$Mol3=="RILPIVI")]<-"RILPIVIRINA"


Catalog$Con1[grepl("1 G",Catalog$`Product Name`,ignore.case = TRUE )]<-1000



CHLMAP$Eng1<-NA
CHLMAP$dist1<-NA
CHLMAP$Eng2<-NA
CHLMAP$dist2<-NA
CHLMAP$Eng3<-NA
CHLMAP$dist3<-NA
CHLMAP$Eng4<-NA
CHLMAP$dist4<-NA
CHLMAP$CatalogName<-NA
meds<-c(Catalog$Mol1,Catalog$Mol2,Catalog$Mol3,Catalog$Mol4)
meds<-as.data.frame(unique(meds))
meds<-as.data.frame(meds[!is.na(meds$`unique(meds)`),])
names(meds)<-"meds"
#ERRORES
CHLMAP$Mol1[grepl("CICLOFOSFAMIDA",CHLMAP$`Nombre producto genérico`)]<-"CYCLOPHOSPHAMIDE"
CHLMAP$Mol1[grepl("ABACAV/LAMIV/DOLUTEGRAV",CHLMAP$`Nombre producto genérico`)]<-"ABACAVIR"
CHLMAP$Mol2[grepl("ABACAV/LAMIV/DOLUTEGRAV",CHLMAP$`Nombre producto genérico`)]<-"LAMIVUDINE"
CHLMAP$Mol3[grepl("ABACAV/LAMIV/DOLUTEGRAV",CHLMAP$`Nombre producto genérico`)]<-"DOLUTEGRAVIR"
CHLMAP$Mol1[grepl("IMIPENEM-CILASTATINA",CHLMAP$`Nombre producto genérico`)]<-"IMIPENEM"
CHLMAP$Mol1[grepl("IMIPENEM-CILASTATINA",CHLMAP$`Nombre producto genérico`)]<-"CILASTATINA"
#FIND THE CLOSEST MOLECULE FROM THE CATALOG TO EACH ENTRY USING STRINGDIST
for(i in seq(1,nrow(CHLMAP))){
  #PeruMed$NomCatalog[i]<-DatosFinal$Nombre[which.min(stringdist(PeruMed$NomProducto[i],DatosFinal$Nombre))]
  CHLMAP$Eng1[i]<-meds$meds[which.min(stringdist(CHLMAP$Mol1[i],meds$meds))]
  CHLMAP$dist1[i]<-stringdist(CHLMAP$Eng1[i],CHLMAP$Mol1[i])
  if(!is.na(CHLMAP$Mol2[i])){
    CHLMAP$Eng2[i]<-meds$meds[which.min(stringdist(CHLMAP$Mol2[i],meds$meds))]
    CHLMAP$dist2[i]<-stringdist(CHLMAP$Eng2[i],CHLMAP$Mol2[i])
  }
  if(!is.na(CHLMAP$Mol3[i])){
    CHLMAP$Eng3[i]<-meds$meds[which.min(stringdist(CHLMAP$Mol3[i],meds$meds))]
    CHLMAP$dist3[i]<-stringdist(CHLMAP$Eng3[i],CHLMAP$Mol3[i])
  }
  if(!is.na(CHLMAP$Mol4[i])){
    CHLMAP$Eng4[i]<-meds$meds[which.min(stringdist(CHLMAP$Mol4[i],meds$meds))]
    CHLMAP$dist4[i]<-stringdist(CHLMAP$Eng4[i],CHLMAP$Mol4[i])
  }
}

#TEST THE MAPPING
test<-CHLMAP%>%
  distinct(`Nombre producto genérico`, .keep_all = TRUE)%>%
  select(`Nombre producto genérico`, Mol1, Eng1, dist1, Mol2, Eng2, dist2,Mol3, Eng3, dist3,Mol4, Eng4, dist4)

#FALTAN
faltan<-test%>%
  filter(dist1>=3)%>%
  select(Mol1,Eng1,`Nombre producto genérico`)

CHLMAP$Eng1[which(CHLMAP$dist1>=3)]<-NA


CHLMAP$Eng1[CHLMAP$Mol1=="HIDROCLOROTIAZIDA"]<-"HYDROCHLOROTHIAZIDE"
CHLMAP$Eng2[CHLMAP$Mol2=="HIDROCLOROTIAZIDA"]<-"HYDROCHLOROTHIAZIDE"
#######Mapping
#MAP THE ENTRIES THAT MATCH THE MOLECULE AND CONCENTRATION
CHLMAP$Subunits<-1
for (i in seq(1,nrow(CHLMAP))){
  if(!is.na(CHLMAP$Mol4[i])){
    if(length(Catalog$`Product Name`[which(Catalog$Mol1==CHLMAP$Eng1[i]
                                           &Catalog$Con1==CHLMAP$Con1[i] 
                                           &Catalog$Mol2==CHLMAP$Eng2[i]
                                           &Catalog$Con2==CHLMAP$Con2[i]
                                           &Catalog$Mol3==CHLMAP$Eng3[i]
                                           &Catalog$Con3==CHLMAP$Con3[i]
                                           &Catalog$Mol4==CHLMAP$Eng4[i]
                                           &Catalog$Con4==CHLMAP$Con4[i])])>0){
    CHLMAP$CatalogName[i]<-Catalog$`Product Name`[which(Catalog$Mol1==CHLMAP$Eng1[i]
                                                &Catalog$Con1==CHLMAP$Con1[i] 
                                                &Catalog$Mol2==CHLMAP$Eng2[i]
                                                &Catalog$Con2==CHLMAP$Con2[i]
                                                &Catalog$Mol3==CHLMAP$Eng3[i]
                                                &Catalog$Con3==CHLMAP$Con3[i]
                                                &Catalog$Mol4==CHLMAP$Eng4[i]
                                                &Catalog$Con4==CHLMAP$Con4[i])]
  }
  }
  if(!is.na(CHLMAP$Mol3[i]) & is.na(CHLMAP$Mol4[i])){
    toconsider<-Catalog[which(is.na(Catalog$Mol4)),]
    if(length(toconsider$`Product Name`[which(toconsider$Mol1==CHLMAP$Eng1[i]
                                           &toconsider$Con1==CHLMAP$Con1[i] 
                                           &toconsider$Mol2==CHLMAP$Eng2[i]
                                           &toconsider$Con2==CHLMAP$Con2[i]
                                           &toconsider$Mol3==CHLMAP$Eng3[i]
                                           &toconsider$Con3==CHLMAP$Con3[i])])>0){
    CHLMAP$CatalogName[i]<-toconsider$`Product Name`[which(toconsider$Mol1==CHLMAP$Eng1[i]
                                                           &toconsider$Con1==CHLMAP$Con1[i] 
                                                           &toconsider$Mol2==CHLMAP$Eng2[i]
                                                           &toconsider$Con2==CHLMAP$Con2[i]
                                                           &toconsider$Mol3==CHLMAP$Eng3[i]
                                                           &toconsider$Con3==CHLMAP$Con3[i])]
  }
                                                          
  }
  if(!is.na(CHLMAP$Mol2[i]) & is.na(CHLMAP$Mol3[i]) & is.na(CHLMAP$Mol4[i])){
    toconsider<-Catalog[which(is.na(Catalog$Mol3) & is.na(Catalog$Mol4)),]
    if(length(toconsider$`Product Name`[which(toconsider$Mol1==CHLMAP$Eng1[i]
                                           &toconsider$Con1==CHLMAP$Con1[i] 
                                           &toconsider$Mol2==CHLMAP$Eng2[i]
                                           &toconsider$Con2==CHLMAP$Con2[i])])>0){
          CHLMAP$CatalogName[i]<-toconsider$`Product Name`[which(toconsider$Mol1==CHLMAP$Eng1[i]
                                                                 &toconsider$Con1==CHLMAP$Con1[i] 
                                                                 &toconsider$Mol2==CHLMAP$Eng2[i]
                                                                 &toconsider$Con2==CHLMAP$Con2[i])]
  }
                                                            
  }
  if(is.na(CHLMAP$Mol2[i]) &is.na(CHLMAP$Mol3[i]) & is.na(CHLMAP$Mol4[i])){
    toconsider<-Catalog[which(is.na(Catalog$Mol2) & is.na(Catalog$Mol3) & is.na(Catalog$Mol4)),]
    if(length(toconsider$`Product Name`[which(toconsider$Mol1==CHLMAP$Eng1[i]
                                           &toconsider$Con1==CHLMAP$Con1[i])])>0){      
      CHLMAP$CatalogName[i]<-toconsider$`Product Name`[which(toconsider$Mol1==CHLMAP$Eng1[i]
                                                              &toconsider$Con1==CHLMAP$Con1[i])] 
    }
  }
  
  if(grepl("X ML",CHLMAP$`Nombre producto genérico`[i]) | grepl("XML",CHLMAP$`Nombre producto genérico`[i])){
    a<-str_replace_all(CHLMAP$`Nombre marca comercial`[i]," ","/")
    b<-str_remove_all(a,"[^0-9/-]")
    #c<-str_remove_all(b,"//")
    d<-unlist(str_split(b,"/"))
    d<-as.numeric(d[!(d=="")])
    CHLMAP$Subunits[i]<-1/max(d)
  }
}
#MAP THE EXCEPTIONS
CHLMAP$Subunits[CHLMAP$`Nombre marca comercial`=="RIFAMPICINA 100 MG/5 ML 50 ML CAJ 1 FRA"]<-1/50
CHL$`Mecanismo de compra`[grepl("trato",CHL$`Mecanismo de compra`, ignore.case = TRUE)]<-"Contrato Directo"

CHLMAP$CatalogName[CHLMAP$`Nombre producto genérico`=="TENOF/EMTRICIT/EFAV 300/200/600MG CM REC"]<-"EFAVIRENZ 600 MG + EMTRICITABINE 200 MG + TENOFOVIR 300 MG, TABLET, 30 TABLETS BOTTLE"
CHLMAP$CatalogName[CHLMAP$`Nombre producto genérico`=="ABACAV/LAMIV/DOLUTEGRAV 600/300/50 MG CM"]<-"ABACAVIR 600 MG + LAMIVUDINE 300 MG + DOLUTEGRAV 50 MG, TBD, TBD"
CHLMAP$CatalogName[CHLMAP$`Nombre producto genérico`=="HIDROCLOROTIAZIDA/TRIAMTERENO 25/50 MG CM"]<-"HYDROCHLOROTHIAZIDE 25 MG + TRIAMTERENE 50 MG, TABLET, BLISTER, TBD"

CHLMAP$CatalogName[CHLMAP$`Nombre producto genérico`=="AMFOTERICINA B LIPOSOMAL 50 MG FAM"]<-"AMPHOTERICIN B LIPOSOMAL 50 MG, POWDER FOR INJECTION, 20 ML VIAL, 1X1"
CHLMAP$CatalogName[CHLMAP$`Nombre producto genérico`=="AMFOTERICINA B 50MG LIOF P/S. INY AM/FAM"]<-"AMPHOTERICIN B (SODIUM DEOXYCHOLATE) 50 MG, POWDER FOR INJECTION, 20 ML VIAL, 1X1"
CHLMAP$CatalogName[CHLMAP$`Nombre producto genérico`=="AMOXICIL/CLAVULAN 875/125 MG CM/CM REC"]<-"AMOXICILLIN 875 MG + CLAVULANIC ACID 125 MG, TABLET, BLISTER, 10X10"
CHLMAP$CatalogName[CHLMAP$`Nombre producto genérico`=="AMOXIC/CLAVULAN 500/125 MG CP/CM/CM REC"]<-"AMOXICILLIN 500 MG + CLAVULANIC ACID 125 MG, TABLET, BLISTER, 10X10"


Therapeutic<-Catalog[,c(3,5)]%>%
  distinct(`Product Name`, .keep_all = TRUE)

CHLMAP2<-CHLMAP%>%
  left_join(Prices_2019_2021, by=c("CatalogName"="Product Name"))%>%
  left_join(Therapeutic, by=c("CatalogName"="Product Name"))

test<-CHLMAP2%>%
  filter(is.na(CatalogName))%>%
  distinct(`Nombre producto genérico`, .keep_all = TRUE)%>%
  select(`Nombre producto genérico`,Eng1)
#write.csv(test,"~/RStudio/PAHO/FaltanCatalog2.csv")

####### TO CATALOG
#TRANSFORM IT TO THE FINAL DATABASE FORMAT
PriceDataBase <- read_excel("~/RStudio/PAHO/PriceDataBaseMar31.xlsx")
PriceDataBase<-PriceDataBase[,c(2:ncol(PriceDataBase))]
FinalCenabast<-as.data.frame(matrix(nrow=nrow(CHLMAP),ncol=ncol(PriceDataBase)))
names(FinalCenabast)<-names(PriceDataBase)
FinalCenabast$`Country Code`<-"CHL"
FinalCenabast$Entity<-"CENABAST"
FinalCenabast$`Entity Type`<-"Central Nacional de Abastecimiento"
FinalCenabast$`Mecanismo de Compra`<-CHL$`Mecanismo de compra`
FinalCenabast$Programa<-CHL$`Líneas de Negocios`
FinalCenabast$`Therapeutic Area`<-CHLMAP2$`Therapeutic Area`
FinalCenabast$`Country Product Name`<-CHL$`Nombre producto genérico`
FinalCenabast$`Country Product Description`<-CHL$`Nombre marca comercial`
FinalCenabast$`Subunits per Unit`<-CHLMAP$Subunits
FinalCenabast$`Catalog Name`<-CHLMAP$CatalogName
FinalCenabast$Supplier<-CHL$Proveedor
FinalCenabast$`Purchase Date`<-as.Date(CHL$`Fecha de compra`, format = "%d-%m-%Y")
FinalCenabast$`Purchase Year`<-CHL$`Año compra`
FinalCenabast$`Total Amount`<-CHL$`Monto total`
FinalCenabast$`Unit Quantity`<-CHL$`Cantidad unitaria`
FinalCenabast$`Min Unit Price USD`<-as.numeric(CHL$`Precio unitario`)/as.numeric(CHLMAP$Subunits)
FinalCenabast$`PAHO Min Unit Price USD`<-CHLMAP2$`Min Unit Cost`

write.csv(FinalCenabast,"~/RStudio/PAHO/Scrappers/Chile/CenabastMar2022.csv" )
#########Municipalidades

CHL3 <-read_excel("~/OneDrive - Pan American Health Organization/Price Analysis/CHL/Municipalidades.xlsx", sheet = "CHL Data (clean)", col_types = c("numeric", "numeric", "text", "numeric", "text", "text", "text", "numeric", "numeric", "date", "text", "numeric", "text", "text", "text", "text", "text", "text","text", "text", "text", "numeric","text", "numeric", "text", "numeric", "numeric", "numeric", "text", "numeric","numeric", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "text", "numeric","numeric", "numeric", "numeric", "numeric"))

CHL32<-CHL3%>%
  left_join(Therapeutic, by=c("CATALOG NAME"="Product Name"))
#************Municipalidades***************
temp<-as.data.frame(matrix(nrow=nrow(CHL3),ncol=ncol(PriceDataBase)))
names(temp)<-names(PriceDataBase)
temp$`Country Code`<-"CHL"
temp$Entity<-CHL3$`Nombre Organismo`
temp$`Entity Type`<-CHL3$`Tipo de Institución`
temp$`Punto de Entrega`<-CHL3$`Region Unidad`
temp$`Mecanismo de Compra`<-CHL3$`Tipo de Adquisicion`
temp$Programa<-CHL3$`Fuente Financiamiento`
temp$Region<-CHL3$`Region Unidad`
temp$`Therapeutic Area`<-CHL32$`Therapeutic Area`
temp$`Country Product Name`<-CHL3$`Descripcion linea Adquisicion`
temp$`Country Product Description`<-CHL3$`Nombre linea Adquisicion`
temp$`Catalog Name`<-CHL3$`CATALOG NAME`
temp$Supplier<-CHL3$`Nombre Proveedor`
temp$`Purchase Date`<-as.Date(CHL3$`Fecha Adjudicacion`)
temp$`Purchase Year`<-CHL3$`Año Adjudicación`
temp$`Total Amount`<-CHL3$`Monto Adjudicado USD`
temp$`Subunits per Unit`<-1
temp$`Unit Quantity`<-CHL3$`Cantidad Adjudicada`
temp$`Min Unit Price USD`<-CHL3$`$ Unit USD`
temp$`PAHO Min Unit Price USD`<-CHL3$`PAHO unit Price`

write.csv(temp,"~/RStudio/PAHO/Scrappers/Chile/Municipalidades.csv" )


CHLFinal<-rbind(FinalCenabast,temp)

###############################3
#DONT USE AFTER THIS LINE
#################################
LE<-read_excel("~/OneDrive - Pan American Health Organization/Price Analysis/LEISH/LEISHMANIASIS Price Analysis.xlsx", sheet = "Data", col_types = c("text","text", "text", "text", "text", "numeric", "text", "text", "numeric", "text", "text", "text", "text", "text", "text","numeric", "numeric", "numeric","numeric", "text", "text")) 
LE<-LE[LE$Country=="CHL",]
LE<-LE[LE$`Unit Quantity`!=1266,]
LE<-LE[LE$`Unit Quantity`!=2215,]

LEI<-as.data.frame(matrix(nrow=nrow(LE),ncol=ncol(PriceDataBase)))
names(LEI)<-names(PriceDataBase)
LEI$`Country Code`<-LE$Country
LEI$Entity<-LE$Entity
LEI$`Entity Type`<-
LEI$`Mecanismo de Compra`<-LE$`Mecanismo de compra`
LEI$Region<-LE$`Region/Municipio`
LEI$`Therapeutic Area`<-LE$`Therapeutic Area`
LEI$`Country Product Name`<-LE$`Generic Product Name`
LEI$`Subunits per Unit`<-LE$`Subunits per unit`
LEI$`Catalog Name`<-LE$`Catalog Name`
LEI$Supplier<-LE$Supplier
LEI$Manufacturer<-LE$Manufacturer
LEI$`Purchase Year`<-LE$`Purchase Year`
LEI$`Total Amount`<-LE$`Total Amount`
LEI$`Unit Quantity`<-LE$`Unit Quantity`
LEI$`Min Unit Price USD`<-LE$`Unit Price USD (minimum unit)`
LEI$`PAHO Min Unit Price USD`<-LE$`PAHO Unit price (minimum unit)`
LEI$Patent<-LE$`Patent?`
LEI$Observaciones<-LE$Observaciones
write.csv(LEI,"~/RStudio/PAHO/Scrappers/Chile/LEISH.csv" )


CHLFinal<-rbind(CHLFinal,LEI)
CHL2 <- read_excel("~/OneDrive - Pan American Health Organization/Price Analysis/CHL/Hearts data CHL.xlsx", col_types = c("text", "text", "text", "text", "text", "text", "numeric", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text"))
temp<-as.data.frame(matrix(nrow=nrow(CHL2),ncol=ncol(PriceDataBase)))
names(temp)<-names(PriceDataBase)
temp$`Country Code`<-"CHL"
temp$Entity<-CHL2$Entity
temp$`Entity Type`<-"Central Nacional de Abastecimiento"
temp$`Therapeutic Area`<-CHL2$`Therapeutic Area`
temp$`Country Product Name`<-CHL2$`Generic Product Name`
temp$`Catalog Name`<-CHL2$`Catalog Name`
temp$Supplier<-CHL2$Supplier
temp$`Purchase Year`<-CHL2$`Purchase Year`
temp$`Total Amount`<-CHL2$`Total Amount`
temp$`Unit Quantity`<-CHL2$`Unit Quantity`
temp$`Min Unit Price USD`<-CHL2$`Unit Price USD (minimum unit)`
temp$`PAHO Min Unit Price USD`<-CHL2$`PAHO Unit price (minimum unit)`
temp$Patent<-CHL2$`Patent?`
temp$Genérico<-CHL2$`Patent?`
temp$Genérico[grepl("Patente",temp$Genérico)]<-NA
write.csv(temp,"~/RStudio/PAHO/Scrappers/Chile/Hearts.csv" )


CHLFinal<-rbind(CHLFinal,temp)
CHLFinal$Observaciones<-"SDC"
CHLFinal$Observaciones[grepl("\\+",CHLFinal$`Catalog Name`)]<-"FDC"
CHLFinal$Observaciones[grepl("insu",CHLFinal$`Country Product Name`, ignore.case = TRUE)]<-"INSULIN"

setwd("~/OneDrive - Pan American Health Organization/Price Analysis/PriceDB")
xlsx::write.xlsx2(CHLFinal,"ChileFinal.xlsx")

PriceDataBaseMAR31 <- read_excel("~/RStudio/PAHO/PriceDataBaseMAR31.xlsx", col_types = c("skip", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric","text", "text", "text", "date", "text", "numeric", "numeric", "numeric", "numeric", "text", "text", "text"))

TODOJUNTO<-rbind(PriceDataBaseMAR31,FinalCenabast)

TODOJUNTO<-TODOJUNTO%>%
  distinct(`Country Product Name`, `Country Product Description`, `Min Unit Price USD`, `Total Amount`, `Unit Quantity`, .keep_all = TRUE)

#################


setwd("~/OneDrive - Pan American Health Organization/Price Analysis/PriceDB")
xlsx::write.xlsx2(TODOJUNTO,"PriceDataBase.xlsx")

test<-CHLMAP%>%
  filter(!is.na(CatalogName))%>%
  distinct(CatalogName, .keep_all = TRUE)%>%
  dplyr::select(`Nombre producto genérico`, CatalogName)

########


