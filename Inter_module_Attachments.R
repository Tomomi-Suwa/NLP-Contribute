#-------------------------------------------------------------------
#Workflow to attach Multimedia (MM), Collection Event(CE) and Taxonomy records
#to Catalogue records in EMu
#steps 6-
#See Steps 1-5 to create batch import spreadsheets
#-------------------------------------------------------------------

#STEP 6:Attach MM to Catalogue records####
#============================================================
library(tidyr)
#6a:Create and upload MM file to spread
MM<-read.csv("LA_MM_to_spread.csv")#this is the file you create in Step 3g,
#containing MMirn and CAtGroup
names(MM)
MM$MMirn<-as.character(MM$MMirn)
MM$CATGroup<-as.character(MM$CAtGroup)
#Sort by Note
MM<-MM[order(MM$CAtGroup),]

#6b: creates a new columna *This step is important to spread*
MM$id.row<-sequence(rle(MM$CATGroup)$length)
MM$id.row<-as.character(MM$id.row)

#6c: Sort by specimen and spread - importatnt to sort first when the same specimen numbers are scattered in the df
library(dplyr)
MM<-MM[order(MM$CATGroup),]
spread_m<-spread(MM,
                 key=id.row,
                 value=MMirn,
                 fill="")
#6d: Create a spreadsheet "LA_MM_spread.csv"
#sort by CatGroup
spread_m$CATGroup<-as.integer(spread_m$CATGroup)
str(spread_m)
spread_m<-spread_m[order(spread_m$CATGroup),]
names(spread_m)
#write.csv(spread_m, "LA_MM_spread.csv", row.names = FALSE)

#6e: Upload CATirn spreadsheet to add CATirn by full_join
CATirn.df<-read.csv("LA_CATirn_Grouping.csv")
names(CATirn.df)

Attach_MM_CAT<-spread_m %>%
  full_join(CATirn.df,by = "CATGroup")
#6F: delete unnecesary columns and rename
Attach_MM_CAT<-Attach_MM_CAT[, c("1","2","3","4","CatIRN")]

#reorder columns
Attach_MM_CAT<-Attach_MM_CAT[,c(5,1,2,3,4)]
#rename columns
library(data.table)
setnames(Attach_MM_CAT, old=c("CatIRN","1", "2", "3", "4"), 
         new=c("irn", "MulMultiMediaRef_tab(1).irn", "MulMultiMediaRef_tab(2).irn", 
               "MulMultiMediaRef_tab(3).irn", "MulMultiMediaRef_tab(4).irn"))
#6g:Create a csv to attach MM to CAT
#write.csv(Attach_MM_CAT, "LA_Attach_MM_to_CAT.csv", row.names = FALSE)

#STep 7 Attach CE to Catalog_sightins####
#=======================================================================
#7a: Parepare dataframes
CEirn.df<-read.csv("LA_CEirn_with_Grouping.csv") #created in step 5h
CATirn.df<-read.csv("LA_CATirn_Grouping.csv")#created in step 4i
head(CEirn.df)
str(CEirn.df)
head(CATirn.df)
str(CATirn.df)
CATirn.df$CEGroup<-as.character(CATirn.df$CEGroup)
CEirn.df$CEGroup<-as.character(CEirn.df$CEGroup)

#7b: left join two df 
library(dplyr)
AttachCE<-CATirn.df %>%
  left_join(CEirn.df, by="CEGroup")
names(AttachCE)

#7c: subset only columns we need
AttachCE2<- subset(AttachCE, select = c("CatIRN", "Ceirn"))

#7e: rename CEirn
library(data.table)
setnames(AttachCE2, old=c( "CatIRN", "Ceirn"), new=c("irn", "CatSightingsEventsRef_tab(1).irn"))

#7f: Create a csv to attach CE to CAT
#write.csv(AttachCE2, "LA_Attach_CE_to_CAT.csv", row.names = FALSE)


#STep 8 Attach Taxonomy to Catalog_sightins####
#=======================================================================
#NOTE: If you already uploaded TaxonomyIRN column when uploading Catalogue spreadsheet, this skip this step. Already done.
#Merge TAXirn (from raw dataframe)  and CATirn (from CAT_irn_with_groups.csv, three columns "CATirn"	"CE_Group"	"MMnote"
) by Grouping
#8a: Format CATirn 
#CATirn.df<-read.csv("LA_CATirn_Grouping.csv")
#head(CATirn.df)

#set the Grouping column as character
#CATirn.df$CATGroup<-as.character(CATirn.df$CATGroup)
#select only necessary columns
#CATirn.df<-CATirn.df[, c("CatIRN", "CATGroup")]

#8b:Format Taxirn
#subset only necessary raws
#str(raw)
#TAX<-raw[, c("TaxIRN", "Grouping")]
#TAX$Grouping<-as.character((TAX$Grouping))
#library(dplyr)

#8c: left_joining CATirn and TAXirn
#Attach.TAX<-CATirn.df %>%left_join(TAX, by=c("CATGroup"="Grouping"))
#names(Attach.TAX)

#8d: Convert NA as blank (use only if there were no IRN for given taxonomy)
#Attach.TAX$TaxIRN<-as.character((Attach.TAX$TaxIRN))
#Attach.TAX$TaxIRN[is.na(Attach.TAX$TaxIRN)]<-" "

#8e:select necesary columns rename and re-arrange them
#Attach.TAX2<-Attach.TAX[,c("CatIRN", "TaxIRN")]
#library(data.table)
#setnames(Attach.TAX2, old=c("TaxIRN", "CatIRN"), 
#         new=c("IdeTaxonRef_tab(1).irn","irn"))
#re-arrange the order
#Attach.TAX2<-Attach.TAX2[c("irn","IdeTaxonRef_tab(1).irn")]
#head(Attach.TAX2)

#8f: only get unique CATirn 
#Attach.TAX3<-unique(Attach.TAX2)
#nrow(Attach.TAX3)
#write.csv(Attach.TAX3, "LA_Attach_TAX_CAT.csv", row.names=FALSE)

#Step 9: Double check to make sure that Taxnomy was attached correctly####
#=======================================================================
#9a: Add CATirn IRN number to raw ("LA_00JUL2018.csv") file by 
#joining raw and CATirn dataframe
names(CATirn.df)
names(raw)
str(CATirn.df)
str(raw)
raw$Grouping<-as.character(raw$Grouping)
CATirn.df$CATGroup<-as.character(CATirn.df$CATGroup)

raw_irn<-raw%>% left_join(CATirn.df, by=c("Grouping"="CATGroup"))
head(raw_irn)

#9b: Now compare raw_irn with "LC_CAT_report_to_check_Tax_Creater.csv"
#(Create this file from Emu Detail View)
CAT_report<-read.csv("LA_CAT_report_to_check_Tax_Creater.csv")
str(CAT_report)
str(raw_irn)
TAX.Comp<-raw_irn%>% left_join(CAT_report, by=c("CatIRN"="CATirn"))
names(TAX.Comp)
TAX.Com2<- subset(TAX.Comp, select = c(FullTaxonomyName, Taxon_from_EMu, CatIRN))
head(TAX.Com2)#Tax_from_Emu is from EMu and FullTAxonomyName is from raw data

TAX.Com2$FullTaxonomyName<-as.character(TAX.Com2$FullTaxonomyName)
TAX.Com2$Taxon_from_EMu<-as.character(TAX.Com2$Taxon_from_EMu)

TAX.Com2$TAXcheck<-TAX.Com2$FullTaxonomyName==TAX.Com2$Taxon_from_EMu

####Issue: Still need to figure out how to test for equality, AFTER removing author names
#In other words test if genus and epithet match between two columns
TAX.Com3<-TAX.Com2[ which(TAX.Com2$TAXcheck=='FALSE'), ]

