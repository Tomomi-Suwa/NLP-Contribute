#-------------------------------------------------------------------
#Workflow to upload NLP images submitted through the website
#steps 6-
#-------------------------------------------------------------------

#STEP 6:Attach MM to Catalogue records
#============================================================
library(tidyr)
MM<-read.csv("LA_MM_to_spread.csv")#this is the file you create in Step 2g
names(MM)
str(MM)
# MM$MM.irn<-as.character(MM$MM.irn)
MM$CATGroup<-as.character(MM$CAtGroup)
# 
#sort by Note
MM<-MM[order(MM$CAtGroup),]
#creates a new columna *This step is important
MM$id.row<-sequence(rle(MM$CATGroup)$length)
MM$id.row<-as.character(MM$id.row)

#sort by specimen-importatnt step when the same specimen numbers are scattered in the df
MM<-MM[order(MM$CATGroup),]
spread_m<-spread(MM,
                 key=id.row,
                 value=MMirn,
                 fill="")
#sort by Note
spread_m$CATGroup<-as.integer(spread_m$CATGroup)
str(spread_m)
spread_m<-spread_m[order(spread_m$CATGroup),]
names(spread_m)
#write.csv(spread_m, "LA_MM_spread.csv", row.names = FALSE)


CATirn.df<-read.csv("LA_CATirn_Grouping.csv")
names(CATirn.df)
Attach_MM_CAT<-spread_m %>%
  full_join(CATirn.df,by = "CATGroup")
#delete unnecesary columns
Attach_MM_CAT<-Attach_MM_CAT[, c(-1,-2,-8)]
#reorder columns
Attach_MM_CAT<-Attach_MM_CAT[,c(5,1,2,3,4)]
#rename columns
library(data.table)
setnames(Attach_MM_CAT, old=c("CatIRN","1", "2", "3", "4"), 
         new=c("irn", "MulMultiMediaRef_tab(1).irn", "MulMultiMediaRef_tab(2).irn", 
               "MulMultiMediaRef_tab(3).irn", "MulMultiMediaRef_tab(4).irn"))

#write.csv(Attach_MM_CAT, "LA_Attach_MM_to_CAT.csv", row.names = FALSE)

#STep 7 Attach CE to Catalog_sightins####
########################################################################
CEirn.df<-read.csv("LA_CEirn_with_Grouping.csv") #manucally create this from EMu detail view
CATirn.df<-read.csv("LA_CATirn_Grouping.csv")
head(CEirn.df)
str(CEirn.df)
head(CATirn.df)
str(CATirn.df)
CATirn.df$CEGroup<-as.character(CATirn.df$CEGroup)
CEirn.df$CEGroup<-as.character(CEirn.df$CEGroup)

#left join
library(dplyr)
AttachCE<-CATirn.df %>%
  left_join(CEirn.df, by="CEGroup")
names(AttachCE)
#subset only columns we need
AttachCE2<- subset(AttachCE, select = c("CatIRN", "Ceirn"))
#rename CEirn
library(data.table)
setnames(AttachCE2, old=c( "CatIRN", "Ceirn"), new=c("irn", "CatSightingsEventsRef_tab(1).irn"))

#write.csv(AttachCE2, "LA_Attach_CE_to_CAT.csv", row.names = FALSE)


#STep 8 Attach Taxonomy to Catalog_sightins####
############################################################################
#Merge TAXirn (from raw dataframe)  and CATirn (from CAT_irn_with_groups.csv) by Grouping
###Format CATirn 
CATirn.df<-read.csv("LA_CATirn_Grouping.csv")
head(CATirn)

#set the Grouping column as character
CATirn.df$CATGroup<-as.character(CATirn.df$CATGroup)
str(CATirn)
#select only necessary columns
CATirn.df<-CATirn.df[, c("CatIRN", "CATGroup")]

###Format Taxirn
#subset only necessary raws
str(raw)
TAX<-raw[, c("TaxIRN", "Grouping")]
TAX$Grouping<-as.character((TAX$Grouping))
library(dplyr)
Attach.TAX<-CATirn.df %>%left_join(TAX, by=c("CATGroup"="Grouping"))
names(Attach.TAX)

#Convert NA as blank (use only if there were no IRN for given taxonomy)
Attach.TAX$TaxIRN<-as.character((Attach.TAX$TaxIRN))
Attach.TAX$TaxIRN[is.na(Attach.TAX$TaxIRN)]<-" "

#select necesary columns
Attach.TAX2<-Attach.TAX[,c("CatIRN", "TaxIRN")]
library(data.table)
setnames(Attach.TAX2, old=c("TaxIRN", "CatIRN"), 
         new=c("IdeTaxonRef_tab(1).irn","irn"))
#re-arrange the order
Attach.TAX2<-Attach.TAX2[c("irn","IdeTaxonRef_tab(1).irn")]
head(Attach.TAX2)
#only get unique CATirn 
Attach.TAX3<-unique(Attach.TAX2)
nrow(Attach.TAX3)
#write.csv(Attach.TAX3, "LA_Attach_TAX_CAT.csv", row.names=FALSE)

#Step 9: Double check to make sure that TAxnomy was attached corectly####
################################################################################
#Add CATirn IRN number to raw ("LA_00JUL2018.csv") file by joining raw and CATirn dataframe
names(CATirn.df)
names(raw)
str(CATirn.df)
str(raw)
raw$Grouping<-as.character(raw$Grouping)
CATirn.df$CATGroup<-as.character(CATirn.df$CATGroup)

raw_irn<-raw%>% left_join(CATirn.df, by=c("Grouping"="CATGroup"))
head(raw_irn)

#Now compare raw_irn with "LC_CAT_report_to_check_Tax_Creater.csv (Create this file from Emu detail View)
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

#Issue: Still need to figure out how to test for equality, AFTER removing author names
#In other words test if first two words match between two columns
TAX.Com3<-TAX.Com2[ which(TAX.Com2$TAXcheck=='FALSE'), ]

