#-------------------------------------------------------------------
#Workflow to upload NLP images submitted through the website
#steps 1-5
#-------------------------------------------------------------------

#STEP 1:Set working directory and upload the raw data####
#=====================================================================

localDir <- Sys.getenv("LOCAL_DIR")
origdir <- getwd()
list.files()
raw<-read.csv("LA_00NOV2018.csv")

#STEP 2:Format raw file####
#=====================================================================
#2a: Clean the file names using Bulk Rename Utility
#2b:Rotate the photos to the correct orientation, upload all the images in LightRoom and save.as jpg. 
#This step ensures that images displays correctly in EMu
#2c:Add the updated file name on raw dataframe
NewFileName<-list.files(pattern = "\\.jpg$|\\.JPG$")
names(raw)
#sort by File.Name
raw2 <- raw[order(raw$FileName),] 
raw3<-cbind(NewFileName, raw2)

#2d: format DateCreated column
library(lubridate)
raw3$DatePhotographed<-as.character(raw3$DatePhotographed)
raw3$DatePhotographed2<-as.Date(raw3$DatePhotographed)
raw3$DatePhotographed<-format(raw3$DatePhotographed, format = "%d-%b-%Y")
library(plyr)
apply(raw3,2,count)
names(raw3)

#STEP3: Creating spreadsheet for Multimedia batch import####
#==================================================================
#IMPORTANT: Before importing the images, save all the images in LightRoom with correct orientatoin. 
#Image orientatoin may not show up correctly otherwise
#3a: pick columns of interests
library(dplyr)
MM<-select(raw3, DatePhotographed,NewFileName, PartyIRN,Grouping)
library(data.table)
setnames(MM, old=c("NewFileName", "DatePhotographed", "PartyIRN", "Grouping"), 
         new=c("MulTitle", "DetResourceDetailsDate01", "MulMultimediaCreatorRef_tab1.irn", "NteText0"))

#3b: Creating new EMu fields (columns)
MM$MulTimedia.path<-as.character("::serenity:Imaging_Scratch:Action:Live Plant Photos:LA_00NOV2018:")
MM$Multimedia<-paste(MM$MulTimedia.path,MM$MulTitle,sep="")
MM$MulDescription<-"live plant photo from Contribute"
MM$DetMediaRightsRef.irn<-"46"
MM$DetResourceDetailsDescription_tab1<- "Created"
MM$DetContributorRole_tab1<- "Photographer"
MM$DetSubject_tab1<-"NLP"
MM$SecDepartment_tab1<-"Action"
MM$SecDepartment_tab2<-"Technology"
MM$AdmPublishWebNoPassword<-"Yes"#for INTERnet
MM$AdmPublishWebPassword<-"Yes"#for INTRAnet
MM$MulMultimediaCreatorRole_tab1<-"Photographer"
MM$MulMultimediaCreatorRole_tab2<-"Creator"
MM$MulMultimediaCreatorRef_tab2.irn<-"173648"
names(MM)

#3c: Re-order columns so that it will be the same as the MM template  
MM2<-MM[c("Multimedia","MulTitle", "MulDescription","DetMediaRightsRef.irn",
          "DetResourceDetailsDate01","DetResourceDetailsDescription_tab1","DetSubject_tab1","SecDepartment_tab1","SecDepartment_tab2",
          "AdmPublishWebNoPassword","AdmPublishWebPassword","NteText0","MulMultimediaCreatorRole_tab1",
          "MulMultimediaCreatorRole_tab2","MulMultimediaCreatorRef_tab1.irn","MulMultimediaCreatorRef_tab2.irn")]
#3d: rename the columns
setnames(MM2, old=c("DetResourceDetailsDescription_tab1", "DetSubject_tab1", "SecDepartment_tab1", 
                    "SecDepartment_tab2","MulMultimediaCreatorRole_tab1","MulMultimediaCreatorRole_tab2",
                    "MulMultimediaCreatorRef_tab1.irn","MulMultimediaCreatorRef_tab2.irn", "DetResourceDetailsDate01"), 
         new=c("DetResourceDetailsDescription_tab(1)", "DetSubject_tab(1)", "SecDepartment_tab(1)", "SecDepartment_tab(2)",
               "MulMultimediaCreatorRole_tab(1)","MulMultimediaCreatorRole_tab(2)","MulMultimediaCreatorRef_tab(1).irn",
               "MulMultimediaCreatorRef_tab(2).irn", "DetResourceDetailsDate0(1)"))
head(MM2)
names(MM2) #should always be 16 total
nrow(MM2)#98


#3e: Replace : to \
MM2$Multimedia
MM2$Multimedia <- gsub(":", "\\", MM2$Multimedia, fixed=TRUE)#note: need two \ because one is to escape the regular expressoin
MM2$Multimedia#looks weird but when you write.csv, it's ok

#create a csv file for import
#write.csv(MM2,"LA_MM.csv",row.names=FALSE)

#3f batch import "LA_MM.csv" in Emu's Multimedia module

#3g: Manyally create a spreadsheet with MM.IRN MMGrouping and CEGrouping from EMu "List View"

#step 4: Creating spreadsheet for Catalogue batch import####
#==================================================================
#4a: select the columns of interst
library("dplyr")
cat<-select(raw3, Grouping,PartyIRN,DatePhotographed,SiteIRN)

setnames(cat, old=c( "DatePhotographed", "PartyIRN", "Grouping","SiteIRN"), 
         new=c("ColEarliestDateCollected", "ColMammalsCollectorRef.irn", "NotFisNotes01", "ColSiteLocationRef.irn"))

#4b: Creating new EMu fields (columns)
cat$CatDepartment<-"Action"
cat$CatCatalog<-"Botany"
cat$CatCatalogSubset<-"Sighting"
cat$GeoSeries<- "NLP"
cat$GeoCatalogNo<-1:nrow(cat)+5013000
cat$CatMammalsProject_tab1<-"Neotropical Live Plants"
cat$SecDepartment_tab1<-"Action"
cat$AdmPublishWebNoPassword<- "Yes"
cat$AdmPublishWebPassword<- "Yes"

#4c: Rename columns to EMu field names 
library(data.table)

#Re-order columns so that it will be the same as the sighting template 
cat <- cat[c( "CatDepartment",	"CatCatalog",	"CatCatalogSubset",	"GeoSeries",	
              "GeoCatalogNo",	"NotFisNotes01","CatMammalsProject_tab1",	"ColMammalsCollectorRef.irn",	
              "SecDepartment_tab1","AdmPublishWebNoPassword","AdmPublishWebPassword","ColEarliestDateCollected", "ColSiteLocationRef.irn")]


#4d: Slect only the unique rows 
#sort by specimen-importatnt step when the same specimen numbers are scattered in the dataframe
cat<-cat[order(cat$NotFisNotes01),]
#creates a new column to select unique rows. This column will be delated in cat3 dataframe
cat$unique<-sequence(rle(cat$NotFisNotes01)$length)
#subset unique=1
cat2<-cat[cat$unique==1,]
#delete the column "unique"
cat3<- subset( cat2, select = - unique)
names(cat3)
nrow(cat3)

#4e: Add Collection Event (CE) Observational Unit column (will be useful later when attaching CE to Catalogue)
cat4<-cat3 %>% 
  mutate(NotFisNotes02= group_indices_(cat3, .dots=c("ColEarliestDateCollected","ColMammalsCollectorRef.irn", "ColSiteLocationRef.irn"))) 
#Adding the letters "ce" to avoid confusion with Observational Unit for MM
cat4$NotFisNotes02<- sprintf('ce%s', cat4$NotFisNotes02)
names(cat4)

#4f: Rename - important to add ( ) at the last stage because () would mess things up
setnames(cat4, old=c("CatMammalsProject_tab1", "SecDepartment_tab1","NotFisNotes01","NotFisNotes02"), 
         new=c("CatMammalsProject_tab(1)", "SecDepartment_tab(1)", "NotFisNotes0(1)", "NotFisNotes0(2)"))
names(cat4)#should always have 14

#4g: Create a csv file
#write.csv(cat4, "LA_CAT2.csv",row.names=FALSE)

#4h: Batch import LA_CAT.csv" in EMu's Catalogue Module 

#4i: Create a file with Catalogue IRN and CatalogueGrouping 
#and CEGrouping, "LA_CATirn_Grouping.csv"


#step 5: Creating spreadsheet for Collection Event (CE) batch impor####
#=======================================================================
#5a: Grouping (aka observational unit) files by unique plants#
ce<-cat2 %>% 
  mutate(Obs.Unit.ce = group_indices_(cat2, .dots=c("ColEarliestDateCollected"," ColMammalsCollectorRef.irn", "ColSiteLocationRef.irn"))) 
names(ce)

#5b: Creating new EMu fields (columns)
ce$ColCollectionType<-"Sighting"
ce$ColCollectionMethod<-"Photograph"
ce$ColParticipantRole_tab1<-"Collector"
ce$SigHowSighted<-"Camera"
ce$AdmPublishWebNoPassword<-"Yes"
ce$AdmPublishWebPassword<-"Yes"	
ce$SecDepartment_tab1<-"Action"

#5c: select the field of interest
ce2<-select(ce, ColCollectionType,	ColCollectionMethod,	ColSiteLocationRef.irn,
            ColMammalsCollectorRef.irn, ColParticipantRole_tab1,	SigHowSighted,
            AdmPublishWebNoPassword,	AdmPublishWebPassword,	SecDepartment_tab1,	
            ColEarliestDateCollected, Obs.Unit.ce)

as.data.frame(ce2)

names(ce2)#should always be 11 columns

#5d: Rename some column names as EMu's CE fields
setnames(ce2, old=c( "ColSiteLocationRef.irn", "ColParticipantRole_tab1", 
                     "SecDepartment_tab1", "Obs.Unit.ce",  "ColEarliestDateCollected", "ColMammalsCollectorRef.irn"), 
         
         new=c("ColSiteRef.irn", "ColParticipantRole_tab(1)", 
               "SecDepartment_tab(1)","NteText0","ColDateVisitedFrom","ColParticipantRef_tab(1).irn"))

names(ce2)


#5e: Pick only the unique Obesrvational Units
#sort by specimen-importatnt step when the same specimen numbers are scattered in the df
ce2<-ce2[order(ce2$NteText0),]
#creates a new columna
ce2$unique<-sequence(rle(ce2$NteText0)$length)
#subset unique=1
ce3<-ce2[ce2$unique==1,]
#delete column "unique"
ce4<- subset( ce3, select = - unique)
names(ce4)
nrow(ce4)

#5f: Create a csv file
#write.csv(ce4, "LA_CE.csv",row.names=FALSE)

#5g:Batch import LA_CE.csv" in EMu's CE Module 

#5h: Create a file with CE.IRN and CEGrouping "LA_CEirn_with_Grouping.csv"
