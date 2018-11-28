#Workflow to upload NLP images submitted through the website
#=====================================================================

#STEP 1:Set working directory and upload the raw data
#=====================================================================

localDir <- Sys.getenv("LOCAL_DIR")
origdir <- getwd()

raw<-read.csv("LA_00NOV2018.csv")

#STEP 2:Format raw file####
#=====================================================================
#2a: Clean the file names using Bulk Rename Utility
#2b:Rotate the photos to the correct orientation, upload all the images in LightRoom and save.as jpg. 
#This step ensures that images displays correctly in EMu
#2c:Add the updated file name on raw dataframe
NewFileName<-list.files(pattern = "\\.jpg$")
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
nrow(MM2)#90


#3e: Replace : to \
MM2$Multimedia
MM2$Multimedia <- gsub(":", "\\", MM2$Multimedia, fixed=TRUE)#note: need two \ because one is to escape the regular expressoin
MM2$Multimedia#looks weird but when you write.csv, it's ok

#create a csv file for import
#write.csv(MM2,"LA_MM.csv",row.names=FALSE)

#3f batch import "LA_MM.csv" in Emu's Multimedia module

#3g: Create a spreadsheet with Catalogue IRN and MMGrouping and CEGrouping from EMu "List View"
