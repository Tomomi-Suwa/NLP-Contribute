# NLP-Contribute
  This is a workflow to import live plant photos submitted through NLP website to EMu. All of the photos submitted through "Contribute" will be processed using this workflow. 


# Steps 1-5: Create EMu spreadsheets for each module (Use "EMu_batch_import_spreadsheet.R")
We create Multimedia (MM), Catalogue, and Collection Event (CE) spreadsheets 
Steps 1 &2: Upload and format raw dataframe<br />
Step 3: Create a spreadsheet for MM batch import <br />
Step 4: Create a spreadsheet for Catalogue batch import<br />
Step 5: Create a spreadsheet for CE batch import<br />

# Steps 6-9: Attach MM, CE, Taxonomy records to Catalogue modules (Use "Inter_module_attachments.R")
Step 6: Attach MM to Catalogue record<br />
Step 7: Attach CE to Catalog_sightins<br />
Step 8: Attach Taxonomy to Catalog_sightins<br />
Step 9: Double check to make sure that Taxnomy was attached correctly<br />