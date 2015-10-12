
getData <- function(){
#Set URL Data path
URLData <<- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/yelp_dataset_challenge_academic_dataset.zip"

#Set directory where it's to be stored
directoryRaw <<- "data/raw"

#Create directory, if it doesn't exists
if (!dir.exists(directoryRaw)) dir.create(directoryRaw, recursive=TRUE)

fileZip <- paste(directoryRaw, "yelp_dataset_challenge_academic_dataset.zip", sep = "/", collapse = NULL)

if (!file.exists(fileZip))
        {
                download.file(URLData, fileZip)
                unzip(fileZip, exdir = directoryRaw)
        }
}

getJSON <- function(){
#Get data from file
# library(rjson)
library(jsonlite)
basePath <- paste(directoryRaw, "yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_", sep = "/")

json_file_business <- paste(basePath, "business.json", sep ="")


#CHECKPOINT!! 20151007
if (file.exists(json_file_business))
        json_data_business <<- fromJSON(paste("[",paste(readLines(json_file_business), collapse=","),"]"), simplifyDataFrame = TRUE)
        #json_data_business <- fromJSON(json_file_business)

business <- with(json_data_business,cbind(business_id, full_address, open, as.character(city), review_count, name,
                                          neighborhoods, longitude, as.character(state), latitude, as.character(type))) 
business <-  as.data.frame(business, stringsAsFactors = FALSE)
colnames(business) <- c("business_id", "full_address", "open", "city", "review_count", "name",
                        "neighborhoods", "longitude", "state", "latitude", "type")

business_categories <- with(json_data_business,cbind(business_id, as.character(categories))) 
business_categories <-  as.data.frame(business_categories, stringsAsFactors = FALSE)
colnames(business_categories) <- c("business_id", "categories")

business_hours <- with(json_data_business,cbind(business_id, hours)) 
business_hours <-  as.data.frame(business_hours, stringsAsFactors = FALSE)

business_attributes <- with(json_data_business,cbind(business_id, as.character(attributes))) 
business_attributes <-  as.data.frame(business_attributes, stringsAsFactors = FALSE)
colnames(business_attributes) <- c("business_id", "attributes")



}

main <- function(){
        getData()
        getJSON()
        
}