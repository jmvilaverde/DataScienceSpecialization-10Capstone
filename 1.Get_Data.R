
#Set URL Data path
URLData <<- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/yelp_dataset_challenge_academic_dataset.zip"

#Set directory where it's to be stored
directoryRaw <<- "data/raw"

#Create directory, if it doesn't exists
if (!dir.exists(directoryRaw)) dir.create(directoryRaw, recursive=TRUE)

fileZip <<- paste(directoryRaw, "yelp_dataset_challenge_academic_dataset.zip", sep = "/", collapse = NULL)

if (!file.exists(fileZip))
        {
                download.file(URLData, fileZip)
                unzip(fileZip, exdir = directoryRaw)
        }

#Get data from file
library(rjson)
basePath <- paste(directoryRaw, "yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_", sep = "/")

json_file_business <- paste(basePath, "business.json", sep ="")


#CHECKPOINT!! 20151007
if (file.exists(json_file_business))
        json_data_business <- fromJSON(paste(readLines(json_file_business), collapse=","))
        #json_data_business <- fromJSON(json_file_business)


main <- function(){
        getData()
        readData(directoryRaw)
        
}
#Code from
#https://www.safaribooksonline.com/blog/2014/01/13/reading-and-parsing-external-data-in-r/

df <- data.frame()

for(n in json_data_business){
        business_id <- n$business_id
        full_address <- n$full_address
        newrow <- data.frame(business_id, full_address, stringsAsFactors=FALSE, check.rows=FALSE)
        df <- rbind(df, newrow)
}