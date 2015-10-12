
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
require(dplyr)

basePath <- paste(directoryRaw, "yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_", sep = "/")

files <- c("business", "checkin", "review", "tip", "user")
json_file <- list()
json_data <- list()


limit = 100

for (set in files) {
        json_file[set] <- paste(basePath, set, ".json", sep ="")

        print(json_file[set])
         if (file.exists(as.character(json_file[set]))){
                 data <- fromJSON(paste("[",paste(readLines(as.character(json_file[set]), n=limit), 
                                                             collapse=","),"]"), simplifyDataFrame = TRUE)
                
                # Forum solution to  flatten file
                # https://class.coursera.org/dsscapstone-005/forum/thread?thread_id=24
                  json_data[set] <- flatten(data)
        }

}

}

main <- function(){
        getData()
        getJSON()
        
}