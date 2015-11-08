
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

getJSON <- function(limit=1000, clean=FALSE){
#Get data from file
# library(rjson)
library(jsonlite)
require(dplyr)

basePath <- paste(directoryRaw, "yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_", sep = "/")

files <- c("business", "checkin", "review", "tip", "user")
json_file <<- list()
rds_file <<- list()
json_data <<- list()

#for (set in files) {write_csv(as.data.frame(json_data[set]),path = as.character(csv_file[set]))}

for (set in files) {
        json_file[set] <<- paste(basePath, set, ".json", sep ="")
        rds_file[set] <<- paste(basePath, set, ".rds", sep ="")

        print(json_file[set])
         if (file.exists(as.character(json_file[set]))){
                 data <- fromJSON(paste("[",paste(readLines(as.character(json_file[set]), n=limit), 
                                                             collapse=","),"]"), simplifyDataFrame = TRUE)
                
                # Forum solution to  flatten file
                # https://class.coursera.org/dsscapstone-005/forum/thread?thread_id=24
                  json_data[[set]] <<- flatten(data)
                
                if(file.exists(as.character(rds_file[set])) && clean){
                          file.remove(as.character(rds_file[set]))
                }
                  
                if(!file.exists(as.character(rds_file[set]))){
                        saveRDS(json_data[[set]],file = as.character(rds_file[set]))
                }
        }

}

}

getBussinesOnCity <- function(filter_city="Phoenix"){
        names(json_data[["business"]])
        json_data[["business"]] %>% 
                filter(city==filter_city) %>%
                distinct(city) %>% arrange(city) %>% select(1:3, city)
}

main <- function(){
        getData()
        getJSON()
        
}