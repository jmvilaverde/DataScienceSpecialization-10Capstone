
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

getJSON <- function(r_limit=10000, clean=FALSE){
#Get data from file
# library(rjson)
library(jsonlite)
require(dplyr)

basePath <- paste(directoryRaw, "yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_", sep = "/")

files <- c("business", "checkin", "tip", "user", "review")
json_file <<- list()
rds_file <<- list()
json_data <<- list()

#Load rds
for (set in files) {
        json_file[set] <<- paste(basePath, set, ".json", sep ="")
        rds_file[set] <<- paste(basePath, set, ".rds", sep ="")

        print(json_file[set])
        print(timestamp())
         if (file.exists(as.character(json_file[set])) && !file.exists(as.character(rds_file[set]))){
                 
                 #Only applies a limit if file is Review
                 if (set=="review") {limit = r_limit} else {limit = -1}
                
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
         else{
                #Load rds 
                if (file.exists(as.character(rds_file[set]))) {
                        print("Loading RDS file into object json_data")
                        json_data[[set]] <<- readRDS(file=as.character(rds_file[set]))
                }
         }

}

#Separate  "business" file into state files
states <- getDistinct("business", "state")
states_list <- paste(directoryRaw,"state",sep="/")
if(!dir.exists(states_list)){dir.create(states_list)}
for (i in 1:nrow(states)) {
        element <- states[i,1]
         dir_state <- paste(states_list,element,sep="/")
         if (!dir.exists(as.character(dir_state))){
                 print(paste("Creating:",dir_state,sep=""))
                 dir.create(dir_state)
                 print(element)
         }
         
         #business files per State
         business_file_RDS <- paste(dir_state,"/","business",".RDS",sep="")
         overwrite <- FALSE
         if(!file.exists(business_file_RDS) || overwrite){
                 aux <- getDataFilteredByState(object="business", filter=element)
                 saveRDS(object= aux,file = business_file_RDS)
         }
         
         #Checkin files per business per State
         overwrite_checkin = TRUE
         
         checkin_file_RDS <- paste(dir_state,"/","checkin",".RDS",sep="")
         if((!file.exists(checkin_file_RDS) || overwrite_checkin) && file.exists(business_file_RDS)){
                aux <- readRDS(business_file_RDS)
                json_data[["checkin"]] %>% 
                        semi_join(aux, by = "business_id") -> aux_checkin
                saveRDS(object= aux_checkin,file = checkin_file_RDS)
                 
         }
         
}

#Filter and save



}

getDistinct <- function(object="business", column="state") {
        pos = which(colnames(json_data[[object]])==column)
        select(json_data[[object]], pos) %>%
                distinct() %>% arrange()
}

getDataFilteredByState <- function(object="business",filter="AZ"){
        json_data[[object]] %>% 
                filter(state==filter)
}

main <- function(){
        getData()
        getJSON()
        
}