###
#Function to get data
###
getData <- function(){

#Set URL Data path
URLData <<- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/yelp_dataset_challenge_academic_dataset.zip"

#Set directory where it's to be stored
directoryRaw <<- "data/raw"

#Create directory, if it doesn't exists
if (!dir.exists(directoryRaw)) dir.create(directoryRaw, recursive=TRUE)

#Set file for ZIP
fileZip <- paste(directoryRaw, "yelp_dataset_challenge_academic_dataset.zip", sep = "/", collapse = NULL)

if (!file.exists(fileZip)){
        download.file(URLData, fileZip)
        unzip(fileZip, exdir = directoryRaw)
        }
}
###
#End Function getData
###

###
#Function to getJSON data from files
#r-limit is the limit of lines to read from review files #TODO: Use it to extract the data in blocks. Find ideal block size.
#clean 
###
getJSON <- function(r_limit=100000, clean=FALSE){
#Get data from file
# library(rjson)
library(jsonlite)
require(dplyr)

#Base path for the JSON dataset
basePath <- paste(directoryRaw, "yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_", sep = "/")

#file types
files <- c("business", "checkin", "tip", "user", "review")

#Initialize variables
json_file <<- list()
rds_file <<- list()
json_data <<- list()

#Load rds
#Explore all file types
for (set in files) {
        #Define origin and destiny file for each file type
        json_file[set] <<- paste(basePath, set, ".json", sep ="")
        rds_file[set] <<- paste(basePath, set, ".rds", sep ="")

        #Log print
        print("Start processing of data:")
        print(timestamp())
        print(json_file[set])
        
        if (file.exists(as.character(json_file[set])) && !file.exists(as.character(rds_file[set]))){
                
                print("Create RDS file")
                print(timestamp())
                
                #Only applies a limit if file is Review
                if (set=="review") {limit = r_limit} else {limit = -1}
                
                data <- fromJSON(paste("[",paste(readLines(as.character(json_file[set]), n=limit), 
                                                     collapse=","),"]"), simplifyDataFrame = TRUE)
                
                
                # Forum solution to  flatten file
                # https://class.coursera.org/dsscapstone-005/forum/thread?thread_id=24
                json_data[[set]] <<- flatten(data)
                
                print(paste(set,"loaded into json_data"))
                print(timestamp())
                
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
                        print(timestamp())
                        json_data[[set]] <<- readRDS(file=as.character(rds_file[set]))
                        print(paste(set,"loaded into json_data"))
                        print(timestamp())
                        }
                }

        }

####################
#Process Review file
####################

#Set file path
basePath2 <- paste(directoryRaw, "yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_", sep = "/")
file_review <- paste(basePath2, "review.json", sep="")

#Get file length
#file_len <- length(readLines(file_review))
file_len <- 1569264
print(paste("File length:", file_len))
print(timestamp())

library(readr)
block_count = 1
begin_block = 0

overwrite_review <- FALSE
file_review_json <- as.character(json_file["review"])

if(file.exists(file_review_json)){
        while(begin_block < file_len){
                
                end_block = begin_block + r_limit
                
                file_review_RDS <- paste(basePath2, "review_", as.character(block_count),".rds", sep="")
                
                if(file_len<end_block) {end_block = file_len}
                                
                print(paste("Block:", block_count))
                print(paste("Block's begin:", begin_block+1))
                print(paste("Block's end:", end_block))
                
                if(!file.exists(file_review_RDS)&&!overwrite_review) {
                        data <- fromJSON(paste("[",paste(read_lines(file=file_review_json, skip = begin_block, 
                                                            n_max=r_limit), 
                                                 collapse=","),"]"), simplifyDataFrame = TRUE)
                        review_data <- flatten(data)
                
                        print(file_review_RDS)
                        saveRDS(review_data, file=as.character(file_review_RDS))
                }
                else{
                        print(paste("File exists",file_review_RDS))
                }
                begin_block = end_block
                block_count = block_count + 1
        }
}
else {
        print(paste("File doesn't exist:",file_review_json))
}

####################
#End Process Review file
####################
        

###Separate  "business" file into state files

#Get unique states
states <- getDistinct("business", "state")

#Directory for states files
states_list <- paste(directoryRaw,"state",sep="/")
if(!dir.exists(states_list)){dir.create(states_list)}

#Process for each state
for (i in 1:nrow(states)) {
        
        #Get state string
        element <- states[i,1]
        
        print(paste("Processing state:", element))
        print(timestamp())
        
        #Set directory state path 
        dir_state <- paste(states_list,element,sep="/")
        
         if (!dir.exists(as.character(dir_state))){
                 print(paste("Creating:",dir_state,sep=""))
                 print(timestamp())
                 
                 dir.create(dir_state)
         }
         
         #business files per State
         business_file_RDS <- paste(dir_state,"/","business",".RDS",sep="")
        
        #Check that indicates if business RDS file must be overwrite.
         overwrite <- FALSE
        
         if(!file.exists(business_file_RDS) || overwrite){
                 
                 #Get only data from business into that state
                 aux <- getDataFilteredByState(object="business", filter=element)
                 
                 print("Saving RDS file business:")
                 print(timestamp())
                 
                 saveRDS(object= aux,file = business_file_RDS)
         }
         
        #Checkin files per business per State
        createFileRDSbyState(overwrite=FALSE, dir=dir_state, bs_file_RDS=business_file_RDS, dataset="checkin")
        
        #Tip files per business per State
        createFileRDSbyState(overwrite=FALSE, dir=dir_state, bs_file_RDS=business_file_RDS, dataset="tip")

        #TODO: Review files per business per State
        createFileRDSbyState(overwrite=TRUE, dir=dir_state, bs_file_RDS=business_file_RDS, dataset="review")
        
        #TODO: user files
        
}

}

createFileRDSbyState <- function(overwrite, dir, bs_file_RDS, dataset="checkin"){
        
        print(timestamp())
        print(paste("File to generate",dataset))
        
        #File destiny for data
        file_RDS <- paste(dir,"/",dataset,".RDS",sep="")
        
        #Check existence of files
        if(file.exists(file_RDS)&& !overwrite){
                print("File RDS exists")
                return}
        if(!file.exists(bs_file_RDS)){
                print("File business RDS doesn't exists")
                return}
        
        #read business file into directory
        #Only business from this State
        aux <- readRDS(bs_file_RDS)
        
        #TODO: 
        if (dataset == "review"){
         files <- list.files(path="data/raw/yelp_dataset_challenge_academic_dataset", pattern="yelp_academic_dataset_review_")
         
         #Initialize data_filtered
         data_filtered = list()
         
         for(file in files){
                 
                file_to_process <- paste("data/raw/yelp_dataset_challenge_academic_dataset",file,sep="/")
                print(timestamp())
                print(paste("Read file:",file_to_process))
                #Read file
                temp_data <- readRDS(file_to_process)
                #Filter file
                temp_data %>% semi_join(aux, by = "business_id") -> temp_data_filtered
                nrow(temp_data_filtered)
                #Append to list
                data_filtered <- list(data_filtered, temp_data_filtered)
                nrow(data_filtered)
                     
         }
        }
        else{
        
                data_to_filter <- json_data[[dataset]]
                
                data_to_filter %>% 
                                semi_join(aux, by = "business_id") -> data_filtered
        }                
                        
                        print(paste("Saving RDS file business:",dataset))
                        print(timestamp())
                        
                        saveRDS(object= data_filtered,file = file_RDS)
                        
        
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