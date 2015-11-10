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
getJSON <- function(r_limit=10000, clean=FALSE){
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

###Separate  "business" file into state files

#Get unique states
states <- getDistinct("business", "state")

#Directory for states files
states_list <- paste(directoryRaw,"state",sep="/")
if(!dir.exists(states_list)){dir.create(states_list)}

#Precess for each state
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

        #Review files per business per State
        createFileRDSbyState(overwrite=FALSE, dir=dir_state, bs_file_RDS=business_file_RDS, dataset="review")
         

        ###################################
        #Process Review file
        #1.Set block size
        #block_size = 
        #2.Point to initial block
        #3.Read block
        #4.Save block into file RDS
        #5.Final block?
        #5.NO.GOTO 3
        ###################################
        
         
}

#Filter and save



}

createFileRDSbyState <- function(overwrite, dir, bs_file_RDS, dataset="checkin"){
        
        print(paste("File to generate",dataset))
        
        file_RDS <- paste(dir,"/",dataset,".RDS",sep="")
        if((!file.exists(file_RDS) || overwrite) && file.exists(bs_file_RDS)){
                aux <- readRDS(bs_file_RDS)
                json_data[[dataset]] %>% 
                        semi_join(aux, by = "business_id") -> aux_data
                
                print(paste("Saving RDS file business:",dataset))
                print(timestamp())
                
                saveRDS(object= aux_data,file = file_RDS)
                
        }
        else{
                if(file.exists(file_RDS)){print("File RDS exists")}
                if(!file.exists(bs_file_RDS)){print("File business RDS doesn't exists")}
                print(timestamp())
        }
        
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