
###
#Function to init global variables and constants
###
initGlobal <- function(){

        #Set directory where it's to be stored
        directoryRaw <<- "data/raw"
        
}
###
#End function
###

###
#Function to download and unzip data
###
getData <- function(URLData = "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/yelp_dataset_challenge_academic_dataset.zip"){


        
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
#Function to getJSON data from files and transform into RDS
#r-limit is the limit of lines to read from review files
#overwrite indicates if is necessary to redo the RDS files
###
getJSON <- function(r_limit=100000, overwrite=FALSE){
        
        #####################
        # Initial variables #
        #####################
        
        library(jsonlite)
        require(dplyr)
        
        #Base path for the JSON dataset
        basePath <- paste(directoryRaw, "yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_", sep = "/")
        
        #file types
        #Review processed appart
        files <- c("business", "checkin", "tip", "user", "review")
        
        #Initialize variables
        json_file <- list()
        rds_file <- list()
        json_data <- list()
        
        for (set in files) {
                
                #Define origin and destiny file for each file type
                json_file[set] <- as.character(paste(basePath, set, ".json", sep =""))
                rds_file[set] <- as.character(paste(basePath, set, ".rds", sep =""))
        }

        #####################
        #Process Json files #
        #####################
        for (set in files) {
                
         if(set!="review"){
                #Log print
                print(timestamp())
                print("Start processing of data:")
                print(json_file[[set]])
                
                #Process file if json_file exists and rds_file doesn't exists
                if (file.exists(json_file[[set]]) && !file.exists(rds_file[[set]])){
                        
                        #Log print
                        print(timestamp())
                        print("Create RDS file")
                        
                        #Read Json file
                        data <- fromJSON(paste("[",paste(readLines(json_file[[set]], n=-1), 
                                                             collapse=","),"]"), simplifyDataFrame = TRUE)
                        
                        # Forum solution to  flatten file
                        # https://class.coursera.org/dsscapstone-005/forum/thread?thread_id=24
                        json_data[[set]] <- flatten(data)
                        
                        #Log print
                        print(timestamp())
                        print(paste(set,"Loaded json_file data into json_data"))
                        
                        #if file doesn't exists or user want to overwrite
                        if(!file.exists(as.character(rds_file[[set]])) || overwrite){
                                saveRDS(json_data[[set]],file = rds_file[[set]])
                                }
                        }
                 else{
                        #Load rds 
                        if (file.exists(rds_file[[set]])) {
                                #Log print
                                print(timestamp())
                                print("Loading RDS file into object json_data")
                                
                                #Read RDS file into json_data
                                json_data[[set]] <- readRDS(file=as.character(rds_file[[set]]))
                                
                                #Log print
                                print(timestamp())
                                print(paste(set,"Loaded into json_data"))
                                
                                }
                        }
        
                }
         
        } #End for (set in files)

        ###########################
        #Process Review Json file #
        ###########################
        
        #Set file path
        #basePath2 <- paste(directoryRaw, "yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_", sep = "/")
        #file_review <- paste(basePath, "review.json", sep="")
        file_review_json <- as.character(json_file["review"])
        
        #Get file length
        file_len <- 1569264
        if (is.null(file_len)) file_len <- length(readLines(file_review))
        
        #Log print
        print(timestamp())
        print(paste("File length:", file_len))
        
        
        library(readr)
        
        ##Initialize variables
        #Counter of blocks
        block_count = 1
        #Index block to start to process the block
        begin_block = 1
        
        
        if(file.exists(file_review_json)){
                
                #Continue until begin_block is equal or greater than file_len
                while(begin_block < file_len){
                        
                        #end of block is the begin of the block added to defined block size
                        end_block = begin_block + r_limit - 1
                        
                        if(file_len<end_block) {end_block = file_len}
                        
                        
                        #File name for partial RDS
                        file_review_RDS <- as.character(paste(basePath, "review_", as.character(block_count),".rds", sep=""))
                        
                        #Log print                                        
                        print(paste("Block:", block_count))
                        print(paste("Block's begin:", begin_block))
                        print(paste("Block's end:", end_block))
                        
                        
                        #If file doesn't exists or overwrite then create file
                        if(!file.exists(file_review_RDS) || overwrite) {
                                
                                #Get data from Json, process and save into RDS
                                data <- fromJSON(paste("[",paste(read_lines(file=file_review_json, skip = begin_block, 
                                                                    n_max=r_limit), 
                                                         collapse=","),"]"), simplifyDataFrame = TRUE)
                                review_data <- flatten(data)
                                saveRDS(review_data, file=file_review_RDS)
                        }
                        else{
                                print(paste("File exists:",file_review_RDS))
                        }
                        begin_block = end_block + 1
                        block_count = block_count + 1
                }
                
        }
        else {
                print(paste("File doesn't exist:",file_review_json))
        }
        
        #############################
        #End Process all Json files #
        #############################
        
        ######################################################
        #Separate  "business" file into states' business files #
        ######################################################
        
        #Get unique states from business json data
        states <- getDistinct(data = json_data, object = "business", column = "state")
        
        #Directory for states files
        states_list <- paste(directoryRaw,"state",sep="/")
        
        if(!dir.exists(states_list)){ dir.create(states_list) }
        
        number_of_states <- nrow(states)
        
        #Process for each state
        for (i in 1:number_of_states) {
                
                #Get state string
                element <- states[i,1]
                
                print(paste("Processing state:", element, ". ",(i/number_of_states*100),"% of progress."))
                print(timestamp())
                
                #Set directory state path 
                dir_state <- paste(states_list,element,sep="/")
                
                if (!dir.exists(as.character(dir_state))){
                         
                         #Log print
                         print(timestamp())
                         print(paste("Creating:",dir_state,sep=""))
                         
                         dir.create(dir_state)
                }
                 
                #Business files per State
                business_file_RDS <- paste(dir_state,"/","business",".RDS",sep="")
                
                
                if(!file.exists(business_file_RDS) || overwrite){
                         
                         #Get only data from business into that state
                         aux <- getDataFilteredByState(data = json_data, object = "business", state_filter = element)
                         
                         #Log print
                         print(timestamp())
                         print("Saving RDS file business:")
                         
                         saveRDS(object= aux,file = business_file_RDS)
                }
                 
                #Checkin files per business per State
                createFileRDSbyState(overwrite=overwrite, data = json_data, dir=dir_state, 
                                     bs_file_RDS=business_file_RDS, dataset="checkin")
                
                #Tip files per business per State
                createFileRDSbyState(overwrite=overwrite, data = json_data, dir=dir_state, 
                                     bs_file_RDS=business_file_RDS, dataset="tip")
        
                #Review files per business per State
                createFileRDSbyState(overwrite=overwrite, data = json_data, dir=dir_state, 
                                     bs_file_RDS=business_file_RDS, dataset="review")
                
        }

}
###
#End Function to getJSON data from files and transform into RDS
###

###
#FUnction to generate file RDS per State
###
createFileRDSbyState <- function(overwrite = FALSE, data = NULL, dir, bs_file_RDS, dataset="checkin"){
        
        #Log print
        print(timestamp())
        print(paste("File to generate",dataset))
        
        #File destiny for data
        file_RDS <- paste(dir,"/",dataset,".RDS",sep="")
        
        #Check existence of files
        if(file.exists(file_RDS)&& !overwrite){
                print("File RDS exists")
                return()
        }
        
        if(!file.exists(bs_file_RDS)){
                print("File business RDS doesn't exists")
                return()
        }
        
        #Read business file into directory
        #Only business from this State
        #This business data is used to filter and get only data for business from this state
        aux <- readRDS(bs_file_RDS)
        
        #If file to obtain is "review" is necessary to iterate over all RDS review files
        if (dataset == "review"){
                 
                #Get all files with partial data from reviews
                files <- list.files(path="data/raw/yelp_dataset_challenge_academic_dataset", 
                                     pattern="yelp_academic_dataset_review_")
                 
                initial_iteration = TRUE
                for(file in files){
                        
                        #Set file to process
                        file_to_process <- paste("data/raw/yelp_dataset_challenge_academic_dataset",file,sep="/")
                        
                        #Log print
                        print(timestamp())
                        print(paste("Read file:",file_to_process))
                        
                        #Read file
                        temp_data <- readRDS(file_to_process)
                        
                        #Filter file
                        temp_data %>% semi_join(aux, by = "business_id") -> temp_data_filtered
                        
                        #Create list or append to list if it exists
                        #If is initial iteration then create list, else, append
                        if(initial_iteration) { 
                                data_filtered <- temp_data_filtered
                                initial_iteration = FALSE
                                }
                        else{   
                                data_filtered <- mapply(c, data_filtered, temp_data_filtered, SIMPLIFY = FALSE)
                                }
                        
                } #End for
        }
        else{
                #Filter data
                data[[dataset]] %>% 
                                semi_join(aux, by = "business_id") -> data_filtered
        }                
        
        #Log print                
        print(timestamp())
        print(paste("Saving RDS file business:",dataset))

        #Save RDS                        
        saveRDS(object= as.data.frame(data_filtered),file = file_RDS)

}
###
#End FUnction to generate file RDS per State
###

###
# Function to get unique values from a column from a file
###
getDistinct <- function(data=NULL, object="business", column="state") {
        pos = which( colnames(data[[object]]) == column )
        select( data[[object]], pos) %>%
                distinct() %>% arrange()
}
###
#End function
###

###
#Function to get data from a set filtered by state
###
getDataFilteredByState <- function(data = NULL, object = "business", state_filter = "AZ"){
        data[[object]] %>% 
                filter(state==state_filter)
}
###
#End function
###

main <- function(){
        
        #Init global variables and constants
        initGlobal()
        
        #Download and unzip data
        getData()
        
        #Process Json, create one directory per state and a set of RDS per Json file type
        getJSON(overwrite = TRUE)
        
}