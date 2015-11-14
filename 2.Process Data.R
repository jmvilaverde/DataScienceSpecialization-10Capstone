###########
#CONSTANTS
###########

#Set directory where it's to be stored
dir_states <- "data/raw/state"

dir_process <- "data/raw/process"

#Get States
list_states <- list.files(dir_states)

#Files types
file_types <- c("business", "checkin", "tip", "review")


log <- function(string){
        #Print at console
        print(paste(timestamp(),string))
        
        #TODO: Save on log
}

###
#Procedure in order to select a State
###
askForState <- function(){

index = 1

for(state in list_states){
        
        print(paste(index,"-",state))
        index = index + 1
        
        }
selection = index + 1

while (selection > index || selection < 1) selection = as.integer(readline("Select state number:"))

list_states[selection]

}

getDatafromRDS <- function(dir="data/raw/state", state="AZ", dataset="business"){
        
        file <- paste(dir,"/",state,"/",dataset,".RDS",sep="")
        if(file.exists(file)) {
                # log(paste("Get data from:",file))
                return(readRDS(file))
        }
        else{
                log(paste("File doesn't exists:",file))
        }

}

getDataFiltered <- function(dataset="business", state_parameter=NULL, filter_column=NULL, filter_criteria=NULL, colname=NULL){
        
        states <- list_states
        if(!is.null(state_parameter)) states <- state_parameter
        
        initial = TRUE
        for(state in states){
                
                aux_data <<- getDatafromRDS(dir_states, state, dataset)
                
                if(!is.null(filter_column) && !is.null(filter_criteria)){
                        
                        aux_data <- aux_data[aux_data[,filter_column]%in%filter_criteria,]
                }
                
                if (initial){
                        data <- aux_data
                        initial = FALSE
                }
                else{
                        data <- mapply(c, data, aux_data, SIMPLIFY = FALSE)
                }
                
        }
        
        data
        
        
}

countAttribute <- function(attribute="stars", data){
        library(dplyr)
        #col <- substitute(attribute)
        
        data %>% select(stars) %>% group_by(stars) %>% count(stars)
        #data %>% group_by(stars) %>% count(as.character(attribute))
#         library(dplyr)
#         count(data,"stars")
}

barGraphState <- function(state, att, data){
        
        t <- data[,att]
        counts <- table(data[,att])
        print(counts)
        barplot(counts, main=state, 
                xlab="Number of Stars")
}

createResumeTable <- function(dataset="business", filter_column=NULL, filter_criteria=NULL, colname=NULL){
        
        acc = 0
        resumeTable <- data.frame(state=character(),Number=integer())
        
        colname <- ifelse(is.null(colname), dataset, paste(dataset,colname,sep=" "))
        
        for(state in list_states){
                data <- getDatafromRDS(dir_states, state, dataset)
                
                if(!is.null(filter_column) && !is.null(filter_criteria)){
                        
                        data <- data[data[,filter_column]%in%filter_criteria,]
                        
#                         data %>% 
#                                 filter(filter_column %in% filter_criteria) -> data
                }
                
                total = nrow(data)
                # log(paste("Total ",colname," in ",state,":",total))
                acc = acc + total
                
                
                aux <- data.frame(state=state,integer=total)
                resumeTable <- rbind(resumeTable, aux)
        }
        # log(paste("Total of rows:",acc))
        aux <- data.frame(state="Total",integer=acc)
        resumeTable <- rbind(resumeTable, aux)
        
        colnames(resumeTable) <- c("state", colname)
        log("Resume table")
        print(resumeTable)
        resumeTable
}

createGlobalResumeTable <- function(file_to_analyze=NULL){
        
        initial <- TRUE
        files <- file_types 
        if(!is.null(file_to_analyze)) files <- file_to_analyze
        
        for (file in files){
                if(initial){
                        global_table <- createResumeTable(dataset=file)
                        initial = FALSE
                }
                else{
                        createResumeTable(dataset=file) %>% 
                                inner_join(global_table,by="state") -> global_table
                }
                        
                #Add >=4 stars
                if (file %in% c("business", "review")){        
                        createResumeTable(dataset=file, filter_column="stars", filter_criteria=c(4, 4.5, 5), colname=">=4 stars") %>% 
                        inner_join(global_table,by="state") -> global_table
                        createResumeTable(dataset=file, filter_column="stars", filter_criteria=c(1, 1.5, 2, 2.5), colname="<3 stars") %>% 
                                inner_join(global_table,by="state") -> global_table
                }
        }
        

        log("Global table")
        print(global_table)
        global_table
}

#Main function
main <- function(){
        
if(!dir.exists(dir_process)) { dir.create(dir_process)}
        
state_selected <- askForState()

print(state_selected)

data <- getDatafromRDS(state=state_selected, dataset="business")

names(data)

countAttribute("stars", data)

barGraphState(state_selected, "stars", data)

}