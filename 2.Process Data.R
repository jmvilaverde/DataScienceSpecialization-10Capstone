###########
#CONSTANTS
###########

#Set directory where it's to be stored
dir_states <- "data/raw/state"


log <- function(string){
        #Print at console
        print(paste(timestamp(),string))
        
        #TODO: Save on log
}

###
#Procedure in order to select a State
###
askForState <- function(){



#Get States
list_states <- list.files(dir_states)

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
                log(paste("Get data from:",file))
                return(readRDS(file))
        }
        else{
                log(paste("Get data from:",file))
        }

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
        counts <- table(data[,att])
        barplot(counts, main=state, 
                xlab="Number of Stars")
}

#Main function
main <- function(){
        
state_selected <- askForState()

print(state_selected)

data <- getDatafromRDS(state=state_selected, dataset="business")

names(data)

countAttribute("stars", data)

barGraphState(state_selected, "stars", data)

}