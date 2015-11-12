###
#Procedure in order to select a State
###
askForState <- function(){

#Set directory where it's to be stored
dir_states <- "data/raw/state"

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

#Main function
main <- function(){
        
state_selected <- askForState()

print(state_selected)

}