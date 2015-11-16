###########
#LIBRARIES
###########

# library(dplyr)


###########
#CONSTANTS
###########

#Set directory where it's to be stored
dir_states <- "data/raw/state"

dir_process <- "data/raw/process"

dir_resources <- "data/raw/resources"

#Get States
list_states <- list.files(dir_states)
list_states <- list_states[!grepl("resources",list_states)]

#Files types
file_types <- c("business", "checkin", "tip", "review")


log <- function(string){
        #Print at console
        print(paste(timestamp(),string))
        
        #TODO: Save on log
}

###controller object
controller <- function(){
        
        
        mainMenu <- function(){
                print("1-Get all Attributes")
                print("2-Get table attributes filtered by state and category")
                s = 3
                while(s<1||s>2){
                        s <- as.integer(readline("Select option number:"))
                }
                
                if(s==1) {
                        data <<- mainData$getTotalsAttStars(overwrite=TRUE)
                        View(data)
                }
                
                if(s==2){
                        state_selected <- askForState()
                        category_selected <- askForCategory()
                        data <<- mainData$getTotalsAttStars(overwrite=TRUE, state_filter = state_selected, category = category_selected)
                        View(data)
                }
                
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
        
        askForCategory <- function(){
                
#                 mainCategories$getUniqueCategories()[,"categories"] %>%
#                         order_by(categories) -> list_categories
                
                list_categories <<- as.character(mainCategories$getUniqueCategories()[,"categories"])
                
                index = 1
                
                for(category in list_categories){
                        
                        print(paste(index,"-",category))
                        index = index + 1
                        
                }
                
                View(list_categories)
                
                selection = index + 1
                
                while (selection > index || selection < 1) selection = as.integer(readline("Select category number:"))
                
                list_categories[selection]
        }
        
        list(mainMenu=mainMenu,
             askForState=askForState,
             askForCategory=askForCategory)
}
###End controller object

###categories object        
categories <- function(){
        
        getBusinessCategories <- function(){
                library(jsonlite)
                
                file_to_save <- paste(dir_resources,"/categories.RDS",sep="")
                
                if(file.exists(file_to_save)){
                        dataCat <- readRDS(file_to_save)
                        return(dataCat)
                }
                
                
                data <- mainData$getDataFiltered(dataset = "business")
                
                rows_dataset <- length(data[[1]])
                initial = TRUE
                
                for(i in 1:rows_dataset){
                        
                        aux <- flatten(as.data.frame(data[["categories"]][i]))
                        
                        rows_cats <- length(aux[[1]])
                        
                        #When there's no categories
                        if(rows_cats == 0) rows_cats = 1
                        
                        for(j in 1:rows_cats){
                                aux_b_id <- c(as.character(data[["business_id"]][i]))
                                aux_cat <- as.character(aux[[1]][j])
                                if(initial){
                                        id <<- c(i)
                                        business <<- c(aux_b_id)
                                        categories <<- c(aux_cat)
                                        initial = FALSE
                                }
                                else{
                                        id <<- c(id,i)
                                        business <<- c(business,aux_b_id)
                                        categories <<- c(categories,aux_cat)
                                }
                                
                        }
                }
                
                dataCat <- data.frame(id, business, categories)
                colnames(dataCat) <- c("id", "business_id", "categories")
                
                saveRDS(dataCat, file=file_to_save)
                
                dataCat
                
        }
        
        getUniqueCategories <- function(cat_to_search=NULL){
                
                data <- getBusinessCategories()
                
                data <- summary(data$categories)
                
                data <- data.frame(data)
                
                data <- cbind(rownames(data), data)
                
                colnames(data) <- c("categories", "number")
                rownames(data) <- c(1:nrow(data))
                
                if(!is.null(cat_to_search)){
                        data <- data[grepl(cat_to_search,data$categories), ]
                }
                
                data
        }
        
        getRelatedCategories <- function(category=NULL){
                
                #Return unique categories with the same business id
        }
        
        list(getBusinessCategories=getBusinessCategories,
             getUniqueCategories=getUniqueCategories,
             getRelatedCategories=getRelatedCategories)
}
###End categories object

### data object
data <- function(){
        
        dir_value <- "data/raw/state"
        
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
        
        getDataFiltered <- function(dataset="business", state_parameter=NULL, filter_column=NULL, filter_criteria=NULL, 
                                    colname=NULL){
                
                states <- list_states
                if(!is.null(state_parameter)) states <- state_parameter
                
                initial = TRUE
                for(state in states){
                        
                        aux_data <- getDatafromRDS(dir_states, state, dataset)
                        
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
                
#                 if(dataset=="review") {
#                         as.data.frame(data)
#                 }
#                 else{
                        data
#                 }
                
                
                
        }
        
        getRelationAttStars <- function(attribute=NULL,state_filter=NULL, 
                                        dataset="business", category = NULL){
                
                if(is.null(category)) {
                        data <- getDataFiltered(state_parameter=state_filter, dataset=dataset)
                }
                else {
                        data <- getDatasetperCat(dataset = dataset, category = category, state_filter = state_filter)
                }
                
                if(is.null(attribute)){ 
                        attributes <- names(data)[grepl("attributes", names(data))]
                }
                else {
                        attributes <- attribute
                }
                
                initial <- TRUE
                counter <- 0
                for(att in attributes){
                        log(att)
                        print(paste("Progress",counter/length(attributes)*100,"%"), digits = 1)
                        counter <- counter+1
                
                #Clean attribute values
                index <- which(is.na(data[[att]])==TRUE)

                data[[att]][index] <- FALSE
                index <- which(data[[att]]=="NULL")
                data[[att]][index] <- FALSE
                
                #get data and put colnames
                aux_data <- data.frame(as.character(data[["state"]]), att, as.logical(data[[att]]), 
                                   as.numeric(data[["stars"]]))
                colnames(aux_data) <- c("state", "attribute", "value","stars")
                
                if(initial){
                        final_data <- aux_data
                        initial <- FALSE
                }
                else{
                        final_data <- rbind(final_data, aux_data)
                }
                
                }
                
                final_data
                #data[,names(data) %in% c(attribute,"stars")]
                

                
        }
        
        getTotalsAttStars <- function(overwrite = FALSE, state_filter = NULL, category = NULL){
                
                if(is.null(state_filter)) {
                        state_file_att = "all"
                }
                else{
                        state_file_att = state_filter
                }
                
                if(is.null(category)){
                        category_file_att = "no_category"
                }
                else{
                        category_file_att = category
                }
                
                file_att <- paste(dir_resources,"/tableAttributes_",state_file_att,"_",category_file_att,".RDS", sep="")
                
                if(!file.exists(file_att) || overwrite){
                
                data <- getRelationAttStars(state_filter = state_filter, category = category)

                #Table with columns: State (one in particular or Total), number of business, att,number positive, 
                #avg positive, number negative, avg negative, avg positive - avg negative
                library(dplyr)

                data %>% 
                        group_by(state, attribute, value) %>% 
                        summarize(number = n(), avg = mean(stars, na.rm = TRUE))  %>%
                        data.frame() -> final_data1
                
                data2 <- data
                
                data2[["state"]] <- "Total"
                
                data2 %>% 
                        group_by(state, attribute, value) %>% 
                        summarize(number = n(), avg = mean(stars, na.rm = TRUE))  %>%
                        data.frame() -> final_data2
                
                final_data <- rbind(final_data1, final_data2)
                
                final_data %>% filter(value==TRUE) %>% 
                        select(state, attribute, number, avg) ->>  data_A
                colnames(data_A) <<- c("state", "attribute", "number_positive", "positive_avg")
                
                final_data %>% filter(value==FALSE) %>% 
                        select(state, attribute, number, avg) ->>  data_B
                colnames(data_B) <<- c("state", "attribute", "number_negative", "negative_avg")
                
                data_A %>% full_join(data_B, by=c("state", "attribute")) %>%
                        mutate(difference_avg = positive_avg - negative_avg) -> final_data
                
                if(!is.null(state_filter)) {
                        print("filter state dplyr")
                        final_data %>% filter(state==state_filter) ->> final_data
                        }
                
                saveRDS(final_data, file=file_att)
                }
                else{
                final_data <- readRDS(file=file_att)        
                }
                
                final_data
                
                
        }

        getDatasetperCat <- function(dataset="business", category="Food", state_filter=NULL){
                library(dplyr)
                
                mainCategories$getBusinessCategories() %>% 
                        filter(categories==category) %>% select(business_id) -> business
                
                states <- list_states
                if(!is.null(state_filter)) states <- state_filter
                
                for(state in states){
                        aux_data <- getDatafromRDS(state=state, dataset=dataset)
                        
                        aux_data %>% inner_join(business, by="business_id") -> aux_data
                        
                }
                
                print(nrow(aux_data))
                aux_data
        }
        
        list(getDatafromRDS=getDatafromRDS,
             getDataFiltered=getDataFiltered,
             getDatasetperCat=getDatasetperCat,
             getRelationAttStars=getRelationAttStars, 
             getTotalsAttStars=getTotalsAttStars)
}
###End data object

countAttribute <- function(attribute="stars", data){
        library(dplyr)

        data %>% 
                select(stars) %>% 
                group_by(stars) %>% count(stars)

}

barGraphState <- function(state, att, data){
        
        t <- data[,att]
        counts <- table(data[,att])
        print(counts)
        barplot(counts, main=state, 
                xlab="Number of Stars")
}

### totals object
totals <- function(){
createResumeTable <- function(dataset="business", filter_column=NULL, filter_criteria=NULL, colname=NULL){
        
        #Create object data
        #d <- data()
        
        acc = 0
        resumeTable <- data.frame(state=character(),Number=integer())
        
        colname <- ifelse(is.null(colname), dataset, paste(dataset,colname,sep=" "))
        
        for(state in list_states){
                
                data <- mainData$getDatafromRDS(dir_states, state, dataset)
                
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
        list(createResumeTable=createResumeTable,
             createGlobalResumeTable=createGlobalResumeTable)
}
###End totals object

###Text analyst object
textAnalyst <- function(){
        
        data <- data.frame()
        test_phrase <- as.character()
        
        getData <- function(state_filter = "NC"){
                data <- mainData$getDataFiltered(dataset="review", state_parameter=state_filter)
                
                #Filter data
                data
        }
        
        getTestPhrase <- function(){
                data <- getData()
                test_phrase <<- as.character(data[1,"text"])
                print(test_phrase)
                test_phrase
        }
        
        
        list(getData=getData,
             getTestPhrase=getTestPhrase)
}


#Main function
main <- function(){
        
if(!dir.exists(dir_process)) { dir.create(dir_process)}

mainController <<- controller()
mainCategories <<- categories()
mainData <<- data()
mainTotals <<- totals()
mainTextAnalyst <<- textAnalyst()

tp <<- mainTextAnalyst$getTestPhrase()

#data <- mainCategories$getUniqueCategories()

#mainController$mainMenu()



}