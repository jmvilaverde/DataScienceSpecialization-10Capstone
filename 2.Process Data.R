###########
#LIBRARIES
###########

library(dplyr)


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

#Default state to be use as filter
default_state <- "BW"


###OBJECT: categories
#Description: Object to manage extracting of categories and get unique categories
categories <- function(){
        
        #Function to get business categories from business.RDS file
        #Input: state_filter to select the state to use as filter
        getBusinessCategories <- function(state_filter=NULL){
                library(jsonlite)
                
                states <- list_states
                
                if(!is.null(state_filter)) states <- state_filter
                
             for(state in states){
                
                if(is.null(state_filter)){file_to_save <- paste(dir_resources,"categories.RDS",sep="/")}
                else{file_to_save <- paste(dir_states,state,"categories.RDS",sep="/")}
                
                if(file.exists(file_to_save)){
                        dataCat <- readRDS(file_to_save)
                        return(dataCat)
                }
                
                
                data <- mainData$getDataFiltered(dataset = "business", state_parameter=state)
                
                rows_dataset <- length(data[[1]])
                initial = TRUE
                
                for(i in 1:rows_dataset){
                        paste("Processing categories: ",i," of ",rows_dataset)
                        
                        aux <- flatten(as.data.frame(data[["categories"]][i]))
                        
                        rows_cats <- length(aux[[1]])
                        
                        #When there's no categories
                        if(rows_cats == 0) rows_cats = 1
                        
                        for(j in 1:rows_cats){
                                
                                aux_b_id <- c(as.character(data[["business_id"]][i]))
                                aux_cat <- as.character(aux[[1]][j])
                                if(initial){
                                        id <- c(i)
                                        business <- c(aux_b_id)
                                        categories <- c(aux_cat)
                                        initial = FALSE
                                }
                                else{
                                        id <- c(id,i)
                                        business <- c(business,aux_b_id)
                                        categories <- c(categories,aux_cat)
                                }
                                
                        }
                }
                
                dataCat <- data.frame(id, business, categories)
                colnames(dataCat) <- c("id", "business_id", "categories")
                
                saveRDS(dataCat, file=file_to_save)
             }
                dataCat
                
        }
        
        #Function to get categories.
        #Input: cat_to_search <- filter by category
        #Input: state_filter <- filter by state
        getUniqueCategories <- function(cat_to_search=NULL, state_filter=NULL){
                
                #Get business categories from one state or all
                data <- getBusinessCategories(state_filter=state_filter)
                
                #Processing data
                data <- summary(data$categories)
                
                data <- data.frame(data)
                
                data <- cbind(rownames(data), data)
                
                colnames(data) <- c("categories", "number")
                rownames(data) <- c(1:nrow(data))
                
                #Filter by cat_to_search
                if(!is.null(cat_to_search)){
                        data <- data[grepl(cat_to_search,data$categories), ]
                }
                
                data
        }
        
        list(getBusinessCategories=getBusinessCategories,
             getUniqueCategories=getUniqueCategories)
}
###End categories object

### data object
data <- function(){
        
        dir_value <- "data/raw/state"
        
        #Function to get data from RDS file
        getDatafromRDS <- function(dir="data/raw/state", state=default_state, dataset="business"){
                
                #Set path, state directory and file name
                file <- paste(dir,"/",state,"/",dataset,".RDS",sep="")
                
                #Get file
                if(file.exists(file)) { return(readRDS(file)) }
                else{ paste("File doesn't exists:",file) }
                
        }
        
        #Function to get data from RDS filtered by a column and a filter criteria
        getDataFiltered <- function(dataset="business", state_parameter=NULL, filter_column=NULL, filter_criteria=NULL){
                
                #Set state/s to get data from
                states <- list_states
                if(!is.null(state_parameter)) states <- state_parameter
                
                initial = TRUE
                for(state in states){
                        
                        aux_data <- getDatafromRDS(dir_states, state, dataset)
                        
                        if(!is.null(filter_column) && !is.null(filter_criteria)){
                                
                                aux_data <- aux_data[aux_data[,filter_column] %in% filter_criteria,]
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
                        mutate(difference_avg = positive_avg - negative_avg, 
                               positive.over.total=(number_positive/(number_positive+number_negative)*100),
                               negative.over.total=(number_negative/(number_positive+number_negative)*100)) %>%
                        arrange(desc(difference_avg)) -> final_data
                
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
                
                mainCategories$getBusinessCategories(state_filter=state_filter) %>% 
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

### data object model
modelController <- function(){
                
        #Range in which an attribute is relevant for our model 
        validity_range <- as.numeric(2.5)
        
        #Difference between have or not an attribute
        validity_difference <- as.numeric(0.15)
                
        
        #Function to create a model for a specifict State and category
        #Not good results, try to use other model instead of LM
        createModelperState <- function(recalculate = FALSE, state_filter="BW", category_filter="Food"){
                
                file <- paste(dir_states,"/",state_filter,"/model_category_",category_filter,".RDS", sep="")
                
                if(file.exists(file) && !recalculate){
                        model <- readRDS(file)
                        return(model)
                }
                
                #Get data
                data <- mainData$getDatasetperCat(dataset = "business", category = category_filter, state_filter = state_filter)
                colnames(data) <- make.names(names(data))
                
                #Get relevant attributes
                attributes_unformated <- mainData$getTotalsAttStars(overwrite=FALSE, state_filter = state_filter, 
                                                          category = category_filter)
                
                #Replace all spaces with dots and filter to only obtain relevant results
                attributes_unformated %>% mutate(attribute=gsub(" ", ".", attribute, fixed = TRUE)) %>%
                        mutate(attribute=gsub("-", ".", attribute, fixed = TRUE)) %>%
                        filter(state==state_filter, positive.over.total>validity_range, negative.over.total>validity_range,
                                      (difference_avg>validity_difference | difference_avg<(-validity_difference))) -> attributes_unformated
                
                View(attributes_unformated)
                
                #Transform data to use only relevant columns
                data_formated <- data[colnames(data) %in% attributes_unformated$attribute | colnames(data) %in% c("stars", "business_id")]
                data_formated[is.na(data_formated) | !data_formated] <- "NO"
                data_formated[data_formated=="NULL"] <- "NO"
                data_formated[data_formated==TRUE] <- "YES"
                data_formated[data_formated==TRUE] <- "YES"
                data_formated[data_formated[colnames(data) %in% attributes_unformated$attribute]>0] <- "YES"
                
                # data_formated %>% mutate(attributes.Accepts.Credit.Cards=as.logical(attributes.Accepts.Credit.Cards)) -> data_formated
                
                View(data_formated)
                #Create model
                #Create formula
                attributes_concat <- paste(as.character(attributes_unformated$attribute), collapse= " + ")
                print(attributes_concat)
                formula <- as.formula(paste("stars ~ ", attributes_concat, sep=""))
                print(formula)
                
                #Create model LM, GLM -> FAIL, R-squared value so low
                model_ineffective <- lm(data = data_formated, formula = formula)
                model_ineffective <- glm(data = data_formated, formula = formula)
                
                #Create a Cross validation
                require(caret)
                require(kernlab)
                require(pROC)
                
                #Seed to be used to obtain the same results
                set.seed(1525)
                
                
                #Create training and testingData
                index <- createDataPartition(y=data_formated$stars, p=0.8, list=FALSE)
                trainingData <- data_formated[index,]
                testingData <- data_formated[-index,]
                
                ##############
                #Try LDA Model -> FAIL, worng model type for regression
                #Try RF Model
                ##############
                View(trainingData)
                
                model <- train(formula, data = trainingData, method = "rf", do.trace=10, ntree=100) 
#                                trControl = trainControl(method = "cv", number = 5),
#                                prox = TRUE, allowParallel = TRUE)
#                 
                
                
#                 prediction <- predict(model, testingData)
#                 
#                 confusionMatrixModel <- confusionMatrix(prediction, testingData$stars)
#                 print(confusionMatrixModel)
#                 
                saveRDS(model, file = file)
                model
                
        }
        

        list(createModelperState=createModelperState)
}

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
        
        wordsToExclude <- c("-", "a", "about", "all", "also", "an", "and", "and the", "are", "at", 
                            "be", 
                            "can",
                            "for", "from", 
                            "get", "go", "great", 
                            "here", 
                            "i", "if", "in", "in the", "is", "it's", "it was",
                            "just", 
                            "like",
                            "my", "more",
                            "of", "of the", "on the", "one", "or", "out", 
                            "really",
                            "she", "some",
                            "that", "the", "there", "they", "to",
                            "up",
                            "very", 
                            "was","were", "where", "where you", "which", "with", 
                            "with a", "has", "too", "for a", "their", "when", "is a",
                            "you can", "your", "to the", "for the",   
                            "it", "her", "what", "we", "as", "have", "on", "me", "so", "had", "you", "but", 
                            "will", "not", "this", "our", "he", "she was", "i have", "back", "would", "could",
                            "his", "him")
        
        
        #Function to count words in a phrase
        #Input phrase, n-grams to count
        #Output data.frame(words, ngram, frequency)
        countWords <- function(phrase, ngrams=c(1:3)){
                aux <- phrase
                #Remove commas, dots, and transform to uppercase
                for(element in c(",", ".", "(", ")", ":", ";")){
                        aux <- gsub(element,"", aux, fixed = TRUE)
                }
                aux <- tolower(aux)
                
                list_aux <- strsplit(aux, " ")[[1]]
                list_aux <- list_aux[list_aux != ""]
                list_aux <- list_aux[!is.na(list_aux)]
                list_aux <- list_aux[!is.null(list_aux)]
                
                len_list_aux <- length(list_aux)
                initial <- TRUE
                
                #Go throught all the list
                for(i in 1:len_list_aux){
                        #get the ngrams
                        for(ngram in ngrams){
                                
                                end <- i+ngram-1
                                
                                if(end<=len_list_aux){
                                        aux_gram <- paste(list_aux[i:end], collapse=" ")
                                }
                                        
                                if(initial){
                                        temp_table <- c(aux_gram,ngram,1)
                                        initial <- FALSE
                                }
                                else{
                                        temp_table <- rbind(temp_table, c(aux_gram,ngram,1))
                                }
                        }
                }
                #Transform table class
                temp_table <- data.frame(temp_table)
                                
                colnames(temp_table) <- c("word", "ngram", "number")
                
                #Get totals
                temp_table %>% 
                        select(word, ngram, number) %>% 
                        group_by(word, ngram) %>% 
                        summarize(total_number=sum(number)) %>% 
                        arrange(desc(total_number)) -> table
                
                table
        }
        
        getDataCountOfWords <- function(overwrite=FALSE, state_filter="CA", category_filter=NULL){
                
                if(is.null(category_filter)) cat_file_name = "all"
                
                #Set file name and path
                file <- paste(dir_states,"/",state_filter,"/wordsCount_",cat_file_name,".RDS", sep="")
                
                #Check if file exists and no overwrite to load exiting data and return it
                if(file.exists(file)&&!overwrite) {
                        print("File exists")
                        return(readRDS(file))
                }
                
                #Get review data
                log(paste("Loading data from State:",state_filter))
                aux <- mainData$getDataFiltered(dataset="review", state_parameter=state_filter)
                aux %>% select(business_id, review_id, stars, type, text) -> aux
                #%>% filter(business_id %in% )
                
                        
                
                #Apply countWords for each review
                log("Start processing texts...")
                nrows_aux <- nrow(aux)
                initial <- TRUE
                for(i in 1:nrows_aux){
                        log(paste("Processed:",(i/nrows_aux*100),"%"))
                        
                        #Get text from register and countWords
                        aux_text <- aux[i,"text"]
                        aux_table_counts <- countWords(aux_text)
                        
                        #Add columns with data of review to data from countWords
                        aux[i,] %>% select(business_id, review_id, stars, type) %>%
                                merge(aux_table_counts, by = integer(0)) -> aux_table_counts
                        
                        #Add data to main table_counts
                        if(initial){
                                table_counts <- aux_table_counts
                                initial <- FALSE
                        }
                        else{
                                table_counts <- rbind(table_counts, aux_table_counts)       
                        }
                }
                                
                saveRDS(file=file, table_counts)
                table_counts
                
        }
        
        getTopNGrams <- function(stars_filter=c(4, 4.5, 5), state_filter="CA"){
                table <- getDataCountOfWords(state_filter = state_filter)
                
                #exclude common words
                table <- table[!(table$word %in% wordsToExclude),]
                
                table %>% filter(stars %in% stars_filter) %>%
                        group_by(word) %>% 
                        summarize(accumulate = sum(total_number)) %>% 
                        arrange(desc(accumulate)) -> result
                
                        
                         
                        
                
                result
        }
        
        list(countWords=countWords,
             getDataCountOfWords=getDataCountOfWords,
             getTopNGrams=getTopNGrams)
}


#Main function
main <- function(){
        
if(!dir.exists(dir_process)) { dir.create(dir_process)}

mainController <<- controller()
mainCategories <<- categories()
mainData <<- data()
mainTotals <<- totals()
mainTextAnalyst <<- textAnalyst()
mainModelController <<- modelController()

#datos <<- mainController$mainMenu()

modelo <<- mainModelController$createModelperState(recalculate = TRUE, state_filter = "BW", category_filter = "Food")

# datos <<- mainCategories$getBusinessCategories("BW")
# datos_f <<- mainCategories$getUniqueCategories(state_filter="BW")
# tp <<- mainTextAnalyst$getPhrase()
# tp_sepparate <<- mainTextAnalyst$countWords(tp)
# state <- "XGL"
# 
# tp <<- mainTextAnalyst$getDataCountOfWords(overwrite=FALSE, state_filter=state)
# t2 <<- mainTextAnalyst$getTopNGrams(state_filter=state)
# t3 <<- mainTextAnalyst$getTopNGrams(stars_filter=c(1,1.5,2,2.5), state_filter=state)
#View(t2)



}