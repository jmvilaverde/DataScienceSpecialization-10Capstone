###########
#LIBRARIES
###########

library(dplyr)


###########
#CONSTANTS
###########

#Set directory where it's to be stored
dir_states <- "data/raw/state"

#Set directory for some RDS files with auxiliary processed data
dir_process <- "data/raw/process"
dir_resources <- "data/raw/resources"

#Get States
list_states <- list.files(dir_states)
list_states <- list_states[!grepl("resources",list_states)]

#Files types
file_types <- c("business", "checkin", "tip", "review")

#Default state to be use as filter
default_state <- "NC"


### Object Categories
#Description: Object to manage extracting of categories and get unique categories
categories <- function(){
        
        ###
        #Function to get business categories from business.RDS file
        #Input: state_filter to select the state to use as filter
        ###
        getBusinessCategories <- function(state_filter=NULL){
                library(jsonlite)
                
                states <- list_states
                
                if(!is.null(state_filter)) states <- state_filter
                
             for(state in states){
                
                if(is.null(state_filter)){file_to_save <- paste(dir_resources,"categories.RDS",sep="/")}
                else{file_to_save <- paste(dir_states,state,"categories.RDS",sep="/")}
                
                if(file.exists(file_to_save)){
                        #print(paste("File exists:",file_to_save))
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
                
                print(paste("Save categories file:",file_to_save))
                saveRDS(dataCat, file=file_to_save)
             }
                dataCat
                
        }
        ###
        #End function
        ###
        
        ###
        #Function to get categories.
        #Input: cat_to_search, is a filter by category
        #Input: state_filter, is a filter by state
        ###
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
        ###
        #End function
        ###
        
        list(getBusinessCategories=getBusinessCategories,
             getUniqueCategories=getUniqueCategories)
}
###End categories object

### Object Data
data <- function(){
        
        dir_value <- "data/raw/state"
        
        ###
        #Function to get data from RDS file
        ###
        getDatafromRDS <- function(dir="data/raw/state", state=default_state, dataset="business"){
                
                #Set path, state directory and file name
                file <- paste(dir,"/",state,"/",dataset,".RDS",sep="")
                
                #Get file
                if(file.exists(file)) { return(readRDS(file)) }
                else{ paste("File doesn't exists:",file) }
                
        }
        ###
        #End function
        ###
        
        ###
        #Function to get data from RDS filtered by a column and a filter criteria
        ###
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
                
                        data

        }
        ###
        #End function
        ###
        
        ###
        #Function to obtain relation between stars, attribute, state and category
        ###
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
                        print(att)
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
        ###
        #End function
        ###
        
        ###
        #Function to get total values
        ###
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
        ###
        #End function
        ###
        
        ###
        #Function to obtain a data set with data from business of a specific category
        ###
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
                
                #print(nrow(aux_data))
                aux_data
        }
        ###
        #End function
        ###
        
        list(getDatafromRDS=getDatafromRDS,
             getDataFiltered=getDataFiltered,
             getDatasetperCat=getDatasetperCat,
             getRelationAttStars=getRelationAttStars, 
             getTotalsAttStars=getTotalsAttStars)
}
###End Object Data

### Object Model
modelController <- function(){
        
        ###
        #Function to init global constants and variables.
        ###
        init <- function(){
                
                #Range in which an attribute is relevant for our model 
                validity_range <<- as.numeric(1)
        
                #Difference between have or not an attribute
                validity_difference <<- as.numeric(0.1)
        }
        ###
        # End function
        ###

        ###
        #Function to transform and clean the data for the model
        ###
        transformAndClean <- function(data, attributes){
                
                data_formated <- data[colnames(data) %in% attributes | colnames(data) %in% c("stars", "business_id", "latitude", "longitude")]
                
                #Configure NO values to transform into factors
                data_formated[is.na(data_formated)] <- "NO"
                data_formated[data_formated=="NULL"] <- "NO"
                data_formated[data_formated==FALSE] <- "NO"
                
                #Configure YES values
                data_formated[data_formated==TRUE] <- "YES"
               
                #All attributes as character
                for(att in attributes){
                        data_formated[,att] <- as.character(data_formated[,att])
                        data_formated[,att] <- as.factor(data_formated[,att])
                }

                data_formated[,"stars"] <- as.factor(data_formated[,"stars"])
                #data_formated[,grepl("attributes", names(data_formated))] <- flatten(data_formated[,grepl("attributes", names(data_formated))])
                
                #Problem to transform all attributes into factors solved with information from URL:
                #http://grokbase.com/t/r/r-help/12614bx2jd/r-redefine-multiple-columns-using-grep-as-factor-variables
                #data_formated[,grepl("attributes", names(data_formated))] <- lapply(data_formated[,grepl("attributes", names(data_formated))], as.factor)
                
                
                
                data_formated
        }
        ###
        #End Function
        ###
        
        #############
        ############
        #TODO!!!!!!!
        ############
        ############
        
        
        ###
        #Function to add top possitive and negative words as atributes
        ###
        addWordsAttributes <- function(data, state_filter, category_filter){
                #Top-n words
                top_n = 5
                
                #Get words for business
                data_businessWords <- mainTextAnalyst$getDataCountOfWords(state_filter=state_filter, category_filter = category_filter)
                
                #Get top words
                data_topPositiveWords <- mainTextAnalyst$getTopNGrams(stars_filter=c(4, 4.5, 5), state_filter=state_filter, category_filter = category_filter, top = top_n)
                data_topPositiveWords[,"order_positive"] <- c(1:top_n)
                data_topNegativeWords <<- mainTextAnalyst$getTopNGrams(stars_filter=c(1, 1.5, 2, 2.5), state_filter=state_filter, category_filter = category_filter, top = top_n)
                data_topNegativeWords[,"order_negative"] <- c(1:top_n)

                
                #Check if word exists for business
                #Get coincidences with positive and negative top words
                data_businessWords %>% 
                        inner_join(data_topPositiveWords, by="word") %>% 
                        select(business_id, order_positive) -> table_pos
                data_businessWords %>% 
                        inner_join(data_topNegativeWords, by="word") %>% 
                        select(business_id, order_negative) -> table_neg
                
                #Create table complete
                for(i in 1:top_n) { 
                        table_pos[,paste("Attribute.Pos.Word.",i,sep="")] <- table_pos[,"order_positive"]==i
                        table_neg[,paste("Attribute.Neg.Word.",i,sep="")] <- table_neg[,"order_negative"]==i
                }
                
                table_pos %>% full_join(table_neg, by="business_id") -> table_complete
                
                table_complete %>% group_by(business_id) %>% 
                        summarize(Attribute.Pos.Word.1=sum(Attribute.Pos.Word.1)>0,
                                  Attribute.Pos.Word.2=sum(Attribute.Pos.Word.2)>0,
                                  Attribute.Pos.Word.3=sum(Attribute.Pos.Word.3)>0,
                                  Attribute.Pos.Word.4=sum(Attribute.Pos.Word.4)>0,
                                  Attribute.Pos.Word.5=sum(Attribute.Pos.Word.5)>0,
                                  Attribute.Neg.Word.1=sum(Attribute.Neg.Word.1)>0,
                                  Attribute.Neg.Word.2=sum(Attribute.Neg.Word.2)>0,
                                  Attribute.Neg.Word.3=sum(Attribute.Neg.Word.3)>0,
                                  Attribute.Neg.Word.4=sum(Attribute.Neg.Word.4)>0,
                                  Attribute.Neg.Word.5=sum(Attribute.Neg.Word.5)>0) %>%
                        data.frame() %>% mutate(business_id = as.character(business_id)) -> dataBusinessAttributesWords
                
                #Add new attributes to the rest of data
                
                #data %>% left_join(dataBusinessAttributesWords, by="business_id") ->> final_data

                dataBusinessAttributesWords
                

        }

        ###
        #Function to create the formula
        ###
        createFormula <- function(attributes){
                attributes_concat <- paste(as.character(attributes), collapse= " + ")
                attributes_concat <- paste(attributes_concat, "latitude", "longitude", sep = " + ")
                formula <- as.formula(paste("stars ~ ", attributes_concat, sep=""))
                formula
        }
        ###
        #End Function
        ###
                        
        ###
        #Function to create a model for a specifict State and category
        #No good results, try to use other model instead of LM
        ###
        createModelperState <- function(recalculate = FALSE, state_filter=NULL, category_filter="Food"){
                
                file <- paste(dir_states,"/",state_filter,"/model_category_",category_filter,".RDS", sep="")
                
                if(file.exists(file) && !recalculate){
                        model <- readRDS(file)
                        return(model)
                }
                
                #Get data
                dataPerCat <- mainData$getDatasetperCat(dataset = "business", category = category_filter, state_filter = state_filter)
                
                #TODO Fix this to add words attributes
                #dataBusinessAttributesWords <- addWordsAttributes(data = dataPerCat, state_filter = state_filter, category_filter = category_filter)
                
                data <- dataPerCat
                
                #Put a new variable, rate "GOOD" >4 stars, "BAD" < 3 stars
                colnames(data) <- make.names(names(data))
                
                #Get relevant attributes
                attributes_unformated <- mainData$getTotalsAttStars(overwrite=FALSE, state_filter = state_filter, 
                                                          category = category_filter)
                
                #Replace all spaces with dots and filter to only obtain relevant results
                attributes_unformated %>% mutate(attribute=gsub(" ", ".", attribute, fixed = TRUE)) %>%
                        mutate(attribute=gsub("-", ".", attribute, fixed = TRUE)) %>%
                        filter(state==state_filter, positive.over.total>validity_range, negative.over.total>validity_range,
                                      (difference_avg>validity_difference | difference_avg<(-validity_difference))) -> attributes_unformated
                
                #View(attributes_unformated)
                
                #Transform data to use only relevant columns
                data_formated <- transformAndClean(data, attributes_unformated$attribute)
                
                ###########
                #RF Model #
                ###########
                
                #Create formula for model
                formula <- createFormula(attributes_unformated$attribute)

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
                
                #Train model
                model <- train(formula, data = trainingData, method = "rf", ntree=500, 
                               trControl = trainControl(method = "cv", number = 5),
                               prox = TRUE, allowParallel = TRUE, trace = FALSE)
                
                prediction <- predict(model, testingData)
                prediction <- as.character(prediction)
                
                confusionMatrixModel <<- confusionMatrix(prediction, testingData$stars)
                
                #Print relevant information
                print(formula)
                
                #print(confusionMatrixModel)
                saveRDS(model, file = file)
                
                model
                
        }
        

        list(init=init,
             transformAndClean=transformAndClean,
             createFormula=createFormula,
             addWordsAttributes=addWordsAttributes,
             createModelperState=createModelperState)
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
                                        temp_table <- rbind(temp_table, NULL)
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
                        mutate(number=as.numeric(number)) %>%
                        group_by(word, ngram) %>% 
                        summarize(total_number=sum(number)) %>% 
                        arrange(desc(total_number)) -> table
                
                table
        }
        ###
        #End Function
        ###
        
        ###
        #Function to create a file with the count of words
        ###
        getDataCountOfWords <- function(overwrite=FALSE, state_filter=NULL, category_filter=NULL){
                
                if(is.null(category_filter)) {
                        cat_file_name = "all"
                }
                else{
                        cat_file_name = category_filter
                }
                
                #Set file name and path
                file <- paste(dir_states,"/",state_filter,"/wordsCount_",cat_file_name,".RDS", sep="")
                
                #Check if file exists and no overwrite to load exiting data and return it
                if(file.exists(file)&&!overwrite) {
                        #print("File exists")
                        return(readRDS(file))
                        }
                
                
                #Get review data
                print(paste("Loading data from State:",state_filter))
                aux <- mainData$getDataFiltered(dataset="review", state_parameter=state_filter)
                aux %>% select(business_id, review_id, stars, type, text) -> aux
                #%>% filter(business_id %in% )
                
                #Part data into blocks in order to speed up processing
                #Block size of 100 allows a speed of 1 block processed every 1,5 seconds
                #
                print(paste("Number of rows:",nrow(aux)))
                block_size = 100
                
                block_count = 1
                begin_block_pos = 1
                
                nrows_aux <- nrow(aux)
                
                #Apply countWords for each review
                print("Start processing texts...")
                print(paste("Number of total rows:",nrows_aux))
                
                while(begin_block_pos<nrows_aux){
                        
                        end_block_pos = begin_block_pos + block_size - 1
                        if(end_block_pos>nrows_aux) end_block_pos = nrows_aux
                        print(paste("Block:",block_count,": initial position = ",begin_block_pos,", final position = ",end_block_pos))
                        
                        initial <- TRUE
                        for(i in begin_block_pos:end_block_pos){
                                #print(paste("Processed:",(i/nrows_aux*100),"%"))
                                
                                #Get text from register and countWords
                                aux_text <- aux[i,"text"]
                                aux_table_counts <- countWords(aux_text, ngrams = c(1))
                                
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
                        
                        if(block_count == 1){
                                final_table_counts <- table_counts
                        }
                        else{
                                
                        }
                        
                        print(timestamp())
                        print(paste(end_block_pos,"processed, rest",(nrows_aux - end_block_pos)))
                        begin_block_pos = end_block_pos + 1
                        block_count = block_count + 1
                         
                }
                                
                saveRDS(file=file, final_table_counts)
                table_counts
                
                
        }
        ###
        #End function
        ###
        
        ###
        #Function to get top ngrams
        ###
        getTopNGrams <- function(stars_filter=c(4, 4.5, 5), state_filter = default_state, category_filter = NULL, top = 10){
                table <- getDataCountOfWords(state_filter = state_filter, category_filter = category_filter)
                
                #exclude common words
                table <- table[!(table$word %in% wordsToExclude),]
                
                table %>% filter(stars %in% stars_filter) %>%
                        group_by(word) %>% 
                        summarize(accumulate = sum(total_number)) %>% 
                        arrange(desc(accumulate)) %>% 
                        head(top) -> result

                result
        }
        ###
        #End Function
        ###
        
        list(countWords=countWords,
             getDataCountOfWords=getDataCountOfWords,
             getTopNGrams=getTopNGrams)
}

###
#Function to draw a map with situation per location
###

mapBusiness <- function(state_filter=default_state, category_filter, title_size=8){
        
        #Get data
        data <- mainData$getDatasetperCat(state_filter=state_filter, category = category_filter)
        
        #Locate central point for business
        avg_long <- mean(data$longitude)
        avg_lat <- mean(data$lat)
        
        #Remove all business at remote location
        data %>% filter(abs(abs(longitude)-abs(avg_long))<2,abs(abs(latitude)-abs(avg_lat))<2) -> data
        
        data %>% filter(stars %in% c(4, 4.5, 5)) -> dataTop

        data %>% filter(stars %in% c(1, 1.5, 2, 2.5)) -> dataBottom                        
                        
#        library(car)
#         scatterplot(latitude ~ longitude | stars, data = data, xlab="Longitude", ylab="Latitude", 
#                     main=paste("State:",state_filter,"- category:",category_filter))
        require(ggplot2)
        require(gridExtra)
        
        t <- theme(plot.title=element_text(size = title_size))
        
        gTop <- ggplot(data = dataTop, aes(x=longitude, y=latitude)) + geom_point(colour="blue")
        gTop <- gTop + ggtitle(paste(">=4 Stars business in state",state_filter,"and category",category_filter)) +t
        gBottom <- ggplot(data = dataBottom, aes(x=longitude, y=latitude)) + geom_point(colour="red")
        gBottom <- gBottom + ggtitle(paste("<3 Stars business in state",state_filter,"and category",category_filter)) +t
        grid.arrange(gTop, gBottom, ncol=2)
#         plot(latitude ~ longitude, data = data, xlab="Longitude", ylab="Latitude", 
#                     main=paste("State:",state_filter,"- category:",category_filter))
        
}

#Main function
main <- function(){
        
if(!dir.exists(dir_process)) { dir.create(dir_process)}

mainCategories <<- categories()
mainData <<- data()
mainTotals <<- totals()
mainTextAnalyst <<- textAnalyst()
mainModelController <<- modelController()

#datos <<- mainController$mainMenu()

mainModelController$init()
modelo <<- mainModelController$createModelperState(recalculate = TRUE, state_filter = default_state, category_filter = "Food")

# datos <<- mainCategories$getBusinessCategories("BW")
# datos_f <<- mainCategories$getUniqueCategories(state_filter="BW")
# tp <<- mainTextAnalyst$getPhrase()
# tp_sepparate <<- mainTextAnalyst$countWords(tp)
# state <- "XGL"
# 
# tp <<- mainTextAnalyst$getDataCountOfWords(overwrite=FALSE, state_filter=default_state)
# t2 <<- mainTextAnalyst$getTopNGrams(state_filter=state)
# t3 <<- mainTextAnalyst$getTopNGrams(stars_filter=c(1,1.5,2,2.5), state_filter=state)
#View(t2)



}