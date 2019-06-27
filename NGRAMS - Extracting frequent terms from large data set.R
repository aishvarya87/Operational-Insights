######################################### Ngrams : Promoters & Detractors ##############################################

library(RWeka)

pro_Corpus <- VCorpus(VectorSource(pro.df$reason))
det_Corpus <- VCorpus(VectorSource(det.df$reason))

pro_Corpus <- tm_map(pro_Corpus, stripWhitespace) # Remove unneccesary white spaces
pro_Corpus <- tm_map(pro_Corpus, removePunctuation) # Remove punctuation

det_Corpus <- tm_map(det_Corpus, stripWhitespace) # Remove unneccesary white spaces
det_Corpus <- tm_map(det_Corpus, removePunctuation) # Remove punctuation

for (i in seq(pro_Corpus)) {
  pro_Corpus[[i]] <- gsub('[^a-zA-Z|[:blank:]]', "", pro_Corpus[[i]])
}
for (i in seq(det_Corpus)) {
  det_Corpus[[i]] <- gsub('[^a-zA-Z|[:blank:]]', "", det_Corpus[[i]])
}

# add extra stop words: 'available' and 'via'
myStopwords1 <- c(stopwords("english"), "null", "rt", "i", "I", "symptom","Symptom", "description","Description", "steps", "callback", "by", "dtv", "triage", "inquiry", "reviewed", "na")
myStopwords2<- c(stopwords(source = "smart"), "ae", "rt")

# remove stopwords from corpus
pro_Corpus <- tm_map(pro_Corpus, removeWords, myStopwords1)
pro_Corpus <- tm_map(pro_Corpus, removeWords, myStopwords2)

# remove stopwords from corpus
det_Corpus <- tm_map(det_Corpus, removeWords, myStopwords1)
det_Corpus <- tm_map(det_Corpus, removeWords, myStopwords2)



## more processing 
pro_Corpus <- tm_map(pro_Corpus, removeNumbers) # Remove numbers
pro_Corpus <- tm_map(pro_Corpus, tolower) # Convert to lowercase
pro_Corpus <- tm_map(pro_Corpus, PlainTextDocument) # Plain text

det_Corpus <- tm_map(det_Corpus, removeNumbers) # Remove numbers
det_Corpus <- tm_map(det_Corpus, tolower) # Convert to lowercase
det_Corpus <- tm_map(det_Corpus, PlainTextDocument) # Plain text

pro_Corpus <- na.omit(pro_Corpus)
det_Corpus <- na.omit(det_Corpus)

pro_Corpus <- lapply(pro_Corpus, function(x) gsub(" +", " ", x))
det_Corpus <- lapply(det_Corpus, function(x) gsub(" +", " ", x))

n_grams_plot <- function(n, data) {
  options(mc.cores=1)
  
  # Builds n-gram tokenizer 
  tk <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
  # Create matrix
  ngrams_matrix <- TermDocumentMatrix(data, control=list(tokenize=tk))
  # make matrix for easy view
  ngrams_matrix <- as.matrix(rollup(ngrams_matrix, 2, na.rm=TRUE, FUN=sum))
  ngrams_matrix <- data.frame(word=rownames(ngrams_matrix), freq=ngrams_matrix[,1])
  # find 20 most frequent n-grams in the matrix
  ngrams_matrix <- ngrams_matrix[order(-ngrams_matrix$freq), ][2:10, ]
  ngrams_matrix$word <- factor(ngrams_matrix$word, as.character(ngrams_matrix$word))
  
  # plots
  ggplot(ngrams_matrix, aes(x=word, y=freq, label=freq)) + 
    geom_bar(stat="Identity",fill = "#009E73") + geom_text(check_overlap = T,vjust = "bottom",nudge_y = 0.2,angle = 30,size = 4) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("n-grams") + 
    ylab("Frequency")+coord_flip()
}

n_grams_plot(n=3, data=pro_Corpus)
n_grams_plot(n=4, data=pro_Corpus)


n_grams_plot(n=3, data=det_Corpus)
n_grams_plot(n=5, data=det_Corpus)

install.packages("MASS")

library("ggplot2")
library("readxl")
library("mice")
library("lattice")
library("MASS")
library("VIM")
library("arm")


setwd("C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Medallia\\Repair")
getwd()

nps <- read_excel("linear_nps.xlsx")
View(nps)

############################## Missing Value Treatment #######################################
options(max.print=1000000)
set.seed(1234)

# sapply(nps, function(x) sum(is.na(x)))
# 
#     
# init = mice(nps, maxit=0)
# meth = init$method
# predM = init$predictorMatrix
# 
# #methods(mice)
# 
# meth[("inoutwty")]="polyreg"
# meth[("wty_exception")]="cart"
# meth[("data_origin")]="logreg"
# meth[("service_type")]="polyreg"
# meth[("svc_prd")]="polyreg"
# meth[("asc_acctno")]="norm"
# meth[("new_center_type")]="polyreg"
# meth[("defect")]="cart"
# meth[("status")]="polyreg"
# meth[("symptom_cat1")]="polyreg"
# meth[("symptom_cat2")]="norm"
# meth[("symptom_cat3")]="norm"
# meth[("redo_grade")]="polyreg"
# meth[("symptom_cat1_des")]="polyreg"
# meth[("explain_doc_for_ps_repair")]="logreg"
# meth[("nps_1")]="cart"
# meth[("nps_2")]="polyreg"
# meth[("ces_1")]="cart"
# meth[("ces_2")]="polyreg"
# meth[("model")]="cart"
# meth[("cust")]="polyreg"
# 
# 
# set.seed(1000)
# imputed = mice(nps, method=meth, predictorMatrix=predM, m=3)
# 
# imputed <- complete(imputed)
# 
# View(imputed)
# 
# 
# library(dplyr)
# nps <- nps %>%
#   mutate(
#     inoutwty = as.factor(inoutwty),
#     wty_exception = as.factor(wty_exception),
#     data_origin = as.factor(data_origin),
#     service_type = as.factor(service_type),
#     svc_prd = as.factor(svc_prd),
#     new_center_type = as.factor(new_center_type),
#     defect = as.factor(defect),
#     status = as.factor(status),
#     symptom_cat1 = as.factor(symptom_cat1),
#     symptom_cat2 = as.factor(symptom_cat2),
#     symptom_cat3 = as.factor(symptom_cat3),
#     redo_grade = as.factor(redo_grade),
#     symptom_cat1_des = as.factor(symptom_cat1_des),
#     explain_doc_for_ps_repair = as.factor(explain_doc_for_ps_repair),
#     nps_1 = as.factor(nps_1),
#     nps_2 = as.factor(nps_2),
#     ces_1 = as.factor(ces_1),
#     ces_2 = as.factor(ces_2),
#     model = as.factor(model),
#     cust = as.factor(cust)
#   )
# 
# str(nps)
# 
# sapply(imputed, function(x) sum(is.na(x)))

##################################### regression ##############################################
############################### regression for nps_1 ########################


# fit1 <- glm(ordered(nps_1) ~ as.factor(inoutwty)+as.factor(wty_exception)+as.factor(data_origin)+as.factor(service_type)+
#               as.factor(svc_prd)+as.factor(new_center_type)+as.factor(defect)+as.factor(status)+as.factor(symptom_cat1)+
#               as.factor(symptom_cat2)+as.factor(symptom_cat3)+as.factor(redo_grade)+as.factor(symptom_cat1_des)+as.factor(explain_doc_for_ps_repair)+
#             as.factor(nps_2)+as.factor(ces_1)+as.factor(model)+as.factor(cust), family=binomial(), na.action=na.omit, data=nps)

fit1 <- glm(ordered(nps_1) ~ as.factor(inoutwty)+as.factor(wty_exception)+as.factor(data_origin)+as.factor(service_type)+as.factor(svc_prd)+
              as.factor(new_center_type)+as.factor(defect)+as.factor(symptom_cat1)+as.factor(symptom_cat2)+as.factor(symptom_cat3)+
              as.factor(ces_1), family=binomial(), na.action=na.omit, data=nps)

fit2 <- lm((nps_1) ~ as.factor(inoutwty)+as.factor(wty_exception)+as.factor(data_origin)+as.factor(service_type)+as.factor(svc_prd)+
             as.factor(new_center_type)+as.factor(defect)+as.factor(symptom_cat1)+as.factor(symptom_cat2)+as.factor(symptom_cat3)+as.factor(redo_grade)+
             as.factor(ces_1),na.action=na.omit, data=nps)

anova(fit2)
summary(fit2)


###################################################################################################################
####################################### Linear Regression - RRR ###################################################

setwd("C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Medallia\\Repair")
getwd()

repeated <- read_excel("linear_rrr.xlsx")
View(repeated)

###############################
library(plyr)

repeated$redo_grade <- revalue(repeated$redo_grade,
                               c("G"="3", "O"="2", "R"="1", "Y"="-1", "No"="4"))

repeated$redo_grade_1 <- as.numeric(as.character(repeated$redo_grade))

##############

fit3 <- lm((redo_grade_1) ~ as.factor(inoutwty)+as.factor(wty_exception)+as.factor(service_type)+as.factor(new_center_type)+as.factor(defect)+ as.factor(symptom_cat1)+
             as.factor(symptom_cat2)+repeated$posting_to_complete_days+as.factor(symptom_cat3),na.action=na.omit, data=repeated)

options(scipen = 999)

anova(fit3)
summary(fit3)



#####################################################################################################################
###################################### Linear Regression - CES ######################################################

ces <- read_excel("flat_extract_ces.xlsx")
View(ces)


## object_id	mc_city1	region	exch_type	inoutwty	wty_exception	service_type	svc_prd	asc_code	asc_name	new_center_type	defect	defectdesc	
#status	reason	detail_type	symptom_cat1	symptom_cat2	symptom_cat3	first_po_date	last_po_date	posting_date	repair_rcv_dt	
#call_rcv_dt	call_rcv_tm	asc_assign_dt	asc_assign_tm	asc_ack_dt	asc_ack_tm	eng_assign_by	eng_assign_dt	eng_assign_tm	first_visit_dt	
#first_visit_tm	last_visit_dt	last_visit_tm	complete_by	redo_grade	unit_rcv_dt	unit_rcv_tm	cic_sub_prd	symptom_cat1_des	symptom_cat2_des	
#symptom_cat3_des	symptom_cat_des	process_type	product_group	sub_product_group	material_group_desc	zzinoutwty	dsat	dimension	text_dimension	
#text_suggestion	explain_doc_for_ps_repair	nps	nps_1	ces	ces_1	posting_to_complete_days	asc_assign_to_repair_complete	asc_assign_to_eng_assign	
#asc_to_comp_cnt	asc_to_eng_cnt	unit_rcv_to_repair_complete_cnt	asc_to_comp_30min_cnt	asc_to_comp_1h_cnt	asc_to_comp_2h_cnt	asc_to_comp_1d_cnt	
#model	imei	complete_dt	complete_tm

fit4 <- lm((ces) ~ as.factor(inoutwty)+as.factor(wty_exception)+as.factor(svc_prd)+as.factor(new_center_type)+as.factor(defect)+ 
             as.factor(symptom_cat1)+as.factor(symptom_cat2)+as.factor(symptom_cat3),na.action=na.omit, data=ces)

anova(fit4)
summary(fit4)




setwd("C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Medallia\\Repair")
getwd()

#install.packages("tm", lib="C:/Program Files/R/R-3.5.0/library")

library(stringi)
library(tm)
library(qdap)
library(ggplot2)
library(slam)
library(gridExtra)
library(RWeka)
library(wordcloud)

#require(quanteda)
library(quanteda)

library(readxl)
flat <- read_excel("flat_extract_mobile.xlsx")
View(flat)

flat.df <- data.frame(flat)

########################################################################################################
########################################################################################################

text_Corpus <- VCorpus(VectorSource(flat.df$text_dimension))

text_Corpus <- tm_map(text_Corpus, stripWhitespace) # Remove unneccesary white spaces
text_Corpus <- tm_map(text_Corpus, removePunctuation) # Remove punctuation
text_Corpus <- tm_map(text_Corpus, removeNumbers) # Remove numbers
text_Corpus <- tm_map(text_Corpus, tolower) # Convert to lowercase
#text_Corpus <- tm_map(text_Corpus, stemDocument)
text_Corpus <- tm_map(text_Corpus, PlainTextDocument) # Plain text


# add extra stop words: 'available' and 'via'
myStopwords1 <- c(stopwords("english"), "null", "rt")
myStopwords2<- c(stopwords(source = "smart"), "ae", "rt")

# remove stopwords from corpus
text_Corpus <- tm_map(text_Corpus, removeWords, myStopwords1)
#text_Corpus <- tm_map(text_Corpus, removeWords, myStopwords2)


n_grams_plot <- function(n, data) {
  options(mc.cores=1)
  
  # Builds n-gram tokenizer 
  tk <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
  # Create matrix
  ngrams_matrix <- TermDocumentMatrix(data, control=list(tokenize=tk))
  # make matrix for easy view
  ngrams_matrix <- as.matrix(rollup(ngrams_matrix, 2, na.rm=TRUE, FUN=sum))
  ngrams_matrix <- data.frame(word=rownames(ngrams_matrix), freq=ngrams_matrix[,1])
  # find 20 most frequent n-grams in the matrix
  ngrams_matrix <- ngrams_matrix[order(-ngrams_matrix$freq), ][2:20, ]
  ngrams_matrix$word <- factor(ngrams_matrix$word, as.character(ngrams_matrix$word))
  
  # plots
  ggplot(ngrams_matrix, aes(x=word, y=freq, label=freq)) + 
    geom_bar(stat="Identity",fill = "#009E73") + geom_text(check_overlap = T,vjust = "bottom",nudge_y = 0.2,angle = 30,size = 4) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("n-grams") + 
    ylab("Frequency")+coord_flip()
}

n_grams_plot(n=2, data=text_Corpus)

n_grams_plot(n=3, data=text_Corpus)

n_grams_plot(n=5, data=text_Corpus)

n_grams_plot(n=7, data=text_Corpus)


# which words are associated with 'Shipping'?
text_tdm <- TermDocumentMatrix(text_Corpus, control = list(wordLengths = c(4, Inf)))
text_tdm

findAssocs(text_tdm, "shipping", 0.1)
findAssocs(text_tdm, "didnt", 0.1)
findAssocs(text_tdm, "charger", 0.1)
findAssocs(text_tdm, "battery", 0.1)
findAssocs(text_tdm, "send", 0.1)
findAssocs(text_tdm, "communication", 0.1)
findAssocs(text_tdm, "data", 0.1)
findAssocs(text_tdm, "repair", 0.1)


##############################################################################################################
################################ Analytics with another field #############################################################


text_Corpus11 <- VCorpus(VectorSource(flat.df$text_suggestion))

text_Corpus11 <- tm_map(text_Corpus11, stripWhitespace) # Remove unneccesary white spaces
text_Corpus11 <- tm_map(text_Corpus11, removePunctuation) # Remove punctuation
text_Corpus11 <- tm_map(text_Corpus11, removeNumbers) # Remove numbers
text_Corpus11 <- tm_map(text_Corpus11, tolower) # Convert to lowercase
#text_Corpus11 <- tm_map(text_Corpus11, stemDocument)
text_Corpus11 <- tm_map(text_Corpus11, PlainTextDocument) # Plain text


# add extra stop words: 'available' and 'via'
myStopwords1 <- c(stopwords("english"), "null", "rt", "No", "no", "yes", "Yes", "actually", "able")
myStopwords2<- c(stopwords(source = "smart"), "ae", "rt")

# remove stopwords from corpus
text_Corpus11 <- tm_map(text_Corpus11, removeWords, myStopwords1)
#text_Corpus11 <- tm_map(text_Corpus11, removeWords, myStopwords2)


n_grams_plot <- function(n, data) {
  options(mc.cores=1)
  
  # Builds n-gram tokenizer 
  tk <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
  # Create matrix
  ngrams_matrix <- TermDocumentMatrix(data, control=list(tokenize=tk))
  # make matrix for easy view
  ngrams_matrix <- as.matrix(rollup(ngrams_matrix, 2, na.rm=TRUE, FUN=sum))
  ngrams_matrix <- data.frame(word=rownames(ngrams_matrix), freq=ngrams_matrix[,1])
  # find 20 most frequent n-grams in the matrix
  ngrams_matrix <- ngrams_matrix[order(-ngrams_matrix$freq), ][2:20, ]
  ngrams_matrix$word <- factor(ngrams_matrix$word, as.character(ngrams_matrix$word))
  
  # plots
  ggplot(ngrams_matrix, aes(x=word, y=freq)) + 
    geom_bar(stat="Identity") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("n-grams") + 
    ylab("Frequency")+coord_flip()
}

n_grams_plot(n=2, data=text_Corpus11)

n_grams_plot(n=3, data=text_Corpus11)

n_grams_plot(n=5, data=text_Corpus11)

n_grams_plot(n=7, data=text_Corpus11)


# which words are associated with 'Shipping'?
text_tdm <- TermDocumentMatrix(text_Corpus11, control = list(wordLengths = c(4, Inf)))
text_tdm

findAssocs(text_tdm, "shipping", 0.1)
findAssocs(text_tdm, "didnt", 0.1)
findAssocs(text_tdm, "charger", 0.1)
findAssocs(text_tdm, "battery", 0.1)
findAssocs(text_tdm, "send", 0.1)



############################################################################################################################
############################################################################################################################
############################################# NGRAMS FOR AGENTS COMMENTS ###################################################

library(readxl)
flat <- read_excel("flat_extract_mobile.xlsx")
View(flat)

flat.df <- data.frame(flat)

########################################################################################################
########################################################################################################

text_Corpus <- VCorpus(VectorSource(flat.df$text_dimension))

text_Corpus <- tm_map(text_Corpus, stripWhitespace) # Remove unneccesary white spaces
text_Corpus <- tm_map(text_Corpus, removePunctuation) # Remove punctuation
text_Corpus <- tm_map(text_Corpus, removeNumbers) # Remove numbers
text_Corpus <- tm_map(text_Corpus, tolower) # Convert to lowercase
#text_Corpus <- tm_map(text_Corpus, stemDocument)
text_Corpus <- tm_map(text_Corpus, PlainTextDocument) # Plain text


# add extra stop words: 'available' and 'via'
myStopwords1 <- c(stopwords("english"), "null", "rt")
myStopwords2<- c(stopwords(source = "smart"), "ae", "rt")

# remove stopwords from corpus
text_Corpus <- tm_map(text_Corpus, removeWords, myStopwords1)
#text_Corpus <- tm_map(text_Corpus, removeWords, myStopwords2)


n_grams_plot <- function(n, data) {
  options(mc.cores=1)
  
  # Builds n-gram tokenizer 
  tk <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
  # Create matrix
  ngrams_matrix <- TermDocumentMatrix(data, control=list(tokenize=tk))
  # make matrix for easy view
  ngrams_matrix <- as.matrix(rollup(ngrams_matrix, 2, na.rm=TRUE, FUN=sum))
  ngrams_matrix <- data.frame(word=rownames(ngrams_matrix), freq=ngrams_matrix[,1])
  # find 20 most frequent n-grams in the matrix
  ngrams_matrix <- ngrams_matrix[order(-ngrams_matrix$freq), ][2:20, ]
  ngrams_matrix$word <- factor(ngrams_matrix$word, as.character(ngrams_matrix$word))
  
  # plots
  ggplot(ngrams_matrix, aes(x=word, y=freq, label=freq)) + 
    geom_bar(stat="Identity",fill = "#009E73") + geom_text(check_overlap = T,vjust = "bottom",nudge_y = 0.2,angle = 30,size = 4) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("n-grams") + 
    ylab("Frequency")+coord_flip()
}

n_grams_plot(n=2, data=text_Corpus)

n_grams_plot(n=3, data=text_Corpus)

n_grams_plot(n=5, data=text_Corpus)

n_grams_plot(n=7, data=text_Corpus)


# which words are associated with 'Shipping'?
text_tdm <- TermDocumentMatrix(text_Corpus, control = list(wordLengths = c(4, Inf)))
text_tdm

findAssocs(text_tdm, "shipping", 0.1)
findAssocs(text_tdm, "didnt", 0.1)
findAssocs(text_tdm, "charger", 0.1)
findAssocs(text_tdm, "battery", 0.1)
findAssocs(text_tdm, "send", 0.1)
findAssocs(text_tdm, "communication", 0.1)
findAssocs(text_tdm, "data", 0.1)
findAssocs(text_tdm, "repair", 0.1)


##############################################################################################################
################################ Analytics with another field #############################################################


text_Corpus11 <- VCorpus(VectorSource(flat.df$text_suggestion))

text_Corpus11 <- tm_map(text_Corpus11, stripWhitespace) # Remove unneccesary white spaces
text_Corpus11 <- tm_map(text_Corpus11, removePunctuation) # Remove punctuation
text_Corpus11 <- tm_map(text_Corpus11, removeNumbers) # Remove numbers
text_Corpus11 <- tm_map(text_Corpus11, tolower) # Convert to lowercase
#text_Corpus11 <- tm_map(text_Corpus11, stemDocument)
text_Corpus11 <- tm_map(text_Corpus11, PlainTextDocument) # Plain text


# add extra stop words: 'available' and 'via'
myStopwords1 <- c(stopwords("english"), "null", "rt", "No", "no", "yes", "Yes", "actually", "able")
myStopwords2<- c(stopwords(source = "smart"), "ae", "rt")

# remove stopwords from corpus
text_Corpus11 <- tm_map(text_Corpus11, removeWords, myStopwords1)
#text_Corpus11 <- tm_map(text_Corpus11, removeWords, myStopwords2)


n_grams_plot <- function(n, data) {
  options(mc.cores=1)
  
  # Builds n-gram tokenizer 
  tk <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
  # Create matrix
  ngrams_matrix <- TermDocumentMatrix(data, control=list(tokenize=tk))
  # make matrix for easy view
  ngrams_matrix <- as.matrix(rollup(ngrams_matrix, 2, na.rm=TRUE, FUN=sum))
  ngrams_matrix <- data.frame(word=rownames(ngrams_matrix), freq=ngrams_matrix[,1])
  # find 20 most frequent n-grams in the matrix
  ngrams_matrix <- ngrams_matrix[order(-ngrams_matrix$freq), ][2:20, ]
  ngrams_matrix$word <- factor(ngrams_matrix$word, as.character(ngrams_matrix$word))
  
  # plots
  ggplot(ngrams_matrix, aes(x=word, y=freq)) + 
    geom_bar(stat="Identity") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("n-grams") + 
    ylab("Frequency")+coord_flip()
}

n_grams_plot(n=2, data=text_Corpus11)

n_grams_plot(n=3, data=text_Corpus11)

n_grams_plot(n=5, data=text_Corpus11)

n_grams_plot(n=7, data=text_Corpus11)


# which words are associated with 'Shipping'?
text_tdm <- TermDocumentMatrix(text_Corpus11, control = list(wordLengths = c(4, Inf)))
text_tdm

findAssocs(text_tdm, "shipping", 0.1)
findAssocs(text_tdm, "didnt", 0.1)
findAssocs(text_tdm, "charger", 0.1)
findAssocs(text_tdm, "battery", 0.1)
findAssocs(text_tdm, "send", 0.1)


Ngrams

setwd("C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Medallia")
getwd()

#install.packages("tm", lib="C:/Program Files/R/R-3.5.0/library")

library(stringi)
library(tm)
library(qdap)
library(ggplot2)
library(slam)
library(gridExtra)
library(RWeka)
library(wordcloud)

#require(quanteda)
library(quanteda)

library(readxl)
reason <- read_excel("IM_results_reason.xlsx")
reason.df <- data.frame(reason)

################################## medallia.df$reason ####################################################

text_Corpus <- VCorpus(VectorSource(reason.df$reason))

#text_Corpus <- tm_map(text_Corpus, stripWhitespace) # Remove unneccesary white spaces
text_Corpus <- tm_map(text_Corpus, removePunctuation) # Remove punctuation

for (i in seq(text_Corpus)) {
  text_Corpus[[i]] <- gsub('[^a-zA-Z|[:blank:]]', "", text_Corpus[[i]])
}

text_Corpus <- tm_map(text_Corpus, removeNumbers) # Remove numbers
text_Corpus <- tm_map(text_Corpus, tolower) # Convert to lowercase
#text_Corpus <- tm_map(text_Corpus, stemDocument)
text_Corpus <- tm_map(text_Corpus, PlainTextDocument) # Plain text


# add extra stop words: 'available' and 'via'
myStopwords1 <- c(stopwords("english"), "null", "rt", "i", "I", "the", "my")
myStopwords2<- c(stopwords(source = "smart"), "ae", "rt")

# remove stopwords from corpus
text_Corpus <- tm_map(text_Corpus, removeWords, myStopwords1)
text_Corpus <- tm_map(text_Corpus, removeWords, myStopwords2)


n_grams_plot <- function(n, data) {
  options(mc.cores=1)
  
  # Builds n-gram tokenizer 
  tk <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
  # Create matrix
  ngrams_matrix <- TermDocumentMatrix(data, control=list(tokenize=tk))
  # make matrix for easy view
  ngrams_matrix <- as.matrix(rollup(ngrams_matrix, 2, na.rm=TRUE, FUN=sum))
  ngrams_matrix <- data.frame(word=rownames(ngrams_matrix), freq=ngrams_matrix[,1])
  # find 20 most frequent n-grams in the matrix
  ngrams_matrix <- ngrams_matrix[order(-ngrams_matrix$freq), ][2:10, ]
  ngrams_matrix$word <- factor(ngrams_matrix$word, as.character(ngrams_matrix$word))
  
  # plots
  ggplot(ngrams_matrix, aes(x=word, y=freq, label=freq)) + 
    geom_bar(stat="Identity",fill = "#009E73") + geom_text(check_overlap = T,vjust = "bottom",nudge_y = 0.2,angle = 30,size = 4) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("n-grams") + 
    ylab("Frequency")+coord_flip()
}

n_grams_plot(n=2, data=text_Corpus)

n_grams_plot(n=3, data=text_Corpus)

n_grams_plot(n=4, data=text_Corpus)

n_grams_plot(n=5, data=text_Corpus)

n_grams_plot(n=7, data=text_Corpus)


# which words are associated with 'Shipping'?
text_tdm <- TermDocumentMatrix(text_Corpus, control = list(wordLengths = c(4, Inf)))
text_tdm

findAssocs(text_tdm, "shipping", 0.1)
findAssocs(text_tdm, "didnt", 0.1)
findAssocs(text_tdm, "damage", 0.1)
findAssocs(text_tdm, "charger", 0.1)
findAssocs(text_tdm, "battery", 0.1)
findAssocs(text_tdm, "warranty", 0.1)
findAssocs(text_tdm, "communication", 0.1)
findAssocs(text_tdm, "data", 0.1)
findAssocs(text_tdm, "repair", 0.1)
findAssocs(text_tdm, "water", 0.1)
findAssocs(text_tdm, "samsung", 0.1)
findAssocs(text_tdm, "ubreakifix", 0.1)
findAssocs(text_tdm, "texas", 0.1)

################################## medallia.df$comment ####################################################

setwd("C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Medallia")
getwd()

library(readxl)
comment <- read_excel("IM_results_comment.xlsx")
View(comment)

comment.df <- data.frame(comment)

text_Corpus22 <- VCorpus(VectorSource(comment.df$comment))

text_Corpus22 <- tm_map(text_Corpus22, stripWhitespace) # Remove unneccesary white spaces
text_Corpus22 <- tm_map(text_Corpus22, removePunctuation) # Remove punctuation

for (i in seq(text_Corpus22)) {
  text_Corpus22[[i]] <- gsub('[^a-zA-Z|[:blank:]]', "", text_Corpus22[[i]])
}

text_Corpus22 <- tm_map(text_Corpus22, removeNumbers) # Remove numbers
#text_Corpus22 <- tm_map(text_Corpus22, tolower) # Convert to lowercase
#text_Corpus22 <- tm_map(text_Corpus22, stemDocument)
text_Corpus22 <- tm_map(text_Corpus22, PlainTextDocument) # Plain text


# add extra stop words: 'available' and 'via'
myStopwords1 <- c(stopwords("english"), "null", "rt", "i", "I", "the", "my")
myStopwords2<- c(stopwords(source = "smart"), "ae", "rt")

# remove stopwords from corpus
text_Corpus22 <- tm_map(text_Corpus22, removeWords, myStopwords1)
text_Corpus22 <- tm_map(text_Corpus22, removeWords, myStopwords2)


n_grams_plot <- function(n, data) {
  options(mc.cores=1)
  
  # Builds n-gram tokenizer 
  tk <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
  # Create matrix
  ngrams_matrix <- TermDocumentMatrix(data, control=list(tokenize=tk))
  # make matrix for easy view
  ngrams_matrix <- as.matrix(rollup(ngrams_matrix, 2, na.rm=TRUE, FUN=sum))
  ngrams_matrix <- data.frame(word=rownames(ngrams_matrix), freq=ngrams_matrix[,1])
  # find 20 most frequent n-grams in the matrix
  ngrams_matrix <- ngrams_matrix[order(-ngrams_matrix$freq), ][2:10, ]
  ngrams_matrix$word <- factor(ngrams_matrix$word, as.character(ngrams_matrix$word))
  
  # plots
  ggplot(ngrams_matrix, aes(x=word, y=freq, label=freq)) + 
    geom_bar(stat="Identity",fill = "#009E73") + geom_text(check_overlap = T,vjust = "bottom",nudge_y = 0.2,angle = 30,size = 4) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("n-grams") + 
    ylab("Frequency")+coord_flip()
}

n_grams_plot(n=2, data=text_Corpus22)

n_grams_plot(n=3, data=text_Corpus22)

n_grams_plot(n=4, data=text_Corpus22)

n_grams_plot(n=5, data=text_Corpus22)

n_grams_plot(n=7, data=text_Corpus22)


# which words are associated with 'Shipping'?
text_tdm22 <- TermDocumentMatrix(text_Corpus22, control = list(wordLengths = c(4, Inf)))
text_tdm22

findAssocs(text_tdm22, "shipping", 0.1)
findAssocs(text_tdm22, "didnt", 0.1)
findAssocs(text_tdm22, "water", 0.1)
findAssocs(text_tdm22, "charger", 0.1)
findAssocs(text_tdm22, "battery", 0.1)
findAssocs(text_tdm22, "send", 0.1)
findAssocs(text_tdm22, "communication", 0.1)
findAssocs(text_tdm22, "data", 0.1)
findAssocs(text_tdm22, "repair", 0.1)


setwd("C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Medallia\\Repair")
getwd()

#install.packages("tm", lib="C:/Program Files/R/R-3.5.0/library")

library(stringi)
library(tm)
library(qdap)
library(ggplot2)
library(slam)
library(gridExtra)
library(RWeka)
library(wordcloud)

#require(quanteda)
library(quanteda)

library(readxl)
agent_comment <- read_excel("agent_comments_svc027.xlsx")

agent_comment.df <- data.frame(agent_comment)
View(agent_comment.df)

################################## medallia.df$reason ####################################################

text_Corpus33 <- VCorpus(VectorSource(agent_comment$feedback))

text_Corpus33 <- tm_map(text_Corpus33, stripWhitespace) # Remove unneccesary white spaces
text_Corpus33 <- tm_map(text_Corpus33, removePunctuation) # Remove punctuation

for (i in seq(text_Corpus33)) {
  text_Corpus33[[i]] <- gsub('[^a-zA-Z|[:blank:]]', "", text_Corpus33[[i]])
}

# add extra stop words: 'available' and 'via'
myStopwords1 <- c(stopwords("english"), "null", "rt", "i", "I", "symptom","Symptom", "description","Description", "steps", "callback", "by", "dtv", "triage", "inquiry", "reviewed", "na")
myStopwords2<- c(stopwords(source = "smart"), "ae", "rt")

# remove stopwords from corpus
text_Corpus33 <- tm_map(text_Corpus33, removeWords, myStopwords1)
text_Corpus33 <- tm_map(text_Corpus33, removeWords, myStopwords2)

## more processing 
text_Corpus33 <- tm_map(text_Corpus33, removeNumbers) # Remove numbers
text_Corpus33 <- tm_map(text_Corpus33, tolower) # Convert to lowercase
#text_Corpus33 <- tm_map(text_Corpus33, stemDocument)
text_Corpus33 <- tm_map(text_Corpus33, PlainTextDocument) # Plain text
#text_Corpus33 <- tm_map(text_Corpus33, stemCompletion)

#write.csv(text_Corpus33, file = "C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Medallia\\Repair\\corpus_agent_comments.csv", row.names = FALSE)


n_grams_plot <- function(n, data) {
  options(mc.cores=1)
  
  # Builds n-gram tokenizer 
  tk <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
  # Create matrix
  ngrams_matrix <- TermDocumentMatrix(data, control=list(tokenize=tk))
  # make matrix for easy view
  ngrams_matrix <- as.matrix(rollup(ngrams_matrix, 2, na.rm=TRUE, FUN=sum))
  ngrams_matrix <- data.frame(word=rownames(ngrams_matrix), freq=ngrams_matrix[,1])
  # find 20 most frequent n-grams in the matrix
  ngrams_matrix <- ngrams_matrix[order(-ngrams_matrix$freq), ][2:20, ]
  ngrams_matrix$word <- factor(ngrams_matrix$word, as.character(ngrams_matrix$word))
  
  # plots
  ggplot(ngrams_matrix, aes(x=word, y=freq, label=freq)) + 
    geom_bar(stat="Identity",fill = "#009E73") + geom_text(check_overlap = T,vjust = "bottom",nudge_y = 0.2,angle = 30,size = 4) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("n-grams") + 
    ylab("Frequency")+coord_flip()
}

n_grams_plot(n=2, data=text_Corpus33)

n_grams_plot(n=3, data=text_Corpus33)

n_grams_plot(n=4, data=text_Corpus33)

n_grams_plot(n=5, data=text_Corpus33)

n_grams_plot(n=7, data=text_Corpus33)


# which words are associated with 'Shipping'?
text_tdm33 <- TermDocumentMatrix(text_Corpus33, control = list(wordLengths = c(4, Inf)))
text_tdm33

findAssocs(text_tdm33, "shipping", 0.1)
findAssocs(text_tdm33, "didnt", 0.1)
findAssocs(text_tdm33, "never", 0.1)
findAssocs(text_tdm33, "charger", 0.1)
findAssocs(text_tdm33, "battery", 0.1)
findAssocs(text_tdm33, "send", 0.1)
findAssocs(text_tdm33, "communication", 0.1)
findAssocs(text_tdm33, "data", 0.1)
findAssocs(text_tdm33, "repair", 0.1)

Search with Hashtag


setwd("C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Analytics")
getwd()

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#install.packages("twitteR")
library("twitteR")
library("openssl")
library("httpuv")

#install.packages("RCurl")
library("RCurl")
library("curl")

library("NLP")
library("syuzhet")
library("tm")
library("SnowballC")
library("stringi")
library("topicmodels")

library("ggplot2")
library("wordcloud")

library("RWeka")

library("magrittr") 
library("purrr")
library("stringi")
library("qdap")
library("slam")
library("gridExtra")
library("quanteda")
library("dplyr")

#Using my personal Twitter keys
api_key <- "KuUiiNah1zaZYtAz0S74k36Jq"
api_secret <- "7ArNyLhrnxXhj5t7BE6NvccWXuaq3pS94jUP9YRRnQU94w5yJH"
token <- "103206999-WX5ezdiQXtat59jwk0yDpqCHZoD8WPTINNoaGHG1"
token_secret <- "Ahguqu5RWk6wEGm5LlIKqUVpUTNsJMuC3zLQNv81j01st"

#Authenticate from Twitter
setup_twitter_oauth(api_key, api_secret, token, token_secret)
origop <- options("httr_oauth_cache")
options(httr_oauth_cache = TRUE)

#########################################################################################################

tweets_s <- searchTwitter("#Samsung OR GalaxyS10", n=5000, lang="en", since="2018-11-01")

samsung_tweets.df <- twListToDF(tweets_s)
View(samsung_tweets.df)

################### Convert to Text - Method 1 ###################################################

# build a corpus, and specify the source to be character vectors
samsung_Corpus <- Corpus(VectorSource(samsung_tweets.df$text))

samsung_Corpus <- sapply(samsung_Corpus, function(row) iconv(row, "latin1", "ASCII", sub=""))
samsung_Corpus <- Corpus(VectorSource(samsung_Corpus))

#convert to lower case
samsung_Corpus <- tm_map(samsung_Corpus, content_transformer(tolower))

# # remove punctuation
samsung_Corpus <- tm_map(samsung_Corpus, removePunctuation)

# remove numbers
samsung_Corpus <- tm_map(samsung_Corpus, removeNumbers)

# remove URLs
removeURL <- function(x) gsub("(ftp|http)(s?)://.*\\b", "", x)

### myCorpus <- tm_map(myCorpus, removeURL, lazy=TRUE)
samsung_Corpus <- tm_map(samsung_Corpus, content_transformer(removeURL))
#

# add extra stop words: 'available' and 'via'
myStopwords1 <- c(stopwords("english"), "available", "via", "rt")
myStopwords2<- c(stopwords(source = "smart"), "ae", "rt")

# remove stopwords from corpus
samsung_Corpus <- tm_map(samsung_Corpus, removeWords, myStopwords1)

#
samsung_Corpus <- tm_map(samsung_Corpus, removeWords, myStopwords2)
##

# completion
samsung_CorpusCopy <- samsung_Corpus

# stem words
#samsung_Corpus <- tm_map(samsung_Corpus, stemDocument)

########## Mining ###########################

for (i in 1:10) {
  cat(paste("[[", i, "]] ", sep = ""))
  #writeLines(myCorpus[[i]])
  writeLines(as.character(samsung_Corpus[[i]]))
}

#################### Stem completion ############################

# myCorpus <- tm_map(myCorpus, content_transformer(stemCompletion), dictionary = myCorpusCopy, lazy=TRUE)
#samsung_Corpus <- tm_map(samsung_Corpus, content_transformer(stemCompletion), dictionary = samsung_CorpusCopy)

#################################################################################################
############################# count frequency of “mining” ######################################

samsung_tdm <- TermDocumentMatrix(samsung_Corpus, control = list(wordLengths = c(3, Inf)))
samsung_tdm

rowTotals <- apply(samsung_tdm , 1, sum) #Find the sum of words in each Document
samsung_tdm.new   <- samsung_tdm[rowTotals> 0, ]

## Freqency words and Association
samsung_idx <- which(dimnames(samsung_tdm.new)$Terms == "samsung")
inspect(samsung_tdm.new[samsung_idx + (0:5), 10:20])

#inspect frequent words
(freq.terms <- findFreqTerms(samsung_tdm.new, lowfreq=20))

term.freq <- rowSums(as.matrix(samsung_tdm.new))
term.freq <- subset(term.freq, term.freq >=5)
samsung_df <- data.frame(term = names(term.freq), freq = term.freq)

#################################### Plot the words - BARCHART ########################################

View(samsung_df)
sorted_samsung_df <- samsung_df[sort(samsung_df$freq[1:20], decreasing= TRUE),]
sorted_samsung_df <- within(sorted_samsung_df, 
                            freq<-factor(freq, 
                                         levels=names(sort(table(freq),
                                                           decreasing = TRUE))))
ggplot(sorted_samsung_df, aes(x=term, y=freq), title="Samsung Barchart") + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") +coord_flip()

# which words are associated with 'Samsung'?
findAssocs(samsung_tdm.new, "Samsung", 0.5)

####################################### WORDCLOUD METHOD 1 ######################################

sa <- as.matrix(samsung_tdm.new)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(sa), decreasing = T)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 10,
          random.order = F, colors = brewer.pal(8, 'Set1'))

############################# Clustering ########################################
####### Samsung ######

# remove sparse terms
samsung_tdm2 <- removeSparseTerms(samsung_tdm.new, sparse = 0.98)
samsung_m2 <- as.matrix(samsung_tdm2)
# cluster terms
distMatrix2 <- dist(scale(samsung_m2))
fit2 <- hclust(distMatrix2, method = "ward.D")

plot(fit2)
rect.hclust(fit2, k = 3) # cut tree into 6 clusters

################################ Topic Model #######################################

samsung_dtm <- DocumentTermMatrix(samsung_m2)
samsung_lda <- LDA(samsung_dtm, k = 8) # find 8 topics
samsung_term <- terms(samsung_lda, 6) # first 6 terms of every topic
samsung_term

require(data.table) #fore IDate
samsung_term <- apply(samsung_term, MARGIN = 2, paste, collapse = ", ")
samsung_term


#############################################################################################################
###### Samsung Sentiment Analysis on #SAMSUNG - Part I - Postive and Negative Sentiment #######

samsung_hashtag.df <- twListToDF(tweets_s)
View(samsung_hashtag.df)
head(samsung_hashtag.df$text)

samsung_tag.df2 <- gsub("http.*","",samsung_hashtag.df$text)
samsung_tag.df2 <- gsub("https.*","",samsung_tag.df2)
samsung_tag.df2 <- gsub("#.*","",samsung_tag.df2)
#samsung_tag.df2 <- gsub("@.*","",samsung_tag.df2)
head(samsung_tag.df2)

############### get sentiment score ##############

word_tag.df <- as.vector(samsung_tag.df2)

emotion_tag.df <- get_nrc_sentiment(word_tag.df)

emotion_tag.df2 <- cbind(samsung_tag.df2, emotion_tag.df) 
View(emotion_tag.df2)

write.csv(emotion_tag.df2, file = "C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Analytics\\2weeks\\hashtag_samsung.csv", row.names = FALSE)

############ get positive sentiment ################

sent.value <- get_sentiment(word_tag.df)
most.positive <- word_tag.df[sent.value == max(sent.value)]
most.positive

########### get negative sentiment ################
most.negative <- word_tag.df[sent.value <= min(sent.value)] 
most.negative 

sent.value

############ segregate positive and negative tweets #############

positive.tweets <- word_tag.df[sent.value > 0]
head(positive.tweets)

negative.tweets <- word_tag.df[sent.value < 0]
head(negative.tweets)
View(negative.tweets)

write.csv(negative.tweets, file = "C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Analytics\\2weeks\\negativetweets.csv", row.names = FALSE)

neutral.tweets <- word_tag.df[sent.value == 0]
head(neutral.tweets)

########### alternate way ##################

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
head(category_senti)


####################### Bind the sentiment to the tweet ####################

head(samsung_tag.df2)
category_senti2 <- cbind(samsung_tag.df2,category_senti, sent.value) 
head(category_senti2)


category_senti3 <- cbind(samsung_tweets.df,category_senti, sent.value) 
############ count of sentiment ################
table(category_senti)

write.csv(category_senti2, file = "C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Analytics\\2weeks\\hashtag_sentiment.csv", row.names = FALSE)
write.csv(category_senti3, file = "C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Analytics\\2weeks\\twitter_sentiment.csv", row.names = FALSE)


##########################################################################################################
####################### Sentiment Analysis - Part II - All Sentiments ####################################

View(samsung_tweets.df)

samsung_tweets.df <- gsub("http.*","",samsung_tweets.df$text)
samsung_tweets.df <- gsub("https.*","",samsung_tweets.df$text)
samsung_tweets.df <- gsub("#.*","",samsung_tweets.df$text)

#getting emotions using in-built function
mysentiment_samsung <- get_nrc_sentiment((samsung_tweets.df$text))

#calculationg total score for each sentiment
Sentimentscores_samsung <- data.frame(colSums(mysentiment_samsung[,]))
Sentimentscores_samsung

names(Sentimentscores_samsung)<-"Score"
Sentimentscores_samsung <-cbind("sentiment"=rownames(Sentimentscores_samsung),Sentimentscores_samsung)
rownames(Sentimentscores_samsung)<-NULL

View(Sentimentscores_samsung)

#plotting the sentiments with scores
ggplot(data=Sentimentscores_samsung,aes(x=sentiment,y=Score, label=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  geom_text(check_overlap = T,# automatically reduce overlap (deletes some labels)
            vjust = "bottom", # adjust the vertical orientation
            nudge_y = 0.01, # move the text up a bit so it doesn't touch the points
            size = 3 # make the text smaller (to reduce overlap more)
  ) +
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of tweets with #Samsung")

samsung_tag.df3 <- cbind(samsung_tweets.df,mysentiment_samsung) 
write.csv(samsung_tag.df3, file = "C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Analytics\\2weeks\\hashtag_sentimentscore.csv", row.names = FALSE)


##############################################################################################################################
####################################### Text Analytics #############################################

View(samsung_tweets.df)

# build a corpus, and specify the source to be character vectors
hashtag_Corpus <- Corpus(VectorSource(samsung_tweets.df$text))

hashtag_Corpus <- sapply(hashtag_Corpus, function(row) iconv(row, "latin1", "ASCII", sub=""))
hashtag_Corpus <- Corpus(VectorSource(hashtag_Corpus))


#convert to lower case
hashtag_Corpus <- tm_map(hashtag_Corpus, content_transformer(tolower))

# # remove punctuation
hashtag_Corpus <- tm_map(hashtag_Corpus, removePunctuation)

# remove numbers
hashtag_Corpus <- tm_map(hashtag_Corpus, removeNumbers)

# remove URLs
removeURL <- function(x) gsub("(ftp|http)(s?)://.*\\b", "", x)

### myCorpus <- tm_map(myCorpus, removeURL, lazy=TRUE)
hashtag_Corpus <- tm_map(hashtag_Corpus, content_transformer(removeURL))

# add extra stop words: 'available' and 'via'
myStopwords1 <- c(stopwords("english"), "available", "via", "rt")
#myStopwords2<- c(stopwords("smart"), "ae", "rt")

# remove stopwords from corpus
hashtag_Corpus <- tm_map(hashtag_Corpus, removeWords, myStopwords1)
#
hashtag_Corpus <- tm_map(hashtag_Corpus, removeWords, myStopwords2)

##

#￼# keep a copy of corpus to use later as a dictionary for stem
# completion
hashtag_CorpusCopy <- hashtag_Corpus

# stem words
#hashtag_Corpus <- tm_map(hashtag_Corpus, stemDocument)


##################################################################################
################################# Mining #########################################

# inspect the first 5 documents (tweets) inspect(myCorpus[1:5]) 
# The code below is used for to make text fit for paper width 
for (i in 1:5) {
  cat(paste("[[", i, "]] ", sep = ""))
  #writeLines(myCorpus[[i]])
  writeLines(as.character(hashtag_Corpus[[i]]))
}

#################### Stem completion ############################

# myCorpus <- tm_map(myCorpus, content_transformer(stemCompletion), dictionary = myCorpusCopy, lazy=TRUE)

#apple_Corpus <- tm_map(apple_Corpus, content_transformer(stemCompletion), dictionary = apple_CorpusCopy)
#samsung_Corpus <- tm_map(samsung_Corpus, content_transformer(stemCompletion), dictionary = samsung_CorpusCopy)
#google_Corpus <- tm_map(google_Corpus, content_transformer(stemCompletion), dictionary = google_CorpusCopy)

#################################################################################################
################################## count frequency of “mining” ##################################


hashtag_tdm <- TermDocumentMatrix(hashtag_Corpus, control = list(wordLengths = c(3, Inf)))
hashtag_tdm

## Freqency words and Association
hashtag_idx <- which(dimnames(hashtag_tdm)$Terms == "battery")
hashtag_idx
inspect(hashtag_tdm[hashtag_idx + (0:10), 1:20])

#inspect frequent words
(freq.terms <- findFreqTerms(hashtag_tdm, lowfreq=100))

#(freq.terms1 <- findFreqTerms(reason_idx, lowfreq=100))

term.freq <- rowSums(as.matrix(hashtag_tdm))
term.freq <- subset(term.freq, term.freq >=5)
hashtag_df <- data.frame(term = names(term.freq), freq = term.freq)
#View(reason_df)

write.csv(hashtag_df, file = "C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Analytics\\2weeks\\freqent_terms.csv", row.names = FALSE)


###################################### Plot the words - BARCHART ########################################

#View(reason_df)

sorted_hashtag_df <- hashtag_df[order(hashtag_df$freq, decreasing= TRUE),]

sorted_hashtag_df <- sorted_hashtag_df %>%
  slice(1:20)

ggplot(sorted_hashtag_df, aes(x=term, y=freq)) + geom_bar(stat = "identity", aes(x=reorder(term, desc(-freq)))) + 
  xlab("Terms") + ylab("Count") +coord_flip() +ggtitle("Frequent words-Hashtag")

##################### WORD ASSOCIATIONS ###########################################

# which words are associated with 'Battery'?
findAssocs(hashtag_tdm, "battery", 0.05)

# which words are associated with 'UPS'?
findAssocs(hashtag_tdm, "unpacked", 0.05)

# which words are associated with 'Shipping'?
findAssocs(hashtag_tdm, "device", 0.1)


################################################################################################################
####################################### WORDCLOUD METHOD #######################################################

#install.packages("wordcloud")
library("wordcloud")

hashtag_wc <- as.matrix(hashtag_tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(hashtag_wc), decreasing = T)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 100,
          random.order = F, colors = brewer.pal(8, 'Set1'))

##############################################################################################
############################# Clustering ########################################

# remove sparse terms
hashtag_tdm2 <- removeSparseTerms(hashtag_tdm, sparse = 0.96)
hashtag_m2 <- as.matrix(hashtag_tdm2)

# cluster terms
distMatrix1 <- dist(scale(hashtag_m2))
fit1 <- hclust(distMatrix1, method = "ward.D")

plot(fit1)
rect.hclust(fit1, k = 8) # cut tree into 6 clusters


################################################################################################
######################################### Ngrams  ##############################################


text_Corpus <- VCorpus(VectorSource(samsung_hashtag.df$text))

#text_Corpus <- tm_map(text_Corpus, stripWhitespace) # Remove unneccesary white spaces
text_Corpus <- tm_map(text_Corpus, removePunctuation) # Remove punctuation

for (i in seq(text_Corpus)) {
  text_Corpus[[i]] <- gsub('[^a-zA-Z|[:blank:]]', "", text_Corpus[[i]])
}

text_Corpus <- tm_map(text_Corpus, removeNumbers) # Remove numbers
text_Corpus <- tm_map(text_Corpus, tolower) # Convert to lowercase
#text_Corpus <- tm_map(text_Corpus, stemDocument)
text_Corpus <- tm_map(text_Corpus, PlainTextDocument) # Plain text


# add extra stop words: 'available' and 'via'
myStopwords1 <- c(stopwords("english"), "null", "rt", "i", "I", "the", "my")
myStopwords2<- c(stopwords(source = "smart"), "ae", "rt")

# remove stopwords from corpus
text_Corpus <- tm_map(text_Corpus, removeWords, myStopwords1)
text_Corpus <- tm_map(text_Corpus, removeWords, myStopwords2)


for (i in 1:10) {
  cat(paste("[[", i, "]] ", sep = ""))
  #writeLines(myCorpus[[i]])
  writeLines(as.character(text_Corpus[[i]]))
}


n_grams_plot <- function(n, data) {
  options(mc.cores=1)
  
  # Builds n-gram tokenizer 
  tk <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
  # Create matrix
  ngrams_matrix <- TermDocumentMatrix(data, control=list(tokenize=tk))
  # make matrix for easy view
  ngrams_matrix <- as.matrix(rollup(ngrams_matrix, 2, na.rm=TRUE, FUN=sum))
  ngrams_matrix <- data.frame(word=rownames(ngrams_matrix), freq=ngrams_matrix[,1])
  # find 20 most frequent n-grams in the matrix
  ngrams_matrix <- ngrams_matrix[order(-ngrams_matrix$freq), ][2:10, ]
  ngrams_matrix$word <- factor(ngrams_matrix$word, as.character(ngrams_matrix$word))
  
  # plots
  ggplot(ngrams_matrix, aes(x=word, y=freq, label=freq)) + 
    geom_bar(stat="Identity",fill = "#009E73") + geom_text(check_overlap = T,vjust = "bottom",nudge_y = 0.2,angle = 30,size = 4) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("n-grams") + 
    ylab("Frequency")+coord_flip()
}

n_grams_plot(n=2, data=text_Corpus)

n_grams_plot(n=3, data=text_Corpus)

n_grams_plot(n=4, data=text_Corpus)

n_grams_plot(n=5, data=text_Corpus)

n_grams_plot(n=7, data=text_Corpus)

################################################################################
################# Text Analytics for most.negative tweets #####################

flat <- read_excel("flat_extract_mobile.xlsx")
View(flat)

flat.df <- data.frame(flat)

########################################################################################################
########################################################################################################

negativetweets <- read.csv("C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Analytics\\2weeks\\negativetweets.csv")

negativetweets.df <- data.frame(negativetweets)
View(negativetweets.df)

text_Corpus <- Corpus(VectorSource(negativetweets.df$x))

for (i in 1:10) {
  cat(paste("[[", i, "]] ", sep = ""))
  #writeLines(myCorpus[[i]])
  writeLines(as.character(text_Corpus[[i]]))
}


n_grams_plot <- function(n, data) {
  options(mc.cores=1)
  
  # Builds n-gram tokenizer 
  tk <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
  # Create matrix
  ngrams_matrix <- TermDocumentMatrix(data, control=list(tokenize=tk))
  # make matrix for easy view
  ngrams_matrix <- as.matrix(rollup(ngrams_matrix, 2, na.rm=TRUE, FUN=sum))
  ngrams_matrix <- data.frame(word=rownames(ngrams_matrix), freq=ngrams_matrix[,1])
  # find 20 most frequent n-grams in the matrix
  ngrams_matrix <- ngrams_matrix[order(-ngrams_matrix$freq), ][2:20, ]
  ngrams_matrix$word <- factor(ngrams_matrix$word, as.character(ngrams_matrix$word))
  
  # plots
  ggplot(ngrams_matrix, aes(x=word, y=freq, label=freq)) + 
    geom_bar(stat="Identity",fill = "#009E73") + geom_text(check_overlap = T,vjust = "bottom",nudge_y = 0.2,angle = 30,size = 4) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("n-grams") + 
    ylab("Frequency")+coord_flip()
}

n_grams_plot(n=2, data=text_Corpus)




#################################################################################################################################
#################################################################################################################################
##### Samsung Sentiment Analysis on SAMSUNG SUPPORT ACCOUNT - Part I - Postive and Negative Sentiment ###########################

samsung_tweets <- userTimeline("SamsungSupport", n=5000, since="2018-11-01")

n.tweet <- length(samsung_tweets)

samsung_tweets.df <- twListToDF(samsung_tweets)
head(samsung_tweets.df)
head(samsung_tweets.df$text)
View(samsung_tweets.df)

samsung_tweets.df2 <- gsub("http.*","",samsung_tweets.df$text)
samsung_tweets.df2 <- gsub("https.*","",samsung_tweets.df2)
samsung_tweets.df2 <- gsub("#.*","",samsung_tweets.df2)
#samsung_tweets.df2 <- gsub("@.*","",samsung_tweets.df2)
head(samsung_tweets.df2)

############### get sentiment score ##############

word.df <- as.vector(samsung_tweets.df2)

emotion.df <- get_nrc_sentiment(word.df)

emotion.df2 <- cbind(samsung_tweets.df2, emotion.df) 
View(emotion.df2)

write.csv(emotion.df2, file = "C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Analytics\\2weeks\\emotion_samsung_account.csv", row.names = FALSE)

############ get positive sentiment ################

sent.value <- get_sentiment(word.df)
most.positive <- word.df[sent.value == max(sent.value)]
most.positive

########### get negative sentiment ################
most.negative <- word.df[sent.value <= min(sent.value)] 
most.negative 

sent.value

############ segregate positive and negative tweets #############

positive.tweets <- word.df[sent.value > 0]
head(positive.tweets)

negative.tweets <- word.df[sent.value < 0]
head(negative.tweets)

neutral.tweets <- word.df[sent.value == 0]
head(neutral.tweets)

########### alternate way ##################

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
head(category_senti)


####################### Bind the sentiment to the tweet ####################

head(samsung_tweets)
category_senti2 <- cbind(samsung_tweets.df2,category_senti, sent.value) 
head(category_senti2)

############ count of sentiment ################
table(category_senti)


########################################################################################
#################Sentiment Analysis - Part II - All Sentiments ##################

#getting emotions using in-built function
mysentiment_samsung <- get_nrc_sentiment((samsung_tweets.df$text))

#calculationg total score for each sentiment
Sentimentscores_samsung <- data.frame(colSums(mysentiment_samsung[,]))
Sentimentscores_samsung

names(Sentimentscores_samsung)<-"Score"
Sentimentscores_samsung <-cbind("sentiment"=rownames(Sentimentscores_samsung),Sentimentscores_samsung)
rownames(Sentimentscores_samsung)<-NULL


View(Sentimentscores_samsung)
#plotting the sentiments with scores
ggplot(data=Sentimentscores_samsung,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people tweeting on Samsung Support account")







