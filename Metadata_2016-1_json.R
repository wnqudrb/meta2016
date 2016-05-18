#' ---
#' title: Metadata 2016 Spring - Text mining with json data
#' author: SY YU
#' date: 2016-05-18
#' ---

library(rjson)
library(RCurl)
library(tm)
library(SnowballC)

###### with talk.creativ.json
j_file2 <-"./talk_creativ.json"
###
### '\n' is REQUIRED at the end of JSON FILE. OMG!!!!!
###
talk_creativ<-fromJSON(file=j_file2, method="C")
str(talk_creativ)
#print(talk_creativ)

talk_names<-names(talk_creativ$talks[[16]]$talk)
talk_names

names(unlist(talk_creativ$talks))

talk_creativ_list<-lapply(talk_creativ$talks, function(x){unlist(x)})
str(talk_creativ_list)
parse_talk_all<-data.frame()
df_parse_talk_all<-do.call("rbind", c(parse_talk_all, talk_creativ_list))
str(df_parse_talk_all)
length(df_parse_talk_all)
length(df_parse_talk_all[1,])
length(df_parse_talk_all[,1])

df_parse_talk_all<-data.frame(df_parse_talk_all)
str(df_parse_talk_all)

###chage datatype of talk description
df_parse_talk_all$talk.description<-as.character(df_parse_talk_all$talk.description)
str(df_parse_talk_all$talk.description)

###change names of variables
names(df_parse_talk_all)<-talk_names
str(df_parse_talk_all)


#####Term Clustering

###Converting object type
class(df_parse_talk_all$description)
ted_docs <- Corpus(VectorSource(df_parse_talk_all$description))
class(ted_docs)

###Pre-processing
ted_docs <- tm_map(ted_docs, tolower)
ted_docs <- tm_map(ted_docs, removeNumbers)
ted_docs <- tm_map(ted_docs, removePunctuation)
ted_docs <- tm_map(ted_docs, removeWords, stopwords("SMART"))

###Tokenizing
strsplit_space_tokenizer <- function(x)
  unlist(strsplit(as.character(x), "[[:space:]]+"))
token_docs<-(sapply(ted_docs, strsplit_space_tokenizer))
#token_docs<-(sapply(ted_docs$content, strsplit_space_tokenizer))
token_docs<-(sapply(token_docs, function(x){gsub(" ","", x)}))
#token_docs<-(sapply(token_docs$content, function(x){gsub(" ","", x)}))
token_freq<-table(unlist(token_docs))
summary(data.frame(token_freq)$Freq)

###Stemming
stem_docs <- sapply(token_docs, stemDocument)
stem_freq<-table(unlist(stem_docs))
summary(data.frame(stem_freq)$Freq)

df_stem_freq<-data.frame(stem_freq)
str(df_stem_freq)
desc_df_stem_freq<- df_stem_freq[order(-df_stem_freq$Freq),]

#####With Stemmed docs

###Term-Doc Matrix with Stemming
class(stem_docs)
stem_docs <- Corpus(VectorSource(stem_docs))
class(stem_docs)

###term weight: Tf
ted_tdm_tf <- TermDocumentMatrix(stem_docs,
                                 control = list(removePunctuation = TRUE,
                                                weighting=weightTf,
                                                stopwords = TRUE))

inspect(ted_tdm_tf[1,])

###Frequent terms:Tf
findFreqTerms(ted_tdm_tf, lowfreq = 3)
findFreqTerms(ted_tdm_tf, lowfreq = 4)

termFrequency_tdm_tf <- rowSums(as.matrix(ted_tdm_tf))
termFrequency_tdm_tf

###term weight: TfIDF
ted_tdm <- TermDocumentMatrix(stem_docs,
                                 control = list(removePunctuation = TRUE,
                                                weighting=weightTfIdf,
                                                stopwords = TRUE))

inspect(ted_tdm[1,])

###Frequent terms with TfIDF for comparing to Tf
findFreqTerms(ted_tdm, lowfreq = 0.3)
findFreqTerms(ted_tdm, lowfreq = 0.1)


#####Compare & Recommend with Stemming
### Frequent terms and 'creativ'
#head(termFrequency_tdm_tf)

creativ_assoc<-findAssocs(ted_tdm, "creativ", corlimit = 0.5)
creativ_assoc

design_assoc<-findAssocs(ted_tdm, "design", corlimit = 0.5)
design_assoc

class(creativ_assoc)
str(creativ_assoc)
creativ_assoc

write.table(design_assoc, "./design_assoc_matrix.txt")

df_design_assoc<-as.data.frame(design_assoc)
df_design_assoc

list_design_assoc<-as.list(df_design_assoc)
list_design_assoc

names(list_design_assoc$design)<-rownames(df_design_assoc)
list_design_assoc

###Write associate terms in file
###cat(sep=" ") is DEFAULT!! Need to set sep="".

fnlist <- function(x, fil){
  for (v in seq_along(x)){
    nam_i<-names(x[v])
    kwd=names(x[[v]])
    for (i in seq_along(x[[v]]) ){ cat(nam_i,"\t",kwd[i],"\t", x[[v]][i],"\n", sep="",
                                       file=fil, append=TRUE) }
  }
}

fnlist_file="./design_assoc_list.txt"

if (file.exists(fnlist_file)){
  unlink(fnlist_file, recursive = TRUE)
  fnlist(list_design_assoc, "./design_assoc_list.txt")
} else fnlist(list_design_assoc, "./design_assoc_list.txt")

#####Hierachical Clustering 
###removing sparse terms
ted_tdm_sparse <- removeSparseTerms(ted_tdm, sparse = 0.90)
ted_tdm_sparse$nrow
ted_tdm<-ted_tdm_sparse

#convert to matrix
ted_m <- as.matrix(ted_tdm)

###dist {stats} Distance Matrix Computation
###scale {base} Scaling and Centering of Matrix-like Objects
distMatrix<- dist(scale(ted_m))

###hclust {stats} Hierarchical Clustering
###method=c("single", complete", "average", "mcquitty", "median, "centroid", "ward.D", "ward.D2)
fit <- hclust(distMatrix, method="ward.D")
plot(fit)

###rect.hclust {stats} Draw Rectangles Around Hierarchical Clusters
rect.hclust(fit, k=10)

###Save Dendrogram as PNG image file
png("./ted_creativ_ward_dendrogram_sparse90.png", width = 1200, height=600)
plot(fit)
#k: number of clusters
rect.hclust(fit, k=10)
dev.off()

#cutree {stats} Cut a Tree into Groups of Data
groups <- cutree(fit, k=10)
df_groups <- data.frame(groups)
str(df_groups)
df_groups$KWD <- rownames(df_groups)
str(df_groups)
###Write the clustering result to text file
write.table(df_groups, "./ted_creativ_ward2_sparse90_res.txt", row.names=FALSE, col.names=TRUE, sep="\t")