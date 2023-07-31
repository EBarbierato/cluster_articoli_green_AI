library(bibliometrix)
library(rscopus)


##### 1) File .bib da scaricare da Scopus: 
# title, abstract, key = {Machine Learning} AND ethics
# year = between 2017 and 2022 (compresi)
file <- "ML_ethics_1722.bib" # disponibile anche 19-22
M <- convert2df(file, dbsource = "scopus", format = "bibtex")
M <- M[M$PY <= 2022, ]
m <- c(names(M))

# Analisi bibliometrica
results <- biblioAnalysis(M)
r <- c(names(results))

# Sources
head(results$Sources, n=3) # top 3
results[["Sources"]][["AI AND SOCIETY"]]
results[["Sources"]][["MINDS AND MACHINES"]]

# Affiliations
head(results$Affiliations, n=10) # top 10
results[["Affiliations"]][["HARVARD MEDICAL SCHOOL"]]
results[["Affiliations"]][["UNIVERSITY OF TORONTO"]]

# Keywords
head(results$DE, n=10) # top 10
results[["DE"]][["FAIRNESS"]]
results[["DE"]][["SUSTAINABILITY"]]
results[["DE"]][["DIVERSITY"]]
results[["DE"]][["EXPLAINABLE AI"]]

# FIGURA 1
sources <- head(results$Sources, n=10)
td <- data.frame(Source = names(sources), Count = sources, stringsAsFactors = FALSE)
td <- head(td, 10)
td$Count.SO <- NULL  # rimuovo colonna ripetuta
colnames(td) <- c("Sources", "Articles")
library(kableExtra)
table <- kable(td, "html") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
table # poi esportare dal viewer.


##### 2) File .bib da scaricare da Scopus: 
# title, abstract, key = {Machine Learning} AND fairness
# year = between 2005 and 2022 (compresi)
file <- "ML_fairness_0522.bib"
M <- convert2df(file, dbsource = "scopus", format = "bibtex")
M <- M[M$PY <= 2022, ]
m <- c(names(M))

# Analisi
results <- biblioAnalysis(M)

# ATTENZIONE: la parte della figura 2 contiene sia dati sul range 2005-2022 che sul range 2019-2022
# sistemare perchè se no è poco chiaro.

# grafico figura 2
# versione uguale a quello dato (dal 2006 al 2022):
plot(x = results)
# versione con meno anni (così il grafico mostra anche la scritta "2022")
M_red <- M[M$PY <= 2022, ]
M_red <- M_red[M_red$PY >= 2006, ]
results_red <- biblioAnalysis(M_red)
plot(x = results_red)
# scorrere a sx nei vari plots, poi esportare

library(kableExtra)
# BOZZA in più, ma può essere carino
years <- table(results$Years)
table <- kable(years, "html") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  row_spec(1:nrow(years), angle = 90) %>%
  row_spec(0, angle = 90)
table

# continua analisi dal 2017 in poi

M <- M[M$PY >= 2017, ]
results <- biblioAnalysis(M)

# most productive countries
head(results$Countries, 5)

# observations (sources)
library(stringr)
library(dplyr)
aies <- M %>% filter(str_detect(SO, "AIES"))  # starting from aies, 3 conferences
nrow(aies) 
aies$J9 # lista aies generica
aies$JI # lista aies generica
aies$SO # lista aies con anno (!)
nrow(aies %>% filter(str_detect(SO, "AIES 2018")))
nrow(aies %>% filter(str_detect(SO, "AIES 2019")))
nrow(aies %>% filter(str_detect(SO, "AIES 2020")))
nrow(aies %>% filter(str_detect(SO, "AIES 2021")))
nrow(aies %>% filter(str_detect(SO, "AIES 2022")))

results[["Sources"]][["AI AND SOCIETY"]]

# affiliations
head(results$Affiliations, 11)
results[["Affiliations"]][["MICROSOFT RESEARCH"]]
results[["Affiliations"]][["GOOGLE RESEARCH"]]


###### 3) File .bib da scaricare da Scopus: 
# title, abstract, key = {artificial intelligence} AND fairness
# year = between 2017 and 2022 (compresi)
file <- "AI_fairness_1722.bib"
M <- convert2df(file, dbsource = "scopus", format = "bibtex")
M <- M[M$PY <= 2022, ]
results <- biblioAnalysis(M)

# keywords
head(results$DE, 10)
results[["DE"]][["NATURAL LANGUAGE PROCESSING"]]
results[["DE"]][["EXPLAINABLE AI"]]


###### 4) File .bib da scaricare da Scopus: 
# title, abstract, key = {explainable artificial intelligence} (controllare da paper)
# year = between 2017 and 2022 (compresi)
file <- "XAI_1722.bib"
M <- convert2df(file, dbsource = "scopus", format = "bibtex")
M <- M[M$PY <= 2022, ]
results <- biblioAnalysis(M)

years <- table(results$Years)

# BOZZA in più, può servire fatto bene?
table <- kable(years, "html") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  row_spec(1:nrow(years), angle = 90) %>%
  row_spec(0, angle = 90)
table

# countries
head(results$Countries, 5)

# sources
head(results$Sources, 10)
results[["Sources"]][["MINDS AND MACHINES"]]

# affiliations
head(results$Affiliations, 15)

# keywords
head(results$DE, 10)
results[["DE"]][["NATURAL LANGUAGE PROCESSING"]]

# BOZZA in più
table <- kable(results$DE, "html") %>%
  kable_styling(bootstrap_options = "striped", full_width = F) %>%
  add_header_above(header = c("Most Relevant Keywords" = 2)) %>%
  scroll_box(width = "100%", height = "400px") 
table

# https://rdrr.io/cran/bibliometrix/src/R/keywordGrowth.R
# https://rdrr.io/cran/bibliometrix/man/KeywordGrowth.html
# BOZZA in più
KeywordGrowth <- function(M, Tag = "ID", sep = ";", top=10, cdf=TRUE, 
                          remove.terms=NULL, synonyms=NULL){
  i<-which(names(M)==Tag)
  PY=as.numeric(M$PY)
  Tab<-(strsplit(as.character(M[,i]),sep))
  Y=rep(PY,lengths(Tab))
  A=data.frame(Tab=unlist(Tab),Y=Y)
  A$Tab=trim.leading(A$Tab)
  A=A[A$Tab!="",]
  A=A[!is.na(A$Y),]
  ### remove terms
  terms <- data.frame(Tab=toupper(remove.terms))
  A <- anti_join(A,terms)
  # end of block
  ### Merge synonyms in the vector synonyms
  if (length(synonyms)>0 & is.character(synonyms)){
    s <- strsplit(toupper(synonyms),";")
    snew <- trimws(unlist(lapply(s,function(l) l[1])))
    sold <- (lapply(s,function(l) trimws(l[-1])))
    for (i in 1:length(s)){
      A <- A %>% 
        mutate(
          # Tab = str_replace_all(Tab, paste(sold[[i]], collapse="|",sep=""),snew[i])
          #Tab= str_replace_all(Tab, str_replace_all(str_replace_all(paste(sold[[i]], collapse="|",sep=""),"\\(","\\\\("),"\\)","\\\\)"),snew[i]),
          Tab= stringi::stri_replace_all_regex(Tab, stringi::stri_replace_all_regex(stringi::stri_replace_all_regex(paste(sold[[i]], collapse="|",sep=""),"\\(","\\\\("),"\\)","\\\\)"),snew[i])
        )
    }
  }
  # end of block
  Ymin=min(A$Y)
  Ymax=max(A$Y)
  Year=Ymin:Ymax
  Tab<-names(sort(table(A$Tab),decreasing=TRUE))[1:top]
  words=matrix(0,length(Year),top+1)
  words=data.frame(words)
  names(words)=c("Year",Tab)
  words[,1]=Year
  for (j in 1:length(Tab)){
    word=(table(A[A$Tab %in% Tab[j],2]))
    words[,j+1]=trim.years(word,Year,cdf)
  }
  return(words)
}
trim.years<-function(w,Year,cdf){
  Y=as.numeric(names(w))
  W=matrix(0,length(Year),1)
  for (i in 1:length(Year)){
    if (Y[1]==Year[i] & length(Y)>0){W[i,1]=w[1]
    Y=Y[-1]
    w=w[-1]}
  }
  if (isTRUE(cdf)) W=cumsum(W)
  names(W)=Year
  W=data.frame(W)
  return(W)}
k = KeywordGrowth(M)

# install.packages("reshape2")
library(reshape2)
library(ggplot2)
DF=melt(k, id='Year')
ggplot(DF,aes(Year,value, group=variable, color=variable))+geom_line()


# FIGURA 3
# Un aggiornamento (?) di bibliometrix fa si che il grafico della figura 3 
# NON sia ottenibile (ci sono errori...)
# Scrivere biblioshiny(): si apre una finestra sul browser (ci sono errori ma funziona).
# importare il file .bib (dicendo che è preso da scopus)
# fare filtering se necessario
# documents > words > most frequent words 
# -> crea il grafico, ma tramite un file .txt da importare si possono mettere 
# le parola da omettere (es xai, explainable, e sinonimi visti ad occhio)
# esportare.


# FIGURA 4
# 5) File .bib da scaricare da Scopus: 
# title, abstract, key = {green ai} AND sustainability
# year = between 2020 and 2022 (compresi)
file <- "greenai_sustainability_2022.bib"
M <- convert2df(file, dbsource = "scopus", format = "bibtex")
results <- biblioAnalysis(M)
# sources
head(results$Sources, 5)
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
net=networkPlot(NetMatrix,
                Title = "Keyword Co-occurrences",
                normalize = NULL,
                n = 20,
                degree = NULL,
                type = "auto",
                label = TRUE,
                labelsize = 1,
                label.cex = FALSE,
                label.color = FALSE,
                label.n = Inf,
                halo = FALSE,
                cluster = "walktrap",
                community.repulsion = 0,
                vos.path = NULL,
                size = TRUE,
                size.cex = FALSE,
                curved = FALSE,
                noloops = TRUE,
                remove.multiple = TRUE,
                remove.isolates = FALSE,
                weighted = TRUE,
                edgesize = 1,
                edges.min = 0,
                alpha = 0.5,
                verbose = TRUE)
# biblioshiny() conceptual structure -> co-occurrence network -> authors keywords 

# 6) File .bib da scaricare da Scopus: 
# title, abstract, key = {artificial intelligence} AND ethics
# year = between 2020 and 2022 (compresi)
file <- "ai_ethics_2022.bib"
M <- convert2df(file, dbsource = "scopus", format = "bibtex")
results <- biblioAnalysis(M)

# sources
head(results$Sources, 5)


# figura 5
# stesso problema della figura 3 (errore su "colore linea nero"...)
# biblioshiny -> network approach -> thematic map
# 400 words
# CS <- conceptualStructure(M,field="ID", method="CA", minDegree=4, clust=5, stemming=FALSE, labelsize=10, documents=10)
# Son state fatte varie versioni sia con codice che con biblioshiny()... online le più significative
thematicMap(
  M,
  field = "DE",
  n = 250,
  minfreq = 5,
  ngrams = 1,
  stemming = FALSE,
  size = 0.75,
  n.labels = 1,
  community.repulsion = 0.1,
  repel = TRUE,
  remove.terms = NULL,
  synonyms = NULL,
  cluster = "optimal",
  subgraphs = TRUE
)
