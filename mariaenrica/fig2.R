library(bibliometrix)
library(rscopus)
options(scopus_key = "METTERE_API_KEY")

### File da scaricare da Scopus: 
# 1) title, abs, key (+anni scelti da mez) # scopus1230.bib
# https://www.scopus.com/results/results.uri?sort=plf-f&src=s&st1=bibliometrics&sid=fb9f929ff57e599b594d01883f3731bb&sot=b&sdt=b&sl=28&s=TITLE-ABS-KEY%28%7BMachine+Learning%7D+AND+ethics%29&origin=searchbasic&editSaveSearch=&yearFrom=2019&yearTo=2022&sessionSearchId=fb9f929ff57e599b594d01883f3731bb&limit=10
# 2) tutti i paper (title, abs, key, 1900-oggi)  # scopustotale.bib
# https://www.scopus.com/results/results.uri?sort=plf-f&src=s&st1=bibliometrics&sid=fb9f929ff57e599b594d01883f3731bb&sot=b&sdt=cl&sl=28&s=TITLE-ABS-KEY%28%7BMachine+Learning%7D+AND+ethics%29&origin=resultslist&editSaveSearch=&sessionSearchId=fb9f929ff57e599b594d01883f3731bb&limit=10

# FIGURA 2: ANNUAL SCIENTIFIC PRODUCTION* 
# *(dopo aver fatto plot, tornare indietro sui grafici con le frecce)

file <- "scopustotale.bib"
M <- convert2df(file, dbsource = "scopus", format = "bibtex")

# versione 1 AG: dal 2001 al 2023
results <- biblioAnalysis(M)
options(width=100)
plot(x = results, k = 10, pause = FALSE)

# versione 2 MEZ: dal 2005 al 2022
M_mezASP <- M[M$PY >= 2005, ]
M_mezASP <- M_mezASP[M_mezASP$PY <= 2022, ]
results <- biblioAnalysis(M_mezASP)
options(width=100)
plot(x = results, k = 10, pause = FALSE)
