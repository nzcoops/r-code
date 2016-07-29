library(rscopus)
library(wordcloud)

store <- author_search(au_id = "34970888100", api_key = "XX")
#store3 <- affiliation_retrieval(id = "60031806", api_key = "XX", identifier = c("affiliation_id", "eid"))

# [[1]]$affiliation[[2]]$affilname
# [1] "Telethon Institute for Child Health Research"
# 
# [[1]]$`prism:publicationName`
# [1] "Diabetic Medicine"
# 
# [[1]]$`prism:coverDate`
# [1] "2013-09-01"
# 
# [[1]]$`dc:title`
# [1] "Hypoglycaemia, fear of hypoglycaemia and quality of life in children with Type 1 diabetes and their parents"
# 
# [[1]]$`citedby-count`
# [1] "16"
# 
# [[1]]$`dc:description`
# [1] "Aim: To evaluate the association between fear of hypoglycaemia, episodes of hypoglycaemia and quality of life in children with Type 1 diabetes and their parents. Methods: This was a cross-sectional, population-based stu"
# 
# [[1]]$`author-count`$`$`
# [1] "8"
# 
# [[1]]$affiliation[[1]]$affilname
# [1] "Princess Margaret Hospital for Children"
# 
# 
# [[1]]$author[[1]]$authid
# [1] "55466683600"

# Extract unique author ids
unique(sapply(store$entries[[1]]$author, function(x) x$authid)) # once per paper (unique for counting) - one paper only
sapply(store$entries, function(x) unique(sapply(x$author, function(x) x$authid))) # for all papers
sapply(sapply(store$entries, function(x) unique(sapply(x$author, function(x) x$authid))), length) # count

# Extract all titles
sapply(store$entries, function(x) x$`dc:title`)

# Extract citations
sapply(store$entries, function(x) x$`citedby-count`)

# Extract journal names
sort(table(sapply(store$entries, function(x) x$`prism:publicationName`)))
unique(sapply(store$entries, function(x) x$`prism:publicationName`))

# Extract affiliations
sapply(store$entries, function(x) unique(sapply(x$affiliation, function(x) x$affilname))) # full list
lapply(store$entries, function(x) lapply(x$affiliation, function(x) x$affilname)) # as a list by paper
sapply(store$entries, function(x) length(unique(sapply(x$affiliation, function(x) x$affilname)))) # count per paper

# Publication dates
sapply(store$entries, function(x) x$`prism:coverDate`)

# "Telethon Institute for Child Health Research" - 60000096
###### Affiliation problem #######
# TKI getting wrapped up into UWA
# Entry 6 shows both SPACH and TKI being given id 60031806
store$entries[[6]]$affiliation

60000096
60031806

docs <- sapply(store$entries, function(x) x$`dc:title`)
vs <- VectorSource(docs)
myCorp <- VCorpus(vs)
#myCorp <- tm_map(myCorp, removePunctuation)
myCorp <- tm_map(myCorp, PlainTextDocument)
myCorp <- tm_map(myCorp, removeWords, stopwords('english'))

wordcloud(myCorp, max.words = 100, random.order = FALSE)
wordcloud(myCorp, scale=c(4,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
