# Getting data from different sources



## medRxiv and bioRxiv
Here, we opted for a very simple solution. We grab all DOIs of the last 75 record, and requiry these to get the complete citation data:

```R
prep.doi.biorxiv<-function(population){
  pops<-html_text(population)
  doi<-pops[seq(3,length(pops),by=4)]
  doi<-paste0("10.1101/",doi)
  doi <- str_remove(doi, "; ")
}
```

```R
biorxivmed <- function(url){
  dois <- url %>%
    xml2::read_html() %>%
    html_nodes('.highwire-cite-metadata')
  doi <- prep.doi.biorxiv(dois)
  list.all.doi <- c()
  for (i in 1:length(doi)){
    cat("nr:",i,"doi:",doi[i],"status:")
    doi.get <- get_doi(doi[i])
    cat("retrieved \n")
    list.all.doi <- append(list.all.doi, doi.get)
  }
  return(list.all.doi)
}
```

```R
get_doi<-function(doi) { #https://rdrr.io/github/wkmor1/doi2bib/src/R/doi2bib.r
  content(
    GET(
      url    = paste0("http://dx.doi.org/", doi),
      config = accept("application/rdf+xml;q=0.5, application/vnd.citationstyles.csl+json;q=1.0")
    ),
    as = "text",
    encoding = "UTF-8"
  )
}
```

We want to return a dataframe that is uniform for all different sources:

```R
doijson <- function(jsondoi){ # returns dataframe from json 
  authors <- vector()
  author1 <- vector()
  for(n in 1:nrow(jsondoi$author)){
    tmp1 <- paste0(jsondoi$author$family[n],", ", gsub("[^A-Z]*([A-Z])[^A-Z]*", "\\1", jsondoi$author$given[n]))
    authors <- append(authors,tmp1)
    authors <- paste(authors, collapse="; ")
    author1 <- jsondoi$author$family[1]
  }
  
  # we need to add additional fields
  
  # some checks for empty fields:
  abstract=ifelse(is.null(jsondoi$abstract),NA,jsondoi$abstract) # check for empty
  journal=ifelse(is.list(jsondoi$`container-title`) & length(jsondoi$`container-title`) == 0,NA,jsondoi$`container-title`) # check for empty
  
  doi_data <- data.frame('authors'=authors, 'title'= jsondoi$title, 'author1'=author1, 'abstract'= str_remove(abstract, "<jats:p>"),
                         'doi'=jsondoi$DOI, 'url1'= jsondoi$URL, 'journal'= journal, "year" = jsondoi$posted$`date-parts`[1], 
                         "month" = jsondoi$posted$`date-parts`[2], "date_entrez" = jsondoi$posted$`date-parts`[3],
                         "institution" = jsondoi$institution$name, "publisher" = jsondoi$publisher)  # add other fields.
  return(doi_data)
}
```
