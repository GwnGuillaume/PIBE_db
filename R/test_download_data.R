library(httr)
library(jsonlite)

# Spécifiez l'URL Elasticsearch
url_elasticsearch <- "http://localhost:9200/acoustic-a1-10min/_search?pretty"
# url_elasticsearch <- "http://localhost:9200/acoustic_*/_search"

# Requête Elasticsearch avec filtre sur le timestamp
requete_elasticsearch <- '{
  "query": {
    "bool": {
      "must": [
        {
          "range": {
            "@timestamp": {
              "gte": "2020-07-19T01:40:00",
              "lt": "2020-07-19T02:40:00"
            }
          }
        }
      ]
    }
  },
  "_source": ["@timestamp", "Leq_A", "Leq_Z"],
  "size": 10000
}
'

# Effectuez la requête POST avec la requête Elasticsearch et l'en-tête "Content-Type"
reponse <- httr::POST(url_elasticsearch, body = requete_elasticsearch, encode = "json", add_headers("Content-Type" = "application/json"))


# Obtenez le contenu de la réponse
contenu_reponse <- httr::content(reponse, "parsed")

# Convertissez la réponse JSON en dataframe
data_sample <- as.data.frame(do.call(rbind, lapply(contenu_reponse$hits$hits, `[[`, "_source")))

# Affichez le dataframe
print(data_sample)
