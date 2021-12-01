get.challenges_all <- function(api_url,method){

  con <- GraphqlClient$new(url = api_url) # Sets up the connection with the subgraph

  qry <- Query$new() # First query

  qry$query('challenges',
            '{challenges(
    first: 1000
    skip: 0
    orderBy: creationTime
    orderDirection: desc
  ) {
    id

    request{
     evidence {
        id
        URI
      }
    }
  }
} ')

  (x <- con$exec(qry$queries$challenges))


  if(method == "string"){
    return(x)
  }

  if(method == "json1"){
    pre_transform <- jsonlite::fromJSON(x)
    return(pre_transform)
  }

  if(method == "urlid"){
    return(unnest(fromJSON(x,flatten = TRUE)$data$challenges) %>%
             mutate(URL = paste0("https://ipfs.kleros.io",URI)) %>%
             select(URL, id))
  }

}
