#' Gets all the challenges from PoH Graph
#'
#' @param api_url the api address
#' @param method the method to return the results
#' @return Depends on the methods used.
#' @examples
#' get.challenges_all("https://api.thegraph.com/subgraphs/name/kleros/proof-of-
#' humanity-mainnet", "json1")
#' @import jsonlite
#' @import ghql
#' @import tidyverse
#' @export
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


#' Gets gets all the submissions in the registry
#'
#' @return A dataframe with all submissions, including removed ones.
#' @examples
#' getRegistry()
#' @import jsonlite
#' @import ghql
#' @import tidyverse
#' @export
getRegistry <- function() {
  registry <- data.frame()
  myurl <- "https://api.poh.dev/profiles?order_by=creation_time&order_direction=asc&include_unregistered=true"

  r <- GET(url = myurl)
  message("Getting ", myurl)
  s <- content(r, as = "text", encoding = "UTF-8")
  t <- as.data.frame(fromJSON(s,flatten = TRUE))
  registry <-  bind_rows(registry, t)

  while(fromJSON(s,flatten = TRUE)$meta$has_more){
    r <- GET(url = myurl)
    message("Getting ", myurl)
    s <- content(r, as = "text", encoding = "UTF-8")
    t <- as.data.frame(fromJSON(s,flatten = TRUE))
    registry <-  bind_rows(registry, t)
    myurl <- fromJSON(s,flatten = TRUE)$meta$next_url
    Sys.sleep(0.01)
  }
  return(registry)
}

#' Gets daily UBI/USD values from UniSwap v2
#'
#' @return An object data.frame with date as POSIXct and priceUSD as numeric.
#' @param daynumber an integer or a vector of day numbers. Day 0 is March 11,
#' 2021
#' @examples
#' get.dailyUBIUSD(seq(0:10))
#' @import jsonlite
#' @import ghql
#' @import tidyverse
#' @export
get.dailyUBIUSD <- function(daynumbers) {
  url_api <- "https://api.thegraph.com/subgraphs/name/uniswap/uniswap-v2"

  con <- GraphqlClient$new(url = url_api)

  dailyUBIUSD <- data.frame()

  for(i in daynumbers){
    qry <- Query$new()
    theQuery <- paste0('
    {
    tokenDayDatas(first:1,skip:',i,',where: {token: "0xdd1ad9a21ce722c151a836373babe42c868ce9a4"} ){
    date
    priceUSD
      }
    }
    ')

    qry$query('tokenDayDatas',theQuery)

    (x <- con$exec(qry$queries$tokenDayDatas))
    pre_transform <- jsonlite::fromJSON(x)$data$tokenDayDatas
    dailyUBIUSD <- rbind(dailyUBIUSD, pre_transform %>%
                           mutate(
                             date = as.POSIXct(as.numeric(date), origin = "1970-01-01", tz = "UTC"),
                             priceUSD = as.numeric(priceUSD)
                           )
    )
    Sys.sleep(0.1)
  }
  return(dailyUBIUSD)
}

#' Gets all transactions from a contract address
#'
#' @return An object data.frame
#' @param contract_address a character value of the contract address
#' @examples
#' get.txns_all("0xdd1ad9a21ce722c151a836373babe42c868ce9a4")
#' @import jsonlite
#' @import ghql
#' @import tidyverse
#' @export
get.txns_all <- function(contract_address){

  txns <- data.frame()
  p <- data.frame()
  page <- 1
  currentblock <- 1

  while(page < 5){

    url <- paste0("https://api.etherscan.io/api?module=account&action=txlist",
                  "&address=", contract_address,
                  "&startblock=", currentblock,
                  "&endblock=99999999",
                  "&page=", 1,
                  "&offset=", 10000,
                  "&apikey=RJTGUE9BRRAWMIDYREKQDB9TGGJAPYHZMD")

    res <- jsonlite::fromJSON(url)

    message(" Results show message ", print(res$mes))

    p <- data.frame(blockNumber = as.integer(res$result$blockNumber),
                    timeStamp = as.POSIXct(as.numeric(res$result$timeStamp),
                                           origin = "1970-01-01"), hash = res$result$hash,
                    nonce = as.integer(res$result$nonce), blockHash = res$result$blockHash,
                    transactionIndex = as.integer(res$result$transactionIndex),
                    from = res$result$from, to = res$result$to, value = res$result$value,
                    gas = res$result$gas, gasPrice = res$result$gasPrice,
                    isError = res$result$isError == "1", input = res$result$input,
                    contractAddress = res$result$contractAddress, cumulativeGasUsed = res$result$cumulativeGasUsed,
                    gasUsed = res$result$gasUsed, confirmations = as.integer(res$result$confirmations),
                    stringsAsFactors = FALSE)


    if(nrow(p)==1){
      txns <- bind_rows(txns, p)
      print("Mission: Accomplished")
      break
    }

    # Get the last block number, redefine it as the next block
    currentblock <-  p[length(p$blockNumber),1]

    p <- filter(p, blockNumber != currentblock) # remove possibly truncated block


    #tail(p$blockNumber)

    txns <- bind_rows(txns, p)

    p <- data.frame() # reset

    page <- page +1

    Sys.sleep(7)
  }

  return(txns)

}


