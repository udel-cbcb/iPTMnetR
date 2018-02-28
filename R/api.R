library(httr)
library(jsonlite)

get_info <- function(id){
  # Get information for the given iptmnet_id
  #
  # Args:
  #    id: iPTMnet ID
  #
  # Returns:
  #    List containing the information for the iPTMnet ID

  url <- sprintf("https://annotation.dbi.udel.edu/iptmnet/api/%s/info",id)
  httr::set_config(httr::config(ssl_verifypeer = 0L))
  result <- httr::GET(url)
  if(httr::status_code(result) == 200){
    data = httr::content(result, "parsed")
    return <- data
  }else{
    error_msg = .build_error_msg(result)
    stop(error_msg)
  }
}

get_proteoforms <- function(id){
  # Get proteoforms for the given iptmnet_id
  #
  # Args:
  #    id: iPTMnet ID
  #
  # Returns:
  #   Dataframe containing the proteoforms for given iPTMnet ID

  url <- sprintf("https://annotation.dbi.udel.edu/iptmnet/api/%s/proteoforms",id)
  httr::set_config(httr::config(ssl_verifypeer = 0L))
  result <- httr::GET(url,add_headers("Accept"="text/plain"))
  if(httr::status_code(result) == 200){
    proteoforms <- .to_dataframe(httr::content(result,"text"))
    return <- proteoforms
  }else{
    error_msg = .build_error_msg(result)
    stop(error_msg)
  }
}

get_ptm_dependent_ppi <- function(id){
  url <- sprintf("https://annotation.dbi.udel.edu/iptmnet/api/%s/ptmppi",id)
  httr::set_config(httr::config(ssl_verifypeer = 0L))
  result <- httr::GET(url,add_headers("Accept"="text/plain"))
  if(httr::status_code(result) == 200){
    ptmppi <- .to_dataframe(httr::content(result,"text"))
    return <- ptmppi
  }else{
    error_msg = .build_error_msg(result)
    stop(error_msg)
  }
}

get_ppi_for_proteoforms <- function(id){
  url <- sprintf("https://annotation.dbi.udel.edu/iptmnet/api/%s/proteoformppi",id)
  httr::set_config(httr::config(ssl_verifypeer = 0L))
  result <- httr::GET(url,add_headers("Accept"="text/plain"))
  if(httr::status_code(result) == 200){
    ppi_proteoforms = .to_dataframe(httr::content(result,"text"))
    return <- ppi_proteoforms
  }else{
    error_msg = .build_error_msg(result)
    stop(error_msg)
  }
}

get_ptm_enzymes_from_list <- function(items){
  url <- "https://annotation.dbi.udel.edu/iptmnet/api/batch_ptm_enzymes"
  httr::set_config(httr::config(ssl_verifypeer = 0L))

  # convert the sites to json
  json_data <- jsonlite::toJSON(items, pretty=TRUE)

  # send the request
  result <- httr::POST(url,body=items, encode="json",add_headers("Content-Type"="text/plain"))

  if(httr::status_code(result) == 200){
    data = httr::content(result)
    con <- textConnection(data)
    enzymes <- read.csv(con)
    return <- enzymes
  }else{
    error_msg = .build_error_msg(result)
    stop(error_msg)
  }
}

get_ptm_enzymes_from_file <- function(file_name){
  sites <- .sites_from_file(file_name)
  enzymes <- get_ptm_enzymes_from_list(sites)
  return <- enzymes
}

get_ptm_ppi_from_list <- function(items){
  url <- "https://annotation.dbi.udel.edu/iptmnet/api/batch_ptm_ppi"
  httr::set_config(httr::config(ssl_verifypeer = 0L))

  # convert the sites to json
  json_data <- jsonlite::toJSON(items, pretty=TRUE)

  # send the request
  result <- httr::POST(url,body=items, encode="json",add_headers("Content-Type"="text/plain"))

  if(httr::status_code(result) == 200){
    data = httr::content(result)
    con <- textConnection(data)
    ppi <- read.csv(con)
    return <- ppi
  }else{
    error_msg = .build_error_msg(result)
    stop(error_msg)
  }
}

get_ptm_ppi_from_file <- function(file_name){
  sites <- .sites_from_file(file_name)
  enzymes <- get_ptm_ppi_from_list(sites)
  return <- enzymes
}

.build_error_msg <- function(result){
  # Build an error string from the httr result
  #
  # Args:
  #   result: Result obtained after making a httr request
  #
  # Returns:
  #   A string containing the error msg

  content <- httr::content(result, "text")
  code <- httr::status_code(result)
  error_msg = sprintf("Request failed with code : %d and error : %s",code,content)
  return <- error_msg
}

.to_dataframe <- function(data) {
  con <- textConnection(data)
  dataframe <- read.csv(con)
  return <- dataframe
}

.sites_from_file <- function(file_name){
  data <- read.csv(file_name, sep="\t", header = F)
  sites <- list()
  rows = nrow(data)
  for(row in 1:rows){
    item = data[row,]
    ac = item$V1
    residue = item$V2
    position = toString(item$V3)
    site <- list(substrate_ac=ac,site_residue=residue,site_position=position)
    sites[[length(sites)+1]] <- site
  }
  return <- sites
}

