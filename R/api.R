library(httr)
library(jsonlite)

get_info <- function(id){
  url <- sprintf("https://annotation.dbi.udel.edu/iptmnet/api/%s/info",id)
  httr::set_config(config(ssl_verifypeer = 0L))
  result <- GET(url)
  if(status_code(result) == 200){
    data = content(result, "parsed")
    return <- data
  }else{
    throw("Request failed with code : %d and Error : %s",status_code(result),str((content(result))))
  }
}

get_proteoforms <- function(id){
  url <- sprintf("https://annotation.dbi.udel.edu/iptmnet/api/%s/proteoforms",id)
  httr::set_config(config(ssl_verifypeer = 0L))
  result <- GET(url)
  if(status_code(result) == 200){
    data = content(result, "parsed")
    return <- data
  }else{
    throw("Request failed with code : %d and Error : %s",status_code(result),str((content(result))))
  }
}

get_ptm_dependent_ppi <- function(id){
  url <- sprintf("https://annotation.dbi.udel.edu/iptmnet/api/%s/ptmppi",id)
  httr::set_config(config(ssl_verifypeer = 0L))
  result <- GET(url)
  if(status_code(result) == 200){
    data = content(result, "parsed")
    return <- data
  }else{
    throw("Request failed with code : %d and Error : %s",status_code(result),str((content(result))))
  }
}

get_ppi_for_proteoforms <- function(id){
  url <- sprintf("https://annotation.dbi.udel.edu/iptmnet/api/%s/proteoformppi",id)
  httr::set_config(config(ssl_verifypeer = 0L))
  result <- GET(url)
  if(status_code(result) == 200){
    data = content(result, "parsed")
    return <- data
  }else{
    throw("Request failed with code : %d and Error : %s",status_code(result),str((content(result))))
  }
}

get_ptm_enzymes_from_list <- function(items){
  url <- "https://annotation.dbi.udel.edu/iptmnet/api/batch_ptm_enzymes"
  httr::set_config(config(ssl_verifypeer = 0L))
  json_data <- toJSON(items, pretty=TRUE)
  result <- POST(url,body=items, encode="json")
  print(status_code(result))
  if(status_code(result) == 200){
    data = content(result, "parsed")
    return <- data
  }else{
    print(str((content(result))))
    throw("Request failed with code : %d and Error : %s",status_code(result),str((content(result))))
  }
}

get_ptm_enzymes_from_file <- function(file_name){
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

  enzymes <- get_ptm_enzymes_from_list(sites)
  return <- sites
}

get_ptm_ppi_from_list <- function(items){
  url <- "https://annotation.dbi.udel.edu/iptmnet/api/batch_ptm_ppi"
  httr::set_config(config(ssl_verifypeer = 0L))
  json_data <- toJSON(items, pretty=TRUE)
  result <- POST(url,body=items, encode="json")
  print(status_code(result))
  if(status_code(result) == 200){
    data = content(result, "parsed")
    return <- data
  }else{
    print(str((content(result))))
    throw("Request failed with code : %d and Error : %s",status_code(result),str((content(result))))
  }
}

get_ptm_ppi_from_file <- function(file_name){
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

  enzymes <- get_ptm_ppi_from_list(sites)
  return <- sites
}

