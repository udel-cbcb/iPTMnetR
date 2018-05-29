library(httr)
library(jsonlite)

iptmnet_env <- new.env()

.onLoad <- function(libname, pkgname) {
    set_host_url("http://aws3.proteininformationresource.org")
}

set_host_url <- function(url){
  assign("host_url",url, envir = iptmnet_env)
}

get_host_url <- function(){
  url <- get("host_url",envir = iptmnet_env)
  return <- url
}

get_info <- function(id){
  # Get information for the given iptmnet_id
  #
  # Args:
  #    id: iPTMnet ID
  #
  # Returns:
  #    List containing the information for the iPTMnet ID
  url <- sprintf("%s/%s/info",get_host_url(),id)
  result <- GET(url)
  if(status_code(result) == 200){
    data = content(result, "parsed")
    return <- data
  }else{
    error_msg = .build_error_msg(result)
    stop(error_msg)
  }
}

search_iptmnet <- function(search_term,term_type,role,ptm_vector=c(),organism_vector=c()){
  # Searches iPTMNet with the given search parameters
  #
  # Args:
  #    search_term: Search Term
  #    term_type: The type of search tem. Example - TermType()$.UniProtID,
  #    role: The role to filter by. Example - Role()$Enzyme
  #    pmt_vector: A vector containing the PTM's to filter by. Example - c(PTMType()$Acetylation,PTMType()$Phosphorylation)
  #    orgnism_vector: A vector containing organisms taxon code
  # Returns:
  #    A dataframe with search results

  query_params <- list(
    search_term=search_term,
    term_type=term_type,
    ptm_type=ptm_vector,
    role=role,
    organism=organism_vector
  )
  url <- sprintf("%s/search",get_host_url())
  result <- GET(url,query=query_params,add_headers("Accept"="text/plain"))
  if(status_code(result) == 200){
    search_results <- .to_dataframe(content(result,"text"))
    return <- search_results
  }else{
    error_msg = .build_error_msg(result)
    stop(error_msg)
  }
}


get_substrates <- function(id){
  # Get substrates for the given iptmnet_id
  #
  # Args:
  #    id: iPTMnet ID
  #
  # Returns:
  #   Dataframe containing the substrates for given iPTMnet ID

  url <- sprintf("%s/%s/substrate",get_host_url(),id)
  result <- GET(url,add_headers("Accept"="text/plain"))
  if(status_code(result) == 200){
    proteoforms <- .to_dataframe(content(result,"text"))
    return <- proteoforms
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
  #   Dataframe

  url <- sprintf("%s/%s/proteoforms",get_host_url(),id)
  result <- GET(url,add_headers("Accept"="text/plain"))
  if(status_code(result) == 200){
    proteoforms <- .to_dataframe(content(result,"text"))
    return <- proteoforms
  }else{
    error_msg = .build_error_msg(result)
    stop(error_msg)
  }
}

get_ptm_dependent_ppi <- function(id){
  # Get Post translational modification(PTM) dependent Protein-Protein interactions for the given iptmnet_
  #
  # Args:
  #    id: iPTMnet ID
  #
  # Returns:
  #   Dataframe
  url <- sprintf("%s/%s/ptmppi",get_host_url(),id)
  set_config(config(ssl_verifypeer = 0L))
  result <- GET(url,add_headers("Accept"="text/plain"))
  if(status_code(result) == 200){
    ptmppi <- .to_dataframe(content(result,"text"))
    return <- ptmppi
  }else{
    error_msg = .build_error_msg(result)
    stop(error_msg)
  }
}

get_ppi_for_proteoforms <- function(id){
  # Get Protein-Protein interactions along with corresponding proteoforms for the given iPTMnet ID
  #
  # Args:
  #    id: ID of the proteoform
  #
  # Returns:
  #   Dataframe
  url <- sprintf("%s/%s/proteoformsppi",get_host_url(),id)
  set_config(config(ssl_verifypeer = 0L))
  result <- GET(url,add_headers("Accept"="text/plain"))
  if(status_code(result) == 200){
    ppi_proteoforms = .to_dataframe(content(result,"text"))
    return <- ppi_proteoforms
  }else{
    error_msg = .build_error_msg(result)
    stop(error_msg)
  }
}

get_ptm_enzymes_from_list <- function(items){
  url <- sprintf("%s/batch_ptm_enzymes",get_host_url())
  set_config(config(ssl_verifypeer = 0L))

  # convert the sites to json
  json_data <- toJSON(items, pretty=TRUE)

  # send the request
  result <- POST(url,body=items, encode="json",add_headers("Accept"="text/plain"))

  if(status_code(result) == 200){
    data = content(result)
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
  enzymes <- .get_data(sites,get_ptm_enzymes_from_list)
  return <- enzymes
}

get_ptm_ppi_from_list <- function(items){
  url <- sprintf("%s/batch_ptm_ppi",get_host_url())
  set_config(config(ssl_verifypeer = 0L))

  # convert the sites to json
  json_data <- toJSON(items, pretty=TRUE)

  # send the request
  result <- POST(url,body=items, encode="json",add_headers("Accept"="text/plain"))

  if(status_code(result) == 200){
    data = content(result)
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
  ptm_ppi <- .get_data(sites,get_ptm_ppi_from_list)
  return <- ptm_ppi
}

TermType <- function(){
  return <- list(
    ALL = "All",
    UniprotID = "UniprotID",
    ProteinOrGeneName = "Protein/Gene Name",
    PMID = "PMID"
  )
}

Role <- function() {
  return <- list(
    EnzymeOrSubstrate = "Enzyme or Substrate",
    Enzyme = "Enzyme",
    Substrate = "Substrate",
    EnzymeAndSubstrate = "Enzyme and Substrate"
  )
}

PTMTypes <- function() {
  return <- list(
    Acetylation="Acetylation",
    CGlycosylation="C-Glycosylation",
    Myristoylation="Myristoylation",
    Ubiquitination="Ubiquitination",
    NGlycosylation="N-Glycosylation",
    SGlycosylation="S-Glycosylation",
    Phosphorylation="Phosphorylation",
    SNitrosylation="S-Nitrosylation",
    OGlycosylation="O-Glycosylation",
    Methylation="Methylation",
    Sumoylation="Sumoylation"
  )
}

.build_error_msg <- function(result){
  # Build an error string from the httr result
  #
  # Args:
  #   result: Result obtained after making a httr request
  #
  # Returns:
  #   A string containing the error msg

  content <- content(result, "text")
  code <- status_code(result)
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

.get_data <- function(sites,get_data_func){
  if(length(sites) <= 1000){
    data <- get_data_func(sites)
    return <- data
  }else {
    # get the first 1000
    data <- NULL

    loops = length(sites) %/% 1000
    for(index in 0:(loops-1)){
      start_index <- (index*1000)+1
      end_index <- start_index + 999
      sub_sites <- sites[start_index:end_index]
      if(index == 0){
        data <- get_data_func(sub_sites)
      }else{
        data <- rbind(data,get_data_func(sub_sites))
      }
    }

    remainders = length(sites)%%1000
    if(remainders != 0){
      start_index <- (loops*1000)+1
      end_index <- length(sites)
      data <- rbind(data,get_data_func(sites[start_index:end_index]))
    }

    return <- data
  }
}







