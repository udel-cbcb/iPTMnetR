library(httr)
library(jsonlite)

iptmnet_env <- new.env()

.onLoad <- function(libname, pkgname) {
    set_host_url("https://research.bioinformatics.udel.edu/iptmnet/api")
    set_api_version("v1")
    httr::set_config(httr::config(ssl_verifypeer = FALSE))
}

#' Set the url of iPTMnet API server.
#'
#' This function can be used to change the URL to use your own self hosted instance of iPTMnet API server.
#'
#' @param url A string representing the new iTPMnet api server URL.
#'
#' @return NULL
#' @export
#'
#' @examples
#' set_host_url("http://www.example.com")
set_host_url <- function(url){
  assign("host_url",url, envir = iptmnet_env)
}

#' Get the url of iPTMnet API server.
#'
#' Get the URl that is being used by the client for making the requests.
#'
#' @return A string representing the URL.
#' @export
#'
#' @examples
#' url <- get_host_url()
get_host_url <- function(){
  url <- get("host_url",envir = iptmnet_env)
  return <- url
}


#' Set the version of the API to use.
#'
#' This function can be used to change the version of the api to use.
#'
#' @param version A string representing the new iTPMnet api version.
#'
#' @return NULL
#' @export
#'
#' @examples
#' set_api_version("v1")
set_api_version <- function(version){
  assign("api_version",version, envir = iptmnet_env)
}

#' Get the version of iPTMnet API being used.
#'
#' Get the version of the api that is being used by the client for making the requests.
#'
#' @return A string representing the version being used.
#' @export
#'
#' @examples
#' version <- get_api_version()
get_api_version <- function(){
  version <- get("api_version",envir = iptmnet_env)
  return <- version
}

#' Get information.
#'
#' Get the information for given iPTMnet ID.
#'
#' @param id A string representing iPTMnet ID
#'
#' @return A list containing the information for the iPTMnet ID.
#' @export
#'
#' @examples
#' info <- get_info("Q15796")
get_info <- function(id){
  url <- sprintf("%s/%s/%s/info",get_host_url(),get_api_version(),id)
  result <- httr::GET(url)
  if(httr::status_code(result) == 200){
    data = httr::content(result, "parsed")
    return <- data
  }else{
    error_msg = .build_error_msg(result)
    stop(error_msg)
  }
}



#' Search iPTMNet
#'
#' Searches the iPTMNet database with the given search parameters
#'
#' @param search_term A string representing the search Term
#' @param term_type A string specifying type of search term. Supported values are "All", "UniprotID", "Protein/Gene Name","PMID".
#'  Use the \link{TermType} function for getting a list of possible values.
#'
#' @param role A string representing the roles to filter by.
#'  Supported Values are "Enzyme or Substrate", "Enzyme", "Substrate","Enzyme and Substrate".
#'  Use the \link{Role} function for getting a list of possible values.
#'  Example - \code{TermType()$.UniProtID}
#' @param ptm_vector A vector representing the PTM types to filter by.
#'  Use the \link{PTMTypes} function for getting a list of possible values. Pass empty vector if you don't want filter by PTMTypes.
#'  Example - c(PTMType()$Acetylation,PTMType()$Phosphorylation)
#' @param organism_vector A vector of integers representing the taxon codes for organisms to filter by.
#'
#' @return A dataframe with search results.
#' @export
#'
#' @examples
#' result <- search_iptmnet(search_term = "smad2",
#'                                         term_type=TermType()$ALL,
#'                                         Role()$EnzymeOrSubstrate,
#'                                         ptm_vector=c(),
#'                                         organism_vector=c())
search_iptmnet <- function(search_term,term_type,role,ptm_vector=c(),organism_vector=c()){
  query_params <- list(
    search_term=search_term,
    term_type=term_type,
    ptm_type=ptm_vector,
    role=role,
    organism=organism_vector
  )
  url <- sprintf("%s/%s/search",get_host_url(),get_api_version())
  result <- httr::GET(url,query=query_params,httr::add_headers("Accept"="text/plain"))
  if(httr::status_code(result) == 200){
    search_results <- .to_dataframe(httr::content(result,"text"))
    return <- search_results
  }else{
    error_msg = .build_error_msg(result)
    stop(error_msg)
  }
}


#' Get substrates.
#'
#' Retrieves the substrates for the given iPTMnet ID.
#'
#' @param id A string representing iPTMnet ID.
#'
#' @return A dataframe containing the substrates for the given iPTMnet ID.
#' @export
#'
#' @examples
#' substrates <- get_substrates("Q15796")
get_substrates <- function(id){
  url <- sprintf("%s/%s/%s/substrate",get_host_url(),get_api_version(),id)
  result <- httr::GET(url,httr::add_headers("Accept"="text/plain"))
  if(httr::status_code(result) == 200){
    proteoforms <- .to_dataframe(httr::content(result,"text"))
    return <- proteoforms
  }else{
    error_msg = .build_error_msg(result)
    stop(error_msg)
  }
}


#' Get proteoforms
#'
#' @param id A string representing iPTMnet ID.
#'
#' @return A dataframe containing the proteoforms for the given iPTMnet ID.
#' @export
#'
#' @examples
#' proteoforms <- get_proteoforms("Q15796")
get_proteoforms <- function(id){
  url <- sprintf("%s/%s/%s/proteoforms",get_host_url(),get_api_version(),id)
  result <- httr::GET(url,httr::add_headers("Accept"="text/plain"))
  if(httr::status_code(result) == 200){
    proteoforms <- .to_dataframe(httr::content(result,"text"))
    return <- proteoforms
  }else{
    error_msg = .build_error_msg(result)
    stop(error_msg)
  }
}


#' Get Post translational modification(PTM) dependent Protein-Protein interactions.
#'
#' Get Post translational modification(PTM) dependent Protein-Protein interactions for the given iPTMnet ID.
#'
#' @param id A string representing iPTMnet ID.
#'
#' @return A dataframe containing the PTm dependent PPI for the given iPTMnet ID.
#' @export
#'
#' @examples
#' ptm_dependent_ppi <- get_ptm_dependent_ppi("Q15796")
get_ptm_dependent_ppi <- function(id){
  url <- sprintf("%s/%s/%s/ptmppi",get_host_url(),get_api_version(),id)
  httr::set_config(httr::config(ssl_verifypeer = 0L))
  result <- httr::GET(url,httr::add_headers("Accept"="text/plain"))
  if(httr::status_code(result) == 200){
    ptmppi <- .to_dataframe(httr::content(result,"text"))
    return <- ptmppi
  }else{
    error_msg = .build_error_msg(result)
    stop(error_msg)
  }
}


#' Get Protein-Protein interactions along with corresponding proteoforms.
#'
#' Get Protein-Protein interactions along with corresponding proteoforms for the given iPTMnet ID.
#'
#' @param id A string representing iPTMnet ID.
#'
#' @return A dataframe containing the PTm dependent PPI for the given iPTMnet ID.
#' @export
#'
#' @examples
#' ppi_proteoforms <- get_ppi_for_proteoforms("Q15796")
get_ppi_for_proteoforms <- function(id){
  url <- sprintf("%s/%s/%s/proteoformsppi",get_host_url(),get_api_version(),id)
  httr::set_config(httr::config(ssl_verifypeer = 0L))
  result <- httr::GET(url,httr::add_headers("Accept"="text/plain"))
  if(httr::status_code(result) == 200){
    ppi_proteoforms = .to_dataframe(httr::content(result,"text"))
    return <- ppi_proteoforms
  }else{
    error_msg = .build_error_msg(result)
    stop(error_msg)
  }
}

#' Get variants.
#'
#' Get variants for the given iPTMnet ID.
#'
#' @param id A string representing iPTMnet ID.
#'
#' @return A dataframe containing the variants for the given iPTMnet ID.
#' @export
#'
#' @examples
#' variants <- get_variants("Q15796")
get_variants <- function(id){
  url <- sprintf("%s/%s/%s/variants",get_host_url(),get_api_version(),id)
  httr::set_config(httr::config(ssl_verifypeer = 0L))
  result <- httr::GET(url,httr::add_headers("Accept"="text/plain"))
  if(httr::status_code(result) == 200){
    variants = .to_dataframe(httr::content(result,"text"))
    return <- variants
  }else{
    error_msg = .build_error_msg(result)
    stop(error_msg)
  }
}



#' Get PTM Enzymes from list
#'
#' Retrieve PTM enxymes from the given list of kinases.
#' The kinase object should have three fields - "substrate_ac","site_residue","site_position". All with the type String
#'
#' @param items A list of kinases.
#'
#' @return A Dataframe representing the enzymes
#' @export
#'
#' @examples
#' \dontrun{
#'kinases = list(
#'   list(
#'       substrate_ac="Q15796",
#'       site_residue="K",
#'       site_position="19"
#'   ),
#'   list(
#'       substrate_ac="Q15796",
#'       site_residue="T",
#'       site_position="8"
#'  ),
#'  list(
#'       substrate_ac="P04637",
#'       site_residue="K",
#'       site_position="120"
#'  )
#')
#'enzymes = get_ptm_enzymes_from_list(kinases)
#'}
get_ptm_enzymes_from_list <- function(items){
  url <- sprintf("%s/%s/batch_ptm_enzymes",get_host_url(),get_api_version())
  httr::set_config(httr::config(ssl_verifypeer = 0L))

  # convert the sites to json
  json_data <- jsonlite::toJSON(items, pretty=TRUE)

  # send the request
  result <- httr::POST(url,body=items, encode="json",httr::add_headers("Accept"="text/plain"))

  if(httr::status_code(result) == 200){
    data = httr::content(result)
    con <- textConnection(data)
    enzymes <- utils::read.csv(con)
    return <- enzymes
  }else{
    error_msg = .build_error_msg(result)
    stop(error_msg)
  }
}



#' Get PTM Enzymes using a file
#'
#' This function is similar to \link{get_ptm_enzymes_from_list} except that it loads the kinases from TSV file.
#'
#' @param file_name A string representing file name of the file containing the list of kinases.
#' The file should be a Tab seperated file with three columns "substrate_ac", "site_residue" and "position" without headers.
#' Example :
#' Q13619	S	10
#' Q8TDM6	S	1021
#' Q6ZRV2	S	1025
#' Q15121	S	104
#' O15164	S	1042
#' Q8NDI1	S	1058
#' P00533	S	1064
#' Q16555	S	107
#' Q8NFC6	S	1077
#'
#' @return A Dataframe representing the enzymes
#' @export
#'
#' @examples
#' \dontrun{enzymes = get_ptm_enzymes_from_file("kinases.txt")}
get_ptm_enzymes_from_file <- function(file_name){
  sites <- .sites_from_file(file_name)
  enzymes <- .get_data(sites,get_ptm_enzymes_from_list)
  return <- enzymes
}



#' Get post translational modification (PTM) depentdent protein-protein interaction for the given list of kinases.
#'
#' Retrieve a list of post translational modification (PTM) depentdent protein-protein interaction for the given list of kinases.
#' The kinase object should have three fields - "substrate_ac","site_residue","site_position". All with the type String
#'
#' @param items A list of kinases.
#'
#' @return A dataframe containing the PTM dependent PPI interaction information.
#' @export
#'
#' @examples
#'\dontrun{
#'kinases = list(
#'   list(
#'       substrate_ac="Q15796",
#'       site_residue="K",
#'       site_position="19"
#'   ),
#'   list(
#'       substrate_ac="Q15796",
#'       site_residue="T",
#'       site_position="8"
#'  ),
#'  list(
#'       substrate_ac="P04637",
#'       site_residue="K",
#'       site_position="120"
#'  )
#')
#'ptm_dep_ppi = get_ptm_ppi_from_list(kinases)
#'}
get_ptm_ppi_from_list <- function(items){
  url <- sprintf("%s/%s/batch_ptm_ppi",get_host_url(),get_api_version())
  httr::set_config(httr::config(ssl_verifypeer = 0L))

  # convert the sites to json
  json_data <- jsonlite::toJSON(items, pretty=TRUE)

  # send the request
  result <- httr::POST(url,body=items, encode="json",httr::add_headers("Accept"="text/plain"))

  if(httr::status_code(result) == 200){
    data = httr::content(result)
    con <- textConnection(data)
    ppi <- utils::read.csv(con)
    return <- ppi
  }else{
    error_msg = .build_error_msg(result)
    stop(error_msg)
  }
}


#' Get post translational modification (PTM) depentdent protein-protein interaction for kinases from file
#'
#' This function is similar to \link{get_ptm_ppi_from_list} except that it loads the kinases from TSV file
#'
#' @param file_name A string representing file name of the file containing the list of kinases.
#' The file should be a Tab seperated file with three columns "substrate_ac", "site_residue" and "position" without headers.
#' Example :
#' Q13619	S	10
#' Q8TDM6	S	1021
#' Q6ZRV2	S	1025
#' Q15121	S	104
#' O15164	S	1042
#' Q8NDI1	S	1058
#' P00533	S	1064
#' Q16555	S	107
#' Q8NFC6	S	1077
#'
#' @return A dataframe containing the PTM dependent PPI interaction information.
#' @export
#'
#' @examples
#' \dontrun{ptm_dep_ppi = get_ptm_ppi_from_file(kinases.txt)}
get_ptm_ppi_from_file <- function(file_name){
  sites <- .sites_from_file(file_name)
  ptm_ppi <- .get_data(sites,get_ptm_ppi_from_list)
  return <- ptm_ppi
}

#' TermTypes
#' Returns a list of strings that represent the term types to be used in \link{search_iptmnet} function.
#' @return A list of strings that represent the term types.
#' @export
#'
#' @examples
#' TermType()$ALL
#' TermType()$UniprotID
TermType <- function(){
  return <- list(
    ALL = "All",
    UniprotID = "UniprotID",
    ProteinOrGeneName = "Protein/Gene Name",
    PMID = "PMID"
  )
}


#' Role
#' Returns a list of strings that represent the role to be used in \link{search_iptmnet} function
#' @return A list of strings representing the role
#' @export
#'
#' @examples
#' Role()$EnzymeOrSubstrate
#' Role()$Enzyme
#' Role()$Substrate
#' Role()$EnzymeAndSubstrate
Role <- function() {
  return <- list(
    EnzymeOrSubstrate = "Enzyme or Substrate",
    Enzyme = "Enzyme",
    Substrate = "Substrate",
    EnzymeAndSubstrate = "Enzyme and Substrate"
  )
}


#' PTMTypes
#' Returns a list of strings that represents the PTM types to be used in \link{search_iptmnet} function
#' @return A list of strings representing PTM types
#' @export
#'
#' @examples
#' PTMTypes()$Acetylation
#' PTMTypes()$CGlycosylation
#' PTMTypes()$Myristoylation
#' PTMTypes()$Ubiquitination
#' PTMTypes()$NGlycosylation
#' PTMTypes()$SGlycosylation
#' PTMTypes()$Phosphorylation
#' PTMTypes()$SNitrosylation
#' PTMTypes()$OGlycosylation
#' PTMTypes()$Methylation
#' PTMTypes()$Sumoylation
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

  content <- httr::content(result, "text")
  code <- httr::status_code(result)
  error_msg = sprintf("Request failed with code : %d and error : %s",code,content)
  return <- error_msg
}

.to_dataframe <- function(data) {
  con <- textConnection(data)
  dataframe <- utils::read.csv(con)
  return <- dataframe
}

.sites_from_file <- function(file_name){
  data <- utils::read.csv(file_name, sep="\t", header = F)
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







