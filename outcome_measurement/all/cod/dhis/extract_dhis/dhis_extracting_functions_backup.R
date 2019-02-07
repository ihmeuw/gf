# DHIS2 Extraction for DRC: Extraction functions
# Functions to source in order to download DHIS2 data and metadata
# Run or source this code file before running other code

#------------------------------
# Caitlin O'Brien-Carelli
# 12/10/2018
#------------------------------

#------------------------------
# install and load the dhisextractr package 

# cluster only: install dhisextractr 
# this package is not automatically installed on the cluster
#install.packages("dhisextractr", lib=dir)

# load dhisextractr
library(dhisextractr, lib.loc=dir)

#------------------------------
# check if the dhis_extractr package uploaded correctly
# if the help file works, the package loaded 
? extract_categories 
#------------------------------

#------------------------------
# FUNCTIONS 
# load the necessary functions for the extraction 

#------------------------------
# parse_page function

#' @param url The url of the page to parse in the DHIS api, as a character string. The
#' function is made to parse xml pages, so input url should be an xml adress or a
#' generic web adress without extension
#' @param userID your username in DHIS2, as a character string
#' @param password your password in DHIS2, as a character string

parse_page = function(url, userID, password) {
  
  # generate arguments for getURLContent and xmlParse
  url = as.character(url)
  userpwd = paste0(userID, ':', password)
  
  # extract_data
  response = getURLContent(url = url, userpwd = userpwd, httpauth = 1L, header=FALSE, ssl.verifypeer = FALSE)
  parsed_page = xmlParse(response)
  root = xmlRoot(parsed_page)
  return(root)
}

#------------------------------

#------------------------------
# extract_info function

#' Generic function to extract relevant nodes in a DHIS element
#' \code{extract_info} goes to a specific DHIS2 element url, and extracts attributes
#' name, id and href. It can extract elements that span multiple pages.
#'
#' @param url_page The default url of the elements to parse in the DHIS api, as a
#' character string. The function is made to parse xml pages, so the input url should be an xml
#' adress or a generic web adress without extension.
#' @param root root of this page, as extracted by \code{\link{parse_page}}
#' @param node_name the name of the node we wish to extract
#' @param out an empty dataframe in which to return the output

extract_info = function(url_page, root, node_name, userID, password, monitor = TRUE){
  
  # determine number of pages of meta data 
  # Turns NPages to 1 if there are no pages
  NPages = as.numeric(xmlValue(root[['pager' ]][['pageCount']]))
  NPages[is.na(NPages)] = 1
  out = data.frame(matrix(ncol = 2, nrow = 0))
  root2 = root[[2]]

  for (page in 1:NPages) {
    
    if(monitor == TRUE){
      print(paste('Parsing page' , page , 'out of' , NPages , sep = ' '))
    }
    print(url_page)
    
    if(NPages > 1) {
      url_read = paste(url_page , '?page=' , page , sep = '')
    } else {
      url_read = url_page
    } 
    print(url_read)
    
    root = parse_page(url_read, userID, password )
    root2 = root[[2]]
    root_children = xmlChildren(root2)
    
    url_page = as.character(url_page)
    ID = c()
    url_list = c()
    if (!is.null(root2[[node_name]]) & length(root2[[node_name]]) > 0) {
      ID = xmlSApply(root2, xmlGetAttr, 'id')
      names = xmlToDataFrame(root2)
      url_list = paste0(substr(url_page, 1, nchar(url_page)-4), '/', ID, '.xml')
    }

    loop_out = data.frame(ID , names, url_list)
    out = rbind(out, loop_out)
  }
  return(out)
}

#------------------------------

#------------------------------
#  extract_dhis_datasets function

#' Extracts the list of datasets in DHIS
#' @param url The url of the datasets list in the DHIS web api, as a character string.
#' @param userID your username in DHIS2, as a character string
#' @param password your password for DHIS2, as a character string
#' @return Returns a data frame with each dataset as a line and for each data set, its
#' unique ID, its name and its url.

# extracts a list of dhis data sets
extract_dhis_datasets = function(url, userID, password){
  root = parse_page(url, userID, password)
  out = data.frame(datasets_ID = character(),
                    datasets_name = character(),
                    datasets_url = character())
  print(url)
  extract_info(url, root, node_name = 'dataSet', userID, password)
}

#------------------------------

#------------------------------
#  extract_data_elements and extract_data_elements_ds functions

#' Extract the list of data elements in a DHIS data set
#' \code{extract_data_elements} extracts the data elements in a given dataset.
#'
#' @param url The url of the dataset page in the DHIS api, from which we want to
#' extract the data elements. 
#' @param userID your username in DHIS2 , as a character string
#' @param password your password in DHIS2, as a character string
#' @return Returns a data frame with each data element as a line and for each data
#' element, its unique ID, its name and its url.

# extracts data element names, urls, and ids
extract_data_elements = function(url, userID, password) {
  root = parse_page(url, userID, password)
  out =  data.frame(data_element_ID = character(),
                  data_element_name = character(),
                  data_element_url = character())
  extract_info(url, root, node_name = 'dataElement', userID, password)
}

# extracts data element ids and the associated data sets
extract_data_elements_ds = function(url, userID, password){
  print(url)
  root = parse_page(url, userID, password)
  root_children = xmlChildren(root)
  elements_list = root_children$dataSetElements
  
  data_set_elements = xmlChildren(elements_list)
  x = 1:length(data_set_elements)
  print(data_set_elements)
  print(x)
  
  data_element_id = unlist(lapply(x, get_element_id, data_set_elements)) 
  
  print("found data_element")
  out = data.frame(data_element_id)
  return(out)
}

#------------------------------

#------------------------------
#  extract_org_unit_ds and get_org_unit_id functions

#' Extract 
#' \code{extract_data_elements} extracts the data elements in a given dataset.
#'
#' @param url The url of the dataset page in the DHIS api, from which we want to
#' extract the data elements. 
#' @param userID your username in DHIS2 , as a character string
#' @param password your password in DHIS2, as a character string
#' @return Rl.


extract_org_unit_ds <- function(data_set_url, userID, password){
  root <- parse_page(data_set_url, userID, password)
  root_children <- xmlChildren(root)
  #print(root_children)
  org_unit_list <- xmlChildren(root_children$organisationUnits)
  #print(org_unit_list)
  org_unit_id <- unlist(lapply(1:length(org_unit_list), get_org_unit_id, org_unit_list))
  out <- data.frame(org_unit_id)

}

# Extracts the Organisational Unit ID # 
get_org_unit_id <- function(x, org_unit_list){
  org_unit_id <- xmlGetAttr(org_unit_list[x]$organisationUnit, 'id')
}

get_element_id <- function(x, data_set_elements){
  #xmlApply(xmlChildren(data_set_elements), xmlGetAttr, 'id')
  #print('successfully passed')
    data_elements <- xmlChildren(data_set_elements[x]$dataSetElement)
    data_element_id <- xmlGetAttr(data_elements$dataElement, 'id')
}



#------------------------------
#  extract_orgunits_list function

#'Extract the list of Organisational Units 
#'
#' \code{extract_orgunits_list} extracts the list of Organisational Units 
#'
#' @param url The url of the organisation units page in the DHIS api. The function is
#' made to parse xml pages, so input url should be an xml adress or a generic web
#' adress without extension.
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password in DHIS2 , as a character string
#' @return Returns a data frame with each organisation unit as a line and for each
#' organisation unit, its unique ID, its name and its url.

extract_orgunits_list = function(org_unit_page_url, userID, password){
  out = data.frame(org_unit_ID = character() ,
                    org_unit_name = character()  ,
                    org_unit_url = character())
  root = parse_page(org_unit_page_url, userID , password)
  extract_info(org_unit_page_url, root, node_name = 'organisationUnit', userID, password, TRUE)
}

#------------------------------
#  extract_org_unit function
#'Extract information about each Organizational Unit
#'
#' \code{extract_org_unit} extracts all the information about an Organizational Unit
#'
#' @param url The url of the organisational unit for which we want to extract
#' information. The function is made to parse xml pages, so input url should be an xml
#' adress or a generic web adress without extension.
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' @return Returns a list with three elements :
#' * __Metadata__ For each organization unit, include its geolocation and
#' reference to the parent unit
#'
#' * __Group __ Groups in which the Organizational Unit is included. This is where the
#' type of organization unit is stored

# Extract meta data for each Organizational Unit
extract_org_unit = function(url, userID, password) {
  root = parse_page(url = url, userID = userID, password = password)
  
  #Extract org unit metadata
  id = xmlAttrs(root)[['id']]
  coordinates = xmlValue(root[['coordinates']])
  opening_date = xmlValue(root[['openingDate']])
  name = xmlValue(root[['displayName']])
  
  # to get the associated health areas, health zones and dps
  # transform the xml into a nested list of lists and extract the ancestors list
  tmp = xmlToList(root)
  ancestors = tmp$ancestors
  
  # create a data frame of the meta data and return it
  org_unit_metadata = data.table(id, coordinates, opening_date,
                                 name, ancestors) 
  
  return(org_unit_metadata) }


#------------------------------
# extract_categories function 

#'Extract the categories for data elements
#' \code{extract_categories} extracts the list of categories that are used for different
#' data elements.
#'
#' @param categories_url The url of the categories page in the DHIS api. The function is
#' made to parse xml pages, so input url should be an xml adress or a generic web
#' adress without extension.
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' @return Returns a data frame with each category as a line and for each
#' category, its unique ID, its name and its url.

extract_categories <- function(categories_url, userID, password){
  root <- parse_page(categories_url, userID, password)
  extract_info(categories_url, root, 'categoryOptionCombo', userID , password)
}

#------------------------------
# make_dhis_urls function

#'Make relevant urls in DHIS web api
#'
#' \code{make_dhis_urls} takes the main adress of a DHIS implementation and returns
#' the relevant adresses in the web api that will be used for extracting data.
#' @param base_url The url of the DHIS implementation
#' 
make_dhis_urls = function(base_url) {
  data_sets_url = paste(base_url , '/api/dataSets.xml' , sep = '')
  data_elements_url = paste(base_url , '/api/dataElements.xml' , sep = '')
  org_units_url = paste(base_url , '/api/organisationUnits.xml' , sep = '')
  data_elements_categories = paste(base_url , '/api/categoryOptionCombos.xml' , sep = '')
  data.frame(data_sets_url, data_elements_url, data_elements_categories, org_units_url, stringsAsFactors = FALSE)
}

#------------------------------


