
## This is a function to create a network purely from a contacts list
## Join nodes with more attributes to get
## Edge list with weighted seconds

## Function to return  an igraph object from a contact list
#' Create network for igraph
#' This function takes a contacts list, and returns an igraph network object for  a  weighted network
#' @param contacts the contacts list should take the form of a  data frame titled
#' id1, id2 and seconds, where the ids are  the individuals
#' @param id1 first id column
#' @param id2 second id column
#' @param seconds length of contact (seconds)
#' @param nodes optional vector of node ids (can be used to ensure 0 contacts are included, giving networks of equal size)
#' @return returns an igraph object
#' @export
#'
#' @examples # add later
create_network_igraph <- function(contacts, id1 = id1, id2 = id2,
                                  seconds = seconds, nodes = NULL){
  per_route <- contacts %>%
    dplyr::group_by(id1, id2)  %>%
    dplyr::summarise(weight = sum(seconds)) %>%
    dplyr::ungroup()


    ## Find id1 not in id2 and rev
     unique(per_route$id1)[which(unique(per_route$id1) %in% unique(per_route$id2) == FALSE)]
    missing <- unique(per_route$id2)[which(unique(per_route$id2) %in% unique(per_route$id1) == FALSE)]

    if(is.null(nodes)){
      nodes <- c(unique(per_route$id1), missing)
      nodes <- data.frame(nodes)
      colnames(nodes) <- "Proximity.Sensor"
    } else{
      nodes <- data.frame(nodes)
      colnames(nodes) <- "Proximity.Sensor"
    }


    g1 <- igraph::graph_from_data_frame(d=per_route, vertices=nodes, directed=F)

    ## Set edge weight as sum of seconds in contact
    igraph::E(g1)$weight <- per_route$weight

    ##  Return the graph
    return(g1)
}

## Make an adjanceny matrix from the graph
#' Create_network_adjacency
#'
#' @param contacts a dataframe of contacts in  the form ake the form of a  dataframe titled
#' id1, id2 and seconds, where the ids are  the individuals
#' @param diag logical indicating if  the diagonal should  be included. If false, will be NA, indicating no self loops
#' @param id1 first id column
#' @param id2 second id column
#' @param seconds length of contact (seconds)
#' @param nodes optional vector of node ids (can be used to ensure 0 contacts are included, giving netorks of equal size)
#'
#' @return  an adjacnecy matrix
#' @export
#'
#' @examples #' # add later

create_network_adjacency  <- function(contacts, diag = FALSE,
                                      id1 = id1, id2 = id2,
                                      seconds = seconds, nodes = NULL){
  per_route <- contacts %>%
    dplyr:: group_by(id1, id2)  %>%
    dplyr::summarise(weight = sum(seconds)) %>%
    dplyr::ungroup()


  ## Find id1 not in id2 and rev
  unique(per_route$id1)[which(unique(per_route$id1) %in% unique(per_route$id2) == FALSE)]
  missing <- unique(per_route$id2)[which(unique(per_route$id2) %in% unique(per_route$id1) == FALSE)]


  if(is.null(nodes)){
    nodes <- c(unique(per_route$id1), missing)
    nodes <- data.frame(nodes)
    colnames(nodes) <- "Proximity.Sensor"
  } else{
    nodes <- data.frame(nodes)
    colnames(nodes) <- "Proximity.Sensor"
  }


  g1 <- igraph::graph_from_data_frame(d=per_route, vertices=nodes, directed=F)

  ## Set edge weight as sum of seconds in contact
  igraph::E(g1)$weight <- per_route$weight

  ## Return the graph
  net <- igraph::as_adjacency_matrix(g1, attr = "weight", names = TRUE)
  net <- as.matrix(net)

  if(diag == FALSE){
    diag(net) <- NA
    return(net)
  } else {
    return(net)
  }
}

#' Create association indexes
#'
#' @param contacts_matrix  dataframe of contacts in  the form ake the form of a  dataframe titled
#' id1, id2 and seconds, where the ids are  the individuals
#' @param sampling_period sampling period of the contact sensors. 20s for sheep work
#' @param rownames logical indicating if there are row/column names on the matrix
#' @param diag logical indicating if  the diagonal should  be included (i.e. are self loops possible). Default is false - self loops are not possible.
#' @return new.mat - a new matrix consisting of the association indexes between individuals.
#' @export
#'
#' @examples #'  #sort later
convert_AI<- function(contacts_matrix = contacts_matrix,
                      sampling_period = sampling_period,
                      rownames = TRUE,
                      diag = FALSE){
  new.mat <- matrix(nrow= nrow(contacts_matrix), ncol = ncol(contacts_matrix))
  contacts_matrix  <- contacts_matrix/sampling_period
  for(i in 1:nrow(contacts_matrix)){
    for(j in 1:ncol(contacts_matrix) ){
      new.mat[i,j] <-   contacts_matrix[i,j]/ (contacts_matrix[i,j] + (colSums(contacts_matrix, na.rm = TRUE)[j]-contacts_matrix[i,j])  + (rowSums(contacts_matrix, na.rm = TRUE)[i]-contacts_matrix[i,j]) )
    }
  }
  if(rownames == TRUE){
    colnames(new.mat) <- colnames(contacts_matrix)
    rownames(new.mat) <- rownames(contacts_matrix)
  }

  if(diag == FALSE){
    diag(new.mat) <- NA
    return(new.mat)
  } else {
   return(new.mat)
}
}

## Create a network that includes 0 contacts
create_network_all_nodes <- function (contacts, diag = FALSE, id1 = id1, id2 = id2, seconds = seconds, nodes) {
  per_route <- contacts %>% dplyr::group_by(id1, id2) %>% dplyr::summarise(weight = sum(seconds)) %>%
    dplyr::ungroup()
  nodes <- data.frame(nodes)
  colnames(nodes) <- "Proximity.Sensor"
  g1 <- igraph::graph_from_data_frame(d = per_route, vertices = nodes,
                                      directed = F)
  igraph::E(g1)$weight <- per_route$weight
  net <- igraph::as_adjacency_matrix(g1, attr = "weight", names = TRUE)
  net <- as.matrix(net)
  if (diag == FALSE) {
    diag(net) <- NA
    return(net)
  }
  else {
    return(net)
  }
}

