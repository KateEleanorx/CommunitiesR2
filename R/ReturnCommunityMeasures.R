#' Title
#'
#' @param network an igraph network object
#'
#' @return an igraph communities object
#' @export
#'
#' @examples ## place holder
return.community <- function(network){
  set.seed(123)
  communities <- igraph::cluster_leading_eigen(graph = network,
                                       options = list(maxiter= 1000000))
  return(communities)
}

## Factors that  contribute to modularity
#' Title Network measures related to the communities
#'
#' @param x.net a network object, in matrix format
#' @param community.net an igraph communities object
#'
#' @return A dataframe of the measures that are related to the community formation.
#' Sub-group cohesion -  proportion of contacts that occur within sub-groups
#' mean_sub_group_cohesion = mean sub-group cohesion
#' var_sub_group_cohesion = variation in sub-group cohesion
#' In  and out of community strength for all individuals
#' mean_w_degree = mean weighted degree (assuming a weighted network)
#' var_w_degree = variance weighted degree (assuming a weighted network)
#' @export
#'
#' @examples example
return.community.measures <- function(x.net, community.net){


  ## 1. Network size - number of individuals
  network_size <- nrow(x.net)
  ## 2. Network fragmentation - log number of sub-groups
  network_fragmentation <- log(max(community.net$membership))


  ## 3. Sub-group cohesion -  proportion of contacts that occur within sub-groups
  return.subgroup.cohesion<- function(x.net, community.memberships, network_size){
    indivs <- 1:network_size
    socgroups <- community.memberships
    IGS <- rep(NA, network_size)
    OGS <- rep(NA, network_size)

    ## Calc in and out of community strength for all individuals
    for(i in 1:length(indivs)){
      IGS[i] <- sum(x.net[which(community.memberships%in%community.memberships[i]==TRUE),i], na.rm = TRUE)
      OGS[i] <- sum(x.net[which(community.memberships%in%community.memberships[i]==FALSE),i],  na.rm = TRUE)
    }

    #create a dataframe of all individual-level metrics
    output<-data.frame(IGS, OGS)
    output$Total <- output$IGS + output$OGS
    output$Cohesion <- output$IGS/output$Total
    return(output)
  }

  e1 <- return.subgroup.cohesion(x.net = x.net,
                                 community.memberships = community.net$membership,
                                 network_size = network_size)

  mean_sub_group_cohesion <- mean(e1$Cohesion)

  ## 4. Variation in sub-group cohesion
  var_sub_group_cohesion <- stats::var(e1$Cohesion)

  ## 5. Mean w  degree
  mean_w_degree <- mean(e1$Total)

  ## 6. Variation in w degree
  var_w_degree <- stats::var(e1$Total)

  df <-  data.frame(network_size,
                    network_fragmentation,
                    mean_sub_group_cohesion,
                    var_sub_group_cohesion,
                    mean_w_degree,
                    var_w_degree)

  return(df)

}

## Return Pi and Zi - what's the paper

#' Return in/out of community strength and Pi/Zi ()
#'
#' @param x.net a network object, in matrix format
#' @param memberships a vector of community memberships (e.g from igraph communties $memberhip)
#'
#' @return Pi -
#' Zi - a normalised measure of an individuals interactions within its subgroup
#' @export
#'
#' @examples
calc_Zi_Pi <- function(x.net, memberships){
  indivs <- colnames(x.net)
  socgroups <- memberships
  IGS <- rep(NA, length(indivs))
  OGS <- rep(NA, length(indivs))
  Zi <- rep(NA, length(indivs))

  ## Calc in and out of community strength for all individuals
  for(i in 1:length(indivs)){
    IGS[i] <- sum(x.net[which(memberships%in%memberships[i]==TRUE),i], na.rm = TRUE)
    OGS[i] <- sum(x.net[which(memberships%in%memberships[i]==FALSE),i],  na.rm = TRUE)
  }


  #calculate Pi from formula 2 in the main text
  Pi<-1-(OGS/(IGS+OGS))^2

  #calculate Zi using formula 1 from the main text
  for(i in 1:length(indivs)){
    Zi[i]<-(IGS[i]-mean(IGS[which(memberships%in%memberships[i]==TRUE)]))/sd(IGS[which(memberships%in%memberships[i]==TRUE)])
  }

  #create a dataframe of all individual-level metrics
  output<-data.frame(Zi,Pi, IGS, OGS)
  return(output)
}



