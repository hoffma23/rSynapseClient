## Get the Child IDs from a Synapse Entity
##
## Author: Colin Hoffman <colin.hoffman48@gmail.com>
##################################################################
##


setMethod(
  f = "getChildIds",
  signature = signature("character"),
  definition = function(entity){
    synapseQuery(paste('select id from entity where entity.parentId=="', entity, '"', sep=""))      
  }
)


setMethod(
  f = "getChildIds",
  signature = signature("numeric"),
  definition = function(entity){
    cat("changing numeric to integer, then converting to synapse ID\n")
    getChildIds(paste('syn', as.character(as.integer(entity)), sep=""))
  }
)


setMethod(
  f = "getChildIds",
  signature = signature("integer"),
  definition = function(entity){
    cat("converting integer to synapse ID\n")
    getChildIds(paste('syn', as.character(entity), sep=""))
  }
)


setMethod(
  f = "getChildIds",
  signature = signature("SynapseEntity"),
  definition = function(entity){
    id <- propertyValue(entity, "id")
    if(is.null(id))
      stop("entity id cannot be null")
    
    getChildIds(as.character(id))
  }
)


setMethod(
  f = "getChildIds",
  signature = signature("list"),
  definition = function(entity){
    if(!("id" %in% names(entity)))
      stop("entity must have a field named id")
    if(is.null(entity$id))
      stop("id cannot be null")
    
    getChildIds(as.character(entity$id))
  }
)



