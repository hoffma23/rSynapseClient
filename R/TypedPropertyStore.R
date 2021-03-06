# TODO: Add comment
# 
# Author: furia
###############################################################################

is.scalar<-function(x) {!is.list(x) && (!is.vector(x) || length(x)<2)}

setMethod(
  f = "TypedPropertyStore",
  signature = signature("missing", "missing", "missing"),
  definition = function(file){
    new("TypedPropertyStore")
  }
)

setMethod(
  f = "TypedPropertyStore",
  signature = signature("character", "missing", "missing"),
  definition = function(file){
    json <- fromJSON(file)
    TypedPropertyStore(json = json)
  }
)

setMethod(
  f = "TypedPropertyStore",
  signature = signature("missing", "list", "missing"),
  definition = function(data){
    types <- c("stringAnnotations", "doubleAnnotations", "longAnnotations", "dateAnnotations")
    types <- intersect(types, names(data))
    store <- new("TypedPropertyStore")
    lapply(types, function(t){
        for(n in names(data[[t]])){
          ## ensure that all data are stored internally as characters
          ## type conversion will occur on retrieval
          data[[t]][[n]] <- as.character(data[[t]][[n]])
          slot(store, t) <<- data[[t]]
        }
      }
    )
  }
)

setMethod(
  f = "TypedPropertyStore",
  signature = signature("missing", "missing", "character"),
  definition = function(json){
    TypedPropertyStore(list = fromJSON(json))
  }
)

setMethod(
  f = "propertyNames",
  signature = "TypedPropertyStore",
  definition = function(object){
    types <- c("stringAnnotations", "doubleAnnotations", "longAnnotations", "dateAnnotations")
    nms <- lapply(types, function(t){
        names(slot(object, t))
      }
    )
    unlist(nms)
  }
)

setMethod(
  f = "propertyValues",
  signature = "TypedPropertyStore",
  definition = function(object){
    vals <- lapply(propertyNames(object), function(n) getProperty(object,n))
    if(length(vals) == 1L && as.character(class(vals)) %in% c("data.frame","list"))
      return(vals[[1]])
    unlist(vals)
  }
)

setMethod(
    f = "propertyValues<-",
    signature = signature("TypedPropertyStore", "data.frame"),
    definition = function(object, value){
      stop("data frame is not a supported property type")
    }
)

setMethod(
    f = "propertyValues<-",
    signature = signature("TypedPropertyStore", "list"),
    definition = function(object, value){
      if(any(names(value) == ""))
        stop("All elements of list must be named")
      lapply(names(value), function(n){
            object <<- setProperty(object, n, value[[n]])
          }
      )
      object
    }
)

setMethod(
  f = "propertyType",
  signature = signature("TypedPropertyStore", "character"),
  definition = function(object, which){
    types <- c("stringAnnotations", "doubleAnnotations", "longAnnotations", "dateAnnotations", "blobAnnotations")
    if(!(which %in% propertyNames(object)))
      return(NULL)
    for(t in types){
      if(which %in% names(slot(object, t)))
        return(t)
    }
    NULL
  }
)

setMethod(
  f = "getProperty",
  signature = signature("TypedPropertyStore", "character"),
  definition = function(object, which){
    nms <- propertyNames(object)
    if(!any(which %in% nms))
      return(NULL)
    if(sum(nms == which) > 1L)
      warning("Multiple values found for the key provided. Returning the first occurrence")
    type <- propertyType(object, which)
    val <- slot(object, type)[[which]]
    
    ## coerce the return value to the correct type
    switch(type,
      doubleAnnotations = as.numeric(val),
      longAnnotations = as.integer(val),
      dateAnnotations =  as.POSIXct(as.POSIXlt(as.numeric(val), origin=ISOdatetime(1970,1,1,0,0,0, tz="UTC"), tz= Sys.timezone())),
      val
    )
  }
)

setMethod(
  f = "setUpdatePropValue",
  signature= signature("TypedPropertyStore", "character", "missing", "missing"),
  definition = function(object, which, type){
    ## unset the property value
    type <- propertyType(object, which)
    for(t in type){
      object <- setUpdatePropValue(object, which, type = t)
    }
    object
  }
)

setMethod(
  f = "setUpdatePropValue",
  signature= signature("TypedPropertyStore", "character", "missing", "character"),
  definition = function(object, which, type){
    ## unset the property value
    nms <- propertyNames(object)
    if(!any(which %in% nms))
      return(object)
    nms <- names(slot(object, type))
    slot(object, type) <- slot(object, type)[setdiff(nms, which)]
    object
  }
)

setMethod(
  f = "setUpdatePropValue",
  signature= signature("TypedPropertyStore", "character", "character", "character"),
  definition = function(object, which, value, type){
    nms <- propertyNames(object)
    
    ## clear the existig value if it exists
    if(any(which %in% nms))
      object <- setUpdatePropValue(object, which)
    
    ## assign the new value to the correct type slot
	# note: by convention the values in the key-value pairs are *vectors* not *scalars*
    slot(object, type)[[which]] <- value
    object
  }
)

setMethod(
  f = "setProperty",
  signature = signature("TypedPropertyStore", "character", "logical"),
  definition = function(object, which, value){
    setUpdatePropValue(object, which, as.character(value), type = "stringAnnotations")
  }
)

setMethod(
  f = "setProperty",
  signature = signature("TypedPropertyStore", "character", "character"),
  definition = function(object, which, value){
    setUpdatePropValue(object, which, value, type = "stringAnnotations")
  }
)

setMethod(
  f = "setProperty",
  signature = signature("TypedPropertyStore", "character", "integer"),
  definition = function(object, which, value){
    ## convert the value to a string
    value <- as.character(value)
    
    setUpdatePropValue(object, which, value, type = "longAnnotations")
  }
)

setMethod(
  f = "setProperty",
  signature = signature("TypedPropertyStore", "character", "numeric"),
  definition = function(object, which, value){
    ## convert the value to a string
    value <- as.character(value)
    
    setUpdatePropValue(object, which, value, type = "doubleAnnotations")
  }
)

setMethod(
  f = "setProperty",
  signature = signature("TypedPropertyStore", "character", "POSIXct"),
  definition = function(object, which, value){
    
    ## set the origin
    value <- as.POSIXlt(value, tz = "UTC")
    
    ## convert the value to a string
    value <- as.character(as.numeric(value))
    
    setUpdatePropValue(object, which, value, type = "dateAnnotations")
  }
)

setMethod(
  f = "setProperty",
  signature = signature("TypedPropertyStore", "character", "POSIXlt"),
  definition = function(object, which, value){
    
    ## set the origin
    value <- as.POSIXlt(value, tz = "UTC")
    
    ## convert the value to a string
    value <- as.character(as.numeric(value))
    
    setUpdatePropValue(object, which, value, type = "dateAnnotations")
  }
)

setMethod(
  f = "setProperty",
  signature = signature("TypedPropertyStore", "character", "Date"),
  definition = function(object, which, value){
    ## convert the value POSIXct
    setProperty(object, which, as.POSIXct(value))
  }
)

setMethod(
  f = "setProperty",
  signature = signature("TypedPropertyStore", "character", "NULL"),
  definition = function(object, which, value){
    deleteProperty(object, which)
  }
)

setMethod(
  f = "deleteProperty",
  signature = signature("TypedPropertyStore", "character"),
  definition = function(object, which){
    if(which %in% propertyNames(object)){
      type <- propertyType(object, which)
      for(i in 1:length(which)){
        t <- type[i]
        slot(object, t) <- slot(object, t)[setdiff(propertyNames(object), which[i])]
        if(length(slot(object, t)) == 0L)
          slot(object, t) <- emptyNamedList
      }
    }
    object
  }
)

as.list.TypedPropertyStore <- function(x, ...){
  ret <- list(
    stringAnnotations = emptyNamedList,
    doubleAnnotations = emptyNamedList,
    longAnnotations = emptyNamedList,
    dateAnnotations = emptyNamedList,
    blobAnnotations = emptyNamedList
  )
  for(t in names(ret)){
      ret[[t]] <- slot(x, t)
    }
  ret
}



