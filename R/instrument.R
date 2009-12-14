###############################################################################
# R (http://r-project.org/) Instrument Class Model
#
# Copyright (c) 2009-2010
# Peter Carl, Dirk Eddelbuettel, Jeffrey Ryan, Joshua Ulrich and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################

.onLoad <- function(lib, pkg) {
    .instrument <<- new.env()
}

## we should probably assign instruments into a special namespace and create get* functions.  Jeff?

is.instrument <- function( x ) {
  x <- get(x,pos=.instrument,inherits=TRUE)
  inherits( x, "instrument" )
}

instrument<-function(primary_id , currency , multiplier , identifiers = NULL, ...,type=NULL ){
  if(is.null(primary_id)) stop("you must specify a primary_id for the instrument")

  # not sure this is correct, maybe should store the primary_id for the currency instead.  Why doesn't R have pointers?
  if(!is.currency(currency)) stop("currency must be an object of type 'currency'")

  if(!hasArg(identifiers)) identifiers = list()

  ## note that multiplier could be a time series, probably add code here to check
  if(!is.numeric(multiplier) | length(multiplier) > 1) stop("multiplier must be a single number")

  if(is.null(type)) tclass="instrument" else tclass = c(type,"instrument")

  ## now structure and return
  assign(primary_id, structure( list(primary_id = primary_id,
                         type = type,
                         currency = currency,
                         multiplier = multiplier,
                         identifiers = identifiers
                        ),
                    class = tclass
                  ), # end structure
        pos=.instrument,inherits=TRUE
        )
}

stock <- function(primary_id , currency , multiplier, identifiers = NULL, ...){
  stock_temp = instrument(primary_id , currency , multiplier , identifiers = identifiers, ..., type="stock" )
  ## now structure and return
  assign(primary_id, structure( list(primary_id = primary_id,
                         currency = currency,
                         multiplier = multiplier,
                         identifiers = identifiers
                        ),
                    class=c("stock","instrument")
                  ), # end structure
         pos=.instrument,inherits=TRUE
  )
}

future <- function(primary_id , currency , multiplier , identifiers = NULL, ..., underlying_id){
  future_temp = instrument(primary_id , currency , multiplier , identifiers = identifiers, ... , type="future" )

  if(is.null(underlying_id)) warning("underlying_id should only be NULL for cash-settled futures")

  if(!exists(underlying_id, pos=.instrument,inherits=TRUE)) warning("underlying_id not found") # assumes that we know where to look
  ## now structure and return
  assign(primary_id, structure( list(primary_id = future_temp$primary_id,
                         currency = future_temp$currency,
                         multiplier = future_temp$multiplier,
                         identifiers = future_temp$identifiers,
                         underlying_id = future_temp$underlying_id
                        ),
                    class=c("future","instrument")
                  ), # end structure
         pos=.instrument,inherits=TRUE
  )
}

future_series <- function(primary_id , suffix_id, first_traded, expires, identifiers = NULL, ...){
  contract<-try(getInstrument(primary_id))
  if(!inherits(contract,"future")) stop("futures contract spec must be defined first")

  # TODO add check for Date equivalent in first_traded and expires

  ## with futures series we probably need to be more sophisticated,
  ## and find the existing series from prior periods (probably years)
  ## and then add the first_traded and expires to the time series
  temp_series<-try(getInstrument(paste(primary_id, suffix_id)))
  if(inherits(temp_series,"future_series")) {
    temp_series$first_traded<-c(temp_series$first_traded,first_traded)
    temp_series$expires<-c(temp_series$expires,expires)
  } else {
    temp_series = structure( list(primary_id = contract$primary_id,
                         suffix_id = suffix_id,
                         first_traded = first_traded,
                         expires = expires,
                         identifiers = identifiers
                        ),
                    class=c("future_series", "future", "instrument")
             ) # end structure
  }

  assign(paste(primary_id, suffix_id, sep=""), temp_series, pos=.instrument,inherits=TRUE)
}

option <- function(primary_id , currency , multiplier , identifiers = NULL, ..., underlying_id){
  option_temp = instrument(primary_id , currency , multiplier, identifiers = identifiers, ..., type="option")

  if(is.null(underlying_id)) warning("underlying_id should only be NULL for cash-settled options")

  if(!exists(underlying_id, pos=.instrument,inherits=TRUE)) warning("underlying_id not found") # assumes that we know where to look
  ## now structure and return
  assign(primary_id, structure( list(primary_id = option_temp$primary_id,
                         currency = option_temp$currency,
                         multiplier = option_temp$multiplier,
                         identifiers = option_temp$identifiers,
                         underlying_id = option_temp$underlying_id
                        ),
                    class=c("option","instrument")
                  ), # end structure
         pos=.instrument,inherits=TRUE
  )
}

option_series <- function(primary_id , suffix_id, first_traded, expires, identifiers = NULL, ...){
  contract<-try(getInstrument(primary_id))
  if(!inherits(contract,"option")) stop("options contract spec must be defined first")
  ## with options series we probably need to be more sophisticated,
  ## and find the existing series from prior periods (probably years)
  ## and then add the first_traded and expires to the time series
  temp_series<-try(getInstrument(paste(primary_id, suffix_id)))
  if(inherits(temp_series,"option_series")) {
    temp_series$first_traded<-c(temp_series$first_traded,first_traded)
    temp_series$expires<-c(temp_series$expires,expires)
  } else {
    temp_series = structure( list(primary_id = contract$primary_id,
                         suffix_id = suffix_id,
                         first_traded = first_traded,
                         expires = expires,
                         identifiers = identifiers
                        ),
                    class=c("option_series", "option", "instrument")
             ) # end structure
  }

  assign(paste(primary_id, suffix_id,sep=""), temp_series, pos=.instrument,inherits=TRUE)
}

currency <- function(primary_id , currency=NULL , multiplier=1 , identifiers = NULL, ...){
  ## now structure and return
  assign(primary_id, structure( list(primary_id = primary_id,
                         type = "currency",
                         currency = primary_id,
                         multiplier = 1,
                         identifiers = identifiers
                        ),
                    class=c("currency","instrument")
                  ), # end structure
         pos=.instrument,inherits=TRUE
  )
}

is.currency <- function( x ) {
  x <- get(x,pos=.instrument,inherits=TRUE)
  inherits( x, "currency" )
}

exchange_rate <- function (primary_id , currency , second_currency, identifiers = NULL, ...){
  exchange_rate_temp = instrument(primary_id , currency , multiplier=1 , identifiers = identifiers, ..., type="exchange_rate")

  if(!exists(currency, pos=.instrument,inherits=TRUE)) warning("currency not found") # assumes that we know where to look
  if(!exists(second_currency, pos=.instrument,inherits=TRUE)) warning("second_currency not found") # assumes that we know where to look

  ## now structure and return
  assign(primary_id, structure( list(primary_id = primary_id,
                         currency = currency,
                         second_currency = second_currency,
                         identifiers = identifiers
                        ),
                    class=c("exchange_rate","instrument")
                  ), # end structure
         pos=.instrument,inherits=TRUE
  )
}

getInstrument <- function(x){
  get(x,pos=.instrument,inherits=TRUE)
}
