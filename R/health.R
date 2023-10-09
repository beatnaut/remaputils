##' This function checks whether ohlccodes are dubious.
##'
##' This is a description
##'
##' @param resources A data-frame with the resources from rdecaf.
##' @param jaccardCoeff The threshold for the jaccard string distance coefficient.
##' @return The resource data-frame with dubious ohlccodes.
##' @export
dubiousOHLCcodes <- function(resources, jaccardCoeff) {

    ## Get the resources which have OHLC codes:
    resourceWithOHLCcode <- resources[!isNAorEmpty(resources[, "ohlccode"]), ]

    ## If none has ohlccodes, return NULL:
    if (NROW(resourceWithOHLCcode) == 0) {
        return(NULL)
    }

    ## Which ones fail to grep the ohlccode in symbol:
    grepMethod <- apply(resourceWithOHLCcode, MARGIN=1, function(row) safeGrep(row["symbol"], row["ohlccode"]) == "0")

    ## Which ones have a very dissimilar ohlccode compared to symbol:
    jaccardMethod <- as.numeric(apply(resourceWithOHLCcode, MARGIN=1, function(x) stringdist(as.character(x["symbol"]), as.character(x["ohlccode"]), method="jaccard"))) > jaccardCoeff

    ## Get the dubious ohlc codes:
    dubiousOHLCcodes <- resourceWithOHLCcode[grepMethod | jaccardMethod,]

    ## If none dubious, return NULL:
    if (NROW(dubiousOHLCcodes) == 0) {
        return(NULL)
    }

    ## Done, return:
    dubiousOHLCcodes

}


##' This function checks whether trade dates are dubious.
##'
##' This is a description
##'
##' @param trades A data-frame with the trades from rdecaf.
##' @param backdatedness The number of days after which a trade should have been created.
##' @return The trades data-frame with dubious trade dates.
##' @export
dubiousTradeDates <- function(trades, backdatedness) {

    ## Trades with creation dates over N days of commitment date:
    aPriorCreation <- as.Date(trades[, "created"]) - as.Date(trades[, "commitment"]) > backdatedness

    ## Dates with commitment date later than creation date:
    exAnteCreation <- as.Date(trades[, "created"]) < as.Date(trades[, "commitment"])

    ## Filter and return:
    trades <- trades[aPriorCreation | exAnteCreation, ]

}


##' This is a wrapper function for ohlc health function.
##'
##' This is a description
##'
##' @param asof The asof date. Default is Sys.Date().
##' @param ohlccodes The ohlccodes to be check. If NULL, it gets it from the stocks. Default is NULL.
##' @param underlying Shall the underlying be checked as well? Default is FALSE
##' @param lookBack The lookback periodin days. Default is 365.
##' @param bbg logical for whether to run the bbg nomenclature check above.
##' @param session The rdecaf session.
##' @return A data frame with unhealthy ohlc observations.
##' @export
ohlcHealthWrapper <- function(asof=Sys.Date(), ohlccodes=NULL, underlying=FALSE, lookBack=365, bbg=FALSE, session) {

    runLink <- FALSE

    ## If no ohlc codes are provided, get from stocks:
    if (is.null(ohlccodes)) {

        ## Get the stocks:
        stocks <- as.data.frame(getResource("stocks", params=list("format"="csv", "page_size"=-1, "asof"=asof), session=session))

        ## Get the resources:
        resources <- getResourcesByStock(stocks, session, underlying)

        NROW(resources) > 0 || return(data.frame())
        runLink <- TRUE

        resources <- resources %>%
          dplyr::mutate(ohlccodeX=dplyr::if_else(is.na(ohlccode),symbol,ohlccode)) %>%
          dplyr::group_by(ohlccodeX) %>%
          dplyr::filter(row_number()==1) %>%
          dplyr::ungroup()

        ## Get ohlccodes:
        ##ohlccodes <- unique(ifelse(is.na(resources[, "ohlccode"]), resources[, "symbol"], resources[, "ohlccode"]))
        ohlccodes <- unique(resources$ohlccodeX)

    }

    ## Get the ohlc observations:
    ohlcObs <- data.frame() %>%
      bind_rows(
      lapply(ohlccodes, function(code) { 
      ##obs <- getResource("ohlcobservations",params=list("format"="csv", "page_size"=1,"series_symbol"=code,"date__lte"=asof),session=session)
      ##obs <- getOhlcObsForSymbol(session, code, lte=asof, lookBack=lookBack + 100, excludeWeekends = TRUE, addFields = NULL)
      obs <- getOhlcObsForSymbol(session, code, lte=asof, lookBack=lookBack, excludeWeekends = TRUE, addFields = NULL)
      ##NROW(obs) > 0 || return(obs)
      return(
        obs %>%
          dplyr::mutate(across(c(open,high,low,close), ~ as.numeric(.x))) %>%
          dplyr::mutate(date=as.Date(date)) %>%
          dplyr::mutate(across(c(id,symbol), ~ as.character(.x)))

      )
      ##assuming the 100 above is to avoid empty records in API call
    }
    )
    )

    ## Mask data if ohlc observation is empty:
    # # ohlcObs <- lapply(1:length(ohlccodes), function(i) {

    # #     ## If we have ohlc observations, return the same:
    # #     NROW(ohlcObs[[i]]) == 0 || return(ohlcObs[[i]])

    # #     df <- initDF(colnames(ohlcObs[[i]]))

    # #     df[, "symbol"] <- ohlccodes[i]

    # #     df

    # # })

    ohlcObs <- ohlcObs %>%
      dplyr::bind_rows(
        dplyr::bind_cols(
          data.frame("symbol"=ohlccodes[!ohlccodes %in% ohlcObs$symbol],stringsAsFactors=FALSE),
          initDF(colnames(ohlcObs)[!colnames(ohlcObs)=="symbol"])
        )
        ) %>%
    dplyr::filter(!is.na(symbol))

    NROW(ohlcObs) > 0 || return(initDF(colnames(ohlcObs)) %>% dplyr::slice(0:0)) ##failsafe

    ## Run the ohlc health check on ohlc obs:
    ##ohlcObsHealth <- do.call(rbind, lapply(ohlcObs, function(obs) ohlcHealth(obs, asof=asof, lookBack=lookBack, bbg=bbg)))
    ohlcObsHealth <- data.frame() %>%
      dplyr::bind_rows(
        lapply(unique(ohlcObs$symbol), 
          function(obs) {
            ohlcHealth(ohlcObs[ohlcObs$symbol==obs,], asof=asof, lookBack=lookBack, bbg=bbg)
          }
        )
     )

    ## Remove the NA symbols:
    ##ohlcObsHealth <- ohlcObsHealth[!is.na(ohlcObsHealth[, "Symbol"]), ]

    # # if(NROW(ohlcObsHealth)==0) {

    # #   ohlcObsHealth <- initDF(colnames(ohlcObsHealth))
    # #   print("No OBS!")
    # #   return(ohlcObsHealth)

    # # }

    ## Get the suspects:
    ohlcObsSuspects <- ohlcObsHealth[apply(ohlcObsHealth, MARGIN=1, function(x) any(trimws(x[2:4]))), ]

    ## Add link DECAF
    healthCheck <- ohlcObsSuspects
    ##healthCheck[, "Link"] <- paste0(gsub("api", "", session[["location"]]), "/ohlc/observation?symbol=", healthCheck[, "Symbol"])
    ##healthCheck[, "Link"] <- paste0("<a href='", healthCheck[, "Link"], "'>LINK</a>")
    healthCheck$Link <- getEndpointLink(session=session,endpnt="ohlcs",df=healthCheck %>% dplyr::rename(id=Symbol),bridge="/observation?symbol=")
    if(runLink) {
      dat <- healthCheck %>%
        dplyr::inner_join(resources %>% dplyr::select(id,ohlccodeX), by=c("Symbol"="ohlccodeX"))
      healthCheck$Instrument <- getEndpointLink(session=session,endpnt="resources",df=dat,placeholder=dat$id)
    }
    healthCheck[sapply(healthCheck, is.infinite)] <- NA
    ## check for special chars
    healthCheck  <- healthCheck %>%
      ##dplyr::filter(Symbol=="1- Loan to Microfin 11%") %>%
      dplyr::mutate(Link=stringr::str_replace_all(Link,"%","%25"))

    ## Done, return:
    return(healthCheck)

}


##' This function checks the health of the OHLC observations.
##'
##' This is a description
##'
##' @param ohlc A data-frame with the ohlc observations.
##' @param asof The date to compare ohlc observations to.
##' @param lookBack The number of days to look back for the check.
##' @param bbg logical for whether to run the bbg nomenclature check above.
##' @return A data-frame with state of ohlc series.
##' @export
ohlcHealth <- function(ohlc, asof=Sys.Date(), lookBack=5, bbg=FALSE) {

  ## Convert close to numeric, date to date:
  ohlc[, "close"] <- as.numeric(ohlc[, "close"])

  ohlc[, "date"] <- as.Date(ohlc[, "date"])

  ## N days condition:
  ndays <- NROW(ohlc) ==  0 | all(is.na(ohlc[, "close"]))

  ## Full time series to find gaps
  anygaps <- FALSE
  gapDate <- NA %>% as.Date()
  gaps <- data.frame()
  flagDate <- data.frame()

  if(!all(is.na(ohlc$date))) {
  gaps <- data.frame(date=seq.Date(from=min(ohlc$date,na.rm=TRUE),to=max(ohlc$date,na.rm=TRUE),by="day")) %>%
    dplyr::left_join(ohlc,by="date") %>%
    dplyr::mutate(flag=ifelse(is.na(close),1,0),week=paste(lubridate::year(date),lubridate::week(date))) %>%
    dplyr::group_by(week) %>%
    dplyr::mutate(gaps=sum(flag)) %>% 
    dplyr::ungroup()

  flagDate <- gaps  %>%
    dplyr::filter(flag==1&gaps>4) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(date) %>%
    dplyr::slice(1:1)
  }
  if(NROW(flagDate)>0) {
  anygaps <- TRUE
  gapDate <- gaps %>%
    dplyr::filter(!is.na(id),date<flagDate$date)  %>%
    dplyr::arrange(desc(date))  %>%
    dplyr::slice(1:1)  %>%
    .[[1]]
  }

  ## Prepare the data frame:
  result <- data.frame("Symbol"=ohlc[1, "symbol"],
                       "No PX in N days"=as.logical(ifelse(ndays, TRUE, all(asof - ohlc[, "date"] > lookBack))),
                       "No PX at all"=as.logical(ndays),
                       "No PX change"=as.logical(ifelse(ndays, TRUE, all(diff(ohlc[, "close"]) == 0))),
                       "Last PX update"=max(as.Date(ohlc[, "date"]),na.rm=TRUE),
                       "Any PX gaps"=anygaps,
                       "First gap date"=gapDate,
                       check.names = FALSE,
                       stringsAsFactors = FALSE)

  if(bbg) {
    result[,"Dubious Symbol"] <- safeNull(dubiousSymbolBBG(result$Symbol))
  }

  ## Done, return:
  result

}


##' This function checks whether symbols are dubious w.r.t Bloomberg symbology.
##'
##' This is a description
##'
##' @param symbol A vector with symbols
##' @return A vector of length symbols with TRUE or FALSE.
##' @export
dubiousSymbolBBG <- function(symbol) {

    ## Foolproof pre-check
    length(symbol) > 0 && class(symbol) == "character" || return(TRUE)

    ## Split each symbol by space.
    splitSymbol <- strsplit(symbol, " ")

    ## Check each split symbol for inconsistency and return:
    sapply(splitSymbol, function(x) length(x) == 1 | all(is.na(match(tail(x), c("Equity", "Corp", "Curncy", "Index", "Comdty")))))
}


##' This function subsets data from a decaf endpoint and styles it for email ready use.
##'
##' This is a description
##'
##' @param endpnt string for the decaf endpoint, e.g. trades.
##' @param session the rdecaf session.
##' @param prams string containing the expression for the params to prefilter data in addParams of getDBObject. Defaults to NULL.
##' @param func list containing the function and its corresponding parameters to be applied where applicable. Defaults to NULL.
##' @param failSafe data frame to return in short circuits. Can be NULL, defaults to empty data frame showing 'no records'.
##' @param cols vector of the column names to keep - besides mandatory ID - in the returned DF. Defaults to NULL.
##' @param colNames vector of the stylized column names to use for the returned columns. Defaults NULL, in which case the raw names from above are used.
##' @param omitCFlag numeric indicating what cflag to omit if we are to apply omitRecordsByFlag fn. Defaults to NULL.
##' @param addLink logical indicating whether to add the decaf link. Defaults to TRUE, requires that the endpoint data frame have an 'id' column.
##' @param Filter logical indicating whether to filter the data on the condition column from the optional applied func. Defaults to TRUE.
##' @return The endpoint data-frame with dubious data.
##' @export
subsetFromDecaf <- function(endpnt,
                            session,
                            prams="",
                            func=NULL,
                            failSafe=data.frame("No Data"=character(),stringsAsFactors=FALSE),
                            cols=NULL,
                            colNames=NULL,
                            omitCFlag=NULL,
                            addLink=TRUE,
                            Filter=TRUE
                            ) {

  is.null(colNames)|length(cols)==length(colNames) || {
    print("Columns headers length must match columns length!")
    return(failSafe)
  }

  ## Extra condition for endpoints with too much data
  if(endpnt %in% c("trades","quants") & prams == "") { 
    prams <- "list(commitment__gte=dateOfPeriod('M-0', Sys.Date()))"
  }

  dat <- try(getDBObject(endpnt,session=session,addParams=eval(parse(text=prams))))

  if(class(dat)=="try-error") {
    print(class(dat))
    print("Check Endpoint")
    return(failSafe)
  }
  
  if(!is.null(omitCFlag)) {
     dat <- omitRecordsByFlag(endpoint=endpnt, sourceData=dat, session=session, omitFlag=omitCFlag)
  }

  ##start loop
  fns <- length(func)
  cnt <- 1

  while(fns>0) {

  funx <- func[[cnt]]

  if(stringr::str_detect(funx[["fn"]],"<-")) {

    funCust <- eval(parse(text=sapply(stringr::str_split(funx[["fn"]],"<-"), function(x) x[length(x)])))
    funx[["fn"]] <- "funCust"

  }
  
  ##workaround for nested session when its required as a param
  if(any(names(funx[["parms"]])=="session") && funx[["parms"]]$session=="session") { 
    funx[["parms"]]$session <- eval(parse(text="session"))
  }

  dat <- do.call(funx[["fn"]],c(list(dat),funx[["parms"]]))

  if(Filter & any(colnames(dat)=="condition")) {

  dat <- dat %>%
    dplyr::filter(condition)

  }

  ##increment
  fns <- fns - 1
  cnt <- cnt + 1

  }

  if(class(dat)=="try-error") {
    print(class(dat)) 
    print("Check Condition")
    return(failSafe)
  }

  NROW(dat) > 0 || return(failSafe)

  if(!is.null(cols)) {

  dat <- dat %>%
    dplyr::select(id,tidyselect::all_of(cols))

  if(is.null(colNames)) {
    colNames <- col
  }
  colnames(dat) <- c("id",colNames)

  }

  addLink || return(cleanDfReturn(dat))
                                      
  dat$Link <- getEndpointLink(session=session,endpnt=endpnt,df=dat,placeholder=dat$id)

  return(cleanDfReturn(dat))

}

##' This function is an iteration of the subset function above, specifc for trades endpoint.
##'
##' This is a description
##'
##' @param session the rdecaf session.
##' @param filt string of the filter expression to evaluate after pulling the endpoint data. Defaults to non-cash, depo, loan ctypes and pxvalues <1.
##' @param params string of the list expressing to evaluate inside addParams call to getdboject. Defaults to last year of relative data.
##' @param lookBack numerical value of days to lookback when querying the ohlc data to calculate moving average used for deviation derivation. Defaults to 1 year.
##' @param tolerance numerical value indicating threshold above which to return records that deviate too far from a moving average. Defaults to .5.
##' @param addLink logical indicating whether to add the decaf link. Defaults to TRUE, requires that the endpoint data frame have an 'id' column.
##' @return The data frame containing relevant stylized records.
##' @export
tradePxDubiosity <- function(session,
                             filt="!resmain_ctype %in% c('DEPO','LOAN','CCY') & round(as.numeric(pxmain))<=1",
                             params="list(commitment__gte=Sys.Date()-365)",
                             lookBack=365,
                             tolerance=.5,
                             addLink=TRUE) {

  weird <- getDBObject("trades",session=session,addParams=eval(parse(text=params))) %>%
    dplyr::filter(eval(parse(text=filt)))

  NROW(weird) > 0 || return(data.frame())

  weird <- weird %>%
    dplyr::select(id,resmain_symbol,resmain_ctype,commitment,pxmain,qtymain) %>% 
    dplyr::mutate(id=as.character(id),Price=as.numeric(pxmain),QTY=as.numeric(qtymain),Date=as.Date(commitment)) %>%
    dplyr::rename(Symbol=resmain_symbol,Type=resmain_ctype) %>%
    dplyr::group_by(Symbol) %>%
    dplyr::arrange(Date) %>%
    dplyr::select(id,Symbol,Type,Date,Price,QTY)

  dat <- data.frame() %>%
    dplyr::bind_rows(
        lapply(unique(weird$Symbol), function(x) {
          obs <- getOhlcObsForSymbol(session=session, symbol=x, lte=weird[weird$Symbol==x,][1,]$Date,lookBack=lookBack)

          NROW(obs) > 0 || return(
            weird %>%
              dplyr::filter(Symbol==x,Price==0) 
          )

          days <- as.numeric(difftime(as.Date(obs[1,]$date), as.Date(obs[NROW(obs),]$date), units="days"))
          days <- max(1,days,na.rm=TRUE)
          ewma <- head(na.omit(TTR::EMA(obs[, "close"], n=min(NROW(obs), 10))), 1)
          
          dat <- weird %>%
            dplyr::filter(Symbol==x) %>%
            dplyr::mutate(dev=abs(Price/ewma-1),ewma=ewma,days=days) %>%
            dplyr::filter(dev>tolerance) %>%
            dplyr::select(-c(dev,ewma,days))

          dat 

        })
      )

    NROW(dat) > 0 || return(dat)

    dat <- dat %>%
      dplyr::mutate(across(is.numeric, ~beautify(round(.x,2))))

    addLink || return(dat %>% dplyr::select(-id))

    dat$Link <- getEndpointLink(session=session,endpnt="trades",df=dat,placeholder=dat$id)

    return(dat %>% dplyr::select(-id))    

}