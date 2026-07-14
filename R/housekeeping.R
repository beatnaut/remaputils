##' A function to overwrite variables in dataframe according to an outline.
##'
##' This is the description
##'
##' @param data data frame containing the variable to overwrite, e.g. resources.
##' @param bp list dictating the blueprint of the overwriting function that will be converted into expression. See below.
##' @param var vector of the variable name to be changed, e.g. assetclass.
##' @param sweep logical to dictate whether to overwrite values with existing values or not. Defaults to false.
##' @return data frame with the revised column - as character - and an added flag column 'recode' indicating where changes were made.
##' @examples
##' bp <- list(
##' "5" ="ctype=='CCY'",
##' "6" ="ctype %in% c('LOAN', 'DEPO', 'COD')",
##' "13"="ctype=='BOND'",
##' "17"="ctype %in% c('FXFWD', 'FXOPT','DCIP')",
##' "27"="ctype=='INSR'",
##' "11"="ctype=='ELNP'",
##' "9" ="ctype=='SHRE' & safeGrep(trimConcatenate(`attributes.figistype`), 'COMMONSTOCK') == '1'"
##' )
##' @import tidyverse
##' @export
recodeIt <- function(data,bp,var,sweep=FALSE) {

    data <- data %>%
        dplyr::mutate(recoded=FALSE)

    if(!sweep) {
        sweepit <- data %>%
            dplyr::filter(is.na(.data[[var]]))
        keepit <- data %>%
            dplyr::filter(!is.na(.data[[var]]))
        data <- sweepit
    }

    NROW(data) > 0 || return(data)

    expr <- sapply(seq_along(bp),function(x) {paste(bp[[x]],sprintf("'%s'",names(bp)[x]),sep="~")})
    expr <- paste(expr,collapse=",")
    expr <- paste0(var,"=dplyr::case_when(",expr,")")

    data <- within(data, eval(parse(text = expr)))
    data$recoded <- TRUE

    if(!sweep) {
        data <- data %>%
            dplyr::bind_rows(keepit)
    }

    return(data)

}

##' A function to get underlying symbol of an instrument.
##'
##' This is the description
##'
##' @param resources data frame containing the resources endpoint
##' @param instring string dictating what instrument type to search the underlying symbol for. Defaults to Equity.
##' @param sweep logical to dictate whether to overwrite values with existing values or not. Defaults to false.
##' @return data frame with the cleaned up underlying column.
##' @import tidyverse
##' @export
getUnderlyingSymbol <- function(resources, instring="Equity", sweep=FALSE) {

  dat <- resources %>%
    dplyr::filter(is.na(underlying))

  if(sweep) {
    dat <- resources
  }

  dat <- dat %>%
    dplyr::select(-underlying) %>%
    dplyr::mutate(underlyingS=dplyr::if_else(stringr::str_detect(symbol,instring),sapply(stringr::str_split(symbol,' '), function(x) paste(x[1],x[2],instring)),as.character(NA))) %>%
    dplyr::left_join(resources %>% dplyr::select(symbol,id) %>% dplyr::rename(underlying=id),by=c("underlyingS"="symbol")) %>%
    dplyr::select(-underlyingS)

  df <- resources %>%
    dplyr::filter(!id %in% dat$id) %>%
    dplyr::bind_rows(dat)

  return(df)

}

##' A function to derive calls vs puts for option instruments.
##'
##' This is the description
##'
##' @param res data frame containing the resources endpoint
##' @param ctypeF string to filter out for ctype. Defaults to OPT.
##' @param cp string vector dictating how calls and puts are identified. Defaults to C and P.
##' @param xtractFirst string vector identifying the instrument symbol will contain this string, where the above letters are extracted from the first char. Defaults to Equity.
##' @param xtractLast string vector identifying the instrument symbol will contain this string, where the above letters are extracted from the last char. Defaults to commodity and index.
##' @param nameMatch regex string vector identifying the patterns to look for in the instrument name for symbols beyond the above. Defaults to call(s) or put(s).
##' @param sweep logical to dictate whether to overwrite values with existing values or not. Defaults to false.
##' @return data frame with the cleaned up underlying column.
##' @import tidyverse
##' @export
sepCfrmP <- function(res, ctypeF="OPT", cp=c("C","P"), xtractFirst=c("EQUITY"), xtractLast=c("COMDTY","INDEX"), nameMatch="\\b(CALL|PUT|CALLS|PUTS)\\b", sweep=FALSE) {

  dat <- res %>%
    dplyr::filter(is.na(callput))

  if(sweep) {
    dat <- res
  }

  if(!is.null(ctypeF)) {
  resources <- dat[dat$ctype==ctypeF,]
  }

  resources <- resources %>%
    dplyr::rowwise() %>%
    dplyr::mutate(symbolString=as.character(tail(sapply(stringr::str_extract_all(symbol,"((\\b|[:alnum:])+[:alpha:]+[:digit:]+(\\b|[:alnum:]))|((\\b|[:alnum:])+[:digit:]+[:alpha:]+(\\b|[:alnum:]))"),function(x) x[stringr::str_detect(x,paste(cp,collapse="|"))]),1))) %>%
    dplyr::mutate(
    callputF=dplyr::case_when(
        stringr::str_detect(toupper(symbol),paste(xtractFirst,collapse="|")) ~ stringr::str_sub(symbolString,1,1),
        stringr::str_detect(toupper(symbol),paste(xtractLast,collapse="|")) ~ stringr::str_sub(symbolString,nchar(symbolString),nchar(symbolString)),
        !stringr::str_detect(toupper(symbol),paste(c(xtractFirst,xtractLast),collapse="|")) ~ stringr::str_sub(stringr::str_extract(toupper(name),nameMatch),1,1)
    )
    ) %>%
  dplyr::mutate(callputF=dplyr::if_else((is.na(callputF)|!callputF %in% cp) & stringr::str_detect(toupper(symbol),paste(xtractLast,collapse="|")),stringr::str_sub(symbolString,1,1),callputF)) %>% ##sometimes the index is ordered like equity
  dplyr::mutate(callput=dplyr::case_when(
     callputF=="C" ~ TRUE,
     callputF=="P" ~ FALSE
     )
  ) %>%
  dplyr::select(-callputF,-symbolString)

  df <- res %>%
    dplyr::filter(!id %in% resources$id) %>%
    dplyr::bind_rows(resources)

  return(df)

}


##' A function to create/update the corporate actions document for resources for a specific instance.
##'
##' This is the description
##'
##' @param session The session of the instance.
##' @param ctypes The trade ctypes. Either '81', i.e Share Dividend, or '101', Bond Coupon, or both. Default c(81, 101).
##' @param commitment__gte Since which date should the corporate actions be consdered? When NULL for all history. Default is dateOfPeriod(Y-0).
##' @return NULL. The function creates/updates corporate actions documents for resources, whereby ctype is: system+decaf://corporate-actions.
##' @export
##'
updateCorporateActionsDoc <- function(session, ctypes, commitment__gte=dateOfPeriod("Y-3")) {

    ## Define the ctype meta data mapping:
    caDocMapper <- list("81"=list("category"="dividend_per_share",
                                  "name"="Dividend Per Share"),
                        "101"=list("category"="coupon_per_unit",
                                   "name"="Coupon Per Unit"))

    ## Get the corporate actions per unit:
    caPerUnit <- lapply(ctypes, function(x) getCorporateActionPerUnit(session, ctype=x, commitment__gte=commitment__gte))

    ## Iterative over ctype corporate actions:
    for (i in 1:length(caPerUnit)) {

        ## Extract the corporate actions by resource:
        caByRes <- extractToList(caPerUnit[[i]], "resmain")

        ## Iterate over the resources' corporate action:
        lapply(caByRes, function(x) {

            ## Prepare the values:
            vals <- apply(x, MARGIN=1, function(z) list("date"=as.character(z["date"]), "value"=round(as.numeric(z["value"]), 4)))
            names(vals) <- NULL

            ## Prepare the content:
            content <- toJSON(vals, auto_unbox=TRUE)

            ## Construct the guid:
            guid <- digest::digest(sprintf("docs:corporate_action:%s:%s", caDocMapper[[ctypes[i]]]$category, trimws(x[1, "symbol"])))

            ## Prepare the document:
            docs <- list("ctype"="system+decaf://corporate-actions",
                         "category"=caDocMapper[[ctypes[i]]]$category,
                         "content"=content,
                         "guid"=guid,
                         "name"=caDocMapper[[ctypes[i]]]$name,
                         "extref"=x[1, "symbol"],
                         "mimetype"=NA,
                         "filename"=NA,
                         "encoding"=NA)

            ## Get the existing document:
            results <- getResource("documents", params=list("guid"=guid), session=session)$results

            ## If document doesn't exist, create:
            if (length(results) == 0) {
                postMultipart(sprintf("resources/%s/attachments", x[1, "resmain"]), payload=docs, session=session)
            } else {
                ## ... else update:
                putMultipart(sprintf("resources/%s/attachments/%s", x[1, "resmain"],  results[[1]]$id), payload=docs, session=session)
            }})
    }
}
