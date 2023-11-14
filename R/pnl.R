##' A function to provide the pnl preemble.
##'
##' This is the description
##'
##' @param posBeg The beginning period positions from getFlatHoldings.
##' @param posEnd The ending period positions from getFlatHoldings.
##' @param resources The resources from rdecaf.
##' @param quants The quants data frame.
##' @param trades The trades data frame.
##' @param portfolio The portfolio id.
##' @param fxobs The fx rates.
##' @param ccy The portfolio currency.
##' @param session The rdecaf session.
##' @return A list with the pnl preemble.
##' @export
getPnlPreemble <- function(posBeg, posEnd, resources, quants, trades, portfolio, fxobs, ccy, session) {

    ## Filter the quants:
    quants <- quants[quants[, "ctype"] != 700, ]

    ## Filter the trades:
    trades <- trades[trades[, "ctype"] != 300, ]

    if (NROW(quants) == 0) {
        quants <- initDF(colnames(quants))
    }

    if (NROW(trades) == 0) {
        trades <- initDF(colnames(trades))
    }

    ## Append the trade resources for each quant:
    quants <- data.frame(quants, trades[match(quants[, "trade"], trades[, "id"]), c("resmain", "resaltn", "resundr")])

    ## Filter required columns:
    quants <- quants[, c("commitment", "ctype", "type", "quantity", "valamt", "resource", "symbol", "trade", "resmain", "resaltn", "resundr", "trade_reference")]

    ## Append the resource ctype to the quants:
    quants[, "resource_type"] <- resources[match(quants[, "resource"], resources[, "id"]), "ctype"]

    ## Rename the columns:
    colnames(quants) <- c("date", "ctype", "type", "qQty", "valamt", "qRes", "symbol", "trade", "tResmain", "tResaltn", "tResundr", "reference", "resCtype")

    ## Round quantities:
    quants[, "qQty"] <- round(as.numeric(quants[, "qQty"]), 4)

    ## Round quantities:
    quants[, "valamt"] <- round(as.numeric(quants[, "valamt"]), 4)

    ## Extend the start positions by closed ones:
    posBeg <- cleanNARowsCols(extendPositionByClosedQuants(quants, posBeg, resources))

    ## Extend the ned positions by closed ones:
    posEnd <- cleanNARowsCols(extendPositionByClosedQuants(quants, posEnd, resources))

    ## Remove the cash from beginning positions:
    posBeg <- posBeg[posBeg[, "Type"] != "Cash", ]
    posBeg <- posBeg[posBeg[, "Type"] != "FX Forward", ]
    posBeg <- posBeg[posBeg[, "Type"] != "FX Forward Contract", ]
    posBeg <- posBeg[posBeg[, "Type"] != "Time Deposit Contract", ]
    posBeg <- posBeg[posBeg[, "Type"] != "Loan Contract", ]
    posBeg <- posBeg[posBeg[, "Type"] != "Time Deposit", ]
    posBeg <- posBeg[posBeg[, "Type"] != "Loan", ]

    ## Remove the cash from end positions:
    posEnd <- posEnd[posEnd[, "Type"] != "Cash", ]
    posEnd <- posEnd[posEnd[, "Type"] != "FX Forward", ]
    posEnd <- posEnd[posEnd[, "Type"] != "FX Forward Contract", ]
    posEnd <- posEnd[posEnd[, "Type"] != "Time Deposit Contract", ]
    posEnd <- posEnd[posEnd[, "Type"] != "Loan Contract", ]
    posEnd <- posEnd[posEnd[, "Type"] != "Time Deposit", ]
    posEnd <- posEnd[posEnd[, "Type"] != "Loan", ]

    ## Extend the start positions by closed ones:
    posBeg <- cleanNARowsCols(posBeg)

    ## Extend the ned positions by closed ones:
    posEnd <- cleanNARowsCols(posEnd)

    colNames <- c("Name", "ID", "QTY", "Value", "Exposure", "Type", "Symbol")

    ##:
    if (NROW(posBeg) == 0) {
        posBeg <- initDF(colNames)
    }

    ##:
    if (NROW(posEnd) == 0) {
        posEnd <- initDF(colNames)
    }

    ## Append the resource quantity:
    posBeg[, "resqty"] <- resources[match(posBeg[, "ID"], resources[, "id"]), "quantity"]

    ## Append the resource quantity:
    posEnd[, "resqty"] <- resources[match(posEnd[, "ID"], resources[, "id"]), "quantity"]

    ## Append the resource quantity:
    quants[, "resqty"] <- resources[match(quants[, "qRes"], resources[, "id"]), "quantity"]

    ## Append the currency pair:
    quants[, "fx"] <- paste0(resources[match(quants[, "symbol"], resources[, "symbol"]), "ccymain"], ccy)

    matchedFX <- fxobs[match(quants[, "fx"], names(fxobs))]

    quants[, "fxrate"] <- do.call(c, lapply(1:NROW(quants), function(i) {

        matchedDates <- match(quants[i, "date"], matchedFX[[i]][, "date"])

        if (is.na(matchedDates)) {
            return(1)
        }

        matchedFX[[i]][matchedDates, "close"]
    }))

    quants[, "valamt(org)"] <- quants[, "valamt"]
    quants[, "valamt"] <- quants[, "valamt"] * quants[, "fxrate"]

    orderCols <- trades[match(quants[, "trade"], trades[, "id"]), c("pseudorder", "created")]
    quants <- quants[order(orderCols[, 1], orderCols[, 2], decreasing=FALSE), ]

    return(list("extendedQuants"=quants,
                "extendedPosBeg"=posBeg,
                "extendedPosEnd"=posEnd))

}


##' A wrapper function to get the pnl preemble.
##'
##' This is the description
##'
##' @param portfolio The portfolio id.
##' @param startDate The start date.
##' @param endDate The end date.
##' @param session The rdecaf session.
##' @return A list with the PnL preemble
##' @export
pnlPreembleWrapper <- function(portfolio, startDate, endDate, session) {

    ## Construct the portfolio params:
    pParams <- list("format"="csv", "page_size"=-1, "id"=portfolio)

    ## Get the portfolio:
    portfolio <- as.data.frame(getResource("portfolios", params=pParams, session=session))

    ## Get the stocks:
    stocks <- getStocks(portfolio, session, zero=1,endDate, c="portfolio")

    ## Get the resources:
    resources <- getResourcesByStock(stocks, session, getUnderlying=FALSE)

    ## Get the consolidations:
    consolidations <- lapply(c(startDate, endDate), function(dt) {
        getFlatHoldings(getResource("fundreport", params=list("fund"=portfolio[, "id"],
                                                              "ccy"=portfolio[, "rccy"],
                                                              "date"=dt,
                                                              "type"="commitment"),
                                    session=session)[["holdings"]])})

    ## The column selections for the consolidations:
    colSelect <- c("Name", "ID", "QTY", "Value", "Exposure", "Type", "Symbol")

    ## Get the starting period positions:
    posBeg <- consolidations[[1]][, colSelect]

    ## Get the ending period positions:
    posEnd <- consolidations[[2]][, colSelect]

    ## Construct the quants params:
    quantParams <- list("account__portfolio"=portfolio[, "id"], "format"="csv", "commitment__gte"=startDate, "page_size"=-1)

    ## Get the quents:
    quants <- as.data.frame(getResource("quants", params=quantParams, session=session))

    ## Get the trades for portfolio:
    trades <- getTradesFromContainerNames(portfolio[, "name"], session, "portfolios", gte=startDate)[["trades"]]

    ## Get the pnl preemble:
    getPnlPreemble(posBeg, posEnd, resources, quants, trades, portfolio, session)

}


##' A function to contextualize quants using the pnl preemble.
##'
##' This is the description
##'
##' @param pnlPreemble A list with the extended positions and quants.
##' @param dateBeg The beginning date.
##' @param dateEnd The ending date.
##' @return A list with the quants contexts for each position.
##' @export
contextualizeQuants <- function(pnlPreemble, dateBeg, dateEnd) {

    ## Get the extended beginning positions:
    extPosBeg <- pnlPreemble[["extendedPosBeg"]]
    noBeg <- NROW(extPosBeg) == 1 & all(is.na(extPosBeg[1, ]))

    ## Get the extended ending positions:
    extPosEnd <- pnlPreemble[["extendedPosEnd"]]
    noEnd <- NROW(extPosEnd) == 1 & all(is.na(extPosEnd[1, ]))

    if (noBeg & noEnd) {
        return(NULL)
    }

    ## Get the extended quants:
    quants <- pnlPreemble[["extendedQuants"]]

    ## For each position row,
    qu <- lapply(extPosBeg[, "ID"], function(id) quants[apply(quants[, c("tResmain", "tResaltn", "tResundr")], MARGIN=1, function(x) any(!is.na(match(x, id)))), ])

    isEmpty <- all(is.na(qu[[1]][, colnames(qu[[1]]) != "fx" & colnames(qu[[1]]) != "fxrate"])) & length(qu) == 1

    if (length(qu) == 0 | isEmpty) {
        return(NULL)
    }

    ## ##############################
    ## Special treatment for options.
    ## Override valamt with exposure.
    ## ##############################
    ## qu <- lapply(qu, function(x) {
    ##     isOption <- x[, "resCtype"] == "OPT"
    ##     any(isOption) || return(x)
    ##     x[isOption, "valamt"] <- abs(as.numeric(x[isOption, "resqty"]) * as.numeric(x[isOption, "qQty"])) * (as.numeric(x[isOption, "valamt"] / abs(as.numeric(x[isOption, "qQty"]))))
    ##     return(x)
    ## })
    ## ##############################
    ## ##############################

    ## Prepare the quant context list and return:
    retval <- lapply(1:NROW(qu), function(i) {

        ## Get the quants for current position
        quT <- qu[[i]]

        ## If no quants, mask a row:
        if (NROW(quT) == 0) {

            ## Initialise a data frame with NA's (the beginning position)
            temp <- as.data.frame(t(rep(NA, length(quT))), stringsAsFactors=FALSE)

            ## Remove colnames:
            colnames(temp) <- colnames(quT)

            ## Assign back to quT:
            quT <- temp

        } else {
            ## Else, append the beginning position:
            quT <- rbind(rep(NA, NCOL(quT)), quT)
        }

        ## The columns to be filled:
        tCols <- c("type", "date", "qQty", "valamt", "qRes", "symbol", "resqty", "reference")

        if (safeGrep(extPosBeg[i, "Type"], "Option") == "1") {
            isOption <- TRUE
        } else {
            isOption <- FALSE
        }


        ## Fill the columns with the beginning position values:
        quT[1, tCols] <- c("Start",
                           as.character(dateBeg),
                           as.numeric(extPosBeg[i, "QTY"]),
                           ifelse(isOption, as.numeric(safeNull(extPosBeg[i, "Value"])), as.numeric(safeNull(extPosBeg[i, "Exposure"]))),
                           ## as.numeric(extPosBeg[i, "Value"]),
                           ## as.numeric(safeNull(extPosBeg[i, "Exposure"])),
                           as.numeric(extPosBeg[i, "ID"]),
                           as.character(extPosBeg[i, "Symbol"]),
                           as.numeric(extPosBeg[i, "resqty"]),
                           NA)

        ## Append a row for the ending position:
        quT <- rbind(quT, rep(NA, NCOL(quT)))

        if (isOption) {
            posEndVal <- as.numeric(safeNull(extPosEnd[match(quT[1, "qRes"], extPosEnd[, "ID"]), "Value"]))
        } else {
            posEndVal <- as.numeric(safeNull(extPosEnd[match(quT[1, "qRes"], extPosEnd[, "ID"]), "Exposure"]))
        }

        ## Fill the column with the ending position values:
        quT[NROW(quT), tCols] <- c("End",
                                   as.character(dateEnd),
                                   as.numeric(extPosEnd[match(quT[1, "qRes"], extPosEnd[, "ID"]), "QTY"]),
                                   ##as.numeric(safeNull(extPosEnd[match(quT[1, "qRes"], extPosEnd[, "ID"]), "Exposure"])),
                                   posEndVal,
                                   as.numeric(extPosEnd[match(quT[1, "qRes"], extPosEnd[, "ID"]), "ID"]),
                                   as.character(extPosEnd[match(quT[1, "qRes"], extPosEnd[, "ID"]), "Symbol"]),
                                   as.character(extPosEnd[match(quT[1, "qRes"], extPosEnd[, "ID"]), "resqty"]),
                                   NA)

        ## If position type is Share and any quant types have options, get rid of the options:
        if (!is.na(extPosBeg[i, "Type"]) == "Share" & any(safeCondition(quT, "resCtype", "OPT"))) {
            quT <- quT[!safeCondition(quT, "resCtype", "OPT"), ]
        }

        ## If position type is Option and any quant types have shares, get rid of the shares:
        if (!is.na(extPosBeg[i, "Type"]) == "Option Contract" & any(safeCondition(quT, "resCtype", "SHRE"))) {
            quT <- quT[!safeCondition(quT, "resCtype", "SHRE"), ]
        }

        ## Order by date:
        quT <- quT[order(quT[, "date"]), ]

        ## Get rid of cash/currency
        quT <- quT[apply(mgrep(quT[, "type"], c("Cash", "Currency", "Premium Payment", "I/O")), MARGIN=1, function(x) all(x == "0")), ]

        ## Which quants are actions?
        actions <- quT[, "type"] != "Start" & quT[, "type"] != "End"

        if (any(actions)) {
            #quT[actions, "valamt"] <- as.numeric(quT[actions, "valamt"]) * ifelse(quT[actions, "qQty"] > 0, -1, 1)
        }

        quT <- quT[, c("date", "symbol", "type", "qQty", "valamt", "resqty", "reference")]

        ## Append columns for pnl computation:
        quT[, c("PNL::isInc", "PNL::isCls", "PNL::isFee", "PNL::QTY", "PNL::tQTY", "PNL::InvAmt", "PNL::CumInv", "PNL::Income", "PNL::Real")] <- NA

        ## Done, return
        quT
    })

    ## Assign the names and return:
    ## retval <- lapply(1:length(retval), function(i) list("symbol"=extPosBeg[i, "Symbol"], "type"=extPosBeg[i, "Type"], "quants"=retval[[i]]))

    names(retval) <- extPosBeg[, "Symbol"]

    retval

}

##' Extends the positions data frame from getFlatholdings with opened/closed quants in between position dates.
##'
##' This is the description
##'
##' @param quants The quants data frame from rdecaf
##' @param positions The positions data frame from getFlatHoldings
##' @param resources The resources data frame from rdecaf
##' @return The extended positions data frame.
##' @export
extendPositionByClosedQuants <- function(quants, positions, resources) {

    ## Get the columns indices which have the RES key word:
    residCols <- apply(mgrep(toupper(colnames(quants)), c("RESMAIN", "RESALTN", "RESUNDR")), MARGIN=1, function(x) any(x != "0"))

    if (NROW(quants) == 0) {
        return(positions)
    }

    if (NROW(quants) == 1) {
        uniqueQuantres <- na.omit(unique(as.numeric(quants[, residCols])))
    } else {
        ## Get all unique resource id from txns (including the trade resources!!):
        uniqueQuantres <- try(na.omit(unique(do.call(c, apply(quants[, residCols], MARGIN=2, unique)))), silent=TRUE)
        if (class(uniqueQuantres) == "try-error") {
            uniqueQuantres <- try(na.omit(unique(do.call(c, lapply(quants[, residCols], MARGIN=2, unique)))), silent=TRUE)
        }
    }

    ## Match the unique quant resources with the position resource id:
    matchId <- match(uniqueQuantres, positions[, "ID"])

    ## Get the quant id's not present in positions:
    unmatchedID <- uniqueQuantres[is.na(matchId)]

    if (length(unmatchedID) == 0) {
        return(positions)
    }

    ## Initialise the append data.frame:
    appendD <- as.data.frame(matrix(NA, length(unmatchedID), NCOL(positions)))

    ## Assign the colnames:
    colnames(appendD) <- colnames(positions)

    ## Fill the names:
    appendD[, "Name"] <- ellipsify(toupper(resources[match(unmatchedID, resources[, "id"]), "name"]))

    ## Fill the resource id:
    appendD[, "ID"]  <- unmatchedID

    ## Fill the quantities:
    appendD[, "QTY"] <- 0

    ## File the values:
    appendD[, "Value"] <- 0

    ## Fill the types:
    appendD[, "Type"] <- resources[match(unmatchedID, resources[, "id"]), "type"]

    ## File the symbol:
    appendD[, "Symbol"] <- resources[match(unmatchedID, resources[, "id"]), "symbol"]

    ## Rbind unmatched quant id to positions as 0 positions and return:
    rbind(positions, appendD)
}


##' Computes the pnl using the quant context list.
##'
##' This is the description
##'
##' @param quantContext A list with the quant context.
##' @return A list with the quant context extended by pnl computation.
##' @export
computePnL <- function(quantContext) {

    ## If no context is provided, return NULL:
    if (length(quantContext) == 0) {
        return(NULL)
    }

    ## HEBELE:
    retval <- lapply(1:NROW(quantContext), function(row) {

        story <- quantContext[[row]]

        story <- story[story[, "type"] != "PnL", ]

        story <- story[!stringr::str_detect(story$type,"Transfer|Investment"), ]

        if (all(is.na(story$qQty)|story[, "qQty"] == "0")) {
            return(list("PnLs"=NULL,
                        "Totals"=NULL))
        }

        if (all(story[story[, "type"] == "Start" | story[, "type"] == "End", "qQty"] == "0")) {
            return(list("PnLs"=NULL,
                        "Total"=NULL))
        }

        story[, "PNL::isInc"] <- !apply(mgrep(story[, "type"], c("Dividend", "Coupon", "PnL")), MARGIN=1, function(x) all(x == "0"))
        story[, "PNL::isFee"] <- !apply(mgrep(story[, "type"], c("Fee")), MARGIN=1, function(x) all(x == "0"))

        isStart <- story[, "type"] == "Start"

        if (all(!isStart)) {
            isStart[1] <- TRUE
        }

        isEnd   <- story[, "type"] == "End"
        isInc   <- story[, "PNL::isInc"]
        isFee   <- story[, "PNL::isFee"]

        story[, "PNL::QTY"] <- as.numeric(story[, "qQty"])
        story[isEnd, "PNL::QTY"] <- 0
        story[isInc | isFee, "PNL::QTY"] <- 0

        story[, "PNL::tQTY"] <- cumsum(as.numeric(!isInc) * as.numeric(!isFee * as.numeric(!isEnd)) * story[, "PNL::QTY"])

        story[, "PNL::isCls"] <-  c(0, diff(abs(story[, "PNL::tQTY"]))) < 0

        story[, "PNL::isNew"] <-  story[, "qQty"] == story[, "PNL::tQTY"] & story[, "type"] != "Start" & story[, "type"] != "End"

        isCls   <- story[, "PNL::isCls"]

        isInv <- !isEnd & !isFee & !isInc & !isCls

        if (story[isStart, "qQty"] == 0) {
            isStart[which(isInv)[2]] <- TRUE
            isStart[1] <- FALSE
            story[isStart, "valamt"] <- as.numeric(story[isStart, "valamt"]) * sign(as.numeric(story[isStart, "qQty"]))
            isInv[1] <- FALSE
        }

        story[isInv, "PNL::InvAmt"] <- as.numeric(story[isInv, "valamt"])
        story[isCls, "PNL::InvAmt"] <- as.numeric(story[isCls, "valamt"]) * sign(as.numeric(story[isCls, "qQty"]))

        story[is.na(story[, "PNL::InvAmt"]), "PNL::InvAmt"] <- 0

        ## ############################################################################
        ## ############################################################################
        for (row in 1:NROW(story)) {

            if (row == 1) {
                story[row, "PNL::CumInv"] <- story[row, "PNL::InvAmt"]
                next
            }

            if (story[row, "PNL::isNew"]) {
                story[row, "PNL::CumInv"] <- story[row, "PNL::InvAmt"]
                next
            }

            story[row, "PNL::CumInv"] <- story[row, "PNL::InvAmt"] + story[row-1, "PNL::CumInv"]

        }
        ## ############################################################################
        ## ############################################################################

        ## ## #################################
        ## story[, "PNL::CumInv"] <- cumsum(story[, "PNL::InvAmt"])
        ## story[, "PNL::CumInv"] <- cumsum(story[, "PNL::InvAmt"] * ifelse(story[, "PNL::tQTY"] == 0, story[, "PNL::"], 1))
        ## story[, "PNL::CumInv"] <- cumsum(story[, "PNL::InvAmt"] * ifelse(story[, "PNL::tQTY"] == 0, 0, 1))
        ## ## #################################

        story[, "PNL::Income"] <-  as.numeric(isInc) * as.numeric(story[, "qQty"])
        story[, "PNL::Fees"]   <-  as.numeric(isFee) * as.numeric(story[, "qQty"])

        fullAmt <- abs(as.numeric(story[isCls, "valamt"]) / as.numeric(story[isCls, "qQty"])) * -as.numeric(story[which(isCls) -1, "PNL::tQTY"])
        story[isCls, "PNL::Real"] <- (fullAmt + story[which(isCls) -1, "PNL::CumInv"]) * (as.numeric(story[isCls, "qQty"])) / as.numeric(story[which(isCls) -1, "PNL::tQTY"])

        story[is.na(story[, "PNL::Real"]), "PNL::Real"] <- 0

        totalRealised <- sum(story[, "PNL::Real"])

        if (any(isCls)) {
             story[which(isCls)[1]:NROW(story), "PNL::CumInv"] <- story[which(isCls)[1]:NROW(story), "PNL::CumInv"] + totalRealised
        }

        unrlsd <- as.numeric(story[NROW(story), "valamt"]) - as.numeric(story[NROW(story), "PNL::CumInv"])
        unrlsd <- ifelse(is.na(unrlsd), 0, unrlsd)

        realsd <- sum(as.numeric(story[, "PNL::Real"]))

        income <- sum(as.numeric(story[, "PNL::Income"]))

        tofees <- sum(as.numeric(story[, "PNL::Fees"]))

        total <- unrlsd + realsd + income + tofees

        story <- list("PnLs"=story,
                      "Totals"=data.frame("Unrealised"=safeNull(unrlsd),
                                          "Realised"=safeNull(realsd),
                                          "Income"=safeNull(income),
                                          "Fees"=safeNull(tofees),
                                          "Total"=safeNull(total),
                                          "ROI"=safeNull(total) / safeNull(story[tail(which(isInv), 1), "PNL::CumInv"])
                                          )
                                          )

    })

    names(retval) <- names(quantContext)

    retval

}
##' Gets the pnl endpoint from decaf.
##'
##' This is the description
##'
##' @param apiURL the npl endpoint API URL string.
##' @param params a list of additional parameters.
##' @param session the decaf session.
##' @return A list with the pnl ledger data.
##' @export
bare_get <- function (apiURL="/apis/function/valuation-reports/eventsreport", params=list(), session=NULL) {
  
  ## Get or create a session:
  if (is.null(session)) {
    session <- rdecaf::readSession()
  }
  
  ## Get the base url to start to build the endpoint URL:
  url <- httr::parse_url(session$location)
  
  ## Add paths ensuring that path seperator is not duplicated and a
  ## trailing path seperator is added:
  url$path <- c(sub("/$", "", gsub("//", "/", apiURL)), "/")
  
  ## Add params:
  url$query <- params
  
  ## Construct the endpoint URL:
  url <- httr::build_url(url)
  
  ## Get the resource:
  response <- httr::GET(url, httr::add_headers(Authorization=rdecaf:::.authorizationHeader(session)))
  
  ## Get the status:
  status <- response$status_code
  
  ## If the status code is not 200, raise an error:
  if (status != 200) {
    stop(sprintf("%s returned a status code of '%d'.\n\n  Details provided by the API are:\n\n%s", url, status, httr::content(response, as="text")))
  }
  
  ## Return (note that we are suppressing messages):
  suppressMessages(httr::content(response))
}

##' flattens the pnl endpoint list data from decaf.
##'
##' This is the description
##'
##' @param grp the returned list of ledgers from function above.
##' @return A list with the flattened pnl ledger data.
##' @export
flattenEvents <- function(grp) {
  
  nameAccount <- unlist(sapply(grp[["entries"]], function(x) safeNull(sapply(x$event$contents$holding$accounts, function(y) y$name)))) %>% as.vector() %>% unique()
  nameAccount <- paste(nameAccount[!is.na(nameAccount)],collapse="")
  aClass <- unlist(sapply(grp[["entries"]], function(x) safeNull(sapply(x$holding$tags$classification, function(y) y$name)))) %>% as.vector() %>% unique()
  aClass <- paste(aClass[!is.na(aClass)],collapse="|")
  
  ## Construct the ledger:
  ledger <- data.frame("id"=as.character(safeNull(grp[["artifact"]]$id)),
                       "type"=safeNull(grp[["artifact"]]$type),
                       "subtype"=safeNull(grp[["artifact"]]$subtype),
                       "symbol"=safeNull(grp[["artifact"]]$symbol),
                       "name"=safeNull(grp[["artifact"]]$name),
                       "currency"=safeNull(grp[["artifact"]]$currency),
                       "tag"=sapply(grp[["entries"]], function(x) x$event$tag), ##TODO - opening and closing = valuation, stock = trade
                       "date"=sapply(grp[["entries"]], function(x) x$date),
                       "qty"=sapply(grp[["entries"]], function(x) x$quantity),
                       "valRef"=sapply(grp[["entries"]], function(x) x$value_ref_qty),
                       "valOrg"=sapply(grp[["entries"]], function(x) x$value_org_qty),
                       "valQty"=sapply(grp[["entries"]], function(x) safeNull(x$event$contents$holding$artifact$quantity)),
                       "pxcostRef"=sapply(grp[["entries"]], function(x) safeNull(x$cost_price_ref)),
                       "pxcostOrg"=sapply(grp[["entries"]], function(x) safeNull(x$cost_price_org)),
                       "pxlastRef"=sapply(grp[["entries"]], function(x) safeNull(x$last_price_ref)),
                       "pxlastOrg"=sapply(grp[["entries"]], function(x) safeNull(x$last_price_org)),
                       "quantType"=sapply(grp[["entries"]], function(x) safeTry(try(x$quant$action_quant_type, silent=TRUE))),
                       "nameAccount"=nameAccount,
                       "typeDetail"=sapply(grp[["entries"]], function(x) safeNull(x$event$contents$holding$artifact$type$name)),
                       "valAbsOrg"=sapply(grp[["entries"]], function(x) safeNull(x$holding$valuation$value$abs$org)),
                       "valAbsRef"=sapply(grp[["entries"]], function(x) safeNull(x$holding$valuation$value$abs$ref)),
                       "valNetOrg"=sapply(grp[["entries"]], function(x) safeNull(x$holding$valuation$value$net$org)),
                       "valNetRef"=sapply(grp[["entries"]], function(x) safeNull(x$holding$valuation$value$net$ref)),
                       "expAbsOrg"=sapply(grp[["entries"]], function(x) safeNull(x$holding$valuation$exposure$abs$org)),
                       "expAbsRef"=sapply(grp[["entries"]], function(x) safeNull(x$holding$valuation$exposure$abs$ref)),
                       "expNetOrg"=sapply(grp[["entries"]], function(x) safeNull(x$holding$valuation$exposure$net$org)),
                       "expNetRef"=sapply(grp[["entries"]], function(x) safeNull(x$holding$valuation$exposure$net$ref)),
                       "investment"=sapply(grp[["entries"]], function(x) safeNull(x$holding$investment$value$ref)),
                       "investmentOrg"=sapply(grp[["entries"]], function(x) safeNull(x$holding$investment$value$org)),
                       "px"=sapply(grp[["entries"]],function(x) safeNull(x$quant$action$main_price)) ##TODO, grp[["entries"]][[x]]$contents$quant$action$main_price
                       ,stringsAsFactors = FALSE) %>%
     dplyr::mutate(assetClass=aClass
                  ,pxFactor=1
                  ,exclude=FALSE
                  ) %>%
     dplyr::mutate(px=dplyr::if_else(is.na(px),as.numeric(pxlastRef),as.numeric(px)))
  
  
  if(NROW(ledger)==0) {
    return(NULL)
  }
  
  return(list("ledger"=ledger))
  
}


##' flattens the pnl endpoint list data from decaf.
##'
##' This is the description
##'
##' @param session the decaf session.
##' @param portfolio the decaf portfolio ID. Defaults to NULL.
##' @param date the as of date. Defaults to present.
##' @return A flat df containing the session artifacts and associated asset class.
##' @export
getStocksAndAssets <- function(session,portfolio=NULL,date=Sys.Date()) {
    
    ac <- getDBObject("assetclasses",session=session) %>% dplyr::select(id,contains("path"))  %>%  mutate(across(everything(), ~as.character(.x)))
    ac$assetClass <- apply(ac[,-1],1, function(x) paste(x[!is.na(x)],collapse="|")) 
    
    if(is.null(portfolio)) {
      stnA <- getDBObject("resources",session) 
    }

    if(!is.null(portfolio)) {
      stnA <- getResourcesByStock(getStocks(portfolio,session,zero=1,date=date,c="portfolio"),session=session) 
    }
    
    stnA <- stnA%>%
        dplyr::filter(!is.na(symbol)) %>% 
        dplyr::select(id,quantity,country,sector,issuer,assetclass##,symbol,isin,contains("attributes")
                      ) %>% 
        mutate(across(everything(), ~as.character(.x))) %>%
        dplyr::left_join(ac %>% select(-contains("path")),by=c("assetclass"="id")) 
    stnA$assetclass <- NULL
    
    return(stnA)
    
}


##' Overwrites columns that have missing date from the pnl endpoint.
##'
##' This is the description
##'
##' @param ledge the list containing data frame being overwritten from flatten events.
##' @param res the resource data to feed quantity and asset class data columns.
##' @param joinC the column name used to join the data frames.
##' @return A flat df containing the ledgers with the correct asset class info.
##' @export
overwriteEvents <- function(ledge,res,joinC="id") {

  ledger <- ledge$ledger
  
  cnames <- colnames(ledger)
  cnames <- cnames[!cnames %in% c("assetClass","pxFactor","exclude")]
  
  ledger <- ledger[,cnames] %>%
    dplyr::left_join(res,by=joinC) %>%
    dplyr::mutate(pxFactor=if_else(is.na(quantity),1,as.numeric(quantity))) %>%
    dplyr::mutate(px=px*pxFactor)
    

  return(list("ledger"=ledger))

}


##' Contextualizes the flattened PnL data.
##'
##' This is the description
##'
##' @param flat the list containing clean data frame ledgers.
##' @param excludedTags the tags to exclude from context in the ledger df.
##' @return A list containing the contextualized ledger data frame and the original one.
##' @export
contextEvents_ <- function(flat,excludedTags=c("exclude","pnl")) {
  
  ledger <- flat$ledger %>% 
    dplyr::filter(!tag %in% excludedTags) %>% 
##  dplyr::filter(!tag %in% excludedTags,!(!is.na(quantType)&quantType=="position_change")) %>% 
    dplyr::mutate(
      valRef=if_else(type!="BOND"&!is.na(valQty),valQty*if_else(tag %in% c("opening","closing"),qty,abs(qty))*pxlastRef*sign(if_else(tag %in% c("opening","closing"),1,valRef)),valRef) ##for future trade contracts expressed in PNL
      ,valOrg=if_else(type!="BOND"&!is.na(valQty),valQty*if_else(tag %in% c("opening","closing"),qty,abs(qty))*pxlastOrg*sign(if_else(tag %in% c("opening","closing"),1,valOrg)),valOrg)
      ,fees=if_else(!is.na(quantType)&stringr::str_detect(quantType,"fee"),1,0)
      ##,fees=if_else(!is.na(quantType)&str_detect(quantType,"fee")&type!="CCY",1,0)
      ,income=if_else(fees==0&tag=="income",1,0)
      ##,income=if_else(fees==0&tag=="income"&type!="CCY",1,0)
    ) 
  
  isStart <- ledger[, "tag"] == "opening"
  
  isEnd   <- ledger[,"tag"] == "closing"
  isInc   <- ledger[,"tag"] == "income"
  ##isInc   <- ledger[,"income"] == 1
  isFee   <- ledger[,"fees"] == 1
  
  isMrg   <- if_else(is.na(ledger[,"quantType"]),FALSE,stringr::str_detect(unlist(ledger[,"quantType"]),"split"))
  
  ledger[, "tqty"] <-cumsum(as.numeric(!isInc) * as.numeric(!isFee) * as.numeric(!isEnd) * ledger[,"qty"])
  
  ##isCls <- c(0, diff(abs(ledger[, "tqty"]))) < 0 ##should work for both short and long positions
  isCls <- sign(ledger$qty)<0 & !isFee & !isInc & !isMrg &!isEnd
  
  ##isInv <- !isFee & !isInc & !isMrg &!isEnd
  isInv <- sign(ledger$qty)>=0 & !isFee & !isInc & !isMrg &!isEnd
  
  if (ledger[isStart, "qty"] == 0) {
    isStart[which(isInv)[2]] <- TRUE
    isStart[1] <- FALSE
    isInv[1] <- FALSE
  }
  
  ledger[,"isStart"] <- isStart
  
  ledger[,"isEnd"] <- isEnd
  
  ledger[,"isCls"] <- isCls & !isMrg
  
  
  if (ledger[isEnd, "qty"] == 0) {
    ledger <- ledger %>% 
      dplyr::mutate(isEnd=row_number()==max(if_else(isCls,row_number(),as.integer(0))))
    isEnd <- ledger$isEnd
  }
  
  ledger[,"isInv"] <- isInv & !isCls & !isEnd
  
  ledger$isInc <- isInc
  
  ledger$isFee <- isFee
  
  ledger <- ledger %>% 
    dplyr::mutate(sign=if_else(tag %in% c("opening","closing")|isEnd,1,sign(qty))) %>% 
    dplyr::mutate(
      income=cumsum(if_else(income==1,sign*valRef,0)),
      fees=cumsum(if_else(fees==1,sign*valRef,0)),
      realized=as.numeric(NA)
    )
  
  
  dubious <- FALSE
  
  if(!all(is.na(ledger$px))) {
    
    if(min(ledger$px,na.rm=TRUE)>0) {  
      
      pxmin <- min(ledger$px,na.rm=TRUE)
      pxmax <- max(ledger$px,na.rm=TRUE)

# #       pxmin <- min(ledger[ledger$isInv|ledger$isCls,]$px,na.rm=TRUE)
# #       pxmax <- max(ledger[ledger$isInv|ledger$isCls,]$px,na.rm=TRUE)
      
      dubious <- if_else(dubious==FALSE,pxmax/pxmin>2,dubious)
      
    }
    
  }
  
  realized <- 0
  
  
  if(any(ledger$isCls)) {
    
    test <- ledger[1,]$pxcostRef*ledger[NROW(ledger),]$qty/ledger[nrow(ledger),]$investment
    
    if(!is.na(test)&abs(1-test)<.01) {
      
      cutoff <- min(ledger$date)  
      
      if(any(ledger$tqty==0)) {
        cutoff <- max(ledger[which(ledger$tqty==0),]$date)
      }
      wac <- ledger %>% 
        dplyr::filter(date>=cutoff,isInv) %>% 
        dplyr::mutate(px=if_else(is.na(pxlastRef),as.numeric(px),as.numeric(pxlastRef))) %>% 
        dplyr::mutate(px=px*qty/sum(qty)) %>% 
        dplyr::summarise(px=sum(px)) %>% 
        .[[1]]
      
      ledger[nrow(ledger),]$investment <- wac*ledger[nrow(ledger),]$qty
    }
    
    
    
    realized <- ledger[nrow(ledger),]$investment +
      sum(ledger[ledger$isCls,]$valRef) - ##add back sales
      sum(ledger[ledger$isInv,]$valRef)  ##subtract buys
    
    
    if(is.na(realized)|(!is.na(ledger[nrow(ledger),]$investment)&ledger[nrow(ledger),]$investment==0)) {
      
      ledger <- ledger %>% 
        dplyr::mutate(across(c(valRef,valOrg), ~ if_else(round(px)==1&pxFactor==.01,as.numeric(.x/pxFactor),as.numeric(.x))))  %>% 
        dplyr::mutate(bought=if_else(isInv,abs(qty),0),sold=if_else(isCls,abs(qty),0))
      
      if(any(isMrg)) {
        
        ledger <- ledger %>% 
          dplyr::group_by(date) %>% 
          dplyr::mutate(splitFactor=if_else(!is.na(quantType)&stringr::str_detect(quantType,"split"),
                                            max(if_else(sign(qty)==-1,as.numeric(abs(qty)),1))/max(if_else(sign(qty)==1,as.numeric(abs(qty)),1)),
                                            as.numeric(NA)
          ),
          splitFactor=if_else(row_number()==1|is.na(splitFactor),as.numeric(splitFactor),1)
          ) %>% 
          ungroup() %>% 
          fill(splitFactor,.direction="down") %>% 
          dplyr::mutate(splitFactor=cumprod(if_else(is.na(splitFactor),1,splitFactor)),splitFactor=if_else(is.na(splitFactor),1,splitFactor)) %>%
          dplyr::mutate(
            bought=bought*splitFactor,
            sold=sold*splitFactor
          )  %>% 
          select(-splitFactor)
        
      }
      
      ledger <- ledger %>% 
        dplyr::mutate(bought=cumsum(bought),sold=cumsum(sold))
      
      dubious <- if_else(!dubious,ledger[nrow(ledger),]$qty==0&ledger[nrow(ledger),]$sold!=ledger[nrow(ledger),]$bought,dubious)
      
      sales <- abs(sum(ledger[ledger$isCls,]$valRef,na.rm=TRUE))
      
      cost <- abs(sum(ledger[ledger$isInv,]$valRef,na.rm=TRUE))
      
      fctr <- ledger[nrow(ledger),]$bought/ledger[nrow(ledger),]$sold
      
      realized <- (fctr*sales) - cost
      
      ledger[nrow(ledger),]$investment <- cost
      ledger[nrow(ledger),]$investmentOrg <- sum(ledger[ledger$isInv,]$valOrg,na.rm=TRUE)
      
      
      ledger$bought <- NULL
      ledger$sold <- NULL
      
    }
    
  }
  
  
  ledger[1,]$valNetRef <- if_else(is.na(ledger[1,]$valNetRef),as.numeric(ledger[nrow(ledger),]$investment),as.numeric(ledger[1,]$valNetRef))
  
  ledger[nrow(ledger),]$realized <- realized
  
  ledger$dubious <- dubious
  
  return(list("ledger"=ledger,"ledgerOG"=flat$ledger))
  
}

##' Contextualizes the flattened PnL data.
##'
##' This is the description
##'
##' @param flat the list containing clean data frame ledgers.
##' @param excludedTags the tags to exclude from context in the ledger df.
##' @param session the decaf session. Defaults to NULL TODO.
##' @return A list containing the contextualized ledger data frame and the original one.
##' @export
contextEvents__ <- function(flat,excludedTags=c("exclude","pnl"), session=NULL) {
  
  ledger <- flat$ledger %>% 
    dplyr::filter(!tag %in% excludedTags) %>% 
    dplyr::mutate(
      valRef=if_else(type!="BOND"&!is.na(valQty),valQty*if_else(tag %in% c("opening","closing"),qty,abs(qty))*pxlastRef*sign(if_else(tag %in% c("opening","closing"),1,valRef)),valRef) ##for future trade contracts expressed in PNL
      ,valOrg=if_else(type!="BOND"&!is.na(valQty),valQty*if_else(tag %in% c("opening","closing"),qty,abs(qty))*pxlastOrg*sign(if_else(tag %in% c("opening","closing"),1,valOrg)),valOrg)
      ,fees=if_else(!is.na(quantType)&stringr::str_detect(quantType,"fee"),1,0)
      ,income=if_else(fees==0&tag=="income",1,0)
    ) 
  
  isStart <- ledger[, "tag"] == "opening"
  
  isEnd   <- ledger[,"tag"] == "closing"
  isInc   <- ledger[,"tag"] == "income"
  isFee   <- ledger[,"fees"] == 1
  
  isMrg   <- if_else(is.na(ledger[,"quantType"]),FALSE,stringr::str_detect(unlist(ledger[,"quantType"]),"split"))
  
  ledger[, "tqty"] <-cumsum(as.numeric(!isInc) * as.numeric(!isFee) * as.numeric(!isEnd) * ledger[,"qty"])
  
  isCls <- sign(ledger$qty)<0 & !isFee & !isInc & !isMrg &!isEnd

  isInv <- sign(ledger$qty)>=0 & !isFee & !isInc & !isMrg &!isEnd
  
  if (ledger[isStart, "qty"] == 0) {
    isStart[which(isInv)[2]] <- TRUE
    isStart[1] <- FALSE
    isInv[1] <- FALSE
  }
  
  ledger[,"isStart"] <- isStart
  
  ledger[,"isEnd"] <- isEnd
  
  ledger[,"isCls"] <- isCls & !isMrg
  
  
  if (ledger[isEnd, "qty"] == 0) {
    ledger <- ledger %>% 
      dplyr::mutate(isEnd=row_number()==max(if_else(isCls,row_number(),as.integer(0))))
    isEnd <- ledger$isEnd
  }
  
  ledger[,"isInv"] <- isInv & !isCls & !isEnd
  
  ledger$isInc <- isInc
  
  ledger$isFee <- isFee
  
  ledger <- ledger %>% 
    dplyr::mutate(sign=if_else(tag %in% c("opening","closing")|isEnd,1,sign(qty))) %>% 
    dplyr::mutate(
      income=cumsum(if_else(income==1,sign*valRef,0)),
      fees=cumsum(if_else(fees==1,sign*valRef,0)),
      realized=as.numeric(NA)
    )
  
  
  dubious <- FALSE
  
  if(!all(is.na(ledger$px))) {
    
    if(min(ledger$px,na.rm=TRUE)>0) {  
      
      pxmin <- min(ledger$px,na.rm=TRUE)
      pxmax <- max(ledger$px,na.rm=TRUE)
      
      dubious <- if_else(dubious==FALSE,pxmax/pxmin>2,dubious)
      
    }
    
  }
  
  realized <- 0


  if(any(ledger$isCls)) {
    
    test <- ledger[1,]$pxcostRef*ledger[NROW(ledger),]$qty/ledger[nrow(ledger),]$investment
    
    if(!is.na(test)&abs(1-test)<.01) {
      
      cutoff <- min(ledger$date)  
      
      if(any(ledger$tqty==0)) {
        cutoff <- max(ledger[which(ledger$tqty==0),]$date)
      }
      wac <- ledger %>% 
        dplyr::filter(date>=cutoff,isInv) %>% 
        dplyr::mutate(px=if_else(is.na(pxlastRef),as.numeric(px),as.numeric(pxlastRef))) %>% 
        dplyr::mutate(px=px*qty/sum(qty)) %>% 
        dplyr::summarise(px=sum(px)) %>% 
        .[[1]]
      
      ledger[nrow(ledger),]$investment <- wac*ledger[nrow(ledger),]$qty
    }
    
    
    if(round(ledger[nrow(ledger),]$qty)==0) {
      ledger[nrow(ledger),]$investment <- 0
      if(unique(ledger$type)=="DEPO") {
        ledger[nrow(ledger),]$investment <- ledger[ledger$qty>0,] %>% dplyr::filter(date==min(date)) %>% dplyr::select(valRef) %>% unique() %>% as.numeric()
      }
    }

    realized <- ledger[nrow(ledger),]$investment +
      sum(ledger[ledger$isCls,]$valRef) - ##add back sales
      sum(ledger[ledger$isInv,]$valRef)  ##subtract buys

    if(unique(ledger$type)=="DEPO") {realized <- 0}

    if(!is.null(session)&ledger[nrow(ledger),]$type!="CCY"&abs(ledger[nrow(ledger),]$qty)>0&NROW(ledger[!ledger$isStart&ledger$isCls&!ledger$isEnd,])>0) {

    ##instrument <- as.numeric(unique(ledger$id))
    instrument <- unique(ledger$symbol)
    dates      <- as.Date(ledger[!ledger$isStart&ledger$isCls&!ledger$isEnd,]$date) %>% unique() ##TODO

    end   <- NULL
    start <- NULL

    ledge_ <- ledger %>%
      dplyr::mutate(date=as.Date(date))

    for(i in 1:length(dates)) {

      e <- dates[i]
      s <- ledge_ %>%
        dplyr::filter(date < e) %>%
        dplyr::filter(date==min(date)) %>%
        dplyr::select(date) %>%
        .[[1]]

      if(length(s)==0) {
      s <- start[1]  ##workaround TODO
      }

      start <- c(start,s)
      end   <- c(end,e)

      ledge_ <- ledge_ %>%
        dplyr::filter(date > e)
      
    }

    start <- as.Date(start)
    end   <- as.Date(end)

    params <- list(
      "format"="csv",
      "page_size"=1,
      "series__symbol"=instrument
    )

    realised <- sapply(
      1:length(end), function(x) {

        start_ <- start[x]
        end_   <- end[x]

        params[["date__lte"]] <- start_

        if(is.na(start_)) {print("Date Issue in FIFO computation!!")}

        pxBuy <- as.data.frame(getResource("ohlcobservations", params=params, session=session))$close %>% safeNull()
        qtySld <- abs(ledger[ledger$date==end_,]$qty) %>% safeNull()
        value <- abs(ledger[ledger$date==end_,]$valRef) %>% safeNull()

        value - (qtySld * pxBuy)


      }
    )


    realized <- sum(realised,na.rm=TRUE)

    params[["date__lte"]] <- start[1]
    pxInv <- as.data.frame(getResource("ohlcobservations", params=params, session=session))$close %>% safeNull()

    ledger[nrow(ledger),]$investment <- pxInv * ledger[nrow(ledger),]$qty %>% safeNull()
      
      # # ledger[nrow(ledger),]$investment <- cost
      # # ledger[nrow(ledger),]$investmentOrg <- sum(ledger[ledger$isInv,]$valOrg,na.rm=TRUE)
      
    }
    
  }
  
  
  ledger[1,]$valNetRef <- if_else(is.na(ledger[1,]$valNetRef),as.numeric(ledger[nrow(ledger),]$investment),as.numeric(ledger[1,]$valNetRef))
  
  ledger[nrow(ledger),]$realized <- realized
  
  ledger$dubious <- dubious
  
  return(list("ledger"=ledger,"ledgerOG"=flat$ledger))
  
}

##' Contextualizes the flattened PnL data.
##'
##' This is the description
##'
##' @param flat the list containing clean data frame ledgers.
##' @param excludedTags the tags to exclude from context in the ledger df.
##' @param session the decaf session. Defaults to NULL TODO.
##' @return A list containing the contextualized ledger data frame and the original one.
##' @export
contextEvents <- function(flat,excludedTags=c("exclude","pnl"), session=NULL) {
  
  ledger <- flat$ledger %>% 
    dplyr::filter(!tag %in% excludedTags) %>% 
    dplyr::mutate(
      valRef=if_else(type!="BOND"&!is.na(valQty),valQty*if_else(tag %in% c("opening","closing"),qty,abs(qty))*pxlastRef*sign(if_else(tag %in% c("opening","closing"),1,valRef)),valRef) ##for future trade contracts expressed in PNL
      ,valOrg=if_else(type!="BOND"&!is.na(valQty),valQty*if_else(tag %in% c("opening","closing"),qty,abs(qty))*pxlastOrg*sign(if_else(tag %in% c("opening","closing"),1,valOrg)),valOrg)
      ,fees=if_else(!is.na(quantType)&stringr::str_detect(quantType,"fee"),1,0)
      ,income=if_else(fees==0&tag=="income",1,0)
    ) 
  
  isStart <- ledger[, "tag"] == "opening"
  
  isEnd   <- ledger[,"tag"] == "closing"
  isInc   <- ledger[,"tag"] == "income"
  isFee   <- ledger[,"fees"] == 1
  
  isMrg   <- if_else(is.na(ledger[,"quantType"]),FALSE,stringr::str_detect(unlist(ledger[,"quantType"]),"split"))
  
  ledger[, "tqty"] <-cumsum(as.numeric(!isInc) * as.numeric(!isFee) * as.numeric(!isEnd) * ledger[,"qty"])
  
  isCls <- sign(ledger$qty)<0 & !isFee & !isInc & !isMrg &!isEnd

  isInv <- sign(ledger$qty)>=0 & !isFee & !isInc & !isMrg &!isEnd
  
  if (ledger[isStart, "qty"] == 0) {
    isStart[which(isInv)[2]] <- TRUE
    isStart[1] <- FALSE
    isInv[1] <- FALSE
  }
  
  ledger[,"isStart"] <- isStart
  
  ledger[,"isEnd"] <- isEnd
  
  ledger[,"isCls"] <- isCls & !isMrg
  
  
  if (ledger[isEnd, "qty"] == 0) {
    ledger <- ledger %>% 
      dplyr::mutate(isEnd=row_number()==max(if_else(isCls,row_number(),as.integer(0))))
    isEnd <- ledger$isEnd
  }
  
  ledger[,"isInv"] <- isInv & !isCls & !isEnd
  
  ledger$isInc <- isInc
  
  ledger$isFee <- isFee
  
  ledger <- ledger %>% 
    dplyr::mutate(sign=if_else(tag %in% c("opening","closing")|isEnd,1,sign(qty))) %>% 
    dplyr::mutate(
      income=cumsum(if_else(income==1,sign*valRef,0)),
      fees=cumsum(if_else(fees==1,sign*valRef,0)),
      realized=as.numeric(NA)
    )
  
  
  dubious <- FALSE
  
  if(!all(is.na(ledger$px))) {
    
    if(min(ledger$px,na.rm=TRUE)>0) {  
      
      pxmin <- min(ledger$px,na.rm=TRUE)
      pxmax <- max(ledger$px,na.rm=TRUE)
      
      dubious <- if_else(dubious==FALSE,pxmax/pxmin>2,dubious)
      
    }
    
  }
  
  realized <- 0
  investment <- NULL

  if(any(ledger$isCls)) {
    
    test <- ledger[1,]$pxcostRef*ledger[NROW(ledger),]$qty/ledger[nrow(ledger),]$investment
    
    if(!is.na(test)&abs(1-test)<.01) {
      
      cutoff <- min(ledger$date)  
      
      if(any(ledger$tqty==0)) {
        cutoff <- max(ledger[which(ledger$tqty==0),]$date)
      }
      wac <- ledger %>% 
        dplyr::filter(date>=cutoff,isInv) %>% 
        dplyr::mutate(px=if_else(is.na(pxlastRef),as.numeric(px),as.numeric(pxlastRef))) %>% 
        dplyr::mutate(px=px*qty/sum(qty)) %>% 
        dplyr::summarise(px=sum(px)) %>% 
        .[[1]]
      
      ledger[nrow(ledger),]$investment <- wac*ledger[nrow(ledger),]$qty
    }
    
    
    if(round(ledger[nrow(ledger),]$qty)==0) {
      ledger[nrow(ledger),]$investment <- 0
      if(unique(ledger$type)=="DEPO") {
        ledger[nrow(ledger),]$investment <- ledger[ledger$qty>0,] %>% dplyr::filter(date==min(date)) %>% dplyr::select(valRef) %>% unique() %>% as.numeric()
      }
    }

    realized <- ledger[nrow(ledger),]$investment +
      sum(ledger[ledger$isCls,]$valRef) - ##add back sales
      sum(ledger[ledger$isInv,]$valRef)  ##subtract buys

    if(unique(ledger$type)=="DEPO") {realized <- 0}

    if(!is.null(session)&ledger[nrow(ledger),]$type!="CCY"&abs(ledger[nrow(ledger),]$qty)>0&NROW(ledger[!ledger$isStart&ledger$isCls&!ledger$isEnd,])>0) {

    buyNsell <- ledger %>%
      dplyr::filter(!isEnd,isInv|isCls) %>% 
      dplyr::rename(sold=isCls) %>%
      dplyr::select(symbol,tag,currency,date,qty,px,valRef,sold) %>%
      dplyr::mutate(px=round(px,4)) %>%
      dplyr::group_by(symbol,tag,currency,date,px,sold) %>%
      dplyr::summarise(qty=sum(qty),valRef=sum(valRef)) %>%
      dplyr::ungroup() %>%
      as.data.frame()

    fifo <- buyNsell
    fifo$cogs <- as.numeric(NA)

    for (i in 1:sum(as.numeric(buyNsell$sold))) {

      sellDate <- as.Date(fifo[fifo$sold,][i,]$date)
      sellPrice <- as.numeric(fifo[fifo$sold,][i,]$px)

      qtySold  <- fifo %>% dplyr::filter(sold,date==sellDate,is.na(cogs)) %>% dplyr::filter(row_number()==1) %>% dplyr::select(qty) %>% as.numeric() * -1

      cogs <- fifo %>%
        dplyr::filter(date<=sellDate,!sold)

      qty <- qtySold
      valSold <- 0

      ##if(is.na(qty)) {browser()}

      while(qty>0) {

      for (j in 1:NROW(cogs)) {

        invntry <- cogs[j,]$qty
        pxBuy <- cogs[j,]$px
        invntryUsed <- min(qty,invntry)

        valSold <- valSold + invntryUsed * pxBuy
        invntry <- max(0,invntry-invntryUsed,na.rm=TRUE)

        cogs[j,]$qty <- invntry

        qty <- max(0,qty-invntryUsed,na.rm=TRUE)

        if(j==NROW(cogs)) {qty <- 0} ##avoid infinite loop

      }

      fifo <- fifo %>%
        dplyr::left_join(cogs %>% dplyr::select(date,sold,qty,px) %>% dplyr::rename(qtyRmn=qty),by=c("date","sold","px")) %>%
        dplyr::mutate(qty=dplyr::if_else(is.na(qtyRmn),qty,qtyRmn),cogs=dplyr::if_else(sold&date==sellDate&px==sellPrice,valSold,cogs)) %>%
        dplyr::select(-qtyRmn)

      }

    }

    realized <- sum(fifo[fifo$sold,]$valRef-fifo[fifo$sold,]$cogs,na.rm=TRUE)

    investment <- sum(fifo[!fifo$sold,]$qty*fifo[!fifo$sold,]$px)

    if(is.na(investment)) {
      investment <- dplyr::if_else(is.na(ledger[nrow(ledger),]$pxcostRef),as.numeric(ledger[isStart,]$px),as.numeric(ledger[nrow(ledger),]$pxcostRef)) * ledger[nrow(ledger),]$qty %>% safeNull()

    }

    ledger[nrow(ledger),]$investment <- investment
      
    }
    
  }

  if(is.null(investment)) {
    ##investment <- sum(ledger[ledger$isInv,]$valRef)
    investment <- sum(ledger[ledger$isInv,]$qty*ledger[ledger$isInv,]$px)
    if(is.na(investment)) {
    investment <- sum(ledger[ledger$isInv,]$valRef)
    }
    if(!is.na(investment)) {
      ledger[nrow(ledger),]$investment <- investment
    }
  }
  
  ledger[1,]$valNetRef <- if_else(is.na(ledger[1,]$valNetRef),as.numeric(ledger[nrow(ledger),]$investment),as.numeric(ledger[1,]$valNetRef))
  
  ledger[nrow(ledger),]$realized <- realized
  
  ledger$dubious <- dubious
  
  return(list("ledger"=ledger,"ledgerOG"=flat$ledger))
  
}


##' Computes the PnL from the contextualized data.
##'
##' This is the description
##'
##' @param context the list containing contextualized data frame ledgers.
##' @return A list containing the ledger artifact info, the clean ledger data, the summarised one liner data, a dubious flag, and the original ledger data.
##' @export
computeEvents <- function(context) {
  
  ledger <- context$ledger 
  
  netValue <- replace_na(ledger[nrow(ledger),]$valNetRef,0)

  investment <- ledger[1,]$valNetRef + if_else(abs(ledger[ledger$tag=="opening",]$qty)>0,sum(ledger[(ledger$isInv&!ledger$isStart)|ledger$isCls,]$valRef*ledger[(ledger$isInv&!ledger$isStart)|ledger$isCls,]$sign,na.rm=TRUE),0) 

  if(!is.na(ledger[NROW(ledger),]$investment)) {
    investment <- ledger[NROW(ledger),]$investment
  }

  unrealized <- netValue - investment
  
  investment <- investment * if_else(ledger[1,]$type=="LOAN",-1,1)
  
  realized <- ledger[nrow(ledger),]$realized * if_else(ledger[1,]$type=="LOAN",-1,1)
  
  ledger[NROW(ledger),]$pxcostRef <- if_else(is.na(ledger[1,]$pxlastRef),as.numeric(ledger[NROW(ledger),]$pxcostRef),as.numeric(ledger[1,]$pxlastRef)) ##TODO
  
  if(!is.na(ledger[NROW(ledger),]$pxcostRef)&ledger[NROW(ledger),]$pxcostRef>0&NROW(ledger[ledger$isInv,])==1&ledger[1,]$type!="LOAN") {
    investment <- (ledger[NROW(ledger),]$pxcostRef*ledger[NROW(ledger),]$qty*
                     if_else(is.na(ledger[NROW(ledger),]$valQty),1,as.numeric(ledger[NROW(ledger),]$valQty)))
    
    unrealized <- ledger[nrow(ledger),]$valRef - investment
  }
  
  if(ledger[1,]$type=="FXFWD"&abs(ledger[NROW(ledger),]$qty)>0) {
    unrealized <- context$ledgerOG[NROW(context$ledgerOG),]$valRef
  }
  
  if(ledger[1,]$type=="DEPO"&abs(ledger[NROW(ledger),]$qty)>0&!is.na(context$ledgerOG[NROW(context$ledgerOG),]$investment)) {
    unrealized <- context$ledgerOG[NROW(context$ledgerOG),]$valRef-context$ledgerOG[NROW(context$ledgerOG),]$investment
  }
  
  income <- ledger[nrow(ledger),]$income
  
  fees <- ledger[nrow(ledger),]$fees
  
  if(ledger[1,]$type=="CCY")
  {
    income <- 0
    fees <- 0
    unrealized <- 0
    realized <- 0 
  }
  
  if(ledger[nrow(ledger),]$qty==0) {
    unrealized <- 0
  }
 
  if(round(ledger[nrow(ledger),]$qty)==0) {
    unrealized <- 0
  }
  
  totalReturn <- replace_na(realized,0) + replace_na(unrealized,0) + income + fees

  # # print(paste(beautify(replace_na(netValue,0)),"net value"))
  # # print(paste(beautify(replace_na(investment,0)),"investment"))
  # # print(paste(beautify(replace_na(unrealized,0)),"unrealized"))
  # # print(replace_na(unrealized,0)/investment)

  inv2 <- investment
  inv3 <- ledger[NROW(ledger),]$investmentOrg * if_else(ledger[1,]$type=="LOAN",-1,1)

  dat <- data.frame(
    startingNAV=safeNull(ledger[1,]$valRef)
  , startingNAVOrg=safeNull(ledger[1,]$valOrg)
  , capGainsRealized=safeNull(realized)
  , capGainsUnrealized=safeNull(unrealized)
  , pnl=safeNull(totalReturn)
  , account=safeNull(ledger[NROW(ledger),]$nameAccount)
  , instrument=safeNull(ledger[NROW(ledger),]$typeDetail)
  , endingNAV=safeNull(ledger[NROW(ledger),]$valRef)
  , endingNAVOrg=safeNull(ledger[NROW(ledger),]$valOrg)
  , endingQty=safeNull(ledger[NROW(ledger),]$qty)
  , exposureAbs=safeNull(ledger[NROW(ledger),]$expAbsRef)
  , exposureNet=safeNull(ledger[NROW(ledger),]$expNetRef)
  , exposureNetOrg=safeNull(ledger[NROW(ledger),]$expNetOrg)
  , gav=safeNull(ledger[NROW(ledger),]$valAbsRef)
  , nav=safeNull(ledger[NROW(ledger),]$valNetRef)
  , instrumentID=safeNull(ledger[NROW(ledger),]$id)
  , symbol=safeNull(ledger[NROW(ledger),]$symbol)
  , type=safeNull(ledger[NROW(ledger),]$type)
  , name=safeNull(ledger[NROW(ledger),]$name)
  , assetClass=safeNull(ledger[NROW(ledger),]$assetClass)
  , country=safeNull(ledger[NROW(ledger),]$country)
  , sector=safeNull(ledger[NROW(ledger),]$sector)
  , issuer=safeNull(ledger[NROW(ledger),]$issuer)
  , investment=safeNull(inv2)##ledger[NROW(ledger),]$investment
  , investmentOrg=safeNull(inv3)##ledger[NROW(ledger),]$investmentOrg
  , income=safeNull(ledger[NROW(ledger),]$income)
  , fees=safeNull(ledger[NROW(ledger),]$fees)
  , dubious=safeNull(ledger[NROW(ledger),]$dubious)
  , exclude=FALSE
  , currency=safeNull(ledger[NROW(ledger),]$currency)
  ,stringsAsFactors = FALSE)
  
  
  dat <- dat %>% 
    dplyr::mutate(startingNAVSynth=startingNAV,
                  startingNAVOrgSynth=startingNAVOrg,
                  endingNAVSynth=endingNAV,
                  endingNAVOrgSynth=endingNAVOrg,
                  exposureNetSynth=exposureNet,
                  exposureNetOrgSynth=exposureNetOrg
    )
  
  
  if(max(dat$startingNAV,0,na.rm=TRUE)==0|max(dat$endingNAV,0,na.rm=TRUE)==0|max(dat$exposureNet,0,na.rm=TRUE)) {
    
    sVals <- ledger %>% dplyr::filter(abs(qty)>0) %>% slice(1:1) %>% dplyr::mutate(valRef=sign*valRef,valOrg=sign*valOrg)
    if(NROW(sVals)==0) {sVals <- data.frame(valRef=0,valOrg=0)}
    eVals <- ledger %>% dplyr::filter(abs(qty)>0) %>% slice(NROW(.):NROW(.)) %>% dplyr::mutate(valRef=sign*valRef,valOrg=sign*valOrg)
    if(NROW(eVals)==0) {eVals <- data.frame(valRef=0,valOrg=0)}
    
    dat <- dat %>% 
      dplyr::mutate(startingNAVSynth=if_else(startingNAV==0,as.numeric(sVals$valRef),as.numeric(startingNAV)),
                    startingNAVOrgSynth=if_else(startingNAVOrg==0,as.numeric(sVals$valOrg),as.numeric(startingNAVOrg)),
                    endingNAVSynth=if_else(endingNAV==0,as.numeric(eVals$valRef),as.numeric(endingNAV)),
                    endingNAVOrgSynth=if_else(endingNAVOrg==0,as.numeric(eVals$valOrg),as.numeric(endingNAVOrg)),
                    exposureNetSynth=if_else(is.na(exposureNet),as.numeric(sVals$valRef),as.numeric(exposureNet)),
                    exposureNetOrgSynth=if_else(is.na(exposureNetOrg),as.numeric(sVals$valOrg),as.numeric(exposureNetOrg))
      )
  }
  
  dat <- dat %>% 
    dplyr::mutate(##investment=if_else(type %in% c("FXFWD","FUT"),as.numeric(exposureNetSynth),as.numeric(startingNAVSynth)),
                  investmentOrg=case_when(
                    type %in% c("FXFWD","FUT") ~ as.numeric(exposureNetOrgSynth),
                    type == "LOAN" ~ as.numeric(startingNAVOrgSynth)*-1,
                    !type %in% c("FXFWD","FUT","LOAN") ~ as.numeric(startingNAVOrgSynth)
                    ),
                  return=pnl/investment 
    ) %>% 
    dplyr::mutate(fxImpact=(endingNAVSynth/endingNAVOrgSynth-startingNAVSynth/startingNAVOrgSynth)*investmentOrg,
                  fxImpact=if_else(is.nan(fxImpact),as.numeric(NA),fxImpact)
    ) %>% 
    select(-contains("Synth")
    )
  
  dat <-dat %>%
    dplyr::mutate(
      assetClass=if_else(is.na(assetClass),"Undefined",as.character(assetClass)),
      aClass1=stringr::str_split(assetClass,"\\|")[[1]][1],
      aClass2=stringr::str_split(assetClass,"\\|")[[1]][2],
      aClass3=stringr::str_split(assetClass,"\\|")[[1]][3],
      aClass4=stringr::str_split(assetClass,"\\|")[[1]][4],
      aClass5=stringr::str_split(assetClass,"\\|")[[1]][5],
      aClass6=stringr::str_split(assetClass,"\\|")[[1]][6]
    ) 
  
  dubious <- unique(ledger$dubious)
  
  ## Make list and return:
  list(  "artifact"=as.list(as.data.frame(ledger[nrow(ledger),1:6],stringsAsFactors=FALSE))
       , "ledger"=ledger
       , "summary"=dat
       , "dubious"=dubious
       , "ledgerOG"=context$ledgerOG
  )
  
}

##' Wrapper function to return consumable PnL data.
##'
##' This is the description
##'
##' @param pnlst the pnl list of data elements from the return value of bare get.
##' @param res the df containing the asset class elements to overwrite faulty data from getStocksAndAssets.
##' @param session the decaf session. Defaults to NULL TODO.
##' @return A list containing the the granular ledger info from the above and a summarized one liner position data frame of pnl agg info.
##' @export
wrapEvents <- function(pnlst,res,session=NULL) {
  
  ## Flatten the endpoint data:
  aa <- lapply(pnlst, function(grp) flattenEvents(grp))
  ##aa <- aa[which(!sapply(aa, is.null))]
  aa[sapply(aa,is.null)] <- NULL
  
  ##overrides
  AA <- lapply(aa, function(grp) overwriteEvents(grp,res=res))
  
  ## Contextualize the endpoint data
  bb <- lapply(AA, function(grp) contextEvents(grp,session=session))

  ## Compute the PnL
  cc <- lapply(bb, function(grp) computeEvents(grp))
  
  
  ##aggregate summaries portfolio level
  dat <- data.frame() %>% 
    bind_rows(
      lapply(cc, function(x) {
        
        x$summary
      }
      )
    )   
  

  list(
    "granular"=cc,
    "aggSummary"=dat
  )
  
}

##' Compiler fn that returns all elements to construct basic pnl report.
##'
##' This is the description
##'
##' @param portfolio the decaf portfolio id.
##' @param since the start date for the pnl calculation.
##' @param until the end date for the pnl calculation.
##' @param currency the desired currency for the computations.
##' @param session the decaf session.
##' @param res the df containing the asset class elements to overwrite faulty data from getStocksAndAssets.
##' @param cashS the string to identify cash instrument types.
##' @param transfS the string(s) to identify transfer trade types.
##' @param depoS the string to identify deposit instrument types.
##' @param apiURL the npl endpoint API URL string.
##' @param symbol string of the instrument symbol to debug. Defaults to NULL.
##' @param extVal logical whether to overlay external valuation for performance. Defaults to FALSE - needs to be switched TODO.
##' @return A list containing the the granular ledger info  and a summarized one liner position data frame of pnl agg info corrected.
##' @export
pnlReport <- function(portfolio, since, until, currency, session, res, cashS="CCY", transfS="transfer|investment", depoS="DEPO",apiURL="/apis/function/valuation-reports/eventsreport",symbol=NULL,extVal=TRUE) {
    
    ## Get the endpoint data:
    events_report <- bare_get(apiURL,
                              params = list(portfolio=portfolio,
                                            since=since+1, ##bare_get automatically backdates 1 day!!
                                            until=until,
                                            currency=currency,
                                            format = "json"),
                              session=session)
    

    if(length(events_report[["ledgers"]] )==0) {return(NULL)}

    l <- events_report[["ledgers"]]

    if(!is.null(symbol)) {
      l <- l[lapply(l,function(x) x$artifact$symbol)==symbol]
      if(length(l)==0) {
        print("Ledger not found!")
        return(NULL)
      }
    }

    pnl <- wrapEvents(pnlst=l,res=res,session=session)
    print(min(pnl$granular[[1]]$ledgerOG$date,na.rm=TRUE))

    cash <- data.frame() %>% 
        bind_rows(lapply(pnl$granular, function(x) {
            if(x$artifact$type==cashS) {
                return(x$ledger)
            }
            return(NULL)
        }
        )
        )
    
    transfer <- 0

    if (NROW(getDBObject("quants",addParams=list("account__portfolio"=portfolio,"trade__ctype"=30),session))!=0)
    {
    transfer <- getTransferValueAmounts(portfolio=portfolio, since=since, until=until, currency=currency, session=session) 
    ##inception <- getDBObject("portfolios",session=session) %>% dplyr::filter(id==portfolio) %>% dplyr::select(inception) %>% .[[1]] %>% as.Date() ##TODO
    inception <- as.Date(since)
    transfer <- sum(transfer[transfer$commitment!=inception,]$valamt_converted,na.rm=TRUE)
    }
    if(is.na(transfer)) {
        transfer <- 0
    }

    params <- list("portfolios"=portfolio,
                   "start"=as.Date(since),
                   "end"=as.Date(until),
                   "overlayextval"=extVal
                   )

    performance <- rdecaf::getResource("performance", params = params, session = session)

    pnl[["PortfolioPerformance"]] <- unlist(tail(performance[["indexed"]][["data"]], 1)) - 1

    depos <- data.frame() %>% 
        bind_rows(lapply(pnl$granular, function(x) {
            if(x$artifact$type==depoS) {
                return(x$summary)
            }
            return(NULL)
        }
        )
        )


    if(NROW(depos)>0) {

        depo <- depos %>%
          dplyr::filter(endingQty==0) %>%
          dplyr::mutate(pctDerived=sapply(stringr::str_split(name,"@"),function(y) sapply(stringr::str_split(y[2],"%"),function(z) z[1])) %>% trimws() %>% as.numeric()) %>%
          dplyr::mutate(strDerived=sapply(stringr::str_split(name,"%"),function(y) sapply(stringr::str_split(y[2],"/"),function(z) z[1])) %>% trimws() %>% as.Date()) %>%
          dplyr::mutate(endDerived=sapply(stringr::str_split(name,"%"),function(y) sapply(stringr::str_split(y[2],"/"),function(z) z[2])) %>% trimws() %>% as.Date()) %>%
          dplyr::mutate(prdDerived=as.numeric(endDerived-strDerived)) %>%
          dplyr::mutate(prdDerived=dplyr::if_else(is.na(prdDerived),1,prdDerived)) %>%
          dplyr::mutate(pctDerived=pctDerived/100/365 * prdDerived) %>%
          dplyr::mutate(intDerived=pctDerived*investment)

        interest <- getDBObject("trades",addParams=list(
                                                          "accmain__portfolio"=portfolio,
                                                          "commitment__gte"=as.Date(since),
                                                          "commitment__lte"=as.Date(until),
                                                          "resmain__ctype"="CCY"
                                                          ),session=session) 
                                                          
       if(NROW(interest)>0) {
        interest <- interest %>%
          dplyr::filter(tolower(stype) %in% c("payment of interest","interest paid or received")) 
       }
          
        if(NROW(interest)>0) {

          interest <- interest %>%
            dplyr::select(commitment,qtymain,resmain_symbol)

          depo <- depo %>%
            dplyr::group_by(symbol) %>%
            dplyr::left_join(interest,by=c("endDerived"="commitment","currency"="resmain_symbol")) %>% 
            dplyr::mutate(delta=abs(intDerived-qtymain)) %>%
            dplyr::filter(delta==min(delta)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(intDerived=dplyr::if_else(is.na(qtymain),intDerived,qtymain)) %>%
            dplyr::select(-qtymain,-delta)
           
        }

        # # depoTrades <- getDBObject("trades",addParams=list(
        # #                                                   "accmain__portfolio"=portfolio,
        # #                                                   "commitment__gte"=as.Date(since),
        # #                                                   "commitment__lte"=as.Date(until),
        # #                                                   "resmain__ctype"="DEPO"
        # #                                                   ),session=session)
        # # cashTrades <- getDBObject("trades",addParams=list(
        # #                                                   "accmain__portfolio"=portfolio,
        # #                                                   "commitment__gte"=as.Date(since),
        # #                                                   "commitment__lte"=as.Date(until),
        # #                                                   "resmain__ctype"="CCY"
        # #                                                   ),session=session)

        # # interest <- cashTrades %>%
        # #   dplyr::filter(tolower(stype) %in% c("payment of interest","interest paid or received")) %>%
        # #   dplyr::select(commitment,stype,qtymain,resmain_symbol)
        
        smry <-  pnl$aggSummary %>% 
            left_join(depo %>% select(symbol,intDerived),by="symbol") %>% 
            dplyr::mutate(pnl=dplyr::if_else(is.na(intDerived),pnl,intDerived),capGainsRealized=if_else(is.na(intDerived),capGainsRealized,intDerived)) %>% 
            dplyr::mutate(return=pnl/investment) %>% 
            select(-intDerived)
        
        pnl[["aggSummary"]] <- smry

    }
    
    pnl[["aggSummary"]] <- pnl[["aggSummary"]] %>% dplyr::select(-currency)

    pnl$transfer <- transfer
    
    return(pnl)
}


##' Compiler fn that returns all elements to construct RELATIVE pnl report.
##'
##' This is the description
##'
##' @param portfolio the decaf portfolio id.
##' @param until the end date for the pnl relative period calculation.
##' @param session the decaf session.
##' @param symbol string of the instrument symbol to debug. Defaults to NULL.
##' @param ytd logical defining if we should shortcut only for YTD, or other desired period. Defaults to NULL for ALL.
##' @return A list containing the relative period PnL dfs and nav values.
##' @export
compilePnlReport <- function(portfolio, until=Sys.Date(), session, symbol=NULL,ytd=NULL) {
    
    pf <- as.data.frame(getResource("portfolios", params=list("page_size"=-1, "format"="csv", "id"=portfolio), session=session))
    
    currency <- pf$rccy %>% unique()
    
    inception <- min(getDBObject("trades",addParams=list(accmain__portfolio=portfolio),session=session)$commitment,na.rm=TRUE)
    if(is.na(safeNull(inception))) {
      inception <- pf$inception %>% unique() 
    }
    
    pfname <- pf$name %>% unique()
    
    res <- getStocksAndAssets(session=session,portfolio=portfolio,date=until) %>%
        dplyr::mutate(exclude=FALSE)
    
    
    ##to date periods
    seqD <- list(
        "ITD"=inception,
        "YTD"=lubridate::floor_date(as.Date(until),"year")-1,
        "QTD"=lubridate::floor_date(as.Date(until),"quarter")-1,
        "MTD"=lubridate::floor_date(as.Date(until),"month")-1,
        "WTD"=lubridate::floor_date(as.Date(until),"week")-1,
        "DTD"=until
    )

    if(!is.null(ytd)) {
      seqD <- seqD[names(seqD)==ytd]
    }

    
    seqD <- lapply(seqD, function(x) {
        as.Date(max(x,inception))
    })
    
    rs <- data.frame() %>% 
        bind_rows(
            lapply(seq_along(seqD), function(x) {
                
                navBrs <- rdecaf::getResource("fundreport", params=list("fund"=portfolio, ccy=currency, date=seqD[[x]], type="commitment"), session=session)$nav
                pnlRs <- pnlReport(portfolio=portfolio,since=seqD[[x]],until=until,currency=currency,session=session,res=res,symbol=symbol)
                
                cFees <- getDBObject("trades",addParams=list("accmain__portfolio"=portfolio,"remarks__icontains"="custody fee","resmain__ctype"="CCY","commitment__gte"=seqD[[x]],"commitment__lte"=until),session=session) 

            if (NROW(cFees) == 0) {
                cFees <- data.frame("commitment"=seqD[[x]],
                                    "qtymain"=0,
                                    "resmain_symbol"="USD")
            } else {
                cFees <-  cFees %>% dplyr::select(commitment,qtymain,resmain_symbol)
            }

                cFees <- sum(convertValuesByFXRate(ccyFld="resmain_symbol",convertTo=currency,df=cFees,dtFld="commitment",session=session,valFld="qtymain")$qtymain_converted,na.rm=TRUE)

                df <- pnlRs$aggSummary
                if(is.null(df)) {
                    return(NULL)
                }
                df$relPeriod <- safeNull(names(seqD)[[x]])
                df$starting <- safeNull(navBrs+pnlRs$transfer)
                df$performance <- safeNull(pnlRs$PortfolioPerformance)
                df$navBeg <- safeNull(navBrs)
                df$transfer <- safeNull(pnlRs$transfer)
                df$cfees <- safeNull(cFees)
                return(df)
            })
        )

    
    if(NROW(rs)==0) {
        return(NULL)
    }

    PnL <- list("relSeries" = rs)
    
    navL <- rs %>% 
        dplyr::group_by(relPeriod) %>% 
        summarise(nav=max(starting))
    
    navLst <- as.list(navL$nav) 
    names(navLst) <- navL$relPeriod
    
    PnL[["nav"]] <- navLst

    PnL[["relSeries"]] <- PnL[["relSeries"]] %>%  
        dplyr::mutate(portfolio=pfname,currency=currency)

    ##PnL[["EndingNAV"]] <- safeNull(PnL$nav$DTD)
    PnL[["EndingNAV"]] <- rdecaf::getResource("fundreport", params=list("fund"=portfolio, ccy=currency, date=until, type="commitment"), session=session)$nav

    return(PnL)
}

##' Helper fn to clean up the PnL output.
##'
##' This is the description
##'
##' @param df the pnl df, usually from the relative series, e.g. ytd.
##' @param key string of the column used for joining all the dfs. Defaults to symbol.
##' @param sfx string of relative series suffix to add to make the column names unique. Defaults to (itd) for inception to date.
##' @param cols string vector of the column names to add a suffix to. See below.
##' @param exc string of the column name to exclude. Defaults to no exclusions.
##' @return A cleaned-up data frame of pnl relative series.
##' @export
selPnlContrCols_ <- function(df,
                            key="symbol",
                            sfx="(itd)",
                            cols=c("unrealised","realised","income","fees","total","total gross","total net","contr"),
                            exc=NULL
                            ) {

  if(is.null(df)) {
    df <-initDF(c(key,paste0(cols,sfx))) %>%
      dplyr::mutate(across(everything(), ~as.character(.x))) %>%
      dplyr::slice(0:0)
  }

  dat <- df[,!names(df) %in% paste0(exc,sfx)]

  dat <- dat %>%
    dplyr::select(tidyselect::all_of(key),contains(sfx)) %>%
    dplyr::mutate(across(where(is.logical), ~ as.character(.x)))

  return(dat)
    
}

##' Helper fn to clean up the PnL output.
##'
##' This is the description
##'
##' @param df the pnl df, usually from the relative series, e.g. ytd.
##' @param key string of the column used for joining all the dfs. Defaults to symbol.
##' @param sfx string of relative series suffix to add to make the column names unique. Defaults to (itd) for inception to date.
##' @param cols string vector of the column names to add a suffix to. See below.
##' @param exc string of the column name to exclude. Defaults to no exclusions.
##' @return A cleaned-up data frame of pnl relative series.
##' @export
selPnlContrCols <- function(df,
                            key="symbol",
                            sfx="(itd)",
                            cols=c("unrealised","realised","income","fees","total","total gross","total net","contr"),
                            exc=NULL
                            ) {

  if(is.null(df)) {
    df <-initDF(key,paste0(cols,sfx))
  }

  dat <- df[,!names(df) %in% paste0(exc,sfx)]

  dat <- dat %>%
    dplyr::select(tidyselect::all_of(key),contains(sfx)) %>%
    dplyr::mutate(across(where(is.logical), ~ as.character(.x)))

  return(dat)
    
}

##' Compiler fn that returns the data for the pnl contribution excel report.
##'
##' This is the description
##'
##' @param portfolio the decaf portfolio id.
##' @param date the end date for the pnl relative period calculation via compilepnlreport.
##' @param session the decaf session.
##' @param symbol string of the instrument symbol to debug. Defaults to NULL.
##' @return A data frame of the relative period pnls in a consolidated contribution report.
##' @export
pnlContribReport <- function(portfolio, date, session) {
    
    PnL <- compilePnlReport(portfolio=portfolio, until=date, session=session)
    
    if(is.null(PnL)) {
        return(NULL)
    }

    navEnd <- PnL[["EndingNAV"]]

    pnlS <- lapply(seq_along(PnL$nav), function(x) {
        
        pos <- PnL$relSeries %>%
            dplyr::filter(relPeriod==names(PnL$nav)[[x]]) %>% 
            dplyr::mutate(income=if_else(type=="CCY",0,income),
                          assetclass=aClass1,
                          assetclass1=aClass1,
                          assetclass2=aClass2,
                          assetclass3=aClass3,
                          open=if_else(abs(endingQty)>0,"YES","NO"),
                          quantity=endingQty,
                          value=endingNAV,
                          `value(%)`=safeNull(endingNAV/navEnd),
                          unrealised=capGainsUnrealized,
                          realised=capGainsRealized,
                          total=realised+unrealised+income+fees,
                          `total net`=navEnd-(navBeg+transfer),
                          `pnl/inv`=safeNull(total/investment),
                          contr=safeNull(total/starting) ##old incorrect version
                          ) %>%
            dplyr::mutate(`total gross`=`total net`-cfees) %>%
            dplyr::mutate(`pnl/inv`=dplyr::if_else(round(investment)==0,as.numeric(NA),`pnl/inv`))

        totalPnl <- sum(pos$total,na.rm=TRUE)
        totalPnlNet <- unique(pos$`total net`)
        perf <- unique(pos$performance)
        cfees <- unique(pos$cfees)
        curr <- unique(pos$currency)
        trnsfr <- unique(pos$transfer)
        navBeg <- unique(pos$navBeg)
 
        disc <- (totalPnl + cfees) - totalPnlNet

        print(noquote(""))
        print(names(PnL$nav)[[x]])
        print(noquote(""))

        print(paste(curr,beautify(totalPnlNet),"= Beg NAV - (End NAV + Net Transfer) = Net PnL."))
        print(paste(curr,beautify(navBeg),"= Beginning Value (NAV)."))
        print(paste(curr,beautify(navEnd),"= Ending Value (NAV)."))
        print(paste(curr,beautify(trnsfr),"= Net Transfers."))
        print(paste(curr,beautify(totalPnl),"= Sum of Derived Position PnL."))
        print(paste(curr,beautify(cfees),"= Custodian Fees Calculated."))
        print(paste(curr,beautify(disc),"= (Derived PnL + Net Custodian Fees) - Net PnL = Discrepancy",paste0("(",percentify(disc/navEnd),")")))

        print(noquote(""))

        pos <- pos %>%
          dplyr::bind_rows(
            data.frame(portfolio=unique(pos$portfolio),assetclass1="1) DECAF Notes",currency=curr,symbol="2) Discrepancy",name="2) Uncaptured Trade/PX Movements",total=-disc,performance=perf,stringsAsFactors=FALSE)
          ) %>%
          dplyr::bind_rows(
            data.frame(portfolio=unique(pos$portfolio),assetclass1="1) DECAF Notes",currency=curr,symbol="1) NTR Fees",name="1) Non-Trading Related Fees",total=cfees-disc,performance=perf,stringsAsFactors=FALSE) 
          ) %>%
          dplyr::arrange(assetclass1,assetclass2,assetclass3,name)

        pos <- pos %>%
          dplyr::mutate(pnlRatio = total/totalPnlNet) %>%
          dplyr::mutate(pnlRatio=dplyr::if_else(is.finite(pnlRatio),pnlRatio,as.numeric(NA)))
        notional <- pos$quantity

        pos  <- pos %>%
            dplyr::mutate(contr=pnlRatio*performance) %>%
            dplyr::select(-assetClass) %>% 
            dplyr::select(portfolio,name,symbol,currency,contains("assetclass"),open,value,`value(%)`,`pnl/inv`,unrealised,realised,income,fees,total,`total gross`,`total net`,contr,type) %>% 
            dplyr::select(-type)

        bug <- pos[-c(1,2),] %>%
          dplyr::filter(sign(contr)!=sign(total))

        if(NROW(bug)>0) {
           print("Sign MisMatch ... Stop")
           ##browser()
        }
       
        addNames <- paste0(colnames(pos[,(length(pos)-8):length(pos)]),"(",tolower(names(PnL$nav)[[x]]),")")
        
        colnames(pos) <- c(colnames(pos[,1:(length(pos)-9)]),addNames)

        pos$quantity <- notional
        
        return(pos)
      
    }
        )

    names(pnlS) <- names(PnL$nav)

    
    itd <- pnlS[["ITD"]]
    if(is.null(itd)) {
    itd <-initDF(c("portfolio","name","isin","symbol","currency","smbc_class","assetclass","assetclass1","assetclass2","assetclass3","open","quantity","value","value(%)",paste0(c("pnl/inv","unrealised","realised","income","fees","total","total gross","total net","contr"),"(itd)"))) 
    }

    indata <- itd %>% 
        dplyr::filter(!is.na(symbol)) ##TODO

    if(NROW(indata)==0) {
      indata <- pnlS[["YTD"]] %>%
        dplyr::select(c("portfolio","name","symbol","currency","assetclass","assetclass1","assetclass2","assetclass3","open","quantity","value","value(%)"))
        
    }

    indata <- indata %>% 
        dplyr::mutate(symbol=as.character(symbol))

    if(!is.null(pnlS[["YTD"]])) {
    ytd <- selPnlContrCols(df=pnlS[["YTD"]],
                    key="symbol",
                    sfx="(ytd)",
                    cols=c("pnl/inv","unrealised","realised","income","fees","total","total gross","total net","contr"),
                    exc=NULL
                    )

    
    indata <- indata %>%
        dplyr::left_join(ytd, by="symbol")                 

    }

    if(!is.null(pnlS[["QTD"]])) {

    qtd <- selPnlContrCols(df=pnlS[["QTD"]],sfx="(qtd)",exc="pnl/inv")
    indata <- indata %>%
        dplyr::left_join(qtd, by="symbol")  

    }

    if(!is.null(pnlS[["MTD"]])) {

    mtd <- selPnlContrCols(df=pnlS[["MTD"]],sfx="(mtd)",exc="pnl/inv")
    indata <- indata %>%
        dplyr::left_join(mtd, by="symbol")  

    }

    if(!is.null(pnlS[["WTD"]])) {

    wtd <- selPnlContrCols(df=pnlS[["WTD"]],sfx="(wtd)",exc="pnl/inv")
    indata <- indata %>%
        dplyr::left_join(wtd, by="symbol")  

    }
   
    if(!is.null(pnlS[["DTD"]])) {

    dtd <- selPnlContrCols(df=pnlS[["DTD"]],sfx="(dtd)",exc="pnl/inv")
    indata <- indata %>%
        dplyr::left_join(dtd, by="symbol")  

    }

    indata <- indata %>% 
        dplyr::distinct() 
 
    ## Get the resources:
    resources <- safeRbind(lapply(indata[, "symbol"], function(x) getDBObject("resources", session, addParams=list("symbol"=x))), convertToString=TRUE)
    
    ## Add the smbc classifciation:
    smbcClass <- resources[match(indata[, "symbol"], resources[, "symbol"], incomparables=NA), "attributes.smbc_classification"]
    Isin <- resources[match(indata[, "symbol"], resources[, "symbol"], incomparables=NA), "isin"] ##TO DO .. this takes super long
    
    indata$isin <- safeNull(Isin)
    indata$smbc_class <- safeNull(smbcClass)
  
    indata <- bind_cols(indata[,c("portfolio","name","isin","symbol","currency","smbc_class","assetclass","assetclass1","assetclass2","assetclass3","open","value","value(%)")],
                        indata %>% select(contains("itd")),
                        indata %>% select(contains("ytd")),
                        indata %>% select(contains("qtd")),
                        indata %>% select(contains("mtd")),
                        indata %>% select(contains("wtd")),
                        indata %>% select(contains("dtd")),
                        indata %>% select(quantity)
    )
                        
    return(indata)
    
}
