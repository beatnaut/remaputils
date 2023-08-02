##' A function to overwrite variables in dataframe according to an outline.
##'
##' This is the description
##'
##' @param data data frame containing the variable to overwrite, e.g. resources.
##' @param bp list dictating the blueprint of the overwriting function that will be converted into expression. See below.
##' @param var vector of the variable name to be changed, e.g. assetclass.
##' @param sweep logical to dictate whether to overwrite values with existing values or not. Defaults to false.
##' @return data frame with the revised column and an added flag column 'recode' indicating where changes were made.
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

    expr <- sapply(seq_along(bp),function(x) {paste(bp[[x]],names(bp)[x],sep="~")})
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

# # var <- "assetclass"

# # bp <- list(
# # "5" ="ctype=='CCY'",
# # "6" ="ctype %in% c('LOAN', 'DEPO', 'COD')",
# # "13"="ctype=='BOND'",
# # "17"="ctype %in% c('FXFWD', 'FXOPT','DCIP')",
# # "27"="ctype=='INSR'",
# # "11"="ctype=='ELNP'",
# # "9" ="ctype=='SHRE' & safeGrep(trimConcatenate(`attributes.figistype`), 'COMMONSTOCK') == '1'"
# # ) 

