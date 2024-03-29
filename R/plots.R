##' Provides bar plot.
##'
##' This is the description
##'
##' @param df A data frame with columns 'value' and 'date.
##' @param title The title of the plot.
##' @param colors The color(s).
##' @return A bar plot.
##' @export
barPlot <- function(df, title, colors="darkslategray3") {

    ## If no date, return:
    if (is.null(df)) {
        return(barplot(1,
                       labels="NODATA",
                       border="gray",
                       cex=1,
                       main=colnames(df)[1],
                       cex.main=1.5))
    }

    ## Get the dates:
    date <- as.Date(df[, "date"])

    ## Get the values:
    values <- as.numeric(df[, "values"])

    ## Length out:
    lout <- 10

    ## X-Axis Index:
    axis1Index <- seq(0, length(date), length.out=lout)

    ## X-Axis Labels:
    axis1Labls <- format(as.Date(date[seq(1, length(date), length.out=lout)]), "%b %d, %y")

    ## Y-Axis Index:
    axis2Index <- seq(min(values), max(values), length.out=lout)

    ## X-Axis Labels:
    axis2Labls <- percentify(values[order(values)][seq(1, length(values), length.out=lout)])

    ## Now, run the plot:
    par(mai = c(1.25, 1, 0.75, 0.75))
    barplot(round(values, 4),
            "width"=0.9,
            "main"=title,
            "yaxt"="n", "xaxt"="n", "ylab"="", "xlab"="",
            "col"=colors,
            "border"=NA,
            "cex.main"=2,
            "cex.axis"=1.5)

    ## Add the custom x-axis:
    axis(1, cex.axis=1.3, at=axis1Index, labels=axis1Labls, las=2)

    ## Add the cusotm y-axis:
    axis(2, cex.axis=1.3, at=axis2Index, labels=axis2Labls, las=2)

}



##' Provides a stacked bar chart.
##'
##' This is the description
##'
##' @param df A data frame. df[, 1] == label, df[,2] == value
##' @param barColor The color of the bars.
##' @param labelColor The color of the lables.
##' @param cex The cex of the label and value.
##' @return A stacked bar chart.
##' @export
stackedBarChartTranspose <- function(df, barColor=NA, labelColor=NA, cex=1) {

    ## colors=RColorBrewer::brewer.pal(9, "GnBu")[3:9]) {

    ## If null, return empty pie:
    if (is.null(df)) {
        return(barplot(1,
                       labels="NODATA",
                       col=terrain.colors(1, alpha = 0.8),
                       border="gray",
                       cex=1,
                       main=colnames(df)[1],
                       cex.main=1.5))
    }

    ## If NA value, override:
    df[, 1] <- ifelse(is.na(df[, 1]), "Notavailable", df[,1])
    df[, 2] <- ifelse(is.na(df[, 2]), 0, df[,2])

    ## Aggregate:
    aggs <- aggregate(df[, 2], list(df[, 1]), sum)
    aggs <- aggs[order(aggs[, 2], decreasing=TRUE), ]

    ## Get the slices:
    slices <- aggs[,2]

    ## Get the labels
    if (mean(nchar(aggs[,1])) == 3) {
        lbls <- aggs[,1]
    } else {
        lbls <- capitalise(ellipsify(aggs[,1], 30))
    }

    ## Compute the percentagaes:
    pct <- slices / sum(slices)

    ## Group small values into "Others"
    if (sum(abs(pct) < 0.01) > 1) {
        othersval <- sum(slices[abs(pct) < 0.01])
        slices <- c(slices[!(abs(pct) < 0.01)], othersval)
        lbls <- c(lbls[!(abs(pct) < 0.01)], "Others")
    }

    ## Compute the percentages:
    pct <- as.data.frame(round(slices / sum(slices) * 100))

    labelCount <- NROW(pct)

    ## Assign the row names:
    rownames(pct) <- lbls

    ## Y-Axis Index:
    axis2Index <- seq(min(c(0, pct[, 1])), min(max(pct[, 1] * 2), 100), by=min(max(pct), 10))

    ## X-Axis Labels:
    axis2Labls <- paste0(axis2Index, "%")

    ##xlim <- c(min(axis2Index), min(max(pct[, 1] * 2), 100))
    xlim <- c(max(min(axis2Index),-100), min(max(pct[, 1] * 2), 100))

    ## Plot the stacked bar chart and the legend:
    par(mai = c(0.4, 0.4, 0.4, 0.4))
    mp <- barplot(as.numeric(pct[,1]),
                  col=barColor,
                  border="white",
                  xlab="",
                  ylab=NULL,
                  horiz=TRUE,
                  xlim=xlim,
                  space=0.04,
                  yaxt="n",
                  xaxt="n")
    axis(1, cex.axis=1, at=axis2Index, labels=axis2Labls, las=0.5)

    limXX <- max(pct[, 1]) * 0.05

    text(x=rep(limXX, NROW(pct)),
         y=mp,
         labels=rownames(pct),
         cex=cex,
         font=2,
         adj=0,
         col=labelColor)

    xlim[xlim < 0] <- 0

    text(x=mean(xlim) * 1.15,
         y=mp,
         labels=paste0(pct[, 1], "%"),
         cex=cex,
         adj=0,
         font=2,
         col=labelColor)

    abline(a=0,
           b=0,
           col="black",
           lwd=0.4,
           lty=2,
           h=seq(1.05, NROW(pct) + NROW(pct)*0.05, length.out=NROW(pct)))


}

##' Provides a stacked bar chart.
##'
##' This is the description
##'
##' @param df A data frame. df[, 1] == label, df[,2] == value
##' @param colors The color palette to be used.
##' @return A stacked bar chart.
##' @export
stackedBarChart <- function(df, colors=RColorBrewer::brewer.pal(9, "GnBu")[3:9]) {

    ## If null, return empty pie:
    if (is.null(df)) {
        return(barplot(1,
                       labels="NODATA",
                       col=terrain.colors(1, alpha = 0.8),
                       border="gray",
                       cex=1,
                       main=colnames(df)[1],
                       cex.main=1.5))
    }

    ## Make sure values are numeric:
    df[, 2] <- numerize(df[, 2])

    ## If NA value, override:
    df[, 1] <- ifelse(is.na(df[, 1]), "Notavailable", df[,1])
    df[, 2] <- ifelse(is.na(df[, 2]), 0, df[,2])

    ## Aggregate:
    aggs <- aggregate(df[, 2], list(df[, 1]), sum)
    aggs <- aggs[order(aggs[, 2], decreasing=TRUE), ]

    ## Get the slices:
    slices <- aggs[,2]

    ## Get the labels
    if (mean(nchar(aggs[,1])) == 3) {
        lbls <- aggs[,1]
    } else {
        lbls <- capitalise(ellipsify(aggs[,1], 20))
    }

    ## Compute the percentagaes:
    pct <- slices / sum(slices)

    ## Group small values into "Others"
    if (sum(abs(pct) < 0.01) > 1) {
        othersval <- sum(slices[abs(pct) < 0.01])
        slices <- c(slices[!(abs(pct) < 0.01)], othersval)
        lbls <- c(lbls[!(abs(pct) < 0.01)], "Others")
    }

    ## Compute the percentages:
    pct <- as.data.frame(round(slices / sum(slices) * 100))

    labelCount <- NROW(pct)

    ## Assign the row names:
    rownames(pct) <- lbls

    ## Y-Axis Index:
    axis2Index <- seq(min(c(0, pct[, 1])), min(max(pct[, 1] * 2), 100), by=min(max(pct), 10))

    ## X-Axis Labels:
    axis2Labls <- paste0(axis2Index, "%")

    colorsx <- colors[1:NROW(pct)]

    if (any(is.na(colorsx))) {
        colorsx[is.na(colorsx)] <- rep(colorsx[!is.na(colorsx)], 10)[1:sum(is.na(colors))]
    }

    ## Plot the stacked bar chart and the legend:
    par(mai = c(0.4, 0.4, 0.4, 0.4))
    mp <- barplot(as.numeric(pct[,1]),
            col=adjustcolor(colorsx, alpha.f=0.8),
            border="white",
            xlab="",
            ylab=NULL,
            ylim=c(min(axis2Index), min(max(pct[, 1] * 2), 100)),
            space=0.04,
            yaxt="n",
            xaxt="n")
    axis(2, cex.axis=1, at=axis2Index, labels=axis2Labls, las=0.5)

    text(mp,
         sapply(pct[, 1], function(x) max(1.3, x * 0.90)),
         pos=2,
         offset=c(8, 4, 2, 1, 0.5, 0.025, 0.0012, 0.0001, rep(0, 100))[labelCount],
         labels=paste0(pct[, 1], "%"),
         cex=1,
         font=2,
         col=adjustcolor(colorsx, red.f=0.25, blue.f=0.50, green.f=0.25))

    text(mp,
         #rep(median(axis2Index), NROW(pct)),
         sapply(pct[, 1], function(x) min(x + 3, median(axis2Index))),
         labels=rownames(pct),
         srt=90,
         cex=1,
         ##cex=(1/(log((1:33)*10)+1)*8)[labelCount],
         adj=c(0),
         font=2,
         col="#595959")

    ## legend("topright",
    ##        rownames(pct),
    ##        fill=colors[1:NROW(pct)],
    ##        bg=adjustcolor("white", alpha=0.60),
    ##        cex=1.25,
    ##        pt.cex=1,
    ##        box.lwd=0.01,
    ##        box.lty=0,
    ##        text.col=adjustcolor("black", alpha=0.8))


}

##' Provides an line time series plot.
##'
##' This is the description
##'
##' @param df A data frame with 'value' and 'date' columns.
##' @param smooth The smoothing parameter.
##' @param limitFactor The factor with which to expand the limits.
##' @param title The title of the plot.
##' @param ylab The y-axis name.
##' @param col The color for the polygon.
##' @param lout The number of ticks to display.
##' @return An line time series plot.
##' @export
timeSeriesPlot <- function(df, smooth=0.3, limitFactor=0.1, title, ylab="", col="darkslategray3", lout=10) {

    ## Transform and arrange data frame:
    trans <- timeSeriesTransform(df, smooth, limitFactor=limitFactor)

    ## Get the date:
    date <- trans[["date"]]

    ## Get the value:
    value <- trans[["value"]]

    ## Length out:
    lout <- lout

    ## X-Axis Index:
    axis1Index <- seq(1, length(date), length.out=lout)

    ## X-Axis Labels:
    axis1Labls <- format(as.Date(date[seq(1, length(date), length.out=lout)]), "%b %d, %y")

    ## Y-Axis Index:
    axis2Index <- seq(trans[["limits"]]$lower, trans[["limits"]]$upper, length.out=lout)

    ## X-Axis Labels:
    axis2Labls <- round(axis2Index * 100, 2)

    par(mai = c(1.25, 1, 0.75, 0.75))
    plot(value, main=title, cex.main=2, cex.axis=1.5, lty=4, type="l", lwd=3,
         ylim=c(trans[["limits"]][["lower"]], trans[["limits"]][["upper"]]),
         yaxt="n", xaxt="n", ylab="", xlab="",
         col=col, bty="n")

    ## Add the custom x-axis:
    axis(1, cex.axis=1.3, at=axis1Index, labels=axis1Labls, las=2)

    ## Add the cusotm y-axis:
    axis(2, cex.axis=1.3, at=axis2Index, labels=axis2Labls, las=2)

    ## Place the Y-Label:
    mtext("Index", cex=1.5, font=2, side=2, at=par('usr')[4]*1.0075, las=2)

}

##' Provides a line time series plot, with series for optional benchmarks.
##'
##' This is the description
##'
##' @param df A data frame with columns of all time series values, col1 is the target, 2:n are the benchmarks.
##' @param date A date vector (needs to be same length as the timeseries df) of x axis date values.
##' @param smooth The smoothing parameter.
##' @param limitFactor The factor with which to expand the limits.
##' @param title The title of the plot.
##' @param col The color for the polygon.
##' @param lout The number of ticks to display.
##' @param palette_all An optional vector parameter of colors to assign to addl lines in same order as df e.g. element 1 is the first benchmark color.
##' @param labels An optional vector parameter of legend labels for target and n benchmarks - needs to be same length as df.
##' @param legpos The legend position.
##' @param ylab The y axis header.
##' @param grid A parameter to decide whether to add line grids background.
##' @param ... Option for additional params.
##' @return An line time series plot.
##' @import RColorBrewer
##' @export
mTSPlot <- function(df, date, smooth=.1, limitFactor=.05, title, col="#79CDCD", lout=10, palette_all=NULL, labels=NULL, legpos="topright", ylab="Index", grid=1, ...) {

    ## Create random set of colors, if not provided
    if(is.null(palette_all)) {
    palette_info <- brewer.pal.info[brewer.pal.info$category == "qual", ]  # Extract color info
    palette_all <- unlist(mapply(brewer.pal,                     # Create vector with all colors
                              palette_info$maxcolors,
                              rownames(palette_info)))

    }

    ## create the transformed values into a df
    trans <- lapply(1:NCOL(df), function(i) timeSeriesTransform(data.frame("value"=df[, i], "date"=date), smooth, limitFactor))
    df <- do.call(cbind, lapply(trans, function(x) x[["value"]]))

    ## get default axis limits
    limMax <- max(sapply(trans, function(x) x[["limits"]][["upper"]]))
    limMin <- min(sapply(trans, function(x) x[["limits"]][["lower"]]))

    ## get a vector of colors for the series
    allCols <- sapply(1:NCOL(df), function(i) {

        if (i==1){
            return(col)
        }
        set.seed(1234)
        sample(palette_all[palette_all!=col])[1]

    })

    ## X-Axis Index:
    axis1Index <- seq(1, length(date), length.out=lout)

    ## X-Axis Labels:
    axis1Labls <- format(as.Date(date[seq(1, length(date), length.out=lout)]), "%b %d, %y")

    ## Y-Axis Index:
    axis2Index <- seq(limMin, limMax, length.out=lout)

    ## X-Axis Labels:
    axis2Labls <- round(axis2Index * 100, 2)

    ## Add Legends
    if(is.null(labels) | length(labels) != NCOL(df)) {
        labels <- "Target"
        if(ncol(df)>1)
        labels <- c(labels,paste(rep("Benchmark",ncol(df)-1),c(1:(ncol(df)-1))))
    }

    ## output chart
    par(mai = c(1.25, 1, 0.75, 0.75)) #margins
    plot(df[, 1],
         main=title,
         cex.main=2,
         cex.axis=1.5,
         lty=4,
         type="l",
         lwd=3,
         ylim=c(limMin, limMax),
         yaxt="n",
         xaxt="n",
         ylab="",
         xlab="",
         col=allCols[1],
         bty="n")
    ## add lines for additional series if they exist
    lapply(1:NCOL(df), function(i) {

    if(i==1) {
        return(NULL)
    }

    lines(df[, i],
         col=allCols[i])
    })
    ## Add a legend
    legend(legpos,
           legend=labels,
           col=allCols,
           lty=1,
           box.lty=0,
           cex=1,
           ncol=2
    )
    ## add gridlines
    if (grid==1) {
    grid(col = "#9b6e6e", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
    }
    ## Add the custom x-axis:
    axis(1, cex.axis=1.3, at=axis1Index, labels=axis1Labls, las=2)

    ## Add the cusotm y-axis:
    axis(2, cex.axis=1.3, at=axis2Index, labels=axis2Labls, las=2)

    ## Place the Y-Label:
    mtext(ylab, cex=1.5, font=2, side=2, at=par('usr')[4]*1.0075, las=2)
}



##' Provides an area time series plot.
##'
##' This is the description
##'
##' @param df A data frame with 'value' and 'date' columns.
##' @param smooth The smoothing parameter.
##' @param limitFactor The factor with which to expand the limits.
##' @param title The title of the plot.
##' @param ylab The y-axis name.
##' @param col The color for the polygon.
##' @param ylabFun The transformation function to apply to the ylab values.
##' @return An area time series plot.
##' @export
areaTimeSeriesPlot <- function(df, smooth=0.3, limitFactor=0.1, title, ylab="", col="darkslategray3", ylabFun=NULL) {

    ## Transform and arrange data frame:
    trans <- timeSeriesTransform(df, smooth, limitFactor)

    ## Get the date:
    date <- trans[["date"]]

    ## Get the value:
    value <- trans[["value"]]

    ## Length out:
    lout <- 10

    ## X-Values for Polygon:
    xPolygon <- do.call(c, lapply(1:length(date), function(x) rep(x, 2)))

    ## Y-Values for Polygon:
    yPolygon <- c(c(trans[["limits"]]$lower, value[1]), do.call(c, lapply(value[-1], function(x) rep(x, 2))))
    yPolygon[length(yPolygon)] <- trans[["limits"]]$lower

    ## X-Axis Index:
    axis1Index <- seq(1, length(date), length.out=lout)

    ## X-Axis Labels:
    axis1Labls <- format(as.Date(date[seq(1, length(date), length.out=lout)]), "%b %d, %y")

    ## Y-Axis Index:
    axis2Index <- seq(trans[["limits"]]$lower, trans[["limits"]]$upper, length.out=lout)

    ## X-Axis Labels:
    axis2Labls <- axis2Index

    ## If ylabTransform no null, apply:
    if (!is.null(ylabFun)) {
        axis2Labls <- ylabFun(axis2Labls)
    }

    ## Now, run the plot:
    par(mai = c(1.25, 1.1, 0.75, 0.75))
    plot(value, main=title, cex.main=2, cex.axis=1.5,
         ylim=c(trans[["limits"]][["lower"]], trans[["limits"]][["upper"]]),
         yaxt="n", xaxt="n", ylab="", xlab="",
         col="white", bty="n")

    ## Add the polygon:
    polygon(xPolygon, yPolygon, col=col, border=FALSE)

    ## Add the custom x-axis:
    axis(1, cex.axis=1.3, at=axis1Index, labels=axis1Labls, las=2)

    ## Add the cusotm y-axis:
    axis(2, cex.axis=1.3, at=axis2Index, labels=axis2Labls, las=2)

    ## Place the Y-Label:
    mtext(ylab, cex=1.5, font=2, side=2, at=par('usr')[4]*1.009, las=2)
}


##' This function plots a relative time series plot using the list from getPerformanceV2.
##' It assumes there are benchmark values.
##'
##' This is the description
##'
##' @param performance The performance list from getPerformanceV2.
##' @param primCol The color of the primary time series.
##' @param secdCol The color of the secondary time series.
##' @param portfolioLabel The label of the portfolio. Default is 'Fund'.
##' @return A time series performance plot.
##' @export
relativePerformancePlot <- function(performance, primCol, secdCol, portfolioLabel="Fund") {

    ## :
    hasBenchmark <- !is.null(performance[["benchmark"]][["xts"]])

    ## :
    if (!hasBenchmark) {
        benchmarkXTS <- xts::as.xts(1, order.by=min(zoo::index(performance[["container"]][["xts"]])))
        benchmarkReturns <- xts::as.xts(cbind(0, 0, 0, 0), order.by=min(zoo::index(performance[["container"]][["xts"]])))
        colnames(benchmarkReturns) <- c("raw", "monthly", "yearly", "cumrets")
        performance[["benchmark"]] <- list("xts"=benchmarkXTS, "returns"=benchmarkReturns)
    }

    ## Combine the xts for container and benchmark:
    priceIndex <- cbind("shareclass"=performance[["container"]][["xts"]], "benchmark"=performance[["benchmark"]][["xts"]])

    ## Add 1 to the top:
    priceIndex <- rbind(cbind(xts::as.xts(1, order.by=min(zoo::index(priceIndex)) - 1), 1), priceIndex)

    ## Name the columns:
    colnames(priceIndex) <- c("shareclass", "benchmark")

    ## Replace NA price indices with 1:
    priceIndex[1, is.na(priceIndex[1, ])] <- 1

    ## Multiply the price Index by 100:
    priceIndex <- zoo::na.locf(priceIndex * 100)

    ## Get the yearly returns:
    yearlyRets <- cbind(performance[["container"]][["returns"]][, "yearly"], performance[["benchmark"]][["returns"]][, "yearly"])
    yearlyRets[is.na(yearlyRets)] <- 0
    yrIdx <- which(yearlyRets[, 1] != 0)

    ## yrVal <- as.numeric(yearlyRets[yearlyRets[, 2] != 0, 2])
    yrVal <- as.numeric(xts::apply.yearly(na.omit(diff(log(priceIndex[, 2]))), sum))
    yearlyRets[, 2] <- 0

    if (length(yrVal) == 0) {
        yrVal <- 0
    }

    if (length(yrIdx) == 1 & length(yrVal) == 2) {
        yrVal <- tail(yrVal, 1)
    }

    yearlyRets[yrIdx, 2] <- yrVal

    ## Add 0 to the top:
    yearlyRets <- rbind(cbind(xts::as.xts(0, order.by=min(zoo::index(yearlyRets)) - 1), 0), yearlyRets)

    ## Name the columns:
    colnames(yearlyRets) <- c("shareclass", "benchmark")

    ## Replace NA's with 0:
    yearlyRets[is.na(yearlyRets)] <- 0

    ## Get the dates:
    dates <- zoo::index(yearlyRets)

    ## Get the years:
    years <- substr(dates, 1, 4)

    if (diff(as.numeric(years))[1] == 1 & sum(diff(as.numeric(years))) == 1) {
        years <- years[-1]
    }

    ## Shift the yearly returns to the middle of each yearly time window:
    yearlyRets <- do.call(rbind, lapply(unique(years), function(x) {

        medDate <- median(dates[years == x])
        yrRets <- yearlyRets[years == x, ]

        if (all(substr(zoo::index(yrRets), 6, 12) != "12-31")) {
            return(yrRets)
        }

        rplIdx <- which(as.character(zoo::index(yrRets)) == as.character(medDate))
        rplVal <- as.numeric(yrRets[yrRets[,1] != 0, ])

        if (length(rplVal) == 0) {
            rplVal <- 0
        }

        yrRets[, 1] <- 0
        yrRets[, 2] <- 0
        yrRets[rplIdx, ] <- rplVal

        yrRets
    }))

    ## Combine the yearly returns as numeric:
    xx <- rbind(as.numeric(yearlyRets[, 1]),
                as.numeric(yearlyRets[, 2]))

    ## Get the indices with non-zero values:
    valIdx <- which(xx[1,] != 0)

    ## Get the date:
    date <- zoo::index(priceIndex)

    ## Get the value:
    value <- as.numeric(priceIndex[, 1])

    ## Length out:
    lout <- 10

    ## X-Axis Index:
    ## axis1Index <- seq(1, length(date), length.out=lout)
    axis1Index <- c(1, which(substr(date, 6, 12) == "12-31"), length(dates))

    ## X-Axis Labels:
    ## axis1Labls <- format(as.Date(date[seq(1, length(date), length.out=lout)]), "%b, %Y")
    axis1Labls <- format(as.Date(date[axis1Index]), "%b, %y")

    ## Y-Axis Index:
    axis2Steps <- as.integer(seq(min(priceIndex) * 0.95, max(priceIndex) * 1.05, length.out=lout))
    axis2Index <- seq(nearesth(min(axis2Steps), 2, "down"), max(axis2Steps), 2)

    ## X-Axis Labels:
    axis2Labls <- round(axis2Index, 2)

    ## Start the plot. First determine the margins:
    par(mai = c(0.85, 0.85, 0.25, 0.25))
    plot(value,
         main="",
         cex.main=2,
         cex.axis=1.15,
         lty=1,
         type="l",
         lwd=1.5,
         ylim=c(min(priceIndex) * 0.95, max(priceIndex) * 1.05),
         yaxt="n", xaxt="n", ylab="", xlab="",
         col=primCol,
         bty="n")

    ## Add the benchmark line:
    lines(as.numeric(priceIndex[, 2]), lty=1, lwd=1.5, col=secdCol)

    ## Add the custom x-axis:
    axis(1, cex.axis=0.85, at=axis1Index, labels=axis1Labls, las=2)

    ## Add the cusotm y-axis:
    axis(2, cex.axis=0.85, at=axis2Index, labels=axis2Labls, las=2)

    ## Place the Y-Label:
    mtext("Performance", cex=0.9, font=2, side=2, at=par("usr")[4]*1, las=1)

    ## Add the grid:
    grid(col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)

    ## Add the vertical lines:
    abline(v=axis1Index, lwd=0.4, lty=2)

    ## Add a new plot on top:
    par(new=TRUE)
    mp <- barplot(xx,
                  axes=FALSE,
                  xlab="",
                  ylab="",
                  border=NA,
                  ##space=c(0.04, -1.92),
                  space=c(0.04, -1.9),
                  beside=TRUE,
                  ylim=c(min((yearlyRets+1) * 0.95 - 1), max((yearlyRets+1) * 1.05 - 1)),
                  col=c(adjustcolor(primCol, alpha.f = 0.7),
                        adjustcolor(secdCol, alpha.f = 0.7)))

    ## Parse the labels:
    labels <- gsub(" ", "", (percentify(xx)))
    labels[labels == "0.00%"] <- ""

    xxx <- xx
    xxx[xxx < 0] <- 0

    ## Add the text to the bars:
    ## text(mp, (xx * 0.8) + 0.001, labels=labels, cex=0.80, font=2, col="white")
    text(mp, (xxx + 0.0025), labels=labels, cex=0.825, font=2, col=c(adjustcolor(primCol, alpha.f = 0.9),
                                                                    adjustcolor(secdCol, alpha.f = 0.9)))

    ## Add the horizontal line:
    abline(h=0, lwd=0.4, lty=2)

    if (hasBenchmark) {
        legend <- c(portfolioLabel, "Benchmark", sprintf("%s %s p.a | YTD", portfolioLabel, "%"), "Benchmark % p.a | YTD")
        lwd <- c(2, 2, 0, 0)
        col <- rep(c(adjustcolor(primCol, alpha.f = 0.9), adjustcolor(secdCol, alpha.f = 0.9)), 2)
        pch <- c(NA, NA, 15, 15)
    } else {
        legend <- c(portfolioLabel, sprintf("%s %s p.a | YTD", portfolioLabel, "%"))
        lwd <- c(2, 0)
        col <- rep(adjustcolor(primCol, alpha.f = 0.9), 2)
        pch <- c(NA, 15)
    }

    ## Add the legend:
    legend(2,
           min(c(min((yearlyRets+1) * 0.95 - 1), max((yearlyRets+1) * 1.05 - 1))) * 0.5,
           legend=legend,
           lty=1,
           bg=adjustcolor("gray", alpha.f=0.2),
           box.lty=0,
           lwd=lwd,
           ncol=2,
           col=col,
           pch=pch,
           pt.cex=1.25,
           cex=1)

}


##' This function plots a linear regression plot.
##' It assumes there is an x value that explains a y value (e.g. benchmark values).
##'
##' This is the description
##'
##' @param df the data frame containing the x and y values.
##' @param xval the x value as a string.
##' @param yval the y value as a string.
##' @param col the color of the ponts (usually gradient using centeredColorScale fn).
##' @param scol the smoothing shade color, defaults to grey.
##' @param ttl the title given to the plot as a string.
##' @return A LM plot.
##' @export
smoothPlot <- function(df,xval,yval,col,scol="#444341",ttl) {
    ggplot(df,aes(x=!!sym(xval),y=!!sym(yval)))  +
      geom_point(color=col) +
      geom_smooth(method="lm",color=scol,linetype="dashed") +
      theme_classic() +
      labs(title=ttl) +
      theme(text = element_text(size = 6))
 }


##' This function generates distribution and QQ plots.
##' It assumes there is an x value that explains a y value (e.g. benchmark values).
##'
##' This is the description
##'
##' @param df the data frame containing the x and y values.
##' @param yval the y value as a string.
##' @param cval the categorical color value as a string that segments returns.
##' @param pcol the color of one of the sets of returns.
##' @param ncol the color of the other.
##' @param ttl the title given to the distribution plot.
##' @param ttl2 the title given to the QQ plot.
##' @return A list containing two comparative plots.
##' @export
distPlots <- function(df,yval,cval,pcol="#2C3E50",ncol="#B15928",ttl,ttl2) {

    dplot <- ggplot(df,aes(y=!!sym(yval),color=!!sym(cval),fill=!!sym(cval)))  +
     geom_freqpoly(position="dodge") +
     theme_classic() +
     scale_color_manual(values=c(pcol,ncol)) +
     labs(title=ttl,subtitle=paste(min(df$date),max(df$date),sep=" to "), x="",y="",fill="") +
     coord_flip() +
     theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title=element_blank())

    qplot <- ggplot(df,aes(sample=!!sym(yval),color=!!sym(cval)))  +
     stat_qq() +
     stat_qq_line() +
     theme_classic() +
     scale_color_manual(values=c(pcol,ncol)) +
     labs(title=ttl2,subtitle=paste(min(df$date),max(df$date),sep=" to "), x="Theoretical Return Quartiles",y="Returns")

     return(list("dPlot"=dplot,"qPlot"=qplot))
}


##' This function generates a returns plots given a series.
##'
##' This is the description
##'
##' @param series a series of returns, e.g. weekly.
##' @param ttl the title given to the plot.
##' @return A bar plot with gradient color for positive and negative returns.
##' @export
barRet <- function(series,ttl="Weekly Returns") {
    values <- as.numeric(series[, 1])
    date   <- zoo::index(series)

    if (length(values) < 2) {
        values <- xts::as.xts(0, order.by=as.Date(date))
    }  else {
        values <- xts::apply.weekly(zoo::as.zoo(diff(log(values)), order.by=as.Date(date)), sum)
    }

    values <- values[!is.na(values)&is.finite(values)]
    date   <- zoo::index(values)
    origDate <- date
    values <- as.numeric(values)

    if (length(values) < 2) {
        values <- rep(0, 2)
        date   <- c(origDate, origDate)
    }

    fcol <- centeredColorScale(values)$col

    date <- as.Date(date)

    plot <- ggplot(data.frame("values"=as.numeric(values), "date"=date, stringsAsFactors=FALSE),aes(x=date,y=values))  +
      geom_bar(stat="identity",position="identity",fill=fcol) +
      theme_classic() +
      labs(title=ttl,x="",y='') +
      scale_x_date(breaks=seq(min(date), max(date), length.out=10),
        labels=format(as.Date(date[seq(1, length(date), length.out=10)]), "%b %d, %y")) +
      scale_y_continuous(breaks=seq(min(values), max(values), length.out=10),
        labels=percentify(seq(min(values), max(values), length.out=10))
        ) +
      theme(axis.text.x = element_text(angle = 90, size=15), axis.text.y = element_text(size=15), plot.margin = margin(r = 1.5,unit="cm"),plot.title = element_text(size=20))

    return(plot)
}


##' This function generates a PnL bar chart given a numerical y value (e.g. drawdown) and categorical x value (e.g. month).
##'
##' This is the description
##'
##' @param df the data frame containing the x and y values.
##' @param xval the x value as a string.
##' @param yval the y value as a string.
##' @param fillval the column to use to fill the color of the bars.
##' @param title the title given to the entire plot.
##' @param xtitle the title given to the x axis.
##' @param just the value for the justification of the labels, has a default.
##' @param size the value for the size of the labels, has a default.
##' @return A bar plot.
##' @export
barPnl <- function(df,xval,yval,fillval,title,xtitle,just=-.1,size=3) {
    ggplot(df,aes(x=!!sym(xval),y=!!sym(yval)))  +
      geom_bar(stat="identity",fill=fillval) +
      theme_classic() +
      labs(title=title,x=xtitle,y='') +
      geom_text(aes(label=percentify(!!sym(yval))),vjust=just,size=size)
    }


