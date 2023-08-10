#' miRBase mature database connector class.
#'
#' Connector class for miRBase mature database.
#'
#' @seealso \code{\link{BiodbConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Get a connector:
#' conn <- mybiodb$getFactory()$createConn('mirbase.mature')
#'
#' # Get the first entry
#' e <- conn$getEntry('MIMAT0000433')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import biodb
#' @import R6
#' @export
MirbaseMatureConn <- R6::R6Class("MirbaseMatureConn",
inherit=biodb::BiodbConn,

public=list(
),

private=list(

doDownload=function() {

    u <- self$getPropValSlot('urls', 'dwnld.url')
    url <- biodb::BiodbUrl$new(url=u)
    sched <- self$getBiodb()$getRequestScheduler()
    sched$downloadFile(url=url, dest.file=self$getDownloadPath())
}

,doExtractDownload=function() {

    # Read file
    fd <- file(self$getDownloadPath(), 'r')
    entries <- readLines(fd)
    close(fd)
    
    # Remove HTML tags
    entries <- gsub('</?p>', '', entries, perl=TRUE)
    entries <- gsub('<br>', '\n', entries, perl=TRUE)
    entries <- gsub('&gt;', ">", entries, perl=TRUE)

    # Convert to list
    lines <- strsplit(entries, "\n")[[1]]

    # Get all entry IDs
    ids <- sub('^.*(MIMAT[0-9]+).*$', '\\1', grep('MIMAT', lines, value=TRUE),
        perl=TRUE)
    logDebug('Found %d entries in file "%s".', length(ids),
        self$getDownloadPath())

    if (length(ids) > 0) {
        # Get contents
        contents <- paste(lines[seq(1, 2*length(ids), 2)],
            lines[seq(2, 2*length(ids), 2)], sep="\n")

        # Write all entries into files
        cch <- self$getBiodb()$getPersistentCache()
        cch$deleteFiles(self$getCacheId(),
            ext=self$getPropertyValue('entry.content.type'))
        cch$saveContentToFile(contents, cache.id=self$getCacheId(),
            name=ids, ext=self$getPropertyValue('entry.content.type'))
    }
}

,doGetEntryIds=function(max.results=NA_integer_) {

    ids <- NULL

    # Download
    self$download()

    # Get IDs from cache
    cch <- self$getBiodb()$getPersistentCache()
    ids <- cch$listFiles(self$getCacheId(),
        ext=self$getPropertyValue('entry.content.type'),
        extract.name=TRUE)

    # Filter out wrong IDs
    ids <- ids[grepl("^MIMAT[0-9]+$", ids, perl=TRUE)]

    return(ids)
}

,doGetEntryContentRequest=function(id, concatenate=TRUE) {
    return(rep(NA_character_, length(id)))
}

,doGetEntryPageUrl=function(id) {

#    url <- c(self$getPropValSlot('urls', 'base.url'), 'mature')
#    v <- vapply(id, function(x) biodb::BiodbUrl$new(url=c(url, x))$toString(),
#                FUN.VALUE='')

    # 2023-08-10 Disabled
    # Pages return "Server Error (500)"
    return(rep(NA_character_, length(id)))

    return(v)
}
))
