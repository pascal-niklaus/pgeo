#' Retrieve MODIS HDF file
#'
#' These functions retrieve MODIS Aqua and Terra hdf files and save
#' these locally.
#'
#' \code{get_hdf_list} retrieves the URL of all MODIS hdf files from
#' Aqua, Terra, or both and returns them as a \code{data.frame}.  It
#' is possibly to specify a MODIS file version (currently 6, the
#' default) and to narrow down the list by specifying a date interval
#' (argument \code{dates}).
#'
#' \code{get_hdf_file} retrieves on or more HDF files.
#' \code{username} and \code{password} to access the server need to be
#' passed as arguments.  Data retrieval occurs using \code{curl},
#' which is invoked using \code{system}.  Note that username and
#' password are temporarily stored in a local 'netrc' file, which is
#' removed after the hdf file has been downloaded.  This 'netrc' file
#' is readable by the user only (\code{chmod 0600}) and removed after
#' the hdf file has been downloaded.
#'
#' @param tiles the MODIS tiles to download in format \code{h??v??}
#'     (e.g. \code{h18v04})
#'
#' @param sat the MODIS instruments for which data is retrieved,
#'     e.g. \code{aqua} or \code{terra}, or both (default).
#'
#' @param dates a date interval to which hdf file retrieval should be
#'     restricted. Start and end dates are provided as
#'     \code{yyyy.mm.dd}.  To retrieve the entire data of year 2010,
#'     one could use \code{dates=c('2010.00.00','2010.99.99')}.
#'
#' @param version MODIS file version, currently 6 (default).
#'
#' @param server server to use, defaults to \code{http://e4ftl01.cr.usgs.gov}.
#'
#' @param product product to retrieve, defaults to \code{13Q1}.
#'
#' @param verbose logical indication whether progress should be logged
#'     to the console (defaults to \code{TRUE}).
#'
#' @param url full url of MODIS file to be downloaded.
#'
#' @param file if specifies, \code{file} allows to change the file
#'     name under which the MODIS hdf file is saved.
#'
#' @param username,password username and password for hdf file
#'     retrieval.
#'
#' @param overwrite \code{get_hdf_file} skips the download of files
#'     that already exist on disk, unless \code{overwrite=TRUE}.
#'
#' @return A data frame with columns \code{url}, \code{dir}, and
#'     \code{file}.
#'
#' @examples
#' \dontrun{
#' require(pgeo);
#' h <- get_hdf_list(dates=c("2010.00.00","2010.01.14"))
#' get_hdf_files(h$url, username='user', password='secret')
#' }
#'
#' @rdname gethdf
#' @importFrom utils head
#' @importFrom utils tail
#' @export
get_hdf_list <- function(tiles = c("h18v04"),
                       sat=c("aqua","terra"),
                       dates=c("0000.00.00","9999.99.99"),
                       version=6,
                       product="13Q1",
                       server="http://e4ftl01.cr.usgs.gov",
                       verbose=TRUE)
{
    if(verbose)
        cat("Acquiring list of HDF files for tile(s)",
            paste(tiles,collapse=", "),
            "...\n")

    hdflist <- NULL

    dates <- c(if(is.na(head(dates, 1))) "0000.00.00" else head(dates,1),
               if(is.na(tail(dates, 1))) "9999.99.99" else tail(dates,1))

    dates <- gsub("^(\\d{4}).*(\\d{2}).*(\\d{2})$",
                  "\\1.\\2.\\3",
                  dates,
                  perl=TRUE)

    for(sat_ in sat) {
        isAqua <-  grepl("^[Aa]", sat_, perl=TRUE)
        MODISurl <- sprintf("%s/%s/%s%s.%03d/",
                            server,
                            if(isAqua) "MOLA" else "MOLT",
                            if(isAqua) "MYD" else "MOD",
                            product,
                            version)
        if(verbose)
            cat("  Base URL:",MODISurl,"\n")
        r <- readLines(MODISurl);

        ## filter out lines with links to subdirectories
        dirs <- gsub("^.*href=\\\"([^\\\"]+)\\\".*$","\\1",r);
        idx <- grepl("^[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}/?$",dirs,perl=TRUE);
        dirs <- dirs[idx]

        dirs <- Filter(function(i) i>=dates[1] && i<=dates[2], dirs)

        for(d in dirs) {
            if(verbose)
                cat("    Inspecting directory '",d,"'...\n",sep="")
            try(f <- readLines(dirurl<-sprintf("%s%s",MODISurl, d)))
            while(class(f) == "try-error")
                warning("      *** Error retrieving", dirurl, "... retrying...\n");
            files <- gsub("^.*href=\\\"([^\\\"]+)\\\".*$","\\1", f);
            idx <- grepl(sprintf("^.+(%s)\\.%03d\\..+hdf$",
                                 paste(tiles,collapse="|"),
                                 version),
                         files,
                         perl=TRUE)
            files <- files[idx]
            if(verbose)
                for(j in seq_along(files))
                    cat("      FOUND: ",files[j],"\n");
            hdflist <- rbind(hdflist,
                             data.frame(url=sprintf("%s%s",dirurl,files),
                                        dir=d,
                                        file=files))
        }
    }
    hdflist
}

#' @rdname gethdf
#' @export
get_hdf_file <- function(url=NULL,
                         file=NULL,
                         username="user",
                         password="passw",
                         overwrite=FALSE)
{
    if(is.null(file))
        file <- gsub("^.+/", "", url, perl=TRUE)
    system("echo `pwd`")
    system(sprintf("touch .netrc && chmod 0600 .netrc && echo machine urs.earthdata.nasa.gov login %s password %s >.netrc",
                   username,
                   password))
    system("touch .urs_cookies && chmod 0600 .urs_cookies")
    for(i in seq_along(url)) {
        if(overwrite || !file.exists(file)) {
            cat("Retrieving",file,"...\n")
            system(sprintf("curl -o '%s' -O -b .urs_cookies -c .urs_cookies --location --netrc-file .netrc '%s'",
                           file[i],
                           url[i]))
        } else
            cat("File",file,"already exists, skipping...\n")
    }
    unlink(".urs_cookies")
    unlink(".netrc")
}
