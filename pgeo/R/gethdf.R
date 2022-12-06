#' Retrieve MODIS HDF file
#'
#' These functions retrieve MODIS Aqua and Terra hdf files and save
#' these locally.
#'
#' \code{get_hdf_list} retrieves the URL of all MODIS hdf files from
#' Aqua, Terra, or both and returns them as a \code{data.frame}.  It
#' is possibly to specify a MODIS file version (currently 6, the
#' default) and to narrow down the list by specifying a year interval
#' (argument \code{years}).
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
#' @param years an interval to which hdf file retrieval should be
#'     restricted. Start and end years are provided as
#'     \code{yyyy}.  To retrieve the entire data of year 2010,
#'     one could use \code{years='2010'}.
#'
#' @param version MODIS file version, currently 6 (default).
#'
#' @param server server to use, including base path. defaults to
#'     \code{https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/}.
#'
#' @param product product to retrieve, defaults to \code{13Q1}.
#'
#' @param verbose integer indicating verbosity. Values supported are
#'     0, 1 and 2
#'
#' @param url full url of MODIS file to be downloaded.
#'
#' @param outdir directory where the hdf file is to be stored. This
#'     can be a relative or absolute path.
#'
#' @param file if specifies, \code{file} allows to change the file
#'     name under which the MODIS hdf file is saved.
#'
#' @param appkey App Key for data access, exported rom the LAADS DAAC
#'     site (https://ladsweb.modaps.eosdis.nasa.gov/profile/#app-keys)
#'
#' @param overwrite \code{get_hdf_file} skips the download of files
#'     that already exist on disk, unless \code{overwrite=TRUE}.
#'
#' @param cacheage indicates whether a cache for the URL directories
#'     should be used. Positive values indicate the maximum age in
#'     hours.
#'
#' @return A data frame with columns \code{url}, \code{dir}, and
#'     \code{file}.
#'
#' @examples
#' \dontrun{
#' require(pgeo);
#' h <- get_hdf_list(yearss=c("2010","2015"))
#' get_hdf_files(h$url, addkey='xxxx')
#' }
#'
#' @rdname gethdf
#' @importFrom utils head
#' @importFrom utils tail
#'
#' @export
get_hdf_dirs <-
    function(
             sat=c("aqua","terra"),
             years=c("0000","9999"),
             version=6,
             product="13Q1",
             server="https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/",
             verbose=1,
             cacheage=24)
{
    ## helper with negative indiced for substring 'backwards'
    myss <- function(x, start, stop) {
        x <- as.character(x)
        if (start > 0)
            substr(x, start, stop)
        else {
            n <- nchar(x)
            substr(x,n+start, n+stop)
        }
    }

    srvr <- gsub("(?<!/)/[^/].+","",server,perl=TRUE)
    years <- c(if(is.na(head(years, 1))) "0000" else head(years,1),
               if(is.na(tail(years, 1))) "9999" else tail(years,1))
    years <- sprintf("%04d", as.numeric(as.character(years)))

    ## get top directories for each year
    topdirs <-
        unlist(
            lapply(
                sat,
                function(sat) {
                    isAqua <-  grepl("^[Aa]", sat, perl=TRUE)
                    MODISurl <- sprintf("%s/%d/%s%s",
                                        server,
                                        version,
                                        if(isAqua) "MYD" else "MOD",
                                        product)
                    while (TRUE) {
                        if(verbose > 0)
                            cat("  Retrieving ",MODISurl,"...\n")
                        r <- try(readLinesCached(MODISurl, maxage=cacheage))
                        if (class(r) != "try-error")
                            break
                        warning("      *** Error retrieving",
                                MODISurl,
                                "... retrying after 10 minute...\n")
                        Sys.sleep(600)
                    }

                    ## filter out lines with links to subdirectories
                    r <- gsub("^.*href=\\\"([^\\\"]+)\\\".*$","\\1",r);
                    idx <- grepl(sprintf("%s/[0-9]{4}/$",product), r, perl=TRUE);
                    r <- r[idx]

                    r <- Filter(
                        function(i) {
                            yr <- myss(i, -4, -1)
                            yr >= myss(years[1],1,4) & yr <= myss(years[2],1,4)
                        },
                        r)
                }))

    ## scan all top-level directories and extract subdirectories
    unlist(
        lapply(topdirs,
               function(d) {
                   if(verbose > 0)
                       cat("  Inspecting directory '",d,"'...\n",sep="")
                   dirurl <- sprintf("%s%s", srvr, d)
                   while (TRUE) {
                       try(subdirs <- readLinesCached(dirurl, maxage=cacheage))
                       if(class(subdirs) != "try-error")
                           break
                       warning("        *** Error retrieving",
                               dirurl,
                               "... retrying after 1 minute...\n")
                       Sys.sleep(60)
                   }
                   subdirs <- gsub("^.*href=\\\"([^\\\"]+)\\\".*$","\\1", subdirs);
                   idx <- grepl("/\\d{4}/\\d{3}/$", subdirs, perl=TRUE)
                   subdirs[idx]
               }))
}

#' @rdname gethdf
#' @export
get_hdf_list <-
    function(tiles = c("h18v04"),
             sat=c("aqua","terra"),
             years=c("0000","9999"),
             version=6,
             product="13Q1",
             server="https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/",
             verbose=1,
             cacheage=24)
{
    years <- c(if(is.na(head(years, 1))) "0000" else head(years,1),
               if(is.na(tail(years, 1))) "9999" else tail(years,1))
    years <- sprintf("%04d", as.numeric(as.character(years)))

    srvr <- gsub("(?<!/)/[^/].+","",server,perl=TRUE)

    if(verbose > 0)
        cat("Acquiring list of HDF files for tile(s)",
            paste(tiles,collapse=", "),
            "...\n")

    dirs <- get_hdf_dirs(
        sat=sat,
        years=years,
        version=version,
        product=product,
        server=server,
        verbose=verbose,
        cacheage=cacheage)

    hdflist <- NULL

    ## loop over top-level dirs and get hdf files
    urls <-
        unlist(
            sapply(
                dirs,
                function(sd) {
                    if(verbose > 0)
                        cat("  Scanning",sd,"\r")
                    ff <- readLinesCached(sprintf("%s%s", srvr, sd),
                                          maxage = cacheage)
                    fx <- gsub("^.*href=\\\"([^\\\"]+\\.hdf)\\\".*$","\\1", ff);
                    idx <- grepl("/\\d{4}/\\d{3}/.+h\\d{2}v\\d{2}.+hdf",
                                 fx,
                                 perl=TRUE)
                    fx[idx]
                },
                USE.NAMES = FALSE))


    ## extract file name and limit to selected tiles
    files <- gsub("^.+/","", urls, perl=TRUE);
    idx <- grepl(sprintf("^.+(%s)\\.%03d\\..+hdf$",
                         paste(tiles,collapse="|"),
                         version),
                 files,
                 perl=TRUE)
    files <- files[idx]
    urls <- urls[idx]

    ## retrun as data frame
    data.frame(url=sprintf("%s%s",srvr,urls),
               file=files,
               stringsAsFactors=FALSE)
}

#' @rdname gethdf
#' @export
get_hdf_file <- function(url=NULL,
                         file=NULL,
                         outdir=NULL,
                         appkey=NULL,
                         verbose=0,
                         overwrite=FALSE)
{
    if(is.null(file))
        file <- gsub("^.+/", "", url, perl=TRUE)

    for(i in seq_along(url)) {
        fullname <- if(is.null(outdir))
                        file[i]
                    else
                        paste(outdir, file[i], sep="/")
        if(overwrite || !file.exists(fullname)) {
            cmd <- sprintf("curl %s -H 'Authorization: Bearer %s' -o '%s' -O '%s'",
                           if(verbose > 1) "-v" else "",
                           appkey,
                           fullname,
                           url[i])
            if(verbose > 0)
                cat("Command:",cmd,"\n")
            system(cmd)
        } else
            cat("File",file,"already exists, skipping...\n")
    }
}
