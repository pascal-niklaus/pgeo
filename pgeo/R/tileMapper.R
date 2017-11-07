#' Convert MODIS (line,sample) to (latitude,longitude) and vice versa
#'
#' These functions convert MODIS line/sample coordinates (250, 500 or
#' 1000m pixel size) to latitude/longitude coordinates, assuming a
#' sinusoidal projection.  These calculations are equivalent to the
#' ones offered on-line by the MODLAND tile calculator (see link below).
#'
#' The (latitude,longitude) coordinates refer to the center of the
#' pixel. Note that there are numeric issues at the very location of
#' the poles, but these do not matter because no MODIS data is
#' available for these areas anyway.
#' 
#' @param lat,lon latitude and longitude as separate vectors, or as
#'     data.frame with corresponding column names. The data.frame is
#'     passed as first argument to \code{toMODISxy}, i.e. formally as
#'     parameter \code{lat}.
#'
#' @param gx,gy MODIS global coordinates as separate vectors, or as
#'     data.frame with corresponding column names. The data frame is
#'     passed as first argument to \code{fromMODISxy}, i.e. formally
#'     as argument \code{gx}. The global coordinates are line/sample
#'     data but continuous across tiles.
#'
#' @param grid MODIS
#'     grid size (250, 500, or 1000; defaults to 250).
#'
#' @return Returns a data.frame with MODIS coordinates (including tile
#'     number and sample/line within tile) or latitude/longitude.
#'
#' @examples require(pgeo)
#' ## let's take a test location near Bern, Switzerland
#' lat <- 46.95104
#' lon <- 7.439135
#' r <- toMODISxy(lat,lon)
#' r
#' ##      gx    gy tileh tilev sample line
#' ## 1 88837 20663    18     4   2437 1463
#'
#' fromMODISxy(r)
#' ##        lat      lon
#' ## 1 46.95104 7.439135
#'
#' ## Compare this to the tile mapper output for the forward mapping of this coordinate:
#' ## http://landweb.nascom.nasa.gov/cgi-bin/developer/tilemap.cgi
#' ## Map Projection/Grid: Sinusoidal
#' ## Pixel size: 0.25km
#' ## Mapping type: Tile/image coordinates 
#' ## Forward mapping (geographic coordinates in degrees): latitude, longitude
#' ## [sn q fwd tp] lat 46.951040  long 7.439135  =>  vert tile 4  horiz tile 18  line 1463.00  samp 2437.00
#' @rdname modisTileMapper
#' @seealso \url{https://landweb.modaps.eosdis.nasa.gov/cgi-bin/developer/tilemap.cgi}
#' @export
toMODISxy <- function(lat,lon=NULL, grid=250)
{
    if(is.null(lon)) {
        lon <- lat$lon
        lat <- lat$lat
    }
    tilesize <- 4800*250/grid
    gy <- ((90 - lat)/180 * 18 * tilesize - .5) 
    gx <- (lon/360*36*tilesize * cos(lat * pi / 180) + 18*tilesize - .5) %% (36*tilesize)    
    data.frame(gx = gx,
               gy = gy,
               tileh = floor(gx/tilesize),
               tilev = floor(gy/tilesize),
               sample = gx %% tilesize,
               line = gy %% tilesize)
}

#' @rdname modisTileMapper
#' @export
fromMODISxy <- function(gx, gy=NULL, grid=250)
{
    if(is.null(gy)) {
        gy <- gx$gy
        gx <- gx$gx
    }    
    tilesize <- 4800*250/grid    
    lat <- 90 - ( gy + 0.5 ) * 180 / ( 18 * tilesize )
    lon <- ifelse(
        abs(abs(lat)-90) < 1e-5, # at pole: longitude undefined, set to 0
        0, 
        ( gx + 0.5 - 18 * tilesize ) * 360 / (36 * tilesize * cos( lat * pi / 180 ) ))
    data.frame(lat = lat, lon = lon)
}


