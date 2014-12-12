# R package 'pgeo'

## Purpose

This R package contains functions related to broadly ``geographical'' topics,
as opposed to general functions (which are in the package `pascal` and biodiversity-
related function which are in the package `pdiv`)

## Groups of functions 

### Interconversion of WGS-84 and Swiss Grid

These functions convert coordinates between WGS-84
(latitude/longitude) and the Swiss Grid (X:northing/Y:easting).
The conversion is approximate with an accuracy of a few meters,
based on equations provided by SwissTopo. These functions return
the converted coordinates.

- `WGStoCH(lat, lon)`
- `WGStoCHE(lat, lon = NULL)`
- `WGStoCHN(lat, lon = NULL)`
- `CHtoWGS(N, E = NULL)`
- `CHtoWGSlat(N, E = NULL)`
- `CHtoWGSlon(N, E = NULL)`

### Interconversion of MODIS (line,sample) and Swiss Grid

These functions converts MODIS line/sample coordinates (250, 500
or 1000m pixel size) from tile H18V04 to Swiss Grid coordinates in
meters and vice versa. The accuracy of the conversion is within a
few meters.

- `modisIJtoCH(I, J = NULL, grid = 250)`
- `CHtomodisIJ(N, E = NULL, grid = 250)`

### Conversion from delta-permilles to atom-percent

Often, one needs to convert delta-permil data from isotope-ratio
mass spectrometry into atom-percent, i.e. the percentage (or
fraction) an isotopes makes up of a given element. This is crucial
for mass balance calculations, since delta-values are not
proportional to the extent of labelling. The function provided has built-in R 
data for reference material for several isotopes, but the `R` can also
be provided.

- `delta.to.atpct(delta, R = NULL, isotope = "N15", fraction = FALSE)`

## Installation

* download the ready-built package from the [pkgs directory](https://github.com/pascal-niklaus/pgeo/tree/master/pkgs)
* use `install_github`:  
`library(devtools)`  
`install_github("pascal-niklaus/pgeo/pgeo")`


