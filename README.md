# jradavenport_idl
*General purpose IDL tools written by @jradavenport*

This is a collection of routines that I have created to help make using IDL easier on a daily basis. These include astronomy, plotting, binning/smoothing, and general helper routines. Most have thorough documentation.  


## Other IDL repositories I've authored
- [FBEYE](https://github.com/jradavenport/FBEYE) - an IDL suite for analyzing Kepler light curves and validating flares
- [flaregrid](https://github.com/jradavenport/flare-grid) - Flare colors for SDSS and 2MASS bands for M0-M6 dwarf stars
- [simplephot](https://github.com/jradavenport/simplephot) - a simple routine for time series differential photometry on a stack of images. Can do basic image reduction too. Good for on-the-fly analysis!

## Astronomy Routines
- **astro_const.pro**:  large list of astronomical/physical constants in **cgs units**. Fairly high precision, very useful!
- **brute_match.pro**:  a brute-force 2D nearest neighbor match using a search radius. Good for RA,Dec list matching.
- **ra2id.pro**: convert (ra,dec) in decimal degrees to string with `Jhhmmss.s+ddmmss.s` format. Good for generating object names in tables
- **radec_decimal.pro**: Convert (ra,dec) coordinates to decimal degrees
- **apoexpcal.pro**: A simple exposure time calculator for instruments at Apache Point Observatory

## Plotting Routines
- **contour_plus.pro**:  create contour plots or contour+scatter (for high density data) plots from (x,y) points. Makes many semi-intelligent choices for contour levels, etc. Uses `Histogram` for excellent speed and scalability.
- **cubehelix.pro**:  full implementation of the CubeHelix color map.
- **pixel_contour.pro**:  make a contour plot using pixels
- **posgen.pro**:  position keyword generator, a grid-based framework to create multi-panel plots with varying sizes and positions
- **pixel_plus.pro**:  compute values over third dimension for (x,y) data, such as median
- **luckycharms.pro**:  silly symbol set based on sugary breakfast cereal
- **plotstuff.pro**: set a few system variables for nicer looking plot defaults

## Binning and Smoothing
- **supsmu.pro**:  IDL implementation of SuperSmoother, a variable span smoothing algorithm
- **pdm_sp.pro**:  Phase Dispersion Minimizer using Splines
- **pdm_ss.pro**:  Phase Dispersion Minimizer using SuperSmoother, good to search for periodic signals in data
- **softserve.pro**:  an iterative variable span smoother for light curves... YMMV. Used in FBEYE.
- **medbin.pro**:  in (x,y) data find median values of y in bins of x.
- **todcor.pro**:  Two Dimensional Cross Correlation script, useful for measuring radial velocities using two templates

## Helpful Things & Stats
- **strfunc.pro**:  Compute the "structure function" of a time series or light curve. Uses IDL `Historgam` for speed.
- **randomp.pro**:  generate random numbers from a powerlaw distribution
- **geomean.pro**:  compute geometric mean for an array
- **perror.pro**:  compute asymmetric Poisson error (good for general use)
- **gerror.pro**:  compute symmetric Poisson error (use in a pinch, not as good as perror)
- **mail.pro**:  simple wrapper for using linux `mail` command, send you emails from IDL
- **null99.pro**:  helper script to convert string "null" to -99. Good for reading data from SQL tables, etc (e.g. from IPAC)
- **psym.pro**:  helper script to simply print the default plot symbols when you forget them... which I do every day.
- **real.pro**:  convert NaN and InF (+/-) values in a vector to real floating point numbers, useful to avoid math problems downstream.
