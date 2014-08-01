jradavenport_idl
================

####General purpose IDL tools written by @jradavenport

This is a collection of routines that I have created to help make using IDL easier on a daily basis.


## Astronomy Routines
- **astro_const.pro**:  large list of astronomical/physical constants in *cgs* units.
- **brute_match.pro**:  a brute-force 2D nearest neighbor match using a search radius. Good for RA,Dec list matching.
- **simplephot.pro**:  a stupid (simple) routine for time series differential photometry on a stack of images. Can do basic image reduction too. Good for on-the-fly analysis!

## Plotting Routines
- **contour_plus.pro**:  create contour plots or contour+scatter (for high density data) plots from (x,y) points. Makes many semi-intelligent choices for contour levels, etc. Uses `Histogram` for excellent speed and scalability.
- **cubehelix.pro**:  full implementation of the CubeHelix color map.
- **luckycharms.pro**:  silly symbol set based on sugary breakfast cereal
- **pixel_contour.pro**:  make a contour plot using pixels
- **pixel_plus.pro**:  compute values over third dimension for (x,y) data, such as median
- **plotstuff.pro**: set a few system variables for nicer plots
- **posgen.pro**:  postition keywork generator, a framework to create multi-plots with varying sizes

## Binning and Smoothing
- **supsmu.pro**:  IDL implementation of SuperSmoother, a variable span smoothing algorithm
- **pdm_sp.pro**:  Phase Dispersion Minimizer using Splines
- **pdm_ss.pro**:  Phase Dispersion Minimizer using SuperSmoother
- **softserve.pro**:  a variable span smoother for light curves... YMMV
- **medbin.pro**:  in (x,y) data, find median values of y in bins of x.
- **todcor.pro**:  Two Dimensional Cross Correlation script

## Helpful Things
- **randomp.pro**:  generate random numbers from a powerlaw distribution
- **geomean.pro**:  compute geometric mean for an array
- **perror.pro**:  compute asymmetric Poisson error (good)
- **gerror.pro**:  compute symmetric Poisson error (fair)
- **mail.pro**:  usesing linux `mail` command, send emails from IDL
- **null99.pro**:  helper script to convert string "null" to -99. Good for reading data from SQL tables, etc.
- **psym.pro**:  helper script to simply print the default plot symbols when you forget them... which I do every day.
- **real.pro**:  convert NaN and InF (+/-) values to real numbers to avoid math problems downstream.
