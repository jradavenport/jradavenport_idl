jradavenport_idl
================

General purpose IDL tools written by @jradavenport


This is a collection of routines that I have created to help make using IDL easier on a daily basis. 

## Astronomy Routines
- **astro_const.pro**:  large list of astronomical constants in *cgs* units
- **brute_match.pro**:  a brute-force 2D nearest neighbor match
- **simplephot.pro**:  a stupid (simple) routine for time series differential photometry on a stack of images. Can do image reduction too.

## Plotting Routines
- **contour_plus.pro**:  create contour plots or contour+scatter plots from (x,y) points. 
- **cubehelix.pro**:  full implementation of the CubeHelix color map.
- **luckycharms.pro**:  symbol set based on breakfast cereal
- **pixel_contour.pro**:  make contour plot using pixels
- **pixel_plus.pro**:  compute values over third dimension for (x,y) data, such as median
- **plotstuff.pro**: set system variables for nicer plots  
- **posgen.pro**:  framework to create multiple plots of many sizes using position keyword

## Binning and Smoothing
- **supsmu.pro**:  SuperSmoother, a variable span smoothing algorithm
- **pdm_sp.pro**:  Phase Dispersion Minimizer using Splines
- **pdm_ss.pro**:  Phase Dispersion Minimizer using SuperSmoother
- **softserve.pro**:  a variable span smoother for light curves
- **medbin.pro**:  find median values in (x,y) bins
- **todcor.pro**:  Two Dimensional Cross Correlation script

## Helpful Things
- **randomp.pro**:  generate random numbers from a powerlaw distribution
- **geomean.pro**:  compute geometric mean for an array
- **perror.pro**:  compute asymmetric Poisson error
- **gerror.pro**:  compute symmetric Poisson error
- **mail.pro**:  helper script to send emails from IDL
- **null99.pro**:  helper script to convert "null" to -99
- **psym.pro**:  helper script to simply return the plot symbols when you forget them
- **real.pro**:  convert NaN and InF values to real numbers to avoid problems downstream

