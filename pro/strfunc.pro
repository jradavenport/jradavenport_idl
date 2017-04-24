function strfunc,time,mag,log=log,bin=bin,dtime,dmag
;+
; NAME: STRFUNC
;
; PURPOSE: Create the Structure Function of a light curve or other
;          similar time series data.
;
;    STRFUNC.pro uses this equation for the Structure Function:
;          SF(t) = sqrt( 1/N(t) * sum(dMag(t)^2) )
;
;    This is done for all "N-squared" dTime & dMag measurements from
;    the entire light curve. The Structure Function is returned for
;    bins of dTime.
;
; CALLING SEQUENCE: sf = STRFUNC(time, mag, [/log, bin=] )
;
; INPUTS:
;     TIME - the vector of time measurements
;     MAG  - the vector of data, f(time) measurements
;
; OPTIONAL INPUTS:
;     BIN - the Delta-time bin to measure the SF at.
;
; KEYWORD PARAMETERS:
;     /LOG - calculate the structure function for log(dTime) bins
;
; OUTPUTS: The function output (SF) is a 3-column vector...
;     SF[*,0] = the time bins with non-zero Str.Func. measurements
;     SF[*,1] = the Str.Func. measurements at each time step above
;     SF[*,2] = the poisson counting errors for each measurement
;               which are = 1/sqrt(N(t))
;
; OPTIONAL OUTPUTS:
;     dmag  - the NxN grid of magnitude differences
;     dtime - the NxN grid of time differences
;
; EXAMPLE:
;      t = randonu(ss,1000)*200.    ; Fake Times
;      f = randomn(ss,1000) + 15.   ; Fake Fluxes
;      sf = strfunc(t,f,/log,0.1)
;
; MODIFICATION HISTORY:
;      Feb 2011: Created by James R.A. Davenport (JRAD)
;                jrad@astro.washington.edu
;      March 2011: Rewritten to use the HISTOGRAM function for 
;                  speed (JRAD) 
;-

ntime = n_elements(time)
dmag  = fltarr(ntime,ntime)
dtime = fltarr(ntime,ntime)

; the structure function is defined as:
; SF(t) = sqrt(1/N(t) * sum(dmag(t)^2))

; Do 1 loop over all the epochs to get the N^2 difference calculation
FOR n=0L,(ntime-1L) DO BEGIN
    dmag[n,*]  = (mag - mag[n])^2.
    dtime[n,*] = abs(time - time[n])
ENDFOR

;I typically make the data log binned
if keyword_set(log) then dtime = alog10(dtime)

;now in time bins, find SF power
if not keyword_set(bin) then bin=0.2
if bin lt 0 then bin=0.2

;; tmin = min(dtime,/nan)
;; rng = (max(dtime)-tmin)/bin

;======== OUTLINE =========
;1) make histogram in time space
;   ==> note this only will do EVEN binning
;2) add the dmag^2 values in each time bin
;3) divide by total number of epochs in each time bin (hist)
;4) errors = 1/sqrt(total # per bin)

;================ FAST METHOD ===============================
; This uses histogram's reverse_indicies to run super fast
;      -----------> this is the magic! <---------
hist = float(histogram(dtime,binsize=bin,REVERSE_INDICES=ri,$
                 /NAN,locations=sf_t))
sf_m = fltarr(n_elements(hist))
for k=0L,n_elements(hist)-1L do if ri[k+1] gt ri[k] and hist[k] gt 0 then $
   sf_m[k] = total(dmag[ri[ri[k]:ri[k+1]-1]])/hist[k]
sf_e = 1./sqrt(hist)
; This code was modified from JD Smith's HISTOGRAM tutorial
; http://www.idlcoyote.com/tips/histogram_tutorial.html
;============================================================

sf_m = sqrt(sf_m)
bad = [where(sf_m le 0)]
if bad[0] ne -1 then remove,bad,sf_m,sf_t,sf_e
xx = check_math() ; a *very* stupid way to suppress the error outputs

return,[[sf_t],[sf_m],[sf_e]]
end
