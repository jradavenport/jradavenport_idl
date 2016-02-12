;+
; ROUTINE: softserve
;
; CALLING SEQUENCE: softserve,X,Y [,k]
;
; PURPOSE: 
;       SOFTSERVE is a semi-clever smoothing function, created to
;       smooth light curves from Kepler with flares. It uses a 3 step
;       iterative selection + smooth, and at present has no tunable
;       parameters. It should be robust for use with FBeye.
;
; INPUTS: x,y
;	The X and Y data. The Y vector is smoothed.
;
; OPTIONAL INPUT: 
;       k: a knob to adjust the smoothing timescale.
;
; KEYWORD INPUT:
;       None at present
;
; EXAMPLE:
;       plot,X,Y,psym=3
;       oplot,X,SOFTSERVE(X,Y)
;
; VERSION: - April 2012: Written from scratch [JRAD]
;
; ISSUES: Doesn't work great for all light curves. Maybe add
;         a polynomial smoothing instead of spline
;
; AUTHOR:  J.R.A. Davenport, University of Washington, April 2012
;	   jrad@astro.washington.edu
;I would appreciate a simple acknowledgement for published works using my code:
;   "This publication has made use of code written by James R. A. Davenport."
;
;-

;==================================================================

function softserve,xx,yy,kk

; set options for the compiler
compile_opt defint32, strictarr, strictarrsubs
; suppress some outputs
compile_opt HIDDEN

  On_error,2
  if N_params() LT 2 then begin   
     print, 'Error: must provide X and Y coordinates.'
     return,yy
  endif

if not keyword_set(kk) then kk = 1.
;#1
bb = 75 * kk
yflux = median(yy,bb)
testout = interpol(yflux[findgen(n_elements(xx)/bb)*bb],xx[findgen(n_elements(xx)/bb)*bb],xx,/lsqu)


testout = smooth(testout,1.+5*kk,/edge_truncate)
nn = (yy-testout);/median(yy)

cut = 0.7; pick the good fit data, be strict! 

ok = where(abs(smooth(nn,2,/edge_truncate)) lt stddev(nn)*cut) 

if ok[0] ne -1 then begin
   testout0 = testout

   ;#2
   cut = 0.7
   bb = 35 * kk
   yflux = median(yy[ok],bb)
   testout = interpol(yflux[findgen(n_elements(ok)/bb)*bb],$
                      xx[ok[findgen(n_elements(ok)/bb)*bb]],xx,/spline)

   testout = smooth(testout,5,/edge_truncate)
   nn2 = (yy-testout);/median(yy)
   ;plot,xx,nn2,psym=3,/xsty,yrange=[-.01,.01]
   ok2 = where(abs(smooth(nn2,2,/edge_truncate)) lt stddev(nn2)*cut) 

   testout1 = testout

   ;#3
   bb = 25. * kk
   yflux = median(yy[ok2],bb)
   testout = interpol(yflux[findgen(n_elements(ok2)/bb)*bb],xx[ok2[findgen(n_elements(ok2)/bb)*bb]],xx,/lsqu)

   testout = median(testout,5)
   nn3 = (yy-testout);/median(yy)
endif else begin
   print,'> softserve error: data didnt pass first threshold'
   print,'    switching to a boxcar smooth with k=3'
   print,'> note: softserve is end-of-life and will not be updated'

   testout2 = smooth(yy, 3, /edge_truncate)
endelse

testout2 = testout


return,testout2
end
