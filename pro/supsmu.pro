;+
; ROUTINE: supsmu
;
; CALLING SEQUENCE: supsmu(X,Y[,W])
;
; PURPOSE: 
;       Smooth data using a variable length (span) smoothing. Based on
;       the SuperSmoother algorithm [Friedman, J. H. (1984) 
;       Laboratory for Computational Statistics, Stanford University 
;       Technical Report No. 5.]
;
;       Derived from a vectorized MATLAB version.
;
;       This function was developed to be used in conjunction with a
;       Phase Dispersion Minimization program to search for periodic
;       signals in time-domain data (PDM_SS.pro)
;
; INPUT: X,Y
;       The independent and dependent variables on the scatter plot
;
; OPTIONAL INPUT: W
;       The weights or errors for Y. If not set, W=1 for all points.
;
; EXAMPLE: Show intensity versus phase, and the smooth
;
;       IDL> ploterror, phase, flux, error, psym=6
;       IDL> flux2 = supsmu(phase, flux, error)
;       IDL> oplot, phase, flux2
;
; AUTHOR:  J.R.A. Davenport, University of Washington, May 2012
;
;
;-

FUNCTION supsmu,x0,y0,w0
; set options for the compiler
compile_opt defint32, strictarr, strictarrsubs
; suppress some outputs
compile_opt HIDDEN

On_error,2
if N_params() LT 2 then begin   
   print,'SUPSMU Error: Must provide X,Y'
   return,x0*0.-1.
endif

if n_elements(x0) lt 5 then begin
   print,'Error: Must X,Y[,W] must have 5 or more elements.'
   return,x0*0.-1.
endif

if n_elements(x0) ne n_elements(y0) then begin
   print,'Error: X and Y must have same number of elements'
   return,x0*0.-1.
endif

if not keyword_set(w0) then w0 = x0*0.+1.

; sort the input data, incase it's not already
x = x0[sort(x0)]
y = y0[sort(x0)]
w = w0[sort(x0)]

num = float(n_elements(x0))

ind = findgen(num)
ind = ind[sort(x0)] ; so we can UNsort it later

b1 = 0.05
b2 = 0.20
b3 = 0.50
 ; the smoothing lengths
s_vals = float(floor([b1*num, b2*num, b3*num]))
if s_vals[0] lt 2. then s_vals[0] = 2.
if s_vals[0] lt 3. then s_vals[0] = 3.
if s_vals[0] lt 4. then s_vals[0] = 4.

; the 3 baseline smooths
; using the boxcar since it's fast & intrinsic to IDL
s1 = smooth(y,s_vals[0],/edge_truncate,/nan) ; tweeter
s2 = smooth(y,s_vals[1],/edge_truncate,/nan)  ; midrange
s3 = smooth(y,s_vals[2],/edge_truncate,/nan)  ; woofer

; find the weighted absolute residuals at each data point
r1 = abs(y-s1)/w
r2 = abs(y-s2)/w
r3 = abs(y-s3)/w

s_best = r1*0d0 ; blank array num long

; for each data point, find the span that gave
; the best value and, save the span length
;  (done w/o a loop)
ress = [[r1],[r2],[r3]] ; array of residuals
; array of smooth points used, same size as ress
s_vals_s = [[r1*0+s_vals[0]],[r1*0+s_vals[1]],[r1*0+s_vals[2]]]
; use MIN to find indexes of best spans
tmp = min(ress,dimen=2,pl) ; pl = find the min for each point
; index by best-spans
s_best = s_vals_s[pl]


;    (smooth the smoother!)
; smooth the span lengths with the midrange span length (s_vals[1])
s_best = smooth(s_best,s_vals[1],/edge_truncate,/nan)

; interpolate each point at the s_best span between the 3 initial spans
y_smooth = fltarr(num)
for n=0L,num-1L do y_smooth[n]=interpol([s1[n],s2[n],s3[n]],s_vals,s_best[n])
; rewrite a vectorized linear interpolator here...
; leverage the fact that there is only 3 points ever in the interpolation

; then smooth once more w/ 0.05
;; y_smooth = smooth(y_smooth,s_vals[0],/edge_truncate,/nan)
; ==> instead of tweeter, use mean of s_best (typical best smooth)
y_smooth = smooth(y_smooth,mean(s_best),/edge_truncate,/nan)

; make sure to return it re-sorted for the INPUT data
  return,y_smooth[sort(ind)]
end


