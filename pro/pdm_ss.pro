;+
; ROUTINE: pdm_ss
;          Phase Dispersion Minimizer with SuperSmoother
;
; CALLING SEQUENCE: pdm_ss,t,f,e,p0,p1,dp,periods=periods,chi2=chi2,/silent
;                   
;
; PURPOSE: 
;       Given time domain data (t,f,e) measure the dispersion folded
;       at a range of periods.
;
; INPUT: t,f
;       time & flux
;
; OPTIONAL INPUT:
;       e: errors on each flux point. If not set then = 1
;
;       p0: first period to search from
;
;       p1: last period to search (must be larger than p0)
;
;       dp: the spacing between periods
;
; OPTIONAL OUTPUT:
;       PERIODS: the list of periods searched over
;
;       CHI2: the chi^2 for the data folded at each period
;
; EXAMPLE:
;       search the periods from 0.1 to 10 in steps of 0.01, and then
;       plot the chi^2 at each.
;
;       IDL> pdm_ss,x,y,yerr, 1d-1, 1d1, 1d-2,periods=per,chi2=chi
;       IDL> plot,per,chi
;
; AUTHOR:  J.R.A. Davenport, University of Washington, May 2012
;
; PROCEDURES CALLED
;       SUPSMU
;
;-
pro pdm_ss,t,f,e,p0,p1,dp,periods=periods,chi2=chi2,silent=silent

; set options for the compiler
compile_opt defint32, strictarr, strictarrsubs
; suppress some outputs
compile_opt HIDDEN

On_error,2
if N_params() LT 2 then begin   
   print,'PDM_SS Error: Must provide X,Y'
   return
endif

if not keyword_set(e) then e = fltarr(n_elements(t))+1.
if not keyword_set(p0) then p0 = abs((max(t,/nan)-min(t,/nan))/1d5)
if not keyword_set(p1) then p1 = abs((max(t,/nan)-min(t,/nan))/2d2)
if not keyword_set(dp) then dp = (p1-p0)/5d3

nper = (p1-p0)/dp
if nper lt 2 then nper = 2.

periods = dindgen(nper)*dp+p0
chi2 = dblarr(nper)

if not keyword_set(silent) then print,'Period0 =',P0
if not keyword_set(silent) then print,'Period1 =',P1
if not keyword_set(silent) then print,'dP =',dp

; loop over periods
npts = float(n_elements(t))
for i=0L,nper-1 do begin
; calculate phase folded at this period
   phase = (t mod periods[i])/periods[i]
; calculate smooth model at this period
   model = SUPSMU(phase,f,e)
; calculate chi^2 of data - model
   chi2[i] = total(((f-model)/e)^2.)/(npts-1)
endfor

if not keyword_set(silent) then begin
   print,'Minimum Chi2 found: ',min(chi2,bb)
   print,' with period: ',periods[bb]
endif

return
end
