;+
; NAME: BRUTE_MATCH
;
; PURPOSE: A robust "brute-force", no-frills, spatial matching program
;          with a user-defined search radius. To increase the speed of
;          the simple matching, BRUTE_MATCH uses few simple tricks:
;   - it will automatically loop over the shorter of the input lists
;   - it splits the data into sub-bins to search over
;   - avoids using a nested WHERE statement, though does use a nested
;     SORT for each star... possibly costly.
;
; CALLING SEQUENCE: brute_match,x1,y1,x2,y2,m1,m2,rad[,nxbin=nxbin,
;                   nybin=nybin,no_switch=no_switch]
;
; INPUTS: x1,y1,x2,y2
;         Coordinates of 2 data sets to be matched.
;
;         rad
;         Radius to search over for each star.
;
; OPTIONAL INPUTS:
;         NXBIN = the number of sub-bins in the X axis
;         NYBIN = the number of sub-bins in the Y axis
;
;         The default for NXBIN x NYBIN is 4x4. Try adjusting this to
;         improve performance.
;
; KEYWORD PARAMETERS:
;         /NO_SWITCH - force BRUTE_MATCH to not swap the arrays, in
;                      order to search over the larger array. Not
;                      Reccomended. 
;
; OUTPUTS: m1,m2
;          The matched indicies for the two coordinate pairs, such
;          that: x1[m1] matches x2[m2] and y1[m1] matches y2[m2]
;
; SIDE EFFECTS:
;         It is possible to get multiple objects from X1,Y1 to match
;         to a single X2,Y2 object, no redundancy check is done
;
;         BRUTE_MATCH gives the *nearest* match within the search
;         radius. Typically this will protect against using too large
;         of a search radius, but could allow unintentional
;         cross-matching. There is no explicit catch for multiple
;         matches.
;
; EXAMPLE:
;         brute_match,x1,y1,x2,y2,m1,m2,0.01
;
; AUTHOR: James R.A. Davenport, University of Washington, June 2010
;	  jrad@astro.washington.edu
;I would appreciate a simple acknowledgement for published works using my code:
;   "This publication has made use of code written by James R. A. Davenport."
;
; MODIFICATION HISTORY: 
;    Written June 2010: (JRAD)
;
;    August 2010: Added extra room at the sub-box edges to match
;    objects near the seams, added user-input NXBINS/NYBINS option
;    (JRAD)
;    
;    July 2011: Added compiler options
;
;-

pro brute_match,x1,y1,x2,y2,m1,m2,rad,nxbin=nxbin,nybin=nybin,no_switch=no_switch

; set options for the compiler
compile_opt defint32, strictarr, strictarrsubs
; suppress some outputs
compile_opt HIDDEN

 On_error,2
  if N_params() LT 7 then begin   
     print,'BRUTE_MATCH,x1,y1,x2,y2,m1,m2,rad [,nxbin=nxbin,'
     print,'            nybin=nybin,no_switch=no_switch]'
     return
  endif

;figure out which array is smallest, swap if needed
switched = -1
IF n_elements(x1) GT n_elements(x2) AND NOT keyword_set(no_switch) THEN BEGIN
   switched = 1 ; flag to determine if the swap has happened
   x0=temporary(x1) & y0=temporary(y1)
   x1=temporary(x2) & y1=temporary(y2)
   x2=temporary(x0) & y2=temporary(y0)
ENDIF
; --> X1 is now smaller than X2 by definition

;loop over the short list (x1,y1) and find the matches, if any
; for this 1st version do w/o  the bells & whistles
indx2 = indgen(n_elements(x2))
m2 = -1
m1 = -1


;--- break region up into sub-boxes to search over ---
; 4x4 is usually pretty good at cutting down the run time
if not keyword_set(nxbin) then nxbin=4.
if not keyword_set(nybin) then nybin=4.

xbin0 = min(x1)
ybin0 = min(y1)
xbin = (max(x1) - min(x1))/float(nxbin)
ybin = (max(y1) - min(y1))/float(nybin)
FOR K=0,nxbin-1 DO BEGIN
FOR H=0,nybin-1 DO BEGIN
;the box for the shorter list
rng1 = where(x1 ge k*xbin+xbin0 and x1 le (k+1)*xbin+xbin0 and $
             y1 ge h*ybin+ybin0 and y1 le (h+1)*ybin+ybin0)
; the box for the longer list, it is larger than the other box by 1
; search radius on all sides to allow objects near the seams to match 
rng2 = where(x2 ge k*xbin+xbin0-rad and x2 le (k+1)*xbin+xbin0+rad and $
             y2 ge h*ybin+ybin0-rad and y2 le (h+1)*ybin+ybin0+rad)
if rng1[0] eq -1 or rng2[0] eq -1 then goto,jumper
x3 = x1[rng1] ; shorter
x4 = x2[rng2] ; longer
y3 = y1[rng1] ; shorter
y4 = y2[rng2] ; longer

;   print,k,h,n_elements(x3)

;now search inside the sub-box
FOR j=0L,n_elements(x3)-1 DO BEGIN
   dist = sqrt((x3[j]-x4)^2.+(y3[j]-y4)^2.)
   s = sort(dist)
   if dist[s[0]] le rad then begin
      m2 = [m2,rng2[s[0]]]
      m1 = [m1,rng1[j]]
   endif
ENDFOR
jumper:continue


ENDFOR
ENDFOR
if n_elements(m2) gt 1 then remove,0,m2,m1
if m2[0] eq -1 then print,'BRUTE_MATCH failed! No matched stars in this field. Consider a larger search radius, and double-check that your fields overlap.'

;swap output array IF the swap happened above
if switched eq 1 then begin
   x0=temporary(x1) & y0=temporary(y1)
   x1=temporary(x2) & y1=temporary(y2)
   x2=temporary(x0) & y2=temporary(y0)
ENDIF


;stop

return
end
