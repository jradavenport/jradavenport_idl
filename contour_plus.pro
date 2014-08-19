;+
; ROUTINE: contour_plus
;
; CALLING SEQUENCE: contour_plus,x,y,[max_den,xbin=,ybin=, $
;                   levels=,return_levels=,nlevels=,/no_fill,/no_contour,
;                   /no_line,/no_points,/pixel,/reverse,/all_points,/OVERPLOT]
;
; PURPOSE: 
;       Create plots w/ filled contours over high density
;       regions. Removes data points under contours to reduce file
;       size. 
;
;     * This is a total rewrite of CONTOUR_PLUS, having stolen the
;       idea from Anil Seth's 2005 version. This version uses
;       HISTOGRAM & reverse indexing to remove data under contours,
;       which runs at least 10X faster, and scales better with large
;       data sets.
;
; INPUT: x,y
;	Vectors of values to plot
;
; OPTIONAL INPUT: 
;       LEVELS: an arry of levels to plot with contours. If not set,
;       CONTOUR_PLUS will use a default scheme.
;
;	MAX_DEN: maximum density before contours are plotted. If not
;	set, CONTOUR_PLUS will try to use the lowest LEVELS
;
;       XBIN/YBIN: bin size in the horizontal/vertical direction. If
;       not set, CONTOUR_PLUS will use 25 bins in each direction
;       
;       RETURN_LEVELS: user selected output vector which contains all
;       the levels CONTOUR_PLUS uses.
;           NOTES: a) this *could* already be accomplished by just
;                     setting LEVELS= a new variable, but may be more 
;                     intuitive this way
;                  b) auto-selecting levels is very difficult to
;                     generalize for all types of data, this is a very
;                     helpful way to get the right levels for you
;
;       TRIMLEVEL: the first contour level to remove data from. By
;       default CONTOUR_PLUS plots the data under the first 2 levels
;       to ensure no gaps between points/contours form.
;
;       NLEVELS: the number of levels to automatically use. Does not
;       apply if LEVEL has been used. [Default = 8]
;
; KEYWORD INPUT:
;       /NO_FILL: use only lined contours, not filled contours
;
;	/NO_CONTOUR: suppress the contours, only plot the points which
;	fall below the maximum density
;
;       /NO_POINTS: suppress the points of data, contours only
;
;       /NO_LINE: suppress tracing lines between contour levels.
;
;       /PIXEL: draw pixels at each bin instead of contours. Uses a
;       modified version of my PIXEL_CONTOUR script. /PIXEL works best
;       if you do not set the LEVELS= 
;
;       /REVERSE: reverse the color table for the contours. This uses
;       REVERSE_CT by D Fanning (included)
;
;       /ALL_POINTS: do not remove the data below the contours. 
;       Defeats the purpose of this program, but ensures no gaps
;       between points and contours.
;
;       /OVERPLOT: put on top of existing plot. Very useful in
;       combination with /NO_FILL and /NO_POINTS for overplotting 
;       open contours. 
;
; COMMON ERRORS:
;	- Small gaps between the plotted data and contours can appear,
;	  as happened in the original CONTOUR_PLUS. This can be avoided
;	  with the /PIXEL keyword.
;       - The program does not handle /xlog or /ylog very well.
;
; VERSION:- (April 2010) Written from scratch [JRAD]
;         - (April 2010) problem noticed: if you contour a uniform
;           field, it can crash when given levels... fixed w/ levels? 
;         - (May 2010) Centering of contours fixed, min # of levels
;           added, pixels colors "top-out" at 255 now, dont wrap to
;           back to 0 [JRAD]
;	  - (Sept 2010) Add RETURN_LEVELS option for clarity, tweaked
;           default contour levels [JRAD]
;         - (Feb 2011) Added TRIMLEVEL to control which which contour
;           is used to cut out data [JRAD]
;         - (Feb 2011) Fixed PIXEL color levels to be the same as the
;           contour color levels [JRAD]
;         - (April 2011) Fixed crashes w/ LOW error when contours were
;           too high or no data present within X/Yrange [JRAD]
;         - (April 2011) Fixed fact that /NO_FILL did nothing [JRAD]
;         - (July 2011) Added compiler options
;         - (Jan 2012) Added NLEVEL [JRAD]
;         - (Feb 2012) Adjusted color levels, won't show total
;           white. This may not be what others want, but I like it
;           better, especially using with CUBEHELIX [JRAD]
;         - (Aug 2012) added background color to fix "holes"
;         - (Oct 2012) if MAX_DEN=1 set /NO_POINTS by default
;
;
; AUTHOR:  J.R.A. Davenport, University of Washington, April 2010
;	   jrad@astro.washington.edu
;I would appreciate a simple acknowledgement for published works using my code:
;   "This publication has made use of code written by James R. A. Davenport."
;   
; PROCEDURES CALLED
;
;       REVERSE_CT    [dfanning library] - included
;	HIST_ND       [dfanning library] 
;                     http://www.dfanning.com/programs/hist_nd.pro
;
;-

;==================================================================
PRO Reverse_CT,set=set
; This program reverses the current color table
; Compliments of D Fanning
; http://www.dfanning.com/color_tips/reverse_ct.html
if keyword_set(set) then begin
TVLCT, r, g, b, /Get
TVLCT, Reverse(r), Reverse(g), Reverse(b)
endif
return
END
;==================================================================

pro contour_plus,x,y,max_den,xbin=xbin,ybin=ybin,levels=levels,return_levels=return_levels,no_fill=no_fill,no_contour=no_contour,no_line=no_line,no_points=no_points,pixel=pixel,reverse=reverse,psym=psym,all_points=all_points, overplot=overplot,trimlevel=trimlevel,nlevels=nlevels,_extra = e,hist=hist

; set options for the compiler
compile_opt defint32, strictarr, strictarrsubs
; suppress some outputs
compile_opt HIDDEN

  On_error,2
  if N_params() LT 2 then begin   
     print, 'Error: must provide X and Y coordinates.'
     print,'contour_plus,x,y,max_den,xbin=xbin,ybin=ybin,levels=levels,no_fill=no_fill,no_contour=no_contour,no_line=no_line,no_points=no_points,pixel=pixel,reverse=reverse,psym=psym,all_points=all_points, _extra = e'
     return
  endif

; Use the X/Yrange to set limits of the histogram
  ymin = min(y)                 ; 
  ymax = max(y)                 ; set these incase no others
  xmin = min(x)                 ; are set by the user
  xmax = max(x)                 ; 
  if keyword_set(e) then begin
     if total(tag_names(e) eq 'YRANGE') eq 1 then ymin=min(e.yrange)  
     if total(tag_names(e) eq 'YRANGE') eq 1 then ymax=max(e.yrange)  
     if total(tag_names(e) eq 'XRANGE') eq 1 then xmin=min(e.xrange)  
     if total(tag_names(e) eq 'XRANGE') eq 1 then xmax=max(e.xrange)  
  endif 

; incase no x/ybin has been set, make 25 bins
  if not keyword_set(xbin) then xbin = (abs(xmax-xmin)/25.)
  if not keyword_set(ybin) then ybin = (abs(ymax-ymin)/25.)
  
;;   if total(x ge xmin and x le xmax and y ge ymin and y le ymax) le 1 then begin
;;      print,'ERROR: Not enough data within the specified X/Yrange. Please adjust your range and try again!'
;;      return
;;   endif

; parse up the data in a 2d histogram. use HIST_ND rather than HIST_2D
; for the reverse_indicies functionality
hist = hist_nd(transpose([[x],[y]]),[xbin,ybin],min=[xmin,ymin],max=[xmax,ymax],REVERSE_INDICES=ri)

; FIX error when no data is within range, quit gracefully
if total(hist) eq 0 then begin
   print,'ERROR: No data within specified X/Yrange!' 
   print,'       Please adjust the X/Yrange and try again.'
   return
endif

; If a threshhold density is not set, then use the minimum of the
; levels if available, otherwise choose one based on the Std Dev.
if (not keyword_set(max_den) and keyword_set(levels)) then max_den=min(levels) 
if (not keyword_set(max_den) and not keyword_set(levels)) then $
   max_den = stddev(hist,/NAN)

if max_den eq 1 then no_points = 1

histsz = size(hist)
xx = findgen(histsz[1])*xbin + xmin
yy = findgen(histsz[2])*ybin + ymin

; SOME DEFAULT CONTOUR LEVELS, MAYBE NOT WHAT YOU WANT...
if not keyword_set(nlevels) then nlevels=8
if not keyword_set(levels) then levels = lindgen(nlevels)^2.3*stddev(hist)/(nlevels*1.5)+max_den

;make sure there's a couple levels 
if n_elements(levels) le 1 then begin
   print,'Caution: Minimum level too low, adding 1.'
   levels = [levels,max(levels)+max_den,max(levels)+2.*max_den]
endif

; FIX problem where min level is < 1, which made it barf!
; Note that *no* data will be contoured in this case
if levels[0] lt 1 then levels = levels + 1.


; return levels to user if desired, good for "interactive" use
return_levels = levels

;============================================================
; Use the MIN_DEN to cut the high density data from the plot.
; This uses histogram's reverse_indicies to run super fast
;       -----------> this is the magic! <---------
if not keyword_set(trimlevel) then trimlevel=2
if trimlevel lt 1 then trimlevel = 1
keep = where(hist le levels[trimlevel])
for i=0L,n_elements(keep)-1L do begin
   k=keep[i]
   if ri[k+1] gt ri[k] then $
      if n_elements(low) eq 0 then low=ri[ri[k]:ri[k+1]-1] $
      else low=[low,ri[ri[k]:ri[k+1]-1]]
endfor
; This code was modified from JD Smith's HISTOGRAM tutorial
; http://www.dfanning.com/tips/histogram_tutorial.html
;============================================================

if not keyword_set(psym) then psym=3

; NOT overplot
if keyword_set(all_points) and not keyword_set(overplot) then plot,x,y,psym=psym,_extra=e


if not keyword_set(no_points) and not keyword_set(all_points) and not keyword_set(overplot) then plot,x[low],y[low],_extra=e,psym=psym

if keyword_set(no_points) and not keyword_set(all_points) and not keyword_set(overplot) then plot,x,y,_extra=e,/nodata
;------
; YES overplot
if keyword_set(all_points) and  keyword_set(overplot) then oplot,x,y,psym=psym,_extra=e

if not keyword_set(no_points) and  not keyword_set(all_points) and  keyword_set(overplot) then oplot,x[low],y[low],_extra=e,psym=psym

;------

if keyword_set(reverse) then reverse_ct,/set

if keyword_set(pixel) then no_contour = 1 ; if pixels, dont draw contours

clrz = findgen(n_elements(levels))/(n_elements(levels)+1)*254.
if keyword_set(reverse) then $
   clrz = (findgen(n_elements(levels))+1)/(n_elements(levels)+1)*254.

bcolor=255
if keyword_set(reverse) then bcolor = 0 ; fix holes with proper background color

fillyn = 1
if keyword_set(no_fill) then fillyn = 0
if not keyword_set(no_contour) and not keyword_set(no_fill) then begin
 ; if you do want the FILLED contours
   xx=xx+xbin/2. & yy=yy+ybin/2.
   contour,hist,xx,yy,/overplot,fill=fillyn,levels=levels,c_color=clrz,background=bcolor,/downhill
   if not keyword_set(no_line) then $
      contour,hist,xx,yy,/overplot,color=!p.background,levels=levels,/downhill
endif
if not keyword_set(no_contour) and keyword_set(no_fill) then begin
 ; if you do want the EMPTY contours
   xx=xx+xbin/2. & yy=yy+ybin/2.
   contour,hist,xx,yy,/overplot,fill=fillyn,levels=levels,_extra = e,/downhill
endif

if keyword_set(pixel) then begin ; pixels
   for l=0L,n_elements(xx)-1 do begin
   for n=0L,n_elements(yy)-1 do begin
      if hist[l,n] ge max_den then begin
         xarr = [xx[l],xx[l]+xbin,xx[l]+xbin,xx[l],xx[l]]
         yarr = [yy[n],yy[n],yy[n]+ybin,yy[n]+ybin,yy[n]]

; pixel colors that match contour colors
         color = 255./(n_elements(levels)+1)*(total(levels le hist[l,n])-1)
; ensure the colors dont wrap around, keep the "max" at 255
         if hist[l,n] ge max(levels) then $
            color = 255.-255./(n_elements(levels)+1)
         
         if keyword_set(reverse) then color = color+255./(n_elements(levels)+1)

         polyfill,color=color[0],[xarr],[yarr],/fill ;,_extra = e
      endif
   endfor
   endfor
endif


if keyword_set(reverse) then reverse_ct,/set

return
end

