;+
; NAME: pixel_plus
;
; PURPOSE: 
;	Pixel-grid up a 2D dataset, and compute things over the pixel
;	values besides simply # of objects (2D histogram).
;
;       Usually used for computing the median value of a 3rd parameter
;       within each pixel.
; 	
;
; CALLING SEQUENCE:
;	pixel_plus,x,y,I, [ xbin=xbin, ybin=ybin, nlvl=nlvl,
;                           lvl=lvl, /reverse, mode=mode]
;
; INPUTS:
;	x = vector of x-coordinates
;	y = vector of y-coordinates
;	I = vector of intensities (variable to be computed over in each bin)
;
; OPTIONAL INPUTS:
;	xsz = the x-coord bin size (default = 0.05)
;	ysz = the y-coord bin size (default = 0.10)
;	nlvl = the number of contour levels (default = 10)
;	binlimit = the largest allowed number of bins in any dimension
;		(default = 2000) 
;		NOTE: making this too large can cause a memory
;		overflow!
;
; KEYWORD PARAMETERS:
;	reverse_ct = reverse the color of the contours (best for postscripts)
;
; COMMON BLOCKS:
;	If binlimit is too high the system can hang, but a warning
;	should be displayed before that happens
;
; PROGRAMS CALLED: hist_nd, reverse_ct, pixel_contour
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
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

pro pixel_plus,x,y,I,xbin=xbin,ybin=ybin,nlvl=nlvl,_extra = e,mode=mode,lvl=lvl,alpha=alpha,empty=empty,reverse=reverse,hist=hist,xx=xx,yy=yy,noplot=noplot

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

if not keyword_set(nlvl) then nlvl = 10
if not keyword_set(xbin) then xbin = (xmax-xmin)/50.
if not keyword_set(ybin) then ybin = (ymax-ymin)/50.
if not keyword_set(mode) then mode = 'mean'


alpha = FLTARR((xmax-xmin)/xbin + 1. ,$
               (ymax-ymin)/ybin + 1.)

a = transpose([[x],[y],[I]])

nbins = size(alpha,/dimensions)

im = hist_nd(a[0:1,*], nbins=nbins,reverse_indices = ri,min=[xmin,ymin],max=[xmax,ymax])

if mode eq 'total' then $
   FOR j=0L,n_elements(im)-1 DO IF ri[j+1] GT ri[j] THEN $
      alpha[j] = total(a[2,ri[ri[j]:ri[j+1]-1]])
if mode eq 'mean' then $
   FOR j=0L,n_elements(im)-1 DO IF ri[j+1] GT ri[j] THEN $
      alpha[j] = mean(a[2,ri[ri[j]:ri[j+1]-1]])
if mode eq 'median' then $
   FOR j=0L,n_elements(im)-1 DO IF ri[j+1] GT ri[j] THEN $
      alpha[j] = median(a[2,ri[ri[j]:ri[j+1]-1]],/even)


hist=alpha

;useful if the "I" values might = 0, reassign "empty" cells
if not keyword_set(empty) then empty = 0
tmp = where(im eq 0)
if tmp[0] ne -1 then alpha[tmp] = empty

;; a = -99
;; ri = -99
;; im = -99

xx=findgen((xmax-xmin)/xbin+1.)*xbin+xmin;+xbin/2.
yy=findgen((ymax-ymin)/ybin+1.)*ybin+ymin;+ybin/2.

;----- plot ----
if not keyword_set(noplot) then begin ; /NOPLOT
;;plot,x,y,/nodata,_extra = e,/xstyle,/ystyle
if not keyword_set(lvl) then lvl = [stddev(alpha,/nan)*(findgen(nlvl)/2.+1.)]

; contour,alpha,xbin,ybin,/fill,_extra=e


if keyword_set(reverse) then begin
   if not keyword_set(overplot) then plot,xx,yy,/nodata,_extra = e

   reverse_ct,/set
   pixel_contour,alpha,xx,yy,lvl,_extra = e,thresh=lvl[0],/overplot
   reverse_ct,/set
endif

if not keyword_set(reverse) then $
   if not keyword_set(overplot) then pixel_contour,alpha,xx,yy,lvl,_extra = e,thresh=lvl[0]

if not keyword_set(reverse) then $
   if keyword_set(overplot) then pixel_contour,alpha,xx,yy,lvl,_extra = e,thresh=lvl[0],/overplot
endif ; /NOPLOT

return
end
