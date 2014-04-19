pro pixel_contour,z,x,y,levels,_extra=e,overplot=overplot,xrange=xrange,yrange=yrange,threshold=threshold
;+
; NAME: pixel_contour
;
; PURPOSE: 
;	To display a 2D array of data as colored pixels. Very similar
;	to using CONTOUR,/FILL except each bin is a square pixel. This
;	is particularly useful for showing data with large range in
;	which CONTOUR would look like point sources, or plotting images.
;
; CALLING SEQUENCE:
;	pixel_contour,z,x,y,levels
;
; INPUTS:
;       z = a 2D array (maybe an image or 2D histogram, etc)
;	x = vector of x-coordinates
;	y = vector of y-coordinates
;       levels = the list of levels, as would be fed manually to CONTOUR
;       overplot = allows the pixel contour to be overplotted
;
; COMMON BLOCKS:
;	X and Y must have the proper dimensions to match Z. The
;	program assumes even bins for X & Y (although dx does not have
;	to equal dy)
;
; PROGRAMS CALLED: POLYFILL
;
; EXAMPLE:
;   img = fltarr(20,20) + randomn(s,400)
;   x=findgen(20)
;   y=x
;   levels=[-10,-4,-2,0,2,4,6,8]
;   pixel_contour,img,x,y,levels,xtitle='horiz',ytitle='vert'
;
; MODIFICATION HISTORY:
;	JUNE 2009 - Created by J. Davenport (San Diego State U)
;       March 2011 - added threshold. (JRAD)
;
;        ==> a total redo of the levels & features are needed to put
;            this in line with CONTOUR_PLUS
;
;I would appreciate a simple acknowledgement for published works using my code:
;   "This publication has made use of code written by James R. A. Davenport."
;-
On_error,2
 if N_params() LT 4 then begin   
	print, 'Error: the image, xarray, yarray, and levels'
	return
 endif


;scale the levels to 255 for colors
clvl = float(levels-min(levels))
clvl = clvl/max(clvl) * 255.

dx = abs(x[1]-x[0]) ; kinda big assumption - uniform bin size
dy = abs(y[1]-y[0])

if not keyword_set(threshold) then threshold = min(z)-10.

if keyword_set(xrange) then begin
   if xrange[1] gt xrange[0] then xr=[where(x ge xrange[0] and x le xrange[1])]
   if xrange[0] gt xrange[1] then xr=[where(x ge xrange[1] and x le xrange[0])]
   x = x[xr]
   z = z[xr,*]

endif
if keyword_set(yrange) then begin
   if yrange[1] gt yrange[0] then yr=[where(y ge yrange[0] and y le yrange[1])]
   if yrange[0] gt yrange[1] then yr=[where(y ge yrange[1] and y le yrange[0])]
   y = y[yr]
   z = z[*,yr]
endif

if not keyword_set(xrange) then xrange=[min(x),max(x)]
if not keyword_set(yrange) then yrange=[min(y),max(y)]


if not keyword_set(overplot) then contour,z,x,y,_extra = e,/nodata,xrange=xrange,yrange=yrange

for i=0,n_elements(x)-1 do begin
for j=0,n_elements(y)-1 do begin
   if z[i,j] gt threshold then begin
      xarr = [x[i],x[i]+dx,x[i]+dx,x[i],x[i]] - dx/2.
      yarr = [y[j],y[j],y[j]+dy,y[j]+dy,y[j]] - dy/2.
      ;; if i eq 0 then xarr[[0,3,4]] = xarr[[0,3,4]]+ dx/2.
      ;; if j eq 0 then yarr[[0,1,4]] = yarr[[0,1,4]]+ dy/2.      
      ;; if i eq n_elements(x)-1 then 
      ;; if j eq n_elements(y)-1 then 

      if x[i]+dx gt xrange[1] then xarr[[1,2]] = [xrange[1],xrange[1]]
      if y[j]+dy gt yrange[1] then yarr[[2,3]] = [yrange[1],yrange[1]]
      if x[i]-dx/2. lt xrange[0] then xarr[[0,3,4]] = [1,1,1]*xrange[0]
      if y[j]-dy/2. lt yrange[0] then yarr[[0,1,4]] = [1,1,1]*yrange[0]

;      if x[i] gt xrange[1] then xarr[[1,2]] = xrange[1]

      wclr = where(z[i,j] le levels)
      if wclr[0] ne -1 then color = clvl[wclr]
      if wclr[0] eq -1 then color = clvl[n_elements(clvl)-1]
      polyfill,color=color[0],[xarr],[yarr],/fill ;,_extra = e
   endif
endfor
endfor


;help

return
end
