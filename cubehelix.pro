;+
; NAME: CUBEHELIX
;
; PURPOSE: Calculate a "cube helix" color table.
;          Based on the FORTRAN 77 code provided in 
;          D.A. Green, 2011, BASI, 39, 289
;
;          http://adsabs.harvard.edu/abs/2011arXiv1108.5083G
;
; CALLING SEQUENCE:  CUBEHELIX,[start=start,rot=rot,hue=hue,gamma=gamma,
;                               get=get,hex=hex,/plot,/white]
;
; OPTIONAL INPUTS:  [this wording is taken from the paper]
;          START: color (1=red, 2=green, 3=blue)
;                 e.g.  0.5=purple
;                 DEFAULT = 0.5
;
;          ROT:  rotations in colour (typically -1.5 to 1.5)
;                 DEFAULT = -1.5
;
;          HUE:   hue intensity scaling (in the range 0 (B+W) to 1
;                 to be strictly correct, larger values may be OK with
;                 particular star/end colours)
;                 DEFAULT = 1.2
;               NOTE: new keyword SAT will override HUE
;
;          GAMMA: set the gamma correction for intensity
;                 DEFAULT = 1.0
;
;--- new inputs, taken from python version!
;    nlev : scalar, optional
;        Defines the number of discrete levels to render colors at.
;        Defaults to 256.
;    sat : scalar, optional
;        The saturation intensity factor. Defaults to 1.2
;        NOTE: this was formerly known as "hue" parameter
;    minSat : scalar, optional
;        Sets the minimum-level saturation. Defaults to 1.2
;    maxSat : scalar, optional
;        Sets the maximum-level saturation. Defaults to 1.2
;    minHue : scalar, optional
;        Sets the starting color, ranging from [0, 360], as in
;        D3 version by @mbostock
;        NOTE: overrides values in start parameter
;    maxHue : scalar, optional
;        Sets the ending color, ranging from [0, 360], as in
;        D3 version by @mbostock
;        NOTE: overrides values in rot parameter
;    minLight : scalar, optional
;        Sets the minimum lightness value. Defaults to 0.
;    maxLight : scalar, optional
;        Sets the maximum lightness value. Defaults to 1.
;
; KEYWORD PARAMETERS:
;          PLOT:  Have the program plot a color-bar to the screen,
;                 with colors 0->255 going left->right 
;
;          WHITE: let the last color be white
;
; OPTIONAL OUTPUTS:
;          GET:   Set this to a named vector which will have
;                 dimensions [256,3] to hold the RGB color vectors 
;                 NOTE: these values range from 0-1
;          HEX:   Set this to a named variable to contain the HEX code
;                 colors, converted from the RGB.
;
; EXAMPLE:
;    Create a color table of only greens and plot to the screen
;          IDL> cubehelix,/plot,gamma=1.5,rot=0,start=2
;
; MODIFICATION HISTORY:
;          August 2011: Created by J.R.A. Davenport
;          June 2012: Updated plot, fixed setting optional inputs = 0,
;                     removed the unsightly FOR loop
;          June 2014: Major update to API to bring in line w/ my Python
;                     and mbostock's D3 version. Have tried to
;                     maintain old API functionality also except: now
;                     keyword ROTS == ROT
;
;I would appreciate a simple acknowledgement for published works using my code:
;   "This publication has made use of code written by James R. A. Davenport."
;
;-
function rgb2hex_h,c1

  compile_opt defint32, strictarr, strictarrsubs
  compile_opt HIDDEN

h1 = string(c1[*,0]*255d0,f='(Z02)')+$
     string(c1[*,1]*255d0,f='(Z02)')+$
     string(c1[*,2]*255d0,f='(Z02)')

return,h1
end

pro cubehelix,start=start,rot=rot,hue=hue,gamma=gamma,$
              sat=sat,minsat=minsat,maxsat=maxsat,minHue=minHue,$
              maxHue=maxHue,minlight=minlight,maxlight=maxlight,$
              get=get,plot=plot,white=white, hex=hex,nlev=nlev

  compile_opt defint32, strictarr, strictarrsubs
  compile_opt HIDDEN

  nlo = 0.
  nhi = 0.

;  always assume 256 colors for IDL
  if not keyword_set(nlev) then nlev = 256.

; use defaults from the preprint if not otherwise set
;== updated to deal with user entering 0's
  if n_elements(start) eq 0 then start = 0.5 ; purple
  if n_elements(rot) eq 0 then rot = -1.5
  if n_elements(gamma) eq 0 then gamma = 1.0
  if n_elements(hue) eq 0 then hue = 1.2
  if n_elements(sat) eq 0 then hue = 1.2 ; override HUE w/ SAT

  if keyword_set(minHue) then start = (minHue/360. -1.)*3. else start=0.5
  if keyword_set(maxHue) then rot = maxHue/360. - start / 3. - 1.
  if n_elements(minsat) eq 0 then minsat = hue
  if n_elements(maxsat) eq 0 then maxsat = hue
  if n_elements(minlight) eq 0 then minlight = 0.
  if n_elements(maxlight) eq 0 then maxlight = 1.


;  fract = findgen(nlev)/(nlev-1.)
  fract = findgen(nlev)*(maxlight-minlight)/float(nlev-1.) + minlight
  angle = 2. * !dpi * (start / 3.0 + 1.0 + rot * fract)
  fract = fract^gamma

  satar = findgen(nlev)/float(nlev-1.)*(maxsat-minsat) + minsat
  amp   =  satar * fract * (1. - fract) / 2.

  red   = fract + amp * (-0.14861 * cos(angle) + 1.78277 * sin(angle))
  grn   = fract + amp * (-0.29227 * cos(angle) - 0.90649 * sin(angle))
  blu   = fract + amp * (1.97294  * cos(angle))
  

  nhi = total(blu gt 1) + total(grn gt 1) + total(red gt 1)
  nlo = total(blu lt 0) + total(grn lt 0) + total(red lt 0)

  red = red * (red ge 0)
  blu = blu * (blu ge 0)
  grn = grn * (grn ge 0)

  xb = where(blu gt 1)
  xr = where(red gt 1)
  xg = where(grn gt 1)

  if xb[0] ne -1 then blu[xb] = 1
  if xg[0] ne -1 then grn[xg] = 1
  if xr[0] ne -1 then red[xr] = 1


  if total(nhi) gt 0 then print,'Warning: color-clipping on high-end'
  if total(nlo) gt 0 then print,'Warning: color-clipping on low-end'

  if keyword_set(white) then $
     tvlct,red*255.,grn*255.,blu*255. ; load the new color table
  if not keyword_set(white) then $
     tvlct,red*254.,grn*254.,blu*254.

; output the color vectors if requested
  get=[[red],[grn],[blu]]
  hex = rgb2hex_h(get)

; show on screen if requested
  if keyword_set(plot) then begin
     plot,[-1,256],[1,1],/nodata,/xstyle,/ystyle,yrange=[-1,1],$
          ytickname=replicate(' ',8),xtitle='Color Index',$
          position=[.1,.1,.8,.94]
     for i=0,255 do polyfill,[i-.5,i+.5,i+.5,i-.5],[-1,-1,1,1],/data,color=i
     xyouts,.84,.95,/norm,'Cubehelix',charsize=1.5
     xyouts,.84,.85,/norm,'start='+strtrim(string(start),2)
     xyouts,.84,.8 ,/norm,'rot='+strtrim(string(rot),2)
     xyouts,.84,.75,/norm,'gamma='+strtrim(string(gamma),2)
     xyouts,.84,.7 ,/norm,'hue='+strtrim(string(hue),2)
  endif

return
end

