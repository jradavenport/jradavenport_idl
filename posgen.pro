function posgen,nx,ny,px,py,xspan=xspan,yspan=yspan,xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax
;+
; NAME: POSGEN
;
; PURPOSE: Generate vector for POSITION=[ ] keyword when plotting.
;          Breaks plot into a NxM grid of sub-plots, allowing user to
;          plot over single/multiple sub-regions.
;
;          Note: This function only gives position vectors for the
;          simplest case of a plot grid with every axis shared, and is
;          not a total replacement for routines like MULTIPLOT, but
;          should encourage people to not use !P.MULTI=
;
; CATEGORY: Plotting utilities / life simplification... not exactly
;           super rigorous high-level stuff here...
;
; CALLING SEQUENCE: posgen(nx,ny,px[,py])
;
; INPUTS:
;    NX - the number of plots in the x-direction
;    NY - the number of plots in the y-direction
;
;    PX - the plot number
;         [optionally: the plot number in the x-direction]
;
;    The user can either call the panel # with one digit (which ranges 
;    from 1 to NX*NY), or the panel # with two digits which range
;    between 1 to NX and 1 to NY. Which ever is easier to remember...
;    See the EXAMPLES below...
;
; OPTIONAL INPUTS:
;    PY - the plot number in the y direction
;
; OPTIONAL KEYWORD INPUTS:
;
;    XSPAN > 0: number of x-boxes to extend the plot RIGHT
;          < 0: number of x-boxes to extend the plot LEFT
;    YSPAN > 0: number of y-boxes to extend the plot DOWN
;          < 0: number of y-boxes to extend the plot UP - See Examples
;
;    XMIN/YMIN/XMAX/YMAX - *In device units [0-1]* the x/y min/max
;                          values for the plot. Defaults are:
;                          0.11/0.11/0.95/0.93
;            (I almost never change these values...)
; 
; OUTPUTS: POSGEN returns the 4-element vector needed for the POSITION
;          keyword when plotting.
;
; EXAMPLES:
;  1) Create 3 square plots on a 2x2 grid, which look like:
;      ---- ----
;     | 1  | 2  |
;      ---- ----
;          | 4  |
;           ----
;
;    plot,x1,y1,position=POSGEN(2,2,1)
;    plot,x2,y2,position=POSGEN(2,2,2),/noerase
;    plot,x3,y3,position=POSGEN(2,2,4),/noerase
;
;  2) Create (in random order) 4 plots on a 3x2 grid, using the
;     2-digit plot numbers, which look like:
;      ----- ----- -----
;     | 1,1 | 2,1 | 3,1 |
;      ----- ----- -----
;           | 2,2 |
;            -----
;    plot,x1,y1,position=POSGEN(2,2,1,1)
;    plot,x1,y1,position=POSGEN(2,2,2,2),/noerase
;    plot,x1,y1,position=POSGEN(2,2,1,3),/noerase
;    plot,x1,y1,position=POSGEN(2,2,1,2),/noerase
;
;  3) Create 3-panel plot with different sized panels:
;      ----------- -----
;     | 1         |  3  |
;     |           |     |
;      ----------- -----
;     | 7        |
;      ----------- 
;    plot,x,y,position=posgen(3,3,7,xspan=2)
;    plot,x,y,position=posgen(3,3,3,yspan=2),/noerase
;    plot,x,y,position=posgen(3,3,1,xs=2,ys=2),/noerase 
;
;  4) Create a more complex plot using negative & positive spans
;
;      ------    --
;     | 1    |  | 5|
;     |      |  |  |
;      ------    --
;       |  13|  |15|
;        ----    --
;      plot,x,y,position=posgen(5,3,1,xsp=3,ysp=2)
;      plot,x,y,position=posgen(5,3,5,ysp=2),/noerase
;      plot,x,y,position=posgen(5,3,13,xsp=-2),/noerase
;      plot,x,y,position=posgen(5,3,15),/noerase
;
;
; NOTES:
;    Users will probably want to investigate ways to supress axis
;    labels. A trivial example would be:
;  plot,x,y,position=POSGEN(3,4,5),/noerase,xtickname=replicate(' ',8)
;
;    Clever use of this program would allow users to easily create
;    plots which are not on a simple grid by adjusting the NX & NY and
;    the XSPAN & YSPAN values
;
; MODIFICATION HISTORY:
;     Jan 31, 2011 - written from scratch (JRAD)
;                    jrad@astro.washington.edu
;     April, 2011 - Included support for negative X/YSPAN values to
;                   move the plots RIGHT and UP as desired and added
;                   another example(JRAD)
;     July 2011 - Added compiler options
;
;I would appreciate a simple acknowledgement for published works using my code:
;   "This publication has made use of code written by James R. A. Davenport."
;-

; set options for the compiler
compile_opt defint32, strictarr, strictarrsubs
; suppress some outputs
compile_opt HIDDEN

if n_params() lt 3 then begin
   print,'ERROR> Must supply at least 3 arguments!'
   print,'Syntax: POSGEN(nx,ny,px[,py,xspan=,yspan=])'
   return,[-1,-1,-1,-1]
endif

; floor the input numbers, incase of accidental decimals
nx = floor(nx)
ny = floor(ny)

if n_params() eq 3 then begin
; floor the input number, incase of accidental decimals
   px_tmp = floor(px) - 1
   px = ((px_tmp) mod nx) + 1
   py = floor((px_tmp)/nx) + 1
endif

; floor the input numbers, incase of accidental decimals
px = floor(px)
py = floor(py)

;--- PLOT LIMITS---
if not keyword_set(ymin) then ymin = 0.15
if not keyword_set(ymax) then ymax = 0.93
if not keyword_set(xmin) then xmin = 0.15
if not keyword_set(xmax) then xmax = 0.95
;-----------------

dx = (xmax-xmin)/nx
dy = (ymax-ymin)/ny

posx = dx * findgen(nx+1) + xmin
posy = 1. - (dy * findgen(ny+1) + (1. - ymax))


position_n = [posx[px-1],posy[py],posx[px],posy[py-1]]

if keyword_set(xspan) then $
  if xspan ge 0 then position_n[2] = position_n[2] + (xspan-1)*dx $
  else if xspan lt 0 then position_n[0] = position_n[0] - (-xspan-1)*dx
if keyword_set(yspan) then $
  if yspan ge 0 then position_n[1] = position_n[1] - (yspan-1)*dy $
  else if yspan lt 0 then position_n[3] = position_n[3] + (-yspan-1)*dy



return,position_n
end
