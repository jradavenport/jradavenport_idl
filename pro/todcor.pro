; TODCOR = TwO Dimensional CORrelation
;
; Based on the algorithm presented in 
; Zucker & Mazeh 1994 ApJ 420 806
;
; Coded hastily into IDL by JRAD

;function todcor,f,g1,g2
function todcor,w_f,f,w_g1,g1,w_g2,g2,wrange=wrange,nshift=nshift,R=R,dx=dx,aa=aa,silent=silent
;;    w_X = wavelength, X = flux
;;    f = object
;;    g# = template#

;--------- remap wavelengths ---------
if n_elements(wrange) eq 2 then begin
   wmin = wrange[0]
   wmax = wrange[1]
endif
if n_elements(wrange) ne 2 then begin
   wmin = min(w_f)
   wmax = max(w_f)
endif

wdel = w_f[1]-w_f[0]
a = alog10(wmin)
b = alog10(wdel/wmin+1.)
; the log-linear wavelength space for the target data
wlog = 10.^(findgen(n_elements(w_f))*b+a)
wlog = wlog[where(wlog lt wmax)]
; the velocity of each log-linear wavelength pixel
velpix = 2d0 * !c0 * (wlog[100]-wlog[99])/(wlog[100]+wlog[99]) 

fr  = interpol(f,w_f,wlog)
g1r = interpol(g1,w_g1,wlog)
g2r = interpol(g2,w_g2,wlog)
cubehelix

  if not keyword_set(nshift) then nshift=100.
  nx = nshift
;nx = 100.
  ny = nx
; make big 2-D grid to stuff cross-correlation into
  R = fltarr(nx*2.,ny*2.)

  if not keyword_set(dx) then dx=1.

;== do the individual 1D cross correlations ==
lag = (findgen(nx*2+1)-nx)*dx
;; c1 = c_correlate(fr,g1r,lag) ; use IDL's built-in CCF in 1-D
;; c2 = c_correlate(fr,g2r,lag)
c1 = c_correlate(g1r,fr,lag)
c2 = c_correlate(g2r,fr,lag)

;c12= c_correlate(g1,g2,lag)
lag12 = (findgen(4.*nx+1)-2.*nx)*dx
N = float(n_elements(g1r))
sig_g1 = sqrt(total(g1r^2.)/N)
sig_g2 = sqrt(total(g2r^2.)/N)
;c12 = c_correlate(g1r,g2r,lag12); / (N*sig_g1*sig_g2)
c12 = c_correlate(g2r,g1r,lag12); / (N*sig_g1*sig_g2)

if not keyword_set(aa) then aa=1
alpha = sig_g2/sig_g1 * aa

FOR i=0,2*nx-1 DO BEGIN
   FOR j=0,2*ny-1 DO BEGIN
;      R[i,j] = sqrt((c1[i]^2. -2.*c1[i]*c2[j]* c12[2*nx+(i-j)] +c2[j]^2.)/$
;                    (1.-c12[2*nx+(i-j)]^2.) )
      R[i,j] = (c1[i] + alpha*c2[j])/sqrt(1+2*alpha*c12[2*nx+(j-i)]+alpha^2.)
  ENDFOR
ENDFOR

contour,real(R),/fill,nlevel=20,/xstyle,/ystyle,position=posgen(1,4,2,ysp=3),xtitle='g1 shift (pixels)',ytitle='g2 shift (pixels)'
oplot,[nx,nx],[0,2*nx]
oplot,[0,2*nx],[nx,nx]

 rmax = max(r,b) ; b is subscript of max position
 vx = b mod (2*nx)
 vy = fix(b/(2*nx))
 oplot,[vx],[vy],psym=6,color=150


plot,wlog,fr,/xstyle,/ystyle,/noerase,position=posgen(1,4,1),title='wavelength'

if not keyword_set(silent) then $
   print,'TODCOR: ',[vx-nx,vy-nx]*velpix/1d5,' km/s'


return,[vx-nx,vy-ny]*velpix/1d5
end
