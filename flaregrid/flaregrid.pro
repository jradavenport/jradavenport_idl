function flaregrid,mag,filter=filter,sptype=sptype
;+
; NAME: FLAREGRID
;
; PURPOSE: output the simulated delta-mag for an M dwarf flare with a
;          given amplitude
;
; CALLING SEQUENCE: flaregrid(mag,[filter=filter,sptype=sptype])
;
; INPUTS:
;          mag = a float vector of input amplitudes for the flare
;
; OPTIONAL INPUTS:
;          filter = the filter number [0 to 7] corresponding to the
;                   input mag's filter [u,g,r,i,z,J,H,K]
;                   DEFAULT = 0
;
;          SpType = the stellar spectral type
;                   DEFAULT = 0
;
; OUTPUTS:
;          delta-mag for each band, in the order:
;          [u,g,r,i,z,J,H,K]
;
;          NB: if you feed it N input mags, you will get back an
;          array with dimensions = [N,8], simulating each flare
;
; EXAMPLE: Simulate a 2.5 mag g-band flare on an M4...
;      IDL> print,flaregrid(2.5,s=4,f=1)
;            5.70135
;            2.50000
;            1.24874
;           0.472029
;           0.217732
;          0.0956094
;          0.0615969
;          0.0501580
;
; MODIFICATION HISTORY:
;          Created - Sept 2011, JRAD
;-


  if not keyword_set(mag) then begin
     print,'you must enter a mag'
     return,-1
  end
;'flaregrid = [sptype,band,coverage]'

if not keyword_set(sptype) then sptype=0 ; default = M0
if not keyword_set(filter) then filter=0 ; default = u band

;==> this must be the absolute path to the flaregrid data
restore,'/Users/james/IDL/jrad/flaregrid/flaregrid.sav'

;new Y = INTERPOL(old Y, old X,new X)
u = interpol(flaregrid[sptype,0,*],flaregrid[sptype,filter,*],mag,/spline)
g = interpol(flaregrid[sptype,1,*],flaregrid[sptype,filter,*],mag,/spline)
r = interpol(flaregrid[sptype,2,*],flaregrid[sptype,filter,*],mag,/spline)
i = interpol(flaregrid[sptype,3,*],flaregrid[sptype,filter,*],mag,/spline)
z = interpol(flaregrid[sptype,4,*],flaregrid[sptype,filter,*],mag,/spline)
j = interpol(flaregrid[sptype,5,*],flaregrid[sptype,filter,*],mag,/spline)
h = interpol(flaregrid[sptype,6,*],flaregrid[sptype,filter,*],mag,/spline)
k = interpol(flaregrid[sptype,7,*],flaregrid[sptype,filter,*],mag,/spline)

out = [[u],[g],[r],[i],[z],[j],[h],[k]]
return,out
end
