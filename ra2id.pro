;+
; NAME: ra2id
;
; PURPOSE: convert RA,Dec (decimal degrees) to ID names used by 2MASS, etc
;
; CALLING SEQUENCE:
;     IDL> id = RA2ID(ra,dec)
;
; INPUTS:
;     ra = float vector in units of decimal degrees (0-360)
;     dec = float vector in units of decimal degrees (-90,90)
;
;     NOTE: routine uses stupid FOR loop over RA,Dec arrays. Will be
;           slow for very large arrays.
;
; OUTPUTS:
;     id = string with format HHMMSS.SS+DDMMSS.S
;          will have a "J" in front, e.g.
;          "J123456.7-012345.6"
;
; MODIFICATION HISTORY:
;    written by @jradavenport July 2014
;-
function ra2id, ra, dec

; set options for the compiler
  compile_opt defint32, strictarr, strictarrsubs
; suppress some outputs
  compile_opt HIDDEN

  On_error,2
  if N_params() LT 2 then begin   
     print, 'Error: must provide RA and DEC'
     print,'id = RA2ID(ra,dec)'
     return,'ERROR: Need RA,DEC'
  endif

  if n_elements(ra) ne n_elements(dec) then begin
     print,'Error: RA and DEC must have same length'
     return,'ERROR: len RA != DEC'
  endif

  ID = strarr(n_elements(ra))

  for n=0l,n_elements(ra)-1 do begin
     rtmp = SIXTY(ra[n]/15.)
     dtmp = SIXTY(abs(dec[n]))

     if dec[n]/abs(dec[n]) lt 0 then sign='-' else sign='+'

     ID[n] = 'J' + $
             STRING(rtmp[0],f='(I02)') + $
             STRING(rtmp[1],f='(I02)') + $
             STRING(rtmp[2],f='(F05.2)') + $
             sign + $
             STRING(dtmp[0],f='(I02)') + $
             STRING(dtmp[1],f='(I02)') + $
             STRING(dtmp[2],f='(F04.1)')             
  endfor

  return,ID
end
