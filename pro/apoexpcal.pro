;---------------
; +
;  Simple Exposure Time Calculator for Apache Point Observatory (APO) 3.5m telescope
;  NOTE: using very approximate values! Do not plan your observing run solely on this tool!
;
; EXAMPLE USE:
;    idl> apoexpcal, 'dis', 14.5, 10. 
; -
pro apoexpcal,inst,mag,s2n,filter

if not keyword_set(inst) then begin
   inst = 'spicam'
   print,'Which Instrument?'
   read,inst,prompt='[dis,echelle,spicam,tspec]: '
endif
inst = strlowcase(inst)

if not keyword_set(mag) then begin
   mag  = 15.0
   read,mag,prompt='What apparent magnitude? '
endif

if not keyword_set(s2n) then begin
   s2n  = 100.0
   read,s2n,prompt='What Signal/Noise? '
endif

if not keyword_set(filter) then begin
   filter = 'u'
   if inst eq 'spicam' then read,filter,prompt='What Filter? '
endif
print,''
print,'Signal/Noise = ',s2n
;%%%%%%%%%%%% SPICAM %%%%%%%%%%%%%%%%%
; these values are from the website
if inst eq 'spicam' then begin
print,'SPICAM: '+filter+' filter, '+strtrim(string(mag),2)+' mag'
   spi_filter = ['u','g','r','i','z']
   spi_c = [21.38,24.92,24.93,24.72,23.43]
   spi_K = [.48,.19,.11,.04,.06]

   n=where(spi_filter eq filter)
   spi_flux = 10d0^((mag-spi_c[n])/(-2.5))

; this is the MOST simple case: S/N = sqrt(Counts)
   exptime = s2n^2./spi_flux
   print,'t = ',exptime,' sec'
   return
endif

;%%%%%%%%%%%% DIS %%%%%%%%%%%%%%%%%
; these values are from the website
if inst eq 'dis' then begin
   print,'DIS: '+strtrim(string(mag),2)+' mag'
   exptime_b = (s2n/(3700. * 10^(-0.2*mag)) )^2. ; blue
   exptime_r = (s2n/(2300. * 10^(-0.2*mag)) )^2. ; red
   print,'t(r300) = ',exptime_r,' sec'
   print,'t(b400) = ',exptime_b       ,' sec' ; per Angstrom
   return
endif


;%%%%%%%%%%%% Echelle %%%%%%%%%%%%%%%%%
; this is extrapolated from a summary of John W. data
; V = 3.86, 8k counts in 30sec => c = 9.93
;
; let's try w/ data from Adam (via John)
; V = 1.9 (B3V), t=4sec, 12,000 peak counts => c=10.59
; V = 11.2(GV), t=1200s, 2250 peak counts => c=11.88
if inst eq 'echelle' then begin
   print,'Echelle: '+strtrim(string(mag),2)+' mag'
   echelle_c = 11.88;10.59
   echelle_flux = 10d0^((mag-echelle_c)/(-2.5))
   exptime = s2n^2./echelle_flux
   print,'t = ',exptime,' sec'
   return
endif

;%%%%%%%%%%%% Triple Spec %%%%%%%%%%%%%%%%%
; this is made up from loose estimates of data Giada gave me
; #FS=4, t=20s, m=6.3 (all JHK), average counts somewhere around 2800
; using the star HD 19600 (A0V)
if inst eq 'tspec' then begin
   print,'TripleSpec: '+strtrim(string(mag),2)+' mag, FS=4'
   tspec_c = 11.67
   tspec_flux = 10d0^((mag-tspec_c)/(-2.5))
   exptime = s2n^2./tspec_flux
   print,'t = ',exptime,' sec'
   return
endif

return
end
