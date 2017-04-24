pro astro_const,silent=silent,set=set
;
; DESCRIPTION:
;
; This program contains a large list of physical/useful constants
; which commonly come up in astrophysics. These are set to system
; variables for handy/quick use in code or homework.
;
; NOTE: All constants are in CGS units, unless otherwise noted.
;
; USAGE:
;  To input the system variables into your session, type:
;  IDL> astro_const,/set
;
;  To view the system variables available, type:
;  IDL> astro_const
;
;
; EXAMPLE:
;   IDL> print, !G
;     6.6725900e-08
;   IDL> help,!G
;   <Expression>    DOUBLE    =    6.6725900e-08
;
; AUTHOR: J. R. A. Davenport (Nov 2009)
;
; UPDATES:
;   added !s2yr  (JRAD, Jan 2010)
;   added !N_A, !R, and planets (JRAD, April 2010)
;   added !angstrom (JRAD, Oct 2010)
;   added !k_pm (JRAD, Feb 2011)
;   added !Jy (JRAD, May 2011)
;

;IF KEYWORD_SET(SET) THEN BEGIN
   IF NOT KEYWORD_SET(SILENT) THEN  $
      PRINT,'SETTING COMMONLY USED CONSTANTS (in cgs units)'
;fundamentals
   defsysv,'!c0',2.99792458d10        ; cm/s (speed of light)
; ---> note !c is already taken... unfortunately
   defsysv,'!m_H',1.673534d-24        ; g (mass hydrogen)
   defsysv,'!m_p',1.6726231d-24       ; g (mass of proton)
   defsysv,'!m_n',1.674929d-24        ; g (mass of neutron)
   defsysv,'!m_e',9.10938188d-28      ; g (mass of electron)
   defsysv,'!r_e',2.8179402894d-13    ; cm (classical radius of electron)
   defsysv,'!g_accel',9.78033d2       ; cm/s^2 (gravitational acceleration)
   defsysv,'!k_B',1.380658d-16        ; erg/K = g cm^2/s^2/K (boltzmann const)
   defsysv,'!h',6.626068d-27          ; cm^2 g /s (planck const)
   defsysv,'!sigma',5.67051d-5        ; erg/cm^2/s/K^4 (stefan-boltzmann)
   defsysv,'!a',7.5657d-15            ; erg/cm^3/K^4 (radiation constant)
   defsysv,'!R',8.314472d7            ; erg/K/mol (Gas Constant)
   defsysv,'!N_A',6.02214179d23       ; /mol (Avagadro constant)
   defsysv,'!G',6.67259d-8            ; cm^3/g/s^2 (Gravitational const)
   defsysv,'!u',1.66053886d-24        ; g (atomic mass unit)               
   defsysv,'!M_sun',1.98892d33        ; g
   defsysv,'!M_mercury',3.03022d26    ; g
   defsysv,'!M_venus',4.86900d27      ; g
   defsysv,'!M_earth',5.974d27        ; g
   defsysv,'!M_moon',7.36d25          ; g
   defsysv,'!M_mars',6.4191d26        ; g
   defsysv,'!M_jupiter',1.8986d30     ; g
   defsysv,'!R_sun',6.955d10          ; cm
   defsysv,'!R_mercury',2439.7d5      ; cm
   defsysv,'!R_venus',6051.8d5        ; cm
   defsysv,'!R_earth',6.3781d8        ; cm
   defsysv,'!R_moon',1737.1d5         ; cm
   defsysv,'!R_mars',3396.2d5         ; cm
   defsysv,'!R_jupiter',71492d5       ; cm equatorial, (polar=66854d5 cm)
   defsysv,'!L_sun',3.826d33          ; egs/s
   defsysv,'!Teff_sun',5778d0         ; K (solar effective temp)
   defsysv,'!AU',1.49598d13           ; cm (Astronomical Unit)
   defsysv,'!pc',3.08568025d18        ; cm (parsec)
   defsysv,'!pc2AU',206264.806d0      ; AU / parsec
   defsysv,'!pc2lyr',3.26163626d0     ; Light year / parsec
   defsysv,'!s2yr',31556926d0         ; seconds/year
   defsysv,'!sigma_T',6.6524586d-25   ; cm^2 (Thomson Scattering Cross Section)
   defsysv,'!e_esu',4.80325d-10       ; esu (electron charge)
   defsysv,'!e_coulomb',1.6021892d-19 ; coulomb (electron charge)
   defsysv,'!eV',1.60217733d-12       ; erg
   defsysv,'!k_pm',4.74057581d0       ; 1 AU/year in km/s (PM->V_tan)
   defsysv,'!angstrom', $             ; the Angstrom Symbol - D.Fanning
           '!6!sA!r!u!9 %!6!n'
   defsysv,'!Jy',1d-23                ; 1 Jansky in erg/s/cm^2/Hz  
;ENDIF 



IF (NOT KEYWORD_SET(silent)) THEN BEGIN
   FMT = '(I3,A12,E14.4,A35)'
   print,'ASTRO_CONST> Here are the physical constant system variables:'
   print,f=fmt,1,'!c0',2.99792458d10        ,' cm/s (speed of light)'
   print,f=fmt,2,'!m_H',1.673534d-24        ,' g (mass hydrogen)'
   print,f=fmt,3,'!m_p',1.6726231d-24       ,' g (mass of proton)'
   print,f=fmt,4,'!m_n',1.674929d-24        ,' g (mass of neutron)'
   print,f=fmt,5,'!m_e',9.10938188d-28      ,' g (mass of electron)'
   print,f=fmt,6,'!r_e',2.8179402894d-13    ,' cm (classical radius of electron)'
   print,f=fmt,7,'!g_accel',9.78033d2       ,' cm/s^2 (gravitational acceleration)'
   print,f=fmt,8,'!k_B',1.380658d-16        ,' erg/K = g cm^2/s^2/K (boltzmann const)'
   print,f=fmt,9,'!h',6.626068d-27          ,' cm^2 g /s (planck const)'
   print,f=fmt,10,'!sigma',5.67051d-5        ,' erg/cm^2/s/K^4 (stefan-boltzmann)'
   print,f=fmt,11,'!a',7.5657d-15            ,' erg/cm^3/K^4 (radiation constant)'
   print,f=fmt,12,'!R',8.314472d7            ,' erg/K/mol (Gas Constant)'
   print,f=fmt,13,'!N_A',6.02214179d23       ,' /mol (Avagadro constant)'
   print,f=fmt,14,'!G',6.67259d-8            ,' cm^3/g/s^2 (Gravitational const)'
   print,f=fmt,15,'!u',1.66053886d-24        ,' g (atomic mass unit)'
   print,f=fmt,16,'!M_sun',1.98892d33        ,' g'
   print,f=fmt,17,'!M_mercury',3.03022d26    ,' g'
   print,f=fmt,18,'!M_venus',4.86900d27      ,' g'
   print,f=fmt,19,'!M_earth',5.974d27        ,' g'
   print,f=fmt,20,'!M_moon',7.36d25          ,' g'
   print,f=fmt,21,'!M_mars',6.4191d26        ,' g'
   print,f=fmt,22,'!M_jupiter',1.8986d30     ,' g'
   print,f=fmt,23,'!R_sun',6.955d10          ,' cm'
   print,f=fmt,24,'!R_mercury',2439.7d5      ,' cm'
   print,f=fmt,25,'!R_venus',6051.8d5        ,' cm'
   print,f=fmt,26,'!R_earth',6.3781d8        ,' cm'
   print,f=fmt,27,'!R_moon',1737.1d5         ,' cm'
   print,f=fmt,28,'!R_mars',3396.2d5         ,' cm'
   print,f=fmt,29,'!R_jupiter',71492d5       ,' cm equatorial, (polar=66854d5 cm)'
   print,f=fmt,30,'!L_sun',3.826d33          ,' egs/s'
   print,f=fmt,31,'!Teff_sun',5778d0         ,' K (solar effective temp)'
   print,f=fmt,32,'!AU',1.49598d13           ,' cm (Astronomical Unit)'
   print,f=fmt,33,'!pc',3.08568025d18        ,' cm (parsec)'
   print,f=fmt,34,'!pc2AU',206264.806d0      ,' AU / parsec'
   print,f=fmt,35,'!pc2lyr',3.26163626d0     ,' Light year / parsec'
   print,f=fmt,36,'!s2yr',31556926d0         ,' seconds/year'
   print,f=fmt,37,'!sigma_T',6.6524586d-25   ,' cm^2 (Thomson Scattering Cross Section)'
   print,f=fmt,38,'!e_esu',4.80325d-10       ,' esu (electron charge)'
   print,f=fmt,39,'!e_coulomb',1.6021892d-19 ,' coulomb (electron charge)'
   print,f=fmt,40,'!eV',1.60217733d-12       ,' erg'
   print,f=fmt,41,'!k_pm',4.74057581d0       ,' 1 AU/year in km/s (PM->V_tan)'
   print,f=fmt,42,'!angstrom',-1,' the Angstrom Symbol - D.Fanning'
   print,f=fmt,43,'!Jy',1d-23,'1 Jansky in erg/s/cm^2/Hz'
ENDIF

return
end
