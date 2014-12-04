pro radec_decimal,ra,dec,sep=sep
;+
; NAME: radec_decimal.pro
;
; PURPOSE: Convert RA-Dec coordinates from the common format
;	HR:MIN:SEC +DEG:MIN:SEC to the more useful format of
;	Decimal Degrees for both RA and DEC. This is the format
;	that SDSS uses.
;
; CALLING SEQUENCE: radec_decimal,ra,dec
;
; INPUTS:
;	ra: String array of RA coordinates in HR:MIN:SEC format.
;	dec: String array of DEC coordinates in +DEG:MIN:SEC
;
;	sep: if using some other character to separate 
;
; COMMON PROBLEMS:
;	Input arrays must be strings
;
;	Program assumes that the 3rd character in RA array is
;	the separator used in both arrays. Example: 14:22:36.59982
;	The : would be used to separate the terms in both RA and DEC.
;
;	Progrm assumes spacing between separators is consistant for RA & DEC
;
; AUTHOR: James Davenport, University of Washington, March 2007
;	jrad@astro.washington.edu
;-

;need to add a check for + signs, and add where they are not present in DEC

ra=strtrim(ra,2)
dec=strtrim(dec,2)

if not keyword_set(sep) then S=strmid(ra[0],2,1)
if keyword_set(sep) then S=sep

A=strpos(ra[0],s)
ra1=double(ra)
B=strpos(ra[0],s,A+1)
ra2=double(strmid(ra,A+1,B))
ra3=double(strmid(ra,B+1,12))

ra=ra1*15.+ra2*15./60+ra3*15./3600.

A=strpos(dec[0],s)
dec1=double(dec)
B=strpos(dec[0],s,A+1)
dec2=double(strmid(dec,A+1,B))
dec3=double(strmid(dec,B+1,12))

sn=double(string(strmid(dec,0,1)+'1'))

dec=sn*abs(dec1)+sn*dec2/60.+sn*dec3/3600.

return
end
