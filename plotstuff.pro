pro plotstuff,set=set,default=default,silent=silent
;This program prints to the screen things to include in plotting publish-
;quality data. I use this so that I don't need to go look it up every time I
;have to use it!
;
; if /set, then it actually applies it
;
; usually in the start of my programs I have these lines:
; > set_plot,'X'
; > plotstuff,/set,/silent
;
;  WARNING:
;    device,decomposed=0 will crash your code if not in the X device!
;
;  update (2014) to use !P.font=0 rather than -1
;  because the fonts look much nicer printed, and are searchable
;  in figures. font=-1 are vectors, and have much larger symbol set
;  so, still see those hiding in my plots occasionally...
;
if not keyword_set(silent) then begin
print,'Copy this into your code...'
print,'    device,decomposed = 0'
print,'    device, retain = 2'
print,'    device, true_color = 24'
print,'    !p.thick=2'
print,'    !x.thick=2'
print,'    !y.thick=2'
print,'    !p.charthick=2.5'
print,'    !p.font=0'
print,'    !p.charsize=1.6'
print,''
endif

;set for good postcript output
if keyword_set(set) then begin
    device,decomposed=0
    device, retain = 2
    device, true_color = 24
    !p.thick=2
    !x.thick=2
    !y.thick=2
    !p.charthick=2.5
    !p.font=0
    !p.charsize=1.6
endif




;set for good X window output.. i guess
if keyword_set(default) then begin
    !p.thick=1
    !x.thick=1
    !y.thick=1
    !p.charthick=1
    !p.font=-1
    !p.charsize=1
endif


return
end
