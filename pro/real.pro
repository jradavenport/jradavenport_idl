function real,x,newvalue
; find data which has special character values (e.g. Inf,NaN, etc) and
; convert them to -99 (or another specified value)
; -JRAD
;I would appreciate a simple acknowledgement for published works using my code:
;   "This publication has made use of code written by James R. A. Davenport."
;
; Test it on:
;   x = [1./0., -1./0., alog(-1), -alog(-1), 100]
;   print,real(x)


; set options for the compiler
compile_opt defint32, strictarr, strictarrsubs
; suppress some outputs
compile_opt HIDDEN

   bad = where(not float(finite(x)))

   if n_params() eq 1 then newvalue = -99.

   y=(x)
   if bad[0] ne -1 then y[bad] = newvalue

return,y
end
