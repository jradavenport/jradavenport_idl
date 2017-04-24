function gerror,n
;+
; Calculates the symmetric Poisson eror for small N limit
; as defined by Gehrels 1986 ApJ, 3030, 336
; 
; sigma'(n) ~ 1+sqrt(n+0.75)
;
; Written up by JRAD
;-
print,'use perror if possible'
  return,1. + sqrt(n+0.75)
end
