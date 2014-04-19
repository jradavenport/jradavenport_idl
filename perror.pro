function perror,N
;+
; Calculate the asymmetric Poisson error, using Eqn 7 
; and Eqn 12 in Gehrels 1986 ApJ, 3030, 336
;
; Written up by JRAD
;-

  return,[[n*(1.-1./(9.*n)-1./(3.*sqrt(n)))^3.-n],[n+sqrt(n+0.75)+1.0-n]]
end
