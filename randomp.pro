
;+
; RANDOMP: generate random numbers from a powerlaw distribution
;
; CALLING SEQUENCE: 
;     output = randomp(seed, N_rand, slope, min=min, max=max)
;
; INPUTS:
;     seed = random number seed (passed to RANDOMU)
;     N_rand = # of points to generate
;     slope = powerlaw slope (default = -2)
;
; OPTIONAL INPUTS:
;     min = smallest limit (default = 0.1)
;     max = largest limit (default = 10.)
;
; VERSION:
;     - (April 2014) written by @jradavenport, based on
;                    http://mathworld.wolfram.com/RandomNumber.html
;-

function randomp,seed,n_rand,m,min=min,max=max
  compile_opt defint32, strictarr, strictarrsubs
  compile_opt HIDDEN
  On_error,2

  if N_PARAMS() LT 1 then begin
     print,'% RANDOMP: Incorrect number of arguments'
     return,-1
  endif

  if not keyword_set(m) then m = -2.
  n=m-1.

  if not keyword_set(n_rand) then n_rand = 1
  if not keyword_set(min) then min = 0.1
  if not keyword_set(max) then max = 10.

  p=((max^(n+1.) - min^(n+1.))*randomu(seed,n_rand) + min^(n+1.))^(1./(n+1.))
  return,p
end
