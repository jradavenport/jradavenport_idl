; compute the geometric mean of a vector (arr)
FUNCTION GEOMEAN, arr

  ok = where(arr ne 0)
  if ok[0] eq -1 then return,-1

  RETURN, EXP(TOTAL(ALOG(ABS(arr[ok])))/float(N_ELEMENTS(arr[ok])))
END
