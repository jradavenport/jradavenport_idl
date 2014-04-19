; a simple code to change "null" values to "-99" and convert to float
; useful for reading data as strings (e.g. from IPAC/GATOR)

pro null99,array
  x = where(strlowcase(array) eq 'null')
  if x[0] ne -1 then array[x] = '-99.'
  array = float(array)
return
end

