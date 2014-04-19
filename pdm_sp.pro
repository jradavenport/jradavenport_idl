pro pdm_sp,t,f,e,p0,p1,dp,periods=periods,chi2=chi2,nbin=nbin
; Phase Dispersion Minimizer - using an interpolation

nper = (p1-p0)/dp
if nper lt 2 then nper = 2.
periods = dindgen(nper)*dp+p0
chi2 = dblarr(nper)

if not keyword_set(nbin) then nbin = 20.
if nbin lt 2 then nbin = 10

npts = float(n_elements(t))

for i=0L,nper-1 do begin
   phase = (t mod periods[i])/periods[i]
   meds = fltarr(nbin)
   cts = fltarr(nbin)
   for n=0L,nbin-1 do begin
      x = where(phase ge float(n)/nbin and phase le float(n+1)/nbin, count)
      if x[0] ne -1 then meds[n] = mean(f[x])
      cts[n] = count
   endfor
   model = interpol(meds[where(cts gt 0)],(findgen(nbin)/nbin)[where(cts gt 0)],phase,/lsqu)
   chi2[i] = total(((f-model)/e)^2.)/(npts-1)
endfor


return
end
