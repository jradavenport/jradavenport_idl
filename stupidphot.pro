;+
; NAME: STUPIDPHOT - Stupidly Simple Photometry
;
; PURPOSE:
;        Designed to do simple differential aperture photometry, and
;        thus useful for bright sources in non-crowded fields. Adjusts
;        star centers with each image, but cannot handle big jumps
;        (yet...)
;
;
; CALLING SEQUENCE: 
;        stupidphot, imagelist [, /display, /reduce, ncomp=ncomp,
;        flatlist=flatlist, biaslist=biaslist, darklist=darklist,
;        doneflat = doneflat, donedark = donedark, coord=coord ]
;
; INPUTS:
;        imagelist - string name of text file list of target images
;
; OPTIONAL INPUTS:
;        ncomp - integer number of comparison stars [default = 2]
;
;        flatlist - string name of list of flat images to combine
;        doneflat - string name of combined flat image to use
;        biaslist - string name of list of bias images to combine
;        darklist - string name of list of dark images to combine
;        donedark - string name of combined dark image to use
;
;        coord - string name of file containing X,Y coordinates for
;                stars to analyze. The first star must be the target
;                If used, STUPIDPHOT will not prompt for target
;                identification.
;
; KEYWORD PARAMETERS:
;        /display - show every image
;        /reduce - write the flat & bias reduced images.
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:
;        If flatlist is used, STUPIDPHOT will write flat.fits.
;        If darklist or biaslist is used, STUPIDPHOT will write
;             zero.fits.
;
; EXAMPLE:
;        IDL> stupidphot,'image.list', /reduce, ncomp=3
;
; MODIFICATION HISTORY:
;
;-

; ---to do----
; - if target star isnt recovered properly in search box, reset coords 
; - make output cleaner
; - save all guassfit stuff 
; - estimate FWHM from the first image...
;     - measure gaussian after user clicks target star
;     - mean profile = stddev
;     - FWHM = 2.35*stddev

function flatcombine,flatlisfile
  readcol,flatlisfile,flatlis,f='(A)',/silent
  print,n_elements(flatlis),' flat images to stupidly combine'
  im = mrdfits(flatlis[0],/silent,/dscale)
  im = im/mean(im,/nan,/double)
  for n=1L,n_elements(flatlis)-1 do begin
;     print,n
     im_tmp = mrdfits(flatlis[n],/silent,/dscale)
     im = im + im_tmp/mean(im_tmp,/double,/nan)
  endfor
  im = im/mean(im,/double,/nan)
  writefits,'flat.fits',im
  return,im
end

function zerocombine,flatlisfile
  readcol,flatlisfile,flatlis,f='(A)',/silent
  print,n_elements(flatlis),' bias images to stupidly combine'
  im = mrdfits(flatlis[0],/silent,/dscale)
  for n=1L,n_elements(flatlis)-1 do begin
;     print,n
     im_tmp = mrdfits(flatlis[n],/silent,/dscale)
     im = im + im_tmp
  endfor
  im = im/float(n_elements(flatlis))
  writefits,'zero.fits',im
  return,im
end


pro resetcoords_man,img,ncomp,xx,yy
  im = mrdfits(img,0,/silent,/dscale)
  imsz = size(im)
  cubehelix
  plot,[0],xrange=[0,imsz[1]],yrange=[0,imsz[2]],/xsty,/ysty,/nodata,position=[.1,.1,.95,.95],xtitle='X (pixel)',ytitle='Y (pixel)'
  tvimage,(smooth(im,[1,1],/edge,/nan)),position=[.1,.1,.95,.95]
  print,'> select target'
  x1=1
  y1=1
  CURSOR,x1,y1,/down,/data
  oplot,COS(FINDGEN(17) * (!PI*2/16.))*10+x1, SIN(FINDGEN(17) * (!PI*2/16.))*10+y1,color=250,thick=4
  
  xc = 0
  yc = 0
  for n=0L,ncomp-1 do begin
     print,'> select comparison ',n+1
     xx=1
     yy=1
     CURSOR,xx,yy,/down,/data
     xc = [xc,xx]
     yc = [yc,yy] 
     oplot,COS(FINDGEN(17) * (!PI*2/16.))*10+xx, SIN(FINDGEN(17) * (!PI*2/16.))*10+yy,color=200,thick=4
  endfor
  remove,0,xc,yc
  xx = [x1,xc] 
  yy = [y1,yc]
  return
end

;-=-=-=-=-=-;-=-=-=-=-=-;-=-=-=-=-=-;-=-=-=-=-=-


pro resetcoords,img,xx,yy,fwhm = fwhm
  if not keyword_set(fwhm) then fwhm = 5.0
  im = mrdfits(img,0,/silent,/dscale)
  imsz = size(im)
  for i=0L,n_elements(xx)-1 do begin
      tmpcoord = [(xx[i]-5*fwhm),(xx[i]+5*fwhm),(yy[i]-5*fwhm),(yy[i]+5*fwhm)]
      if tmpcoord[0] lt 0 then tmpcoord[0] = 0
      if tmpcoord[2] lt 0 then tmpcoord[2] = 0
      if tmpcoord[1] ge imsz[1] then tmpcoord[1] = imsz[1]
      if tmpcoord[3] ge imsz[2] then tmpcoord[3] = imsz[2]

      imtest = im[tmpcoord[0]:tmpcoord[1],tmpcoord[2]:tmpcoord[3]]
      gtest = GAUSS2DFIT(imtest,Atest, /tilt)
      stop
      xx[i] = atest[4] - 5.*fwhm + xx[i]
      yy[i] = atest[5] - 5.*fwhm + yy[i]
   endfor
  return
end
;-=-=-=-=-=-;-=-=-=-=-=-;-=-=-=-=-=-;-=-=-=-=-=-


pro stupidphot,imagelist,display=display,ncomp=ncomp,flatlist=flatlist,biaslist=biaslist,darklist=darklist,doneflat = doneflat,donedark = donedark,reduce=reduce,coord=coord

if n_params() lt 1 then begin
   print,'Error> need to include image list'
   print,'STUPIDPHOT, imagelist, /display, ncomp=ncomp, flatlist=flatlist, biaslist=biaslist, darklist=darklist, doneflat = doneflat, donedark = donedark, reduce=reduce,coord=coord'
   return
endif

;; if not keyword_set(imagelist) then begin
;;    spawn,'ls *.fits > images.lis'
;;    imagelist = 'images.lis'
;; endif

if not keyword_set(ncomp) then ncomp = 2.

print,'STUPIDPHOT, imagelist, /display, ncomp=ncomp, flatlist=flatlist, biaslist=biaslist, darklist=darklist, doneflat = doneflat, donedark = donedark, reduce=reduce,coord=coord'
print,''


APERTURE = 15.   ; for ap phot
SKYY = [28,38]   ; inner and outer rad
SMBOX = 15       ; search box side length to match each frame over
FWHM = 5.        ; approx FWHM
timekey = 'DATE-OBS' ; 'UTCSTAMP'
print,'>> using header keyword ',timekey
ROUNDLIM = [-1.,1.]   ; i wouldnt change this
SHARPLIM = [0.2,1.1]  ; i wouldnt change this



; read the first image
; show it
; pick the stars you want interactively (cursor)
; for every image run: FIND, APER, extract time
; match stupidly to the x,y master list, using dumb small-big box
; if target star is not found, show this image and re-pick

readcol,imagelist,f='(A)',images,/silent


im = (mrdfits(images[0],0,hdr,/silent,/dscale))
time = sxpar(hdr,timekey)
imsz = size(im)

bias = 0d0
if keyword_set(zerolist) and not keyword_set(darklist) and not keyword_set(donedark) then begin
   print,'COMBINING BIASES... BE PATIENT'
   bias = zerocombine(zerolist)
endif
if keyword_set(darklist) and not keyword_set(donedark) then begin
   print,'COMBINING DARKS... BE PATIENT'
   bias = zerocombine(darklist)
endif
if keyword_set(donedark) then bias = mrdfits(donedark,0,/silent,/dscale)

flat = 1d0
if keyword_set(flatlist) and not keyword_set(doneflat) then begin
   print,'COMBINING FLATS... BE PATIENT'
   flat = flatcombine(flatlist)
endif
if keyword_set(doneflat) then flat = mrdfits(doneflat,0,/silent,/dscale)

im = (im - bias)/flat

loadct,0,/silent

if not keyword_set(coord) or keyword_set(display) then begin
   plot,[0],xrange=[0,imsz[1]],yrange=[0,imsz[2]],/xsty,/ysty,/nodata,position=[.1,.1,.95,.95],xtitle='X (pixel)',ytitle='Y (pixel)'
   tvimage,(smooth(im,[1,1],/edge,/nan)),position=[.1,.1,.95,.95]

   print,''
   print,n_elements(images),' images to process...'
   print,''
   print,'> size your window, then click in the window'
   clkx = 1
   clky = 1

   CURSOR,clkx,clky,/down,/data
endif

if not keyword_set(coord) then begin
   cubehelix
   plot,[0],xrange=[0,imsz[1]],yrange=[0,imsz[2]],/xsty,/ysty,/nodata,position=[.1,.1,.95,.95],xtitle='X (pixel)',ytitle='Y (pixel)'
   tvimage,smooth(im,[5,5],/edge,/nan),position=[.1,.1,.95,.95]
   print,''
   
   find,im,xtmp,ytmp,fluxf,sharpf,rndf, median(im)+stddev(im,/nan)*3., fwhm ,roundlim, sharplim,/monitor,/silent
   loadct,39,/silent
   oplot,xtmp,ytmp,psym=6,color=150,thick=5
   
   print,'> select target'
   x1=1
   y1=1
   CURSOR,x1,y1,/down,/data
   oplot,COS(FINDGEN(17) * (!PI*2/16.))*10+x1, SIN(FINDGEN(17) * (!PI*2/16.))*10+y1,color=250,thick=4
;==============
   xc = 0
   yc = 0
   for n=0L,ncomp-1 do begin
      print,'> select comparison ',n+1
      xx=1
      yy=1
      
      CURSOR,xx,yy,/down,/data
      xc = [xc,xx]
      yc = [yc,yy]
      
      oplot,COS(FINDGEN(17) * (!PI*2/16.))*10+xx, SIN(FINDGEN(17) * (!PI*2/16.))*10+yy,color=200,thick=4
   endfor
   remove,0,xc,yc

   xx = [x1,xc]  ; the target and the comparison stars
   yy = [y1,yc]
endif 
if keyword_set(coord) then readcol,coord,xx,yy,f='(F)',/silent
ncomp = n_elements(xx)-1.

; now primary loop
outmag = fltarr(n_elements(images),ncomp+1)-99.
outerr = fltarr(n_elements(images),ncomp+1)-99.
timeout = dblarr(n_elements(images)) - 99.d0
gflux =  fltarr(n_elements(images),ncomp+1)-1.
gferr =  fltarr(n_elements(images),ncomp+1)-1.
fwhmout = dblarr(n_elements(images))

   tmparr = '(A, '
   for v=0L,2.*ncomp do tmparr = tmparr+'D,'

close,/all
openw,1,imagelist+'.out'
nimage = n_elements(images)
for n=0L,nimage-1 do begin
;   run find,aper,extract time
   print,n,' / ',nimage-1
   im = (mrdfits(images[n],0,hdr,/silent,/dscale) - bias) / flat
   if keyword_set(reduce) then writefits,'reduced_'+images[n],im,hdr

   Time = sxpar(hdr,timekey)
   imsz = size(im)

   if keyword_set(display) then begin
      cubehelix
      plot,[0],xrange=[0,imsz[1]],yrange=[0,imsz[2]],/xsty,/ysty,/nodata,position=[.1,.1,.95,.95],xtitle='X (pixel)',ytitle='Y (pixel)',title=images[n]
      tvimage,im,position=[.1,.1,.95,.95]

   endif

; find and phot
   find,im,xf,yf,fluxf,sharpf,rndf, median(im)+stddev(im,/nan)*3., fwhm ,roundlim, sharplim,/monitor,/silent
   ;; aper,im,xf,yf,mag,err,sky,skyerr,1,APERTURE,SKYY,readnoise=1,/silent,/nan

;   loadct,39,/silent
;   oplot,xf,yf,psym=6,color=150,thick=4

   for i=0L,ncomp do begin
      mt = where(xf gt xx[i]-smbox and xf lt xx[i]+smbox and yf gt yy[i]-smbox and yf lt yy[i]+smbox)
; just choose the first match, if any
      if mt[0] ne -1 then begin
         ;; outmag[n,i] = mag[mt]
         ;; outerr[n,i] = err[mt]

         xx[i] = xf[mt[0]] ; update coords
         yy[i] = yf[mt[0]]
      endif
      
; -- update: even if find DIDNT fid the right stars, still run aper on
;    the last known position
      aper,im,xx[i],yy[i],mag,err,sky,skyerr,1,APERTURE,SKYY,readnoise=1,/silent,/nan
      outmag[n,i] = mag
      outerr[n,i] = err

; play with fitting a 2D gaussian to each star
; these fit coefficients could be useful later on!
      tmpcoord = [(xx[i]-5*fwhm),(xx[i]+5*fwhm),(yy[i]-5*fwhm),(yy[i]+5*fwhm)]
      if tmpcoord[0] lt 0 then tmpcoord[0] = 0
      if tmpcoord[2] lt 0 then tmpcoord[2] = 0
      if tmpcoord[1] ge imsz[1] then tmpcoord[1] = imsz[1]
      if tmpcoord[3] ge imsz[2] then tmpcoord[3] = imsz[2]

      imtest = im[tmpcoord[0]:tmpcoord[1],tmpcoord[2]:tmpcoord[3]]
      gtest = GAUSS2DFIT(imtest,Atest, /tilt)      
      gflux[n,i] = total(gtest-atest[0])
      gferr[n,i] = sqrt(mean((gtest-imtest)^2.)) 

      if keyword_set(display) then begin
         loadct,39,/silent
         oplot,xx,yy,psym=6,color=150,thick=3
         wait,.1
      endif
   endfor

   if total(gflux[n,*]) le 0 then begin
      print,'OH NO! stuff isnt where it should be...'
      resetcoords_man,images[n],ncomp,xx,yy
   ;;    resetcoords,images[n],ncomp,xx,yy
       for i=0L,ncomp do begin
          aper,im,xx[i],yy[i],mag,err,sky,skyerr,1,APERTURE,SKYY,readnoise=1,/silent,/nan
          outmag[n,i] = mag
          outerr[n,i] = err
   ;;       tmpcoord = [(xx[i]-5*fwhm),(xx[i]+5*fwhm),(yy[i]-5*fwhm),(yy[i]+5*fwhm)]
   ;;       if tmpcoord[0] lt 0 then tmpcoord[0] = 0
   ;;       if tmpcoord[2] lt 0 then tmpcoord[2] = 0
   ;;       if tmpcoord[3] ge imsz[1] then tmpcoord[3] = imsz[1]
   ;;       if tmpcoord[4] ge imsz[2] then tmpcoord[4] = imsz[2]
         
   ;;       imtest = im[tmpcoord[0]:tmpcoord[1],tmpcoord[2]:tmpcoord[3]]
   ;;       gtest = GAUSS2DFIT(imtest,Atest, /tilt)      
   ;;       gflux[n,i] = total(gtest-atest[0])
       endfor
   endif

   printf,1,time,[transpose(outmag[n,*]),transpose(outerr[n,*])],f=tmparr+'D)'

;   timeout[n] = date_conv(time);,'M')
;   timeout[n] = date_conv(sxpar(hdr,timekey),'M')
   yr = strmid(time,0,4)
   mo = strmid(time,5,2)
   dd = strmid(time,8,2)
   hh = strmid(time,11,2)
   mi = strmid(time,14,2)
   ss = strmid(time,17,2)
   timeout[n] = julday(mo,dd,yr,hh,mi,ss) - 2400000.5d0

   fwhmout[n] = (atest[2]+atest[3])/2.

endfor
close,1

openw,2,imagelist+'_gflux.out'
printf,2,transpose(gflux)
close,2

openw,3,imagelist+'_gferr.out'
printf,3,transpose(gferr)
close,3 


;; ploterror,timeout-timeout[0],-2.5*alog10(gflux[*,0]) + 2.5*alog10(total(gflux[*,1:*],2)),outerr[*,0],psym=6,/ysty,ytitle='delta Mag',xtitle='image'

ploterror,(timeout-min(timeout))*24.,(outmag[*,0]) - outmag[*,1],outerr[*,0],psym=6,/ysty,ytitle='delta Mag',xtitle='delta Time (hours)'


;plot,(timeout - min(timeout))*24., gflux[*,0]/(total(gflux[*,1:*],2)),psym=4,/ysty,xtitle='time (hours)',ytitle='flux ratio'
;oplot,(timeout - min(timeout))*24., smooth(gflux[*,0]/(total(gflux[*,1:*],2)),15,/edge)


forprint,textout=imagelist+'gflux_lc.dat',timeout,(gflux[*,0]-(total(gflux[*,1:*],2)))/(total(gflux[*,1:*],2)),/nocomm,f='(D,D,D)'

print,'DONE>>>>'
print,' The file of output results: ',imagelist+'.out'
print,''
print,''
;stop
return
end

