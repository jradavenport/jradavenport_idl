;+
; NAME: SIMPLEPHOT - Stupidly Simple Photometry
;
; PURPOSE:
;        Designed to do simple differential aperture photometry, and
;        thus useful for bright sources in non-crowded fields. Adjusts
;        star centers with each image, but cannot handle big jumps
;        very well... it will try, but caveat emptor. Using IDL
;        versions of DAOPHOT (aper, find)
;
;
; CALLING SEQUENCE: 
;        simplephot, imagelist [, /display, /reduce, ncomp=ncomp,
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
;                stars to analyze. 
;                If used, SIMPLEPHOT will not prompt for target
;                identification.
;
; KEYWORD PARAMETERS:
;        /display - show every image
;        /reduce - write the flat & bias reduced images.
;        /gaussian - fit stars with a 2D gaussian (does anyway) and
;                    save other output files.
;
; OUTPUTS:
;        creates file imagelist.out with columns:
;        TIME Stamp, TIME (JD), mag_target, mag_comp1... 
;                    err_target, err_comp1... 
;
;
; OPTIONAL OUTPUTS:
;        If flatlist is used, SIMPLEPHOT will write flat.fits.
;        If darklist or biaslist is used, SIMPLEPHOT will write
;             zero.fits.
;
; EXAMPLE:
;        IDL> simplephot,'image.list', /reduce, ncomp=3
;
; MODIFICATION HISTORY:
;
;-

; ---to do----
;    - add logfile output
;    - add coord input for entire run, so dont need to re-do all the
;      time
;    - add real-time method

function flatcombine,flatlisfile, bias, mode=mode
  if not keyword_set(mode) then mode = 'median'
  if mode ne 'median' and mode ne 'mean' then begin
     print,'ERROR: invalid mode. Stupidly using "median" now'
     mode = 'median'
  endif
  readcol,flatlisfile,flatlis,f='(A)',/silent
  print,n_elements(flatlis),' flat images to stupidly combine...'
  im = mrdfits(flatlis[0],/silent,/dscale) - bias
  ;normalize first, then add to stack
  if mode eq 'mean' then im = im/mean(im,/nan,/double) else $
  if mode eq 'median' then im = im/median(im)
  for n=1L,n_elements(flatlis)-1 do begin
     im_tmp = mrdfits(flatlis[n],/silent,/dscale) - bias
     if mode eq 'mean' then im = im + im_tmp/mean(im_tmp,/double,/nan) else $
     if mode eq 'median' then im = im + im_tmp/median(im_tmp)
  endfor
  ;normalize stack...this needs to be reconsidered
  im = im/mean(im,/double,/nan)
  writefits,'flat.fits',im
  print,'> FLATCOMBINE stats:'
  print,'    median = ',median(im)
  print,'    mean   = ',mean(im)
  print,'    stddev = ',stddev(im)
  print,'    min    = ',min(im)
  print,'    max    = ',max(im)
  return,im
end

function zerocombine,flatlisfile,dark=dark
  readcol,flatlisfile,flatlis,f='(A)',/silent
  print,n_elements(flatlis),' bias images to stupidly combine'
  im = mrdfits(flatlis[0],/silent,/dscale)
  for n=1L,n_elements(flatlis)-1 do begin
;     print,n
     im_tmp = mrdfits(flatlis[n],/silent,/dscale)
     im = im + im_tmp
  endfor
  im = im/float(n_elements(flatlis))
  if not keyword_set(dark) then writefits,'zero.fits',im
  if keyword_set(dark) then writefits,'dark.fits',im
  print,'> ZEROCOMBINE stats:'
  print,'    median = ',median(im)
  print,'    mean   = ',mean(im)
  print,'    stddev = ',stddev(im)
  print,'    min    = ',min(im)
  print,'    max    = ',max(im)
  return,im
end


pro resetcoords_man,img,ncomp,xx,yy
  im = mrdfits(img,0,/silent,/dscale)
  imsz = size(im)
;  cubehelix

  loadct,5,/silent              ; STD GAMMA-II
  plot,[0],xrange=[0,imsz[1]],yrange=[0,imsz[2]],/xsty,/ysty,/nodata,position=[.1,.1,.95,.95],xtitle='X (pixel)',ytitle='Y (pixel)'
  tvimage_simple,smooth(im,/edge_truncate,2),position=[.1,.1,.95,.95]
  print,'> select target'
  print,'> (green boxes indicate stars auto-found by FIND)'
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


pro simplephot,imagelist, display=display, ncomp=ncomp, reduce=reduce,$
               flatlist=flatlist, biaslist=biaslist, darklist=darklist,$
               doneflat=doneflat, donebias=donebias, donedark=donedark,$
               coord=coord, gaussian=gaussian, $
               smbox=smbox, fwhm=fwhm,aperture=aperture

print,'STARTING SIMPLEPHOT'
print,'.. a time series photometry wrapper by James Davenport ..'

; set graphics device up my way... with apologies to the Coyote
device, retain = 2
device, true_color = 24
device, decomposed = 0

; set options for the compiler
compile_opt defint32, strictarr, strictarrsubs
; suppress some outputs
compile_opt HIDDEN

;On_error,2
;; if n_params() lt 1 then begin
;;    print,'Error: need to include image list as a string'
    print,'SIMPLEPHOT, "imagelist", /display, ncomp=ncomp, flatlist="flatlist", biaslist="biaslist", darklist="darklist", doneflat = "doneflat", donedark = "donedark", /reduce, /gaussian, coord="coord"'
;;    return
;; endif

if keyword_set(imagelist) then begin
   print,'Using image list: ',imagelist
   print,''
   if not keyword_set(ncomp) then begin
      if not keyword_set(coord) then begin
         print,'>> using default # of comparision stars: ncomp=2'
         ncomp = 2.
      endif
      if keyword_set(coord) then begin
         print,'>> using previous coords'
         readcol,coord,/silent,junk
         ncomp = float(n_elements(junk))-1.
      endif
   endif
   
   print,''
   if not keyword_set(APERTURE) then APERTURE = 5. ; for ap phot
   if not keyword_set(SKYY) then SKYY = [20,50]     ; inner and outer rad
   if not keyword_set(SMBOX) then SMBOX = 15. ; search box side length to match each frame over
   if not keyword_set(FWHM) then FWHM = 5.   ; approx FWHM
   
   timekey = 'DATE-OBS'         ; 'UTCSTAMP'
   print,'>> using header keyword ',timekey
   
   ROUNDLIM = [-1.,1.]          ; i wouldnt change this
   SHARPLIM = [0.2,1.1]         ; i wouldnt change this
endif

;------------ how this works --------------------
; read the first image
; show it
; pick the stars you want interactively (cursor)
; for every image run: FIND, APER, extract time
; match stupidly to the x,y master list, using dumb small-big box
; if target star is not found, show this image and re-pick

if keyword_set(imagelist) then begin
   readcol,imagelist,f='(A)',images,/silent
   
   ;-- read first image in
   im = (mrdfits(images[0],0,hdr,/silent,/dscale))
   time = sxpar(hdr,timekey)
   imsz = size(im)
endif

;-- optionally combine the darks/biases/flats
;   NOTE: does not require imagelist to be defined!
bias = 0d0
if keyword_set(biaslist) and not keyword_set(donebias) then begin
   print,'COMBINING BIASES... BE PATIENT'
   bias = zerocombine(biaslist)
endif
if keyword_set(donebias) then bias = mrdfits(donebias,0,/silent,/dscale)

dark = 0d0
if keyword_set(darklist) and not keyword_set(donedark) then begin
   print,'COMBINING DARKS... BE PATIENT'
   dark = zerocombine(darklist,/dark)
endif
if keyword_set(donedark) then dark = mrdfits(donedark,0,/silent,/dscale)

if keyword_set(darklist) or keyword_set(donedark) then begin
   print,'NOTE: SIMPLEPHOT is not using darks right now, '
   print,' but I will happily combine them for you...'
   print,'You can sneak around this by manually handing the combined'
   print,' dark.fits as the "donebias" and re-running'
endif


flat = 1d0
if keyword_set(flatlist) and not keyword_set(doneflat) then begin
   print,'COMBINING FLATS... BE PATIENT'
   flat = flatcombine(flatlist, bias)
endif
if keyword_set(doneflat) then flat = mrdfits(doneflat,0,/silent,/dscale)


;--- now reduce/analyze the data from imagelist
if keyword_set(imagelist) then begin
;-- this is how you reduce data from raw
   im = (im - bias)/flat

   loadct,0,/silent

   if not keyword_set(coord) or keyword_set(display) then begin
      window,0,xsize=750,ysize=750,title='SIMPLEPHOT'
      plot,[0],xrange=[0,imsz[1]],yrange=[0,imsz[2]],/xsty,/ysty,/nodata,position=[.1,.1,.95,.95],xtitle='X (pixel)',ytitle='Y (pixel)'
      tvimage_simple, smooth(im,/edge_truncate,2), position=[.1,.1,.95,.95]
      
      print,''
      print,n_elements(images),' images to process...'
      print,''
      print,'> re-size your window if desired, then click anywhere in the window'
      print,''
      clkx = 1
      clky = 1
      
      CURSOR,clkx,clky,/down,/data
   endif

   if not keyword_set(coord) then begin
   ;cubehelix
      loadct,5,/silent          ; STD GAMMA-II
      plot,[0],xrange=[0,imsz[1]],yrange=[0,imsz[2]],/xsty,/ysty,/nodata,position=[.1,.1,.95,.95],xtitle='X (pixel)',ytitle='Y (pixel)'
      tvimage_simple,smooth(im,/edge_truncate,2),position=[.1,.1,.95,.95]
      print,''
      
      find,im,xtmp,ytmp,fluxf,sharpf,rndf, median(im)+stddev(im,/nan)*3., fwhm ,roundlim, sharplim,/monitor,/silent
      loadct,39,/silent         ; RAINBOW
      oplot,xtmp,ytmp,psym=6,color=150,thick=5
      
      print,'> select target star'
      x1=1
      y1=1
      CURSOR,x1,y1,/down,/data
      oplot,COS(FINDGEN(17) * (!PI*2/16.))*10+x1, SIN(FINDGEN(17) * (!PI*2/16.))*10+y1,color=250,thick=4
      print,''
;==============
      xc = 0
      yc = 0
      for n=0L,ncomp-1 do begin
         print,'> select comparison ',strtrim(string(n+1),2)
         xx=1
         yy=1
         
         CURSOR,xx,yy,/down,/data
         xc = [xc,xx]
         yc = [yc,yy]
         
         oplot,COS(FINDGEN(17) * (!PI*2/16.))*10+xx, SIN(FINDGEN(17) * (!PI*2/16.))*10+yy,color=200,thick=4
      endfor
      remove,0,xc,yc
      
      xx = [x1,xc]              ; the target and the comparison stars
      yy = [y1,yc]
      
                                ; dump a coord file for later use!
      openw,7,imagelist+'.coord'
      for v=0l,ncomp do $
         printf,7,xx[v],yy[v]
      close,7
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
   xout = fltarr(n_elements(images), ncomp+1)
   yout = fltarr(n_elements(images), ncomp+1)

   tmparr = '(A, A, D, '
   for v=0L,2.*ncomp do tmparr = tmparr+'D,'
   
   close,/all
   openw,1,imagelist+'.out'
   nimage = n_elements(images)
   
   for n=0L,nimage-1 do begin
;   run find,aper,extract time
      print,images[n],' ', n,' / ',nimage-1
      im = (mrdfits(images[n],0,hdr,/silent,/dscale) - bias) / flat
      if keyword_set(reduce) then writefits,'reduced_'+images[n],im,hdr
      
      Time = sxpar(hdr,timekey)
      imsz = size(im)
      
      if keyword_set(display) then begin
;      cubehelix
         loadct,39,/silent
         plot,[0],xrange=[0,imsz[1]],yrange=[0,imsz[2]],/xsty,/ysty,$
              /nodata,position=[.1,.1,.95,.95],$
              xtitle='X (pixel)',ytitle='Y (pixel)',title=images[n]
         loadct,5,/silent       ; STD GAMMA-II
         tvimage_simple,smooth(im,/edge_truncate,2),position=[.1,.1,.95,.95]
      endif
      
 ; find and phot
      ;; find,im,xf,yf,fluxf,sharpf,rndf, median(im)+stddev(im,/nan)*3.,$
      ;;      fwhm ,roundlim, sharplim,/monitor,/silent
      ;; aper,im,xf,yf,mag,err,sky,skyerr,1,APERTURE,SKYY,readnoise=1,/silent,/nan

      for i=0L,ncomp do begin         
; play with fitting a 2D gaussian to each star
; these fit coefficients could be useful later on!
         gbox = 3.*fwhm
         tmpcoord = [(xx[i]-gbox),(xx[i]+gbox),$
                     (yy[i]-gbox),(yy[i]+gbox)]
         if tmpcoord[0] lt 0 then tmpcoord[0] = 0
         if tmpcoord[1] lt 0 then tmpcoord[1] = imsz[1]-1
         if tmpcoord[2] lt 0 then tmpcoord[2] = 0
         if tmpcoord[3] lt 0 then tmpcoord[3] = imsz[1]-1
         if tmpcoord[0] ge imsz[1] then tmpcoord[0] = 0
         if tmpcoord[1] ge imsz[1] then tmpcoord[1] = imsz[1]-1
         if tmpcoord[2] ge imsz[1] then tmpcoord[2] = 0
         if tmpcoord[3] ge imsz[1] then tmpcoord[3] = imsz[1]-1
         
         imtest = im[tmpcoord[0]:tmpcoord[1],tmpcoord[2]:tmpcoord[3]]
         gtest = GAUSS2DFIT(imtest,Atest, /tilt)
         gflux[n,i] = total(gtest-Atest[0])
         gferr[n,i] = sqrt(mean((gtest-imtest)^2.)) 
         
         
         xx[i] = xx[i]-gbox + Atest[4]
         yy[i] = yy[i]-gbox + Atest[5]
         
         aper,im,xx[i],yy[i],mag,err,sky,skyerr,1,APERTURE,SKYY,$
              readnoise=1,/silent,/nan
         outmag[n,i] = mag
         outerr[n,i] = err
         
         if keyword_set(display) then begin
            loadct,39,/silent
            oplot,[xx[i]],[yy[i]],psym=6,color=150,thick=3
            wait,.2
         endif
      endfor
      
      
      if n gt 1 then begin
         if total(gflux[n,*]) lt total(gflux[n-1,*])/2. then begin
            print,"OH NO! Stars aren't where they should be..."
            resetcoords_man,images[n],ncomp,xx,yy
            
            for i=0L,ncomp do begin
               tmpcoord = [(xx[i]-gbox),(xx[i]+gbox),(yy[i]-gbox),(yy[i]+gbox)]
               if tmpcoord[0] lt 0 then tmpcoord[0] = 0
               if tmpcoord[2] lt 0 then tmpcoord[2] = 0
               if tmpcoord[1] ge imsz[1] then tmpcoord[1] = imsz[1]
               if tmpcoord[3] ge imsz[2] then tmpcoord[3] = imsz[2]
               imtest = im[tmpcoord[0]:tmpcoord[1],tmpcoord[2]:tmpcoord[3]]
               gtest = GAUSS2DFIT(imtest,Atest, /tilt)      
               gflux[n,i] = total(gtest-Atest[0])
               gferr[n,i] = sqrt(mean((gtest-imtest)^2.)) 
               xx[i] = xx[i]-gbox + Atest[4]
               yy[i] = yy[i]-gbox + Atest[5]

               APER,im,xx[i],yy[i],mag,err,sky,skyerr,1,APERTURE,SKYY,$
                    readnoise=1,/silent,/nan
               outmag[n,i] = mag
               outerr[n,i] = err
           endfor
         endif
      endif
      
      ;-- date format set up for YYYY-MM-DDTHH:MM:SS
      yr = strmid(time,0,4)
      mo = strmid(time,5,2)
      dd = strmid(time,8,2)
      hh = strmid(time,11,2)
      mi = strmid(time,14,2)
      ss = strmid(time,17,2)
      timeout[n] = julday(mo,dd,yr,hh,mi,ss) ;- 2400000.5d0
      
      fwhmout[n] = (atest[2]+atest[3])/2.
      
      printf,1,f=tmparr+'D)',images[n]+' ', time+' ', timeout[n], $
             [transpose(outmag[n,*]),transpose(outerr[n,*])]
   endfor
   close,1
   
   
   if keyword_set(gaussian) then begin
      openw,2,imagelist+'_gflux.out'
      printf,2,transpose(gflux)
      close,2
      
      openw,3,imagelist+'_gferr.out'
      printf,3,transpose(gferr)
      close,3 
   endif
   
   
;compmag = alog10(total(10d0^(outmag[*,1:*]),2))
   compmag = outmag[*,1]
   loadct,39,/silent

   ploterror, (timeout-min(timeout))*24., psym=6, /ysty,$
              (outmag[*,0]) - compmag, outerr[*,0], $
              ytitle='delta Mag (target - comp1)', xtitle='delta Time (hours)',$
              yrange=[max((outmag[*,0]) - outmag[*,1]),$
                      min((outmag[*,0]) - outmag[*,1])],$
              title='Aperture Phot',charsize=1.4
   
   if keyword_set(gaussian) then begin
      gplotout = -2.5*alog10(gflux[*,0]/(total(gflux[*,1:*],2)/ncomp))
      gtimeout = timeout
      gplotout = gplotout[sort(gtimeout)]
      gtimeout = gtimeout[sort(gtimeout)]
      plot,(gtimeout - gtimeout[0])*24., $
           gplotout,$
           psym=4,/ysty,xtitle='time (hours)',$
           ytitle='delta Mag [target / sum(comps)]',$
           title='Gaussian',/xsty,charsize=1.4,$
           yrange=maxmin(gplotout)*[1.02,0.98]
   endif
   
   
   
;; forprint,textout=imagelist+'gflux_lc.dat',timeout,(gflux[*,0]-(total(gflux[*,1:*],2)))/(total(gflux[*,1:*],2)),/nocomm,f='(D,D,D)'


   print,'DONE>>>>'
   print,'>> The file of output results: ',imagelist+'.out'
   print,''
   
endif


print,'>  goodbye'

stop
return
end

