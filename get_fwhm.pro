;from psf.pro & CALCULATE_LSF_LIST.pro developed at DNR (Oslo, Norway) by Arne Skretting, Wibeke Nordhoy, Alise Larsen and Kristine Eldevik
;modified by Ellen Wasbo 2015 (Stavanger University Hospital, Norway)
;modification related to user interface to incorporate code into ImageQC

function get_fwhm, image, center, pixel

szImg=SIZE(image, /DIMENSIONS)

;modified from psf.pro & CALCULATE_LSF_LIST.pro
region    = image[center(0)-15:center(0)+15, center(1)-5:center(1)+4]             ; omr�det rundt kulen, 31 piksler i x-retning, 10 piksler i y-retning
unitv   = REPLICATE(1.,10)                      ; array, 10 elementer med verdien 1
profile   = region # unitv                      ; array, 31 elementer med summen av de 10 y-verdiene for hver piksel i x-retning

centerB=center
centerB(1)=szImg(1)-center(1)

regionB   = image[centerB(0)-15:centerB(0)+15, centerB(1)-5:centerB(1)+4]          ; bakgrunn med samme form og st�rrelse som signalet
backgroundv = regionB # unitv                       ; summerer bakgrunnen slik som signalet
meanbackgr  = MEAN(backgroundv)                     ; finner gjennomsnittlig verdi av bakgrunnen
background  = REPLICATE(meanbackgr, N_ELEMENTS(profile))        ; 1d-array med gjennomsnittlig bakgrunnsverdi i alle 31 elementene
profileC  = profile - background                    ; profilen etter at bakgrunnen er subtrahert

maxvalue  = MAX(profileC, address)                  ; finner maks-verdien i profilen
profileN  = profileC / maxvalue                     ; normaliserer profilen til maxverdi av signalet

;WINDOW, 5, XSIZE=512, YSIZE=512, TITLE='Normalized profile'
;PLOT, profileN                              ; plotter den normaliserte profilen i vindu 0

profile2  = profileN > 0                        ; fjerner elementene i profilen med verdi lavere enn 0
test    = MAX(profile2, pos)
grad    = profile2 - SHIFT(profile2, 1)
nupper    = pos
nlower    = pos

FOR n = pos+1, N_ELEMENTS(profile2)-1   DO BEGIN
  IF grad[n] GT 0.  THEN GOTO, upperset
  nupper  = n
ENDFOR

upperset:
FOR m = 1, pos-1  DO BEGIN
  IF grad[pos-m] LT 0.  THEN GOTO, lowerset
  nlower  = pos - m
ENDFOR

lowerset:
psf_pixel = profile2[nlower:nupper]                 ; reell PSF i bildet
npoints   = nupper - nlower + 1                   ; antall piksler i PSF
cum     = FLTARR(npoints)                     ; array for kumulativ funksjon
cum[0]    = psf_pixel[0]

FOR n=1, npoints-1  DO cum[n] = cum[n-1] + psf_pixel[n]         ; kumulativ funksjon, der hvert element = summen av tidligere elementer

;CALCULATE_LSF_LIST, techniquelist, cum, pixel, resultpath, kernel, psf_pixel      ;

back      = cum[0]                      ; navngir f�rste element i kumulativ funksjon
max_cum     = MAX(cum, index)                 ; finner maksverdien og dens plassering i kumulativ funksjon
relative    = FLOAT(cum) / max_cum                ; normaliserer funksjonen i forhold til maksverdien
normal      = relative[0:index]                 ; normalisert funksjon opp til maksverdien

x = FLTARR(100)
FOR i=0, index-1  DO BEGIN
   x[i] = GAUSS_CVF(normal[i])                  ; finner 'cut-off value' for hvert element
ENDFOR

utvalg      = WHERE(ABS(x) LT 1.9 AND x NE 0.0)         ; finner indeksene der |x|<1.9 og ikke 0
IF N_ELEMENTS(utvalg) GT 2 THEN BEGIN ; probably higher than 2 needed - adjust if crashes 
  y = x[utvalg]
  psf_ny      = psf_pixel[utvalg]
  normal_utvalg = normal[utvalg]
  
  ;WINDOW, 7, XSIZE=512, YSIZE=512
  y = -y
  test      = MAX(y, maxpos)
  
  ;PLOT, y[0:maxpos-1]
  
  no        = N_ELEMENTS(utvalg)
  xx        = FLTARR(1, no)
  xx(0,*)     = FINDGEN(no)
  
  a0 = REGRESS(xx, y, YFIT=yfit, CONST=const, MCORRELATION=mcorr)
  scale = 1 / a0[0]                         ; scale er et standardavvik
  
  sigma_korr = SQRT((scale^2)-((0.28^2)/5.))              ; korrigerer for endelig kulest�rrelse
  
  FWHM      = 2 * SQRT(2. * ALOG(2)) * scale * pixel
  FWHM_korr   = 2 * SQRT(2. * ALOG(2)) * sigma_korr * pixel
  
  ;max_gauss       = 1/(2 * !pi * sigma_korr^2)
  ;
  ;xzero     = -const / a0[0]
  
  
  ;PRINTF, 1, kernel, techniquelist[0].imno, FWHM_korr, mcorr, $   ; resultatene skrives til resultatfilen
   ;      FORMAT='(A-12, I-12, F-12.6, F-12.6 )'
  res=[FWHM, FWHM_korr, mcorr]
  
  ;PRINT,' Gaussfunksjonens middelverdi er', xzero           ; skriver resultat p� skjerm
  ;PRINT,' FWHM ikke korrigert for kulest�rrelse: ', FWHM,' mm'
  ;PRINT,' FWHM korrigert for kulest�rrelse: ', FWHM_korr,' mm'
  
  ;sigmasq = sigma_korr^2
  ;
  ;xgauss      = ((FINDGEN(no*200)) - 100) * 0.05
  ;
  ;gauss_curve   = EXP(-(xgauss-xzero)^2/(2*sigmasq))
  ;
  ;gauss_curve_norm= gauss_curve / TOTAL(gauss_curve)
  ;
  ;psf_norm    = psf_ny / TOTAL(psf_ny)
  ;
  ;psf_display   = FLTARR(no+2)
  ;psf_display[1:no]= psf_norm
  
  ;WINDOW, 8, TITLE='Normal distribution plot', XSIZE=512, YSIZE=512
  ;PLOT, xx(0,*) * pixel, y, TITLE = 'Normal distribution plot', psym=2
  ;OPLOT, xx(0,*)* pixel, yfit
  ;XYOUTS, 110, 360, 'Correlation = '+ STRING(mcorr), /DEVICE
  ;XYOUTS, 110, 400, 'Kernel = '+ kernel, /DEVICE
  ;
  ;im = TVRD()
  ;jpeg_file = resultpath + 'Kum_normal_' + kernel + '.jpg'
  ;WRITE_JPEG, jpeg_file, im
  ;
  ;
  ;ymax = MAX(20 * gauss_curve_norm) * 1.1
  ;WINDOW, 9, TITLE = 'Gauss function', XSIZE = 512, YSIZE = 512
  ;PLOT, FINDGEN(no+2) * pixel, psf_display, psym = 10, xrange = [-1,no+2] * pixel, yrange = [0,ymax], $
  ;  TITLE ='Discrete PSF and fitted Gaussian', $
  ;  XTITLE ='mm'
  ;
  ;OPLOT, (xgauss + 1.5) * pixel,20 * gauss_curve_norm, psym=1     ; forskyvning mot h�yre pga kumulativ karakter
  ;XYOUTS, 110, 460, 'FWHM =' + STRING(FWHM_korr)+' mm', /DEVICE
  ;XYOUTS, 110, 420, 'Kernel = '+ kernel, /DEVICE
  ;
  ;im = TVRD()
  ;jpeg_file = resultpath + 'Gauss_psf_' + kernel + '.jpg'
  ;WRITE_JPEG, jpeg_file, im
  
  
  ;n_pixels  = N_ELEMENTS(profile)
  ;x     = FINDGEN(n_pixels)
  ;xplot   = (x - address) * pixel                   ; normaliserer x-aksen og gj�r om til mm
ENDIF ELSE res=-1

return, res
end