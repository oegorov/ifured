PRO ifured_dqe_sent,star_filename,table_name,w_dir=w_dir,plot=plot

  if not keyword_set(w_dir) then w_dir=""
  if not keyword_set(star_filename) then star_filename="star_cub.fts"
if not(keyword_set(skip)) then skip=20 ; skipping in A around lines
  
  im=readfits(w_dir+star_filename,head)

  Nz=sxpar(head,'naxis3')
  NY=sxpar(head,'naxis2')
  nx=sxpar(head,'naxis1')
  ld0 =sxpar(head,'CRVAL3')
  dld0=sxpar(head,'CDELT3')
  lambda=ld0+dld0*findgen(Nz); wavelength grid
  ADU=sxpar(head,'GAIN')
  z_star=float(sxpar(head,'Z'))
  T_exp=sxpar(head,'EXPTIME')
  
  ; read standard star table
  table=read_asc(table_name)
  if n_elements(table) lt 3 then return
  lambda_tab=reform(table[0,*])
  mag_tab=reform(table[1,*])
  
  s=size(table)
  ; set step
  if s[1] ge 3 then w_tab=reform(table[2,*]) else begin
    w_tab=lambda_tab-shift(lambda_tab,1)
    w_tab[0]=w_tab[1]
  
    nn=n_elements(w_tab)
    w_tab[nn-1]=w_tab[nn-2]
  endelse
  
  
  print,'Apertures in the table:',minmax(w_tab)
  ;calculation extintion
  S=2.51E05       ;total square mirror of telescope in cm^2
  a=0.012 & c=0.12
  extin_tab=(a*1./((lambda_tab/10000.)^4.)+c)*2.5*alog10(exp(1))
  mag_tab=mag_tab+extin_tab/cos(z_star*!DTOR)
  N_tab=948.*S*(10^(-0.4*mag_tab))/ADU
  N_tab=N_tab*(5500./lambda_tab)^2
  
  
  w=5
  tot=fltarr(nz)
  
  for k=w,Nz-w-1 do begin
    map=total(im[*,*,k-w:k+w],3)/(2*w+1)
    gau=gauss2Dfit(map[2:nx-3,2:ny-3],A)
    tot[k]=total(map[2:nx-3,2:ny-3]-A[0])
  endfor
  
  vector=tot/(T_exp*dld0); counts/sec/A


; shift lambda to true wavelengh scale:
  Wmed=Nz*0.2
  temp=INTERPOL(mag_tab,lambda_tab,lambda)
  obs=vector-med_ext(vector,Wmed)
  temp=med_ext(temp,Wmed)-temp
  apod=cosin_apod(Nz,20)
  m=150 & x_cross=findgen(2*M+1)-M
  cross=CROSS_NORM(obs*apod,temp*apod,m)
  gau=multigaus(x_cross,cross,0,fwhm=m/10)
  
  if gau(0).flux eq -1 or abs(gau(0).center) gt m/2 then sh=0 else sh=-gau(0).center
  
  vector=vecshift(vector,dx=sh)
  if sh gt 0 then vector(0:sh)=vector(sh+2)
  print,'Wavelength shift [px]: ',sh
  
  
  
  
  
  
  
  

; **********
; smoothed observed data:

rec=where(lambda_tab gt lambda[0] and lambda_tab lt lambda[Nz-1],num_t)
l_obs=lambda_tab[rec]
f_obs=fltarr(num_t)

w=median(w_tab[rec])


if w GT 2*dld0 then begin
for j=0,num_t-1 do begin
 l_c=l_obs[j]

 w=w_tab(rec[j])
 inde=where(lambda ge l_c-W/2 and lambda Le l_c+W/2,nums)
 if nums gt 0 then f_obs[j]=total(vector[inde])/nums
endfor

endif else f_obs=INTERPOL(vector,lambda,l_obs)


;rec=where(f_obs eq 0,ccc)
DQE_tab=100.*f_obs/N_tab[rec]
dqe_full=dqe_tab
ll=l_obs
; DQE smoothing and interpolation

dw=skip/dld0
bad_lambda=[3930,3960,4340,4680,4861,5420,5740,5770,6300,6562,6860,6900,6950,7650,7600,7650,7700,7750]
 Nbad=N_elements(bad_lambda)

for j=0,Nbad-1 do begin
index=where(abs(l_obs-bad_lambda[j]) gt dw)
l_obs=l_obs[index]
dqe_tab=dqe_tab[index]
endfor

; Smoothing DQE

if w gt 30 then DQE_smo=dqe_tab else dqe_smo=med_ext(dqe_tab,5) ; smoothing for dw<30 A
DQE_average=INTERPOL(dqe_smo,l_obs,lambda)

old=dqe_average
dqe_average=med_ext(dqe_average,5)
dqe_average=smooth(dqe_average,3*dw,/edge_TRUNCATE)
dqe_average(0:1.5*dw)=smooth(old(0:1.5*dw),0.2*dw,/edge_TRUNCATE)
dqe_average(Nz-1.8*dw:*)=smooth(old(Nz-1.8*dw:*),0.2*dw,/edge_TRUNCATE)


if keyword_set(plot) then begin
  cgdisplay, title="DQE",/free
  cgplot,lambda,DQE_average,xst=1,xtit='Wavelength, A',ytit='DQE, %',thick=3,color="red",tit=sxpar(head,'OBJECT')+'   '+sxpar(head,'DATE')
  cgoplot,ll,DQE_full,psym=4,symsize=0.4,color="blue"
endif


openw,u,w_dir+'DQE.txt',/get_lun
printf,u,'Star: '+sxpar(head,'OBJECT') + '  Grating:'+  sxpar(head,'DISPERSE')
for k=0,Nz-1 do printf,u,lambda(k),DQE_average(k)
free_lun,u


;write sent
sent=3.39E-9*ADU/(9.48*DQE_average*S)
mkhdr,h_sent,sent
sxaddpar,h_sent,'CRVAL1', ld0
sxaddpar,h_sent,'CDELT1', dld0
sxaddpar,h_sent,'BUNIT', 'erg/cm^2/sec/A'
sxaddpar,h_sent,'OBJECT',sxpar(head,'OBJECT')
writefits,w_dir+'sent.fts',sent,h_sent








fileps='DQE.eps'
ps_start,w_dir+fileps,/nomatch,/encaps

cgplot,lambda,DQE_average,xst=1,xtit='Wavelength, A',ytit='DQE, %', xr=[4500,6800];,tit=sxpar(head,'OBJECT')+'   '+sxpar(head,'DATE')
cgoplot,l_obs,DQE_tab,psym="Open circle",symsize=0.2

ps_end

  
END