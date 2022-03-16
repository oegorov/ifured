
pro ifured_crea_sent,FILE,star_filename,w_dir=w_dir,PLOT=plot, stand_dir=stand_dir


if not keyword_set(file) then file="star_cub.fts"
on_error,2
message,'creation of spectral sensitivitiy curve',/cont

  im=readfits(w_dir+file,head)
  Nz=sxpar(head,'naxis3')
  NY=sxpar(head,'naxis2')
  nx=sxpar(head,'naxis1')
  ld0 =sxpar(head,'CRVAL3')
  d_lambda=sxpar(head,'CDELT3')
  lambda=ld0+d_lambda*findgen(Nz); wavelength grid
  GAIN=sxpar(head,'GAIN')
  z_star=float(sxpar(head,'Z'))
  Texp=sxpar(head,'EXPTIME')
  star_name=strcompress(sxpar(head,'object'))
  date_obs=strcompress(sxpar(head,'date-obs'))
  grating=strcompress(sxpar(head,'DISPERSE'))
  w=3
  tot=fltarr(nz)
  
  for k=w,Nz-w-1 do begin
    map=total(im[*,*,k-w:k+w],3)/(2*w+1)
    gau=gauss2Dfit(map[2:nx-3,3:ny-4],A)
    tot[k]=total(map[2:nx-3,4:ny-4]-A[0])
  endfor
  
  star=tot



;read table star

fdecomp,star_filename,sd,sfd,sfn,sq
    if not keyword_set(sfd) then sfd=stand_dir
    if not keyword_set(sq) then sq="dat"
    if not keyword_set(sd) then sd=""
    if not keyword_set(sfn) then begin
      sfn="f"+strlowcase(strcompress(star_name,/remove))
      sfn=strjoin(strsplit(sfn,"+",/extr))
    endif
  star_filename=sd+sfd+sfn+"."+sq
  inf=FILE_INFO(star_filename)
  IF NOT (inf.exists) THEN begin
      res=dialog_message("Can't find a table with standard star: "+star_filename)
      return
  ENDIF  
flux=read_st(star_filename,lambda,/print)
;correction shift spectra star
dc=20 & xc=findgen(dc*2+1)-dc
v_obs=star-smooth(star,50,/edge_truncate)
v_tab=flux-smooth(flux,50,/edge_truncate)
vc=cross_norm(v_obs,v_tab,dc)
tmp=max(vc,Nmax)& dc=xc(Nmax)
star=shift(star,-dc-1)


z=findgen(nz)
D=star/flux
D=median(d,3)
LOWESS,z,D,50,D,order=2
D=D/max(D)
if lambda[0]*2 lt lambda[Nz-1] then begin
M=fix((lambda[Nz-1]/2-lambda[0])/d_lambda)
dflux=flux(0:M-1)*D(0:M-1) & dflux=congrid(dflux,2*M,/inter,/cent)
flux(Nz-M*2:Nz-1)=flux(Nz-M*2:Nz-1)+dflux*order2*3
endif





;calculation DQE
S=2.52E5		     ;square 6-m mirror
N_star=flux*2.8E11*S
star_obs=star*GAIN/calc_ext(lambda,Z_star)/Texp/d_lambda


;#### Костыль для прилизывания рисунка для статьи
rat=star_obs/N_star
rec=where(lambda lt 6550 and lambda gt 4920)
res=goodpoly(lambda[rec],rat[rec],3,0.5)
p=poly(lambda,res)
rec=where(lambda ge 6550 or lambda le 4920)
rat[rec]=p[rec]
LOWESS,z,rat,Nz/3,DQE,order=3

;LOWESS,z,star_obs/N_star,Nz/5,DQE,order=3




if keyword_set(plot) then begin
cgdisplay,/free,tit="DQE calculation",wid=20

cgplot,lambda,star,xst=1,title=date_obs+', star '+star_name+', Texp='+$
	string(Texp,format='(I3)')+' s, z='+string(z_star,format='(I3)')+' deg',$
	xtitle='Wavelength, A',ytitle='Flux, ADU',layout=[1,2,1]
dqe_size=n_elements(DQE)
cgplot,lambda,DQE*100,xst=1,thick=1.5,$
	xtitle='Wavelength, A',ytitle='DQE, %',layout=[1,2,2];,yr=[0,1.1*max(DQE[dqe_size/4:3*dqe_size/4])*100.]
	cgoplot,lambda,star_obs*100./N_star,linest=1,thick=0.7

endif


writefits,w_dir+'star_obs.fts',star_obs,h
writefits,w_dir+'star_tab.fts',N_star,h
writefits,w_dir+'DQE.fts',DQE,h


cgps_open,w_dir+'DQE.eps', /encaps,/quiet,xs=6,ys=4
  cgplot,lambda,DQE*100,xst=1,thick=3,$;,title=date_obs+', Grating: '+grating+', Star: '+star_name+', T$\downexp$='+$
	;string(Texp,format='(I3)')+'$\ups$, z='+string(z_star,format='(I2)')+'$\upo$',$
	xtitle='Wavelength, A',ytitle='DQE, %',charsize=1.1,pos=[0.1,0.12,0.99,0.99]
cgps_close

;create sensitivity curves
sent=fltarr(Nz,2)
sent[*,0]=1/(DQE*2.8E11*S)				;erg
sent[*,1]=sent(*,0)*lambda^2*3.3356E7	;mJy

mkhdr,h_sent,sent

sxaddpar,h_sent,'OBJECT',sxpar(head,'OBJECT')

writefits,w_dir+'sent.fts',sent,h_sent
end

