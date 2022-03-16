
pro IFURED_DISPERSION_CURVE,fileneon=fileneon,FWHM=FWHM,N_deg=N_deg,YFIT=yfit,noshift=noshift,$
w_dir=w_dir, root=root, left_shift=left_shift, ifu_dir=ifu_dir,show=show, logfield=logfield

;on_error,1

nameslit=["left", "right"]

if not keyword_set(root) then begin
  dirforifu=ifu_dir;if  !VERSION.OS_family eq 'Windows' then dirforifu=!dir+'\external\IFU_REDUCTION\' else dirforifu='~/Science/IDLWorkspace/IFU_REDUCTION/'
endif else dirforifu=root

if not(keyword_set(fwhm)) then fwhm=[4.,4.]
if not(keyword_set(N_deg)) then N_deg=[3,3]

if keyword_set(w_dir) then w_dir=slash(w_dir) else w_dir=''
if not(keyword_set(fileneon)) then fileneon='neon_s.fts'

filetab=dirforifu+'lines.tab'



;read table of lines comparison spectrum

table=read_asc(filetab,0)
lines0=table(0,*)
weigth0=table(1,*)
intensity0=table(2,*)


header=headfits(w_dir+fileneon)
; preliminary dispersion determination
dispname=sxpar(header,'disperse')
s=strsplit(dispname,' ',/extr)
dispname=strupcase(s(0))

;filedis=dirforifu+dispname+'.txt'
;if file_test(filedis) eq 0 then 
filedis=dirforifu+slash("Grisms")+dispname+'.txt'

dname=['300','400','550','600','940','1200','1720','1800','2310','2300','3000']
Ngr=0
for i=0,n_elements(dname)-1 do begin
 if strpos(dispname,dname(i)) gt -1 then Ngr=fix(dname(i))
endfor

lev=(4-round(Ngr/600.))>1


if file_test(filedis) eq 0 then   begin
        res=dialog_message(['The dipserser '+dispname+' is unknown,','File is not found: '+filedis])
        return
endif else disperser=read_asc(filedis,1)
s=size(disperser)
recommend_wlout=disperser[*,0]
disperser=disperser[*,1:s[2]-1]

;Loop for left and right slits
neon_ini=readfits(w_dir+fileneon,header)
Ny=sxpar(header,'naxis2')
disp_param_out=fltarr(max(n_deg)+2,ny,2)-32000

nx_in=intarr(2)
win_prelim=0
FOR slitnum=0,1 DO BEGIN


  ; read NEON frame
  neon=reform(neon_ini[*,*,slitnum])
   if n_elements(neon) eq 1 then begin
    res=dialog_message('No file '+w_dir+fileneon)
    return
   endif
  
  Nx=sxpar(header,'naxis1')
  Ny=sxpar(header,'naxis2')
  
  if win_prelim gt 0 then vec_neon=reform(total(neon[*,fix(ny/2-win_prelim):fix(ny/2+win_prelim)],2,/nan)) else vec_neon=reform(neon[*,ny/2])
  rec=where(finite(vec_neon) and vec_neon ne 0 ,nx)
  nx=max(rec)-min(rec)+1
  nx_in[slitnum]=nx
  delta_x=rec[0]; additional shift with respect to ref. txt-file due to NAN values on the left side
  neon=neon[delta_x:delta_x+nx-1,*]
   
  rec=where(~finite(neon),nr)
  if nr gt 0 then neon[rec]=0
  s=sxpar(header,'binning')
  bin=float(strmid(strtrim(s,2),0,1)) ; binning
;  bin=1;bin>1
  
  rec=where(disperser[2,*] eq slitnum)
  wav=reform(disperser[1,rec])
  tmp=reform(disperser[0,rec])
  ;if slitnum eq 1 then tmp=tmp-left_shift
  pos=tmp/bin-delta_x
  
  c_prelim=poly_fit(wav,pos,3) ; preliminary convertion to wavelength
  c_revers=poly_fit(pos,wav,3) ; preliminary convertion to wavelength
  tresh=3.5
  
  ; preliminary lines position
  lambda_beg=poly(20,c_revers)
  lambda_end=poly(Nx-20,c_revers)
  
  index=WHERE(lines0 gt lambda_beg and lines0 lt lambda_end,N_lines)
  lines=lines0[index]
  weigth=weigth0[index]
  intensity=intensity0[index]
  
  ;creation full table spectrum in observed coverage
  pos_previus=poly(lines,c_prelim)


  message,'Number lines in observed spectral coverage'+string(n_lines),/cont
  
  ;creation tabulated comparison spectra
  x=findgen(Nx)
  neon_tab=fltarr(Nx)
;  if slitnum eq 1 then shi=150 else shi=0
  for k=0,N_lines-1 do neon_tab=neon_tab+gaussian(x,[intensity[k],pos_previus[k],fwhm/2.35])
  neon_tab=neon_tab-min(neon_tab)
  neon_tab=neon_tab/max(neon_tab)
  
  
  ;********************************************************
;  if slitnum eq 1 then noshift=0 
  if not(keyword_set(noshift)) then begin
  ; offset shift calculation
  M=200
  
  if win_prelim gt 0 then dy=DEF_VECSHIFT(neon_tab,reform(total(neon[*,Ny/2-win_prelim:Ny/2+win_prelim],2,/nan)),bin=50,M=M) else $
        dy=DEF_VECSHIFT(neon_tab,reform(neon[*,Ny/2]),bin=50,M=M)
  print,'Shift=',dy
  
  endif else dy=0
  
  
  lambda_beg=poly(20-dy,c_revers)
  lambda_end=poly(Nx-20-dy,c_revers)
  
  index=WHERE(lines0 gt lambda_beg and lines0 lt lambda_end and weigth0 ge lev,N_lines)
  lines=lines0(index)
  
  
  message,'Number lines used for creating dispersion curve'+string(n_lines,format="(I0)"),/cont
  if keyword_set(logfield) then IFURED_LOG_UPD, ["Number lines for creating disp. curve for "+nameslit[slitnum]+" = "+string(n_lines,format="(I0)")]
  pos_lines=fltarr(N_lines,Ny)
  pos_previus=poly(lines,c_prelim)
  for k=0,N_lines-1 do  pos_lines(k,*)=pos_previus(k)+dy;+shi
  
  ;determination accuracy position of lines
  w=FWHM[slitnum];*1.5
  L_lim=4900
  xx=findgen(2*w+1)
  vector_tmp=fltarr(Nx)
  old=pos_lines
  
  for j=0,Ny-1 do begin
   vector_tmp=neon[*,j]
    for k=0,N_lines-1 do begin
      
      pos=fix(pos_lines(k,j))<(Nx-1)
      pos=pos>0
      
      if lines(k) gt L_lim then w2=w+1 else w2=w+5
      ;definition local maximum
      pos0=(pos-w2-1)>0
      pos1=(pos+w2+1)<(Nx-1)
      xtmp=findgen((pos1-pos0+1))
      
      sl=congrid(xtmp,(pos1-pos0+1)*100,/interp,/cent)
      nv=congrid(vector_tmp[pos0:pos1],(pos1-pos0+1)*100,/interp,/cent)
;      res=poly_fit(sl,nv,2)
;      Nmax=-res[1]/2./res[2]
      
      gau=gaussfit(xtmp,vector_tmp[pos0:pos1],G,Nterms=4)
      nmax=g[1]
      
      tmp=max(vector_tmp((pos-w2-1)>0:(pos+w2+1)<(Nx-1)),Nmax,/nan)
      ;pos=(Nmax+pos-w2)<(Nx-1-w2-1)
      pos=(Nmax+pos0)<(Nx-1-w2-1)
      pos=pos>w
      level=0
      flux=total(vector_tmp(pos-w:pos+w)-level,/nan)
;      if flux eq 0 then stop
      ;if j eq 200 then stop
      if flux ne 0 then pos_lines[k,j]=total((vector_tmp[pos-w:pos+w]-level)*(x[pos-w:pos+w]+delta_x),/nan)/flux;
;      pos_lines[k,j]+=delta_x
      ;if ~finite(pos_lines(k,j)) then stop;print, vector_tmp(pos-w:pos+w) 
    endfor
  endfor
  
  
  y_out=Ny/2
  ; plot line identification
  if keyword_set(show) then begin 
    scrdim = GET_SCREEN_SIZE(RESOLUTION=resolution)
    winxs=1200
    winys=400
    window,10+slitnum,xsize=winxs,ysize=winys, title=nameslit[slitnum]+" slit",xpos=(scrdim[0]-winxs) > 0,ypos=(scrdim[1]-winys-slitnum*winys)
    vector=neon(*,y_out)/max(neon(0.05*Nx:0.95*nx,y_out),/nan)
    vector=sqrt(vector>0)*1000
  
  
    plot,x,vector,xst=1,yst=1,yrange=[-10,1300]
    for k=0,N_lines-1 do begin
     pos=pos_lines(k,y_out)-delta_x
     pos_old=old(k,y_out)
     oplot,[pos,pos],[vector(pos)+0,vector(pos)+80],col=150
     oplot,[pos_old,pos_old]-w,[vector(pos)+0,vector(pos)+80],col=20
     oplot,[pos_old,pos_old]+w,[vector(pos)+0,vector(pos)+80],col=20
     xyouts,pos,vector(pos)+90,string(lines(k),format='(F7.2)'),/data,orientation=90
  endfor
  endif
  ;endif
  rms=fltarr(Ny)
  ;*********************************************************
  
  pos_lines_fit=pos_lines
  
  if keyword_set(yfit) and yfit[slitnum] gt 0 then begin
  ;polynomial fitting position line along slit
   y_pos=dindgen(Ny)
   for k=0,N_lines-1 do begin
    f=goodpoly(y_pos,double(pos_lines[k,*]),YFIT[slitnum],3.,Yfitp)
    pos_lines_fit[k,*]=Yfitp
   endfor
  endif else begin
  
  ; smoothing positions
   y_pos=dindgen(Ny)
   wins=5
   for k=0,N_lines-1 do begin
    d=reform(pos_lines[k,*])
    bad=where(~finite(d),nbad)
    if nbad gt 0 then d[bad]=-100
    pos_lines_fit[k,*]=smooth(median(d,wins),wins,/edge_TRUNCATE)
   endfor
  endelse
  
  ;****************************************************
  ;creation array coefficients dispersion curves
  disper_par=dblarr(N_deg[slitnum]+2,Ny)
  for y1=0,Ny-1 do begin
  f=goodpoly(double(pos_lines_fit[*,y1]),lines,N_deg[slitnum],tresh,Yfit_res,xpk,ypk)
   disper_par[0:N_deg[slitnum],y1]=f[*]
  
    lines_fit=poly(pos_lines[*,y1],f)
   rms(y1)=stdev(lines_fit-lines)
  endfor
  
  disper_par[N_deg[slitnum]+1,*]=rms[*]
  rms_mean=total(rms,/nan)/Ny
  
  
  ;*********************************************************************
  ;calculation and plot errors approximation
  
  lines_calc=fltarr(N_lines,Ny)
  for y=0,Ny-1 do begin
   lines_calc[*,y]=poly(pos_lines(*,y),disper_par(0:N_deg[slitnum],y))
  endfor
  
  dy=2.0
  tit=['(a) Left slit','(b) Right slit']
  For j=0,1 do begin ; PLOT/PS
  if j eq 1 and not keyword_set(show) then continue 
  if j eq 0 then begin
;     set_plot,'PS'
;     device,file=w_dir+'slit_'+string(slitnum,format="(I1)")+'_err_line.ps',/portrait,xsize=18,ysize=26,yoffset=0
     
cgps_open,w_dir+'slit_'+string(slitnum,format="(I1)")+'_err_line.eps',xs=8,ys=12,/encaps
loadct,0,/sil
   endif else begin
    winxs=700
    winys=750
    Window,13+slitnum,xsize=winxs,ysize=winys,tit=nameslit[slitnum]+" slit",xpos=slitnum*winxs,ypos=(scrdim[1]-winys)
    loadct,27,/sil
  endelse
  
  plot,[0,Ny],[-1,N_lines-2]*dy,xst=1,yst=1,/nodata,charsize=1.4,$
    position=[0.085,0.05,0.78,0.95],/norm,$
    title=tit[slitnum],$
    ;title='errors approximation dispersion curve  mean r.m.s='+$
    ;string(rms_mean,format='(F7.3)')+' px',$
    xtitle='Number of fiber',ytitle=greek('delta',/cap)+greek('lambda')+", pix"
  ;xyouts,0.1,0.005,'Comparision spectrum: '+w_dir,/norm
  ;xyouts,0.1,0.98,'number lines '+string(N_lines,format='(I2)')+$
  ;  ' Disperser '+dispname+' Ndeg='+string(N_deg[slitnum],format='(I2)'),/norm
  
  
  for k=1,N_lines-2 do begin
  ;for k=8, 8 do begin
  good=where(finite(lines_calc(k,*)))
  rms=stdev(-lines_calc(k,good)+lines(k),mean)
  ;mean=0
  d_lambda=poly(lines(k),[c_revers(1),c_revers(2)*2])
  oplot,findgen(Ny),(-lines_calc(k,*)+lines(k)-mean)/d_lambda+dy*(k-1),psym=6,symsize=0.3
  oplot,[0,Ny],[dy,dy]*(k-1)
  cgtext,Ny*1.02,dy*(k-1),string(lines(k),format='(F7.2)')+$
      ;string(mean,format='(F8.2)')+
      ' $\+-$'+$
      string(rms,format='(F5.2)'),charsize=1.4,/data
  endfor
   cgtext,Ny*1.11,dy*(N_lines-1-0.5),greek('lambda')+", $\Angstrom$",charsize=1.4,/data
;   cgtext,Ny*1.17,dy*(N_lines-1-0.5),greek('delta',/cap)+greek('lambda')+", $\Angstrom$",charsize=1.4,/data
  
  if j eq 0 then begin
     cgps_close
     ; device,/close
     ;if  !VERSION.OS_family eq 'Windows' then set_plot,'win' else set_plot,'X'
  endif
  
  ENDFOR ; PLOT/PS
  ;save dispersion curve
  disp_param_out[0:N_deg[slitnum]+1,*,slitnum]=float(disper_par)
  
  
  message,'mean error dispersion curves ='+string(rms_mean,format='(F5.2)')+' px',/cont
  if keyword_set(logfield) then IFURED_LOG_UPD, ["Mean error of disp. curve for "+nameslit[slitnum]+" ="+string(rms_mean,format='(F5.2)')+' px']
  f=disper_par(0:N_deg[slitnum],ny/2)
  
  if keyword_set(show) then begin 
    winxs=200
    winys=400
    window,16+slitnum,xs=winxs,ys=winys, title=nameslit[slitnum]+" slit",xpos=scrdim[0]-winxs-(1-slitnum)*winxs,ypos=(scrdim[1]-winys)
    !p.multi=[0,1,2]
    k=0
    xx=findgen(Nx)
    for i=1,n_deg[slitnum] do k=k+f(i)*i*xx^(i-1)
    plot,k,yst=1,xst=1,xtit='X, px',ytit='!7dk/d!3X'
    
    binsize=0.1
    x_hist=findgen(21)*binsize
    plot,x_hist,histogram(disper_par(N_deg[slitnum]+1,*),min=0,max=2.,binsize=binsize),$
    xst=1,yst=1,psym=10,title='errors',xtitle='error, px'
    !p.multi=[0,1,1]
  endif
  ENDFOR
  
  
  
  disper_par=float(disp_param_out)
  
  a=size(disper_par)
  N_deg=a[1]-2 & N_y=a[2]

  dlam=fltarr(2)
  l0=fltarr(2)
  l1=fltarr(2)
  
  
  
  FOR slit=0,1 do begin
    dlam[slit]=disper_par[1,N_y/2,slit]
    for k=0,N_deg do l0[slit]=l0[slit]+disper_par[k,Ny/2,slit]*(double(Nx_in[slit]/2.)^k)
    l0[slit]=l0[slit]-Nx_in[slit]*dlam[slit]/2.
    lam=fltarr(Nx_in[slit])
    x=dindgen(Nx_in[slit])
    for j=0,N_deg do lam=lam+disper_par[j,ny/2,slit]*x^j
    l1[slit]=lam[nx_in[slit]-1]
  ENDFOR
  dl_out=recommend_wlout[2];round(min(dlam)*100)/100.
  l0_out=recommend_wlout[0];ceil(min(l0)/10)*10
  l1_out=recommend_wlout[1];floor(max(l1)/10)*10
  ;nx_out=floor((l1_out-l0_out)/dl_out)
  
  if keyword_set(logfield) then IFURED_LOG_UPD, ["Dispersion along left slit: "+string(dlam[0],format="(F0.2)")+" A/pix",$
            "Dispersion along right slit: "+string(dlam[1],format="(F0.2)")+" A/pix",$
            "Wavelength range for left slit: "+string(l0[0],format="(I0)")+" - "+string(l1[0],format="(I0)")+" AA",$
            "Wavelength range for right slit: "+string(l0[1],format="(I0)")+" - "+string(l1[1],format="(I0)")+" AA",$
            "Recomended values for Lam0, Lam1, Dlam: ",string(l0_out,format="(I0)")+", "+string(l1_out,format="(I0)")+$
                    ", "+string(dl_out,format="(F0.2)")]
;  mkhdr,hdisp,disper_par
;  sxaddpar,hdisp,"CRVAL1",l0_out
;  sxaddpar,hdisp,"CDELT1",dl_out
;  sxaddpar,hdisp,"NPIX1",nx_out
  writefits,w_dir+'disper.fts',disper_par;,hdisp
END
