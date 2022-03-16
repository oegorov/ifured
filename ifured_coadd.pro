; adding all FITS with CH cleaning

PRO IFURED_COADD,filein,out,filetest=filetest,w_dir=w_dir,T=T,lim=lim,flat=flat,no_cr=no_cr,$
  nonorm=nonorm,win=win,d_dir=d_dir,bias=bias,invert_overscan=invert_overscan,objtype=objtype,immask=immask,$
  shiftX=shiftX,shiftY=shiftY,Xlim=Xlim,Ylim=Ylim,ref_frame=ref_frame,separate=separate, logfield=logfield, show=show,mask_y=mask_y


 if n_params() lt 2 then begin
  print,'IFURED_COADD,filein,out,[filetest=filetest,w_dir=w_dir,T=T,lim=lim,spectr=spectr,/flat,cut=cut,/nonorm,win=win,d_dir=d_dir,bias=bias,/invert_overscan,/shiftX,/ShiftY,XLIM=Xlim,Ylim=Ylim'
  return
 endif
 ; spectr -- set this keyword for spectral data cleaning
 ; flat -- for superflat construction

 if not(keyword_set(T)) then T=5
 if not(keyword_set(nonorm)) then nonorm=0.
 if not(keyword_set(filetest)) then filetest='ch_test.fts'
 if not(keyword_set(lim)) then lim=0
 if not(keyword_set(win)) then win=5


 cd,current=old_dir
if keyword_set(w_dir) then begin
  tst=file_test(w_dir)
  if tst eq 0 then begin
  res=dialog_message(['No such directory:',w_dir])
  return
  endif
  cd,w_dir
endif
 
 images_obs=readfits(filein, header)
if n_elements(header) eq 1 then begin
  res=dialog_message(['File is not found: ',filein])
  cd,old_dir
  return
endif

if sxpar(header, "NAXIS3") eq 0 then num=1 else num=sxpar(header, "NAXIS3")
Npair=num/2

xs=sxpar(header,'NAXIS1')
ys=sxpar(header,'NAXIS2')

xs_slit=[separate[0]+1,xs-separate[1]]
xs_out=max(xs_slit)
images=fltarr(xs_out,ys,2,num)
images[*]=!Values.F_NAN
sxaddhist,'IFURED_COADD: CH removed',header
sxdelpar,header,"NAXIS3"
sxdelpar,header,"CRVAL3"
sxdelpar,header,"CDELT3"
sxdelpar,header,"CRPIX3"


images[0:separate[0],*,0,*]=images_obs[0:separate[0],*,*]
images[0:xs-1-separate[1],*,1,*]=images_obs[separate[1]:xs-1,*,*]



if keyword_set(flat) then begin
   flatcicle=-3
   for slit=0,1 do begin
    xx=indgen(xs_slit[slit])
    m=median(images[xx,*,slit,*])
    for i=0,num-1 do images[*,*,slit,I]=images[*,*,slit,I]*m/median(images[xx,*,slit,I])
   endfor
endif else flatcicle=-1

xs=xs_out

; shift along X
 x1=fix(0.1*xs_slit)
 x2=fix(0.9*xs_slit)
; y1=fix(0.2*ys)
; y2=fix(0.8 *ys)
y1=fix(0.4*ys)
 y2=fix(0.65*ys)


if n_elements(Xlim) eq 4 then begin
  for qq=0,1 do begin
    x1[qq]=Xlim[2*qq]<(xs_slit[qq]-1)
    x2[qq]=Xlim[2*qq+1]<(xs_slit[qq]-1)
  endfor
endif
if n_elements(Ylim) eq 2 then begin
  y1=Ylim(0)<(ys-1)
  y2=Ylim(1)<(ys-1)
endif


 
 if n_elements(ref_frame) le 1 then begin
  ref_frame=fltarr(xs,ys,2)
  ref_frame[*]=!Values.F_NAN
  ref_frame[*,*,0]=reform(images[*,*,0,0])
  ref_frame[*,*,1]=reform(images[*,*,1,0])
  start_i=1
 endif else start_i=0
 
 
 
 if objtype eq 'eta' then begin
  ; Add mask to ref_frame (flat) to use only sky fibers for shifting
  immask=reform(images[*,*,*,0])
  for slit=0,1 do begin
    cur_immask=reform(immask[*,*,slit])
    ref_slit=reform(ref_frame[*,*,slit])
    s=size(cur_immask)
    rec=where(finite(cur_immask))
    med=abs(median(cur_immask[rec]))
    std=stddev(cur_immask[rec])
    rec=where(cur_immask lt med+2*std,nr)
    cur_immask[*]=1
    if nr gt 0 then cur_immask[rec]=0
    cur_immask=smooth(cur_immask,[0,8])
    rec0=where(cur_immask eq 0)
    rec1=where(cur_immask gt 0 and ref_slit gt 0)
    
    ref_slit[rec0] = min(ref_slit[rec1])
    ref_frame[*,*,slit]=ref_slit
    immask[*,*,slit]=cur_immask
  endfor
 endif
 
 
 
 dx_shi=fltarr(2, num)
 dy_shi=fltarr(2, num)

if keyword_set(shiftX) and num eq 1 and objtype eq 'obj' then begin
  xx1=[1550,540]; 480]
  xx2=[1600,570];660]
  yy1=600 & yy2=1280
  for slit=0,1 do begin
  if slit eq 1 then begin
  yy1=1130 & yy2=1230
  endif
    vec0=reform(total(ref_frame[xx1[slit]:xx2[slit],yy1:yy2,slit],2))
    for i=0,num-1 do begin
     vec=total(reform(images[xx1[slit]:xx2[slit],yy1:yy2,slit,i]),2)
     m=2.5
     dx=def_vecshift(vec0,vec,m=m,plot=0,bin=50)
     wait,1
     dx_shi[slit,i]=dx
     print,'** Image '+string(i,format='(I2)')+ ' slit '+string(slit,format="(I0)")+' was shifted on X'+string(dx)
     sxaddhist,'IFURED_COADD: Frame '+string(i,format='(I2)')+ ' slit '+string(slit,format="(I0)")+' was shifted on X:'+string(dx),header
     images[*,*,slit,i]=shift_image(reform(images[*,*,slit,i]),-dx,0)
    endfor
  endfor
 ENDIF

 if keyword_set(shiftX) and num gt 1 then begin
  for slit=0,1 do begin
    vec0=total(reform(images[x1[slit]:x2[slit],y1:y2,slit,0]),2)
    for i=1,num-1 do begin
     vec=total(reform(images[x1[slit]:x2[slit],y1:y2,slit,i]),2)
     m=2.5
     dx=def_vecshift(vec0,vec,m=m,plot=0,bin=50)
     dx_shi[slit,i]=dx
     print,'** Image '+string(i,format='(I2)')+ ' slit '+string(slit,format="(I0)")+' was shifted on X'+string(dx)
     sxaddhist,'IFURED_COADD: Frame'+string(i,format='(I2)')+ ' slit '+string(slit,format="(I0)")+' was shifted on X:'+string(dx),header
     images[*,*,slit,i]=shift_image(reform(images[*,*,slit,i]),-dx,0)
    endfor
  endfor
 ENDIF

if keyword_set(shiftY) and num ge start_i+1 then begin
  
  for slit=0,1 do begin

     vec0=reform(total(ref_frame[x1[slit]:x2[slit],y1:y2,slit],1))
    for i=start_i,num-1 do begin
     
     
     if n_elements(immask) gt 1 then begin
      cur_im=reform(images[*,*,slit,i])
      
      cur_immask=reform(immask[*,*,slit])
      recm0=where( cur_immask eq 0)
      recm1=where(cur_immask gt 0 and cur_im gt 0)
      mm=min(cur_im[recm1])
      cur_im[recm0] = mm
      vec=total(cur_im[x1[slit]:x2[slit],y1:y2],1)
      r0=where(vec gt mm)
      md= median(vec[r0])
      rec=where(vec lt md,nrec)
      if nrec gt 0 then vec[rec]=md
      vec=vec-md
      
      
     endif else $
     vec=reform(total(images[x1[slit]:x2[slit],y1:y2,slit,i],1))
     if n_elements(mask_y) eq 2 then begin
      yyy=indgen(n_elements(vec0))
      recmasky=where(yyy lt mask_y[0] or yyy gt mask_y[1],nry)
      if nry gt 0 then begin
        vec0[recmasky]=0.1
        vec[recmasky]=0.1
      endif
     endif
     m=3.5
     dy=def_vecshift(vec0,vec,m=m,bin=50,plot=1)
     wait,0.2
     dy_shi[slit,i]=dy
     
;     y=findgen(n_elements(vec0))
;     cgdisplay,1200,500,wid=11
;     cgplot,y,vec0/max(vec0),col="blue",xr=[100,1700],/xst
;     cgoplot,y+dy,vec/max(vec),col="red"
;     wait,1
     
     print,'** Image '+string(i,format='(I2)')+ ' slit '+string(slit,format="(I0)")+' was shifted on Y:'+string(dy)
     sxaddhist,'IFURED_COADD: Frame '+string(i,format='(I2)')+ ' slit '+string(slit,format="(I0)")+' was shifted on Y:'+string(dy),header
     images[*,*,slit,i]=shift_image(reform(images[*,*,slit,i]),0,-dy)
    endfor
  endfor
ENDIF



if keyword_set(logfield) and (total(dx_shi) ne 0 or total(dy_shi) ne 0) then begin
  
  IFURED_LOG_UPD, "Shifting:"
  for i= start_i,num-1 do begin
  IFURED_LOG_UPD,["Frame " + string(i,format='(I2)')+":", "   Left slit dx="+string(dx_shi[0,i],format="(F0.5)")+"; dy="+string(dy_shi[0,i],format="(F0.5)")+";",$
                  "   Right slit dx="+string(dx_shi[1,i],format="(F0.5)")+"; dy="+string(dy_shi[1,i],format="(F0.5)")]
  endfor
  
endif 


if keyword_set(logfield) then IFURED_LOG_UPD, "CR cleaning with threshold = "+string(t,format="(F0.1)")


Repeat BEGIN
if num eq 1 then begin ; One image!
   ; CH removing
   new=fltarr(xs,ys,2)
   old=reform(images)
   im1=reform(images[*,*,0,0])
   im2=reform(images[*,*,1,0])
   
   if not keyword_set(no_cr) then begin
     acre,im1,new1,T,win
     acre,im2,new2,T,win
     new[*,*,0]=new1
     new[*,*,1]=new2
   endif else begin
     new[*,*,0]=im1
     new[*,*,1]=im2
   endelse
 endif else BEGIN ; more than 1 image!

  old=total(images,4)
  if not keyword_set(no_cr) then begin
    FOR i=0,Npair-1 do begin
     message,"frames" + string(i*2)+'+'+string(i*2+1),/con
     
     for slit=0,1 do begin
       xx=indgen(xs_slit[slit])
       im1=reform(images[xx,*,slit,i*2])
       im2=reform(images[xx,*,slit,i*2+1])
       
       IFURED_CH_BOTH2,im1,im2,t=t,lim=lim,nonorm=nonorm
       images[xx,*,slit,i*2]=im1
       images[xx,*,slit,i*2+1]=im2
     endfor
   ENDFOR
   if (num mod 2) eq 1 then begin
    for slit=0,1 do begin
     xx=indgen(xs_slit[slit])
  ;   im1=reform(images[xx,*,slit,(Npair-1)*2])
  ;   im2=reform(images[xx,*,slit,(Npair-1)*2+1])
     im1=reform(images[xx,*,slit,num-2])
     im2=reform(images[xx,*,slit,num-1])
     IFURED_CH_BOTH2,im1,im2,t=t,lim=lim,nonorm=nonorm 
     images[xx,*,slit,num-2]=im1
     images[xx,*,slit,num-1]=im2
    endfor
   endif
 endif
new=total(images,4)
ENDELSE ; more than one image


; ****** If current type is flat, than update ref_frame to use further to shift eta **** 
if objtype eq 'flat' or objtype eq 'eta' then begin
  ref_frame=fltarr(xs,ys,2)
  ref_frame[*]=!Values.F_NAN
  ref_frame[*,*,0]=reform(images[*,*,0,0])
  ref_frame[*,*,1]=reform(images[*,*,1,0])
endif



 flatcicle=flatcicle+1
 print,'Circle :', flatcicle
 if keyword_set(flat) then images=shift(temporary(images),0,0,0,1)
 ENDREP UNTIL flatcicle eq 0

 if keyword_set(flat) then begin
  ; normalization
  m1=median(images[*,*,0,*])
  m2=median(images[*,*,1,*])
  for i=0,num-1 do begin
    images(*,*,0,I)=images(*,*,0,I)*median(images(*,*,0,I))/m1
    images(*,*,1,I)=images(*,*,1,I)*median(images(*,*,1,I))/m2
  endfor
  new=old
  IFURED_mean_bi,reform(images[*,*,0,*]),med,/nosub
  new1=med/num
  IFURED_mean_bi,reform(images[*,*,1,*]),med,/nosub
  new2=med/num
  new[*,*,0]=new1
  new[*,*,1]=new2
 endif


 if not keyword_set(no_cr) then begin
   res=old-new
   if keyword_set(show) then begin
     tit='Cosmic Hits'
     if keyword_set(objtype) then tit=tit+" ("+objtype+")"
     window,11,xs=1040,ys=520,tit=tit
     tv,-congrid(reform(res[*,*,0]),517,517),0.01,0.01,xs=0.48,ys=0.98,/norm;,/noerase
     tv,-congrid(reform(res[*,*,1]),517,517),0.49,0.01,xs=0.48,ys=0.98,/norm;,/noerase
   endif
 endif
 if keyword_set(w_dir) then cd,w_dir
 ;write files:

  sxdelpar,header,'FILE'
  sxaddpar,header,'BZERO',0
;  sxaddpar,header,'EXPTIME',total(texp),'Total esposition time'
;  all=names[0]
  WRITEFITS,out,new,header
  if not keyword_set(no_cr) then WRITEFITS,filetest,res
cd,old_dir
END