;*****************************************
;*This viewer of IFU data cubes is a     *
;*significatelly reduced version of CVIZ *   
;* originally written by Alexei Moiseev  *
;*****************************************

PRO IFURED_CVIZ_LOADFILE,file=file
  COMMON IFURED_CVIZ_IMCUB
  COMMON IFURED_CVIZ_WIDTXT
  COMMON IFURED_CVIZ_WIDWIN
  COMMON IFURED_CVIZ_WIDPAR
  COMMON IFURED_CVIZ_IMAGES
  COMMON IFURED_CVIZ_DRAW
  COMMON IFURED_CVIZ_PROFILES
  common IFURED_CVIZ_noimage
  common IFURED_CVIZ_cross
  COMMON IFURED_CVIZ_IMFLIPS
  COMMON IFURED_CVIZ_MASK
  COMMON IFURED_CVIZ_HICUBE
  COMMON IFURED_CVIZ_ModeProfileNorm
  COMMON IFURED_CVIZ_NEWBLOCKforIFU
  
          flag_NOimset=1;total(im_set(0)) eq 0
          j=0
      
            cub.file=file
            fdecomp,file,disk,dir,name,qual
            cub.name=name
             cd,w_dir
             ; read FITS
             new=readfits(file,head)
             nn=sxpar(head,'naxis')
             nz=sxpar(head,'naxis3')

             if nn eq 3 and nz gt 2 then begin ; 3D format (IFU, IFP)
              xsnew=sxpar(head,'naxis1')
              ysnew=sxpar(head,'naxis2')
              zsnew=sxpar(head,'naxis3')
             endif else begin ; Not 3D
               res=dialog_message("This file is not data cube!")
               return
;              zsnew=sxpar(head,'naxis1')
;              XYs=sxpar(head,'naxis2')
;              xsnew=sxpar(head,'nx')
;             if xsnew eq 0 then  xsnew=16
;               ysnew=XYs/xsnew
            endelse

;             if ~flag_NOimset and not(((xsnew eq xs) or (xs eq -1)) and ((ysnew eq ys) or (ys eq -1)))    then begin
;              res=widget_message(['New cube XY-size differ from images!','Please CLEAN windows!'])
;              return
;             endif


             if (xs eq -1) then  images=fltarr(xsnew,ysnew,nim)
             if nn eq 3 then flag3D=1 else Flag3D=0



              IF flag3d THEN BEGIN
               CRVAL1=SXPAR(head,'CRVAL3')
               CDELT1=SXPAR(head,'CDELT3')
               CRPIX1=SXPAR(head,'CRPIX3')
                  ctype3=strtrim(sxpar(head,'CTYPE3'),2)
               if ctype3 eq  'FELO-HEL' then flag21=1
              ENDIF ELSE BEGIN
              ;set wavelength scale
               CRVAL1=SXPAR(head,'CRVAL1')
               CDELT1=SXPAR(head,'CDELT1')
               CRPIX1=SXPAR(head,'CRPIX1')
              ENDELSE

               if crval1 eq 0 then CRVAL1=1
               if cdelt1 eq 0 then CDELT1=1

               lda0=(findgen(zsnew)-crpix1+1)*CDELT1+CRVAL1
               if flag21 then lda0=lda0/1000 ;21 cm data cubes
               
               
               
              ; ======= LOAD CUBE FIRST TIME
             if im_set(1) eq 0  then begin

              if flag3D then cubes=fltarr(xsnew,ysnew,zsnew*2,nim) $
                        else cubes=fltarr(zsnew*2,xsnew*ysnew,nim)

               ;Xi0=0 & Yi0=0
               ;xs=xsnew &  ys=ysnew &
               im_set(1)=1
               lam1=round(lda0(0))
               lam2=round(lda0(zsnew-1))
               lda=fltarr(zsnew*2,nim)
              endif ; first time

              s=size(cubes)
              zs(j)=zsnew
              if flag3D then cubes(*,*,0:zs(j)-1,j)=new else cubes(0:zs(j)-1,*,j)=new
              lda(0:zs(j)-1,j)=lda0

              cub(j).uses=1
             ;widget_control,cubwid(j).uses,set_button=cub(j).uses
              widget_control,cw_l1,set_value=lam1
              widget_control,cw_l2,set_value=lam2

              ;IFURED_CVIZ_CUB_ONOFF,j,1

              ; **** If images have not loaded yet -> show the central channel!  ==========

              if flag_NOimset or (zoom_prf eq 3 and J eq 0) then begin
              xs=xsnew
              ys=ysnew

              im_set(J)=1

              new=reform(cubes(*,*, zs(j)/2,J))

              rec=where(new gt 0,cc)
             ; visualisation cuts determination
             if cc gt 1 then  Imin=min(new(rec)) else Imin=min(new)
             Imax=max(new)
             ; cuts rounded
             nsig=2 ; significant number
             pow=exp((nsig-round(alog10(Imax)))*alog(10))
             Imax=round(Imax*pow)/pow
             pow=exp((nsig-round(alog10(Imin)))*alog(10))
             Imin=round(Imin*pow)/pow
             im(j).min=Imin
             im(j).max=Imax
             images(*,*,j)=new
            ; zoom determination
            zoom=min([float(Xwin)/xsnew,float(Ywin)/ysnew])
            if zoom gt 1 then zoom = round(zoom)
            Xi0=0 & Yi0=0
           IFURED_CVIZ_DRAW_IMAGE
           zoom_prf=2
           ;IFURED_CVIZ_IM_ONOFF,j,1
           widget_control,cw_zp,set_value=zoom_prf
              endif
              
END


;load gray-scale(0), invert gray-scale(1) or red-blue (2) color table
PRO IFURED_CVIZ_Loadct_my,ID
  COMMON IFURED_CVIZ_IMCUB
  cgloadct,color_tab.INDEX,brewer=color_tab.BREWER,reverse=color_tab.reversed, /sil
;  case id of
;  0: loadct,0,/sil
;  1: begin
;      bind=255-BINDGEN(256)
;      TVLCT, bind, bind, bind
;     end
;  2: loadct,27,/sil
;  3: begin
;      loadct,27,/sil
;      loadct,33,/sil,ncolors=255,bottom=1
;     end
; endcase
END

; switch senitive of CUBES' widgets
;PRO IFURED_CVIZ_CUB_ONOFF,id,sens
;COMMON IFURED_CVIZ_IMCUB
; WIDGET_CONTROL,cubwid(id).color,sensitive=sens
; WIDGET_CONTROL,cubwid(id).uses,sensitive=sens
;END

; switch senitive of IMAGES' widgets
;PRO IFURED_CVIZ_IM_ONOFF,id,sens
;COMMON IFURED_CVIZ_IMCUB
;COMMON IFURED_CVIZ_WIDTXT
; WIDGET_CONTROL,imwid(id).ct,sensitive=sens
; WIDGET_CONTROL,imwid(id).contour,sensitive=sens
; WIDGET_CONTROL,butim(id),sensitive=sens
; WIDGET_CONTROL,imwid(id).overlap,sensitive=sens and im(id).contour
; ;WIDGET_CONTROL,imwid(id).follow,sensitive=sens and im(id).contour
;END


PRO IFURED_CVIZ_OVERPLOT_PRF,xi1,yi1,lxn,lyn,PS=PS ; overplot profiles on image

COMMON IFURED_CVIZ_IMCUB
COMMON IFURED_CVIZ_IMAGES
COMMON IFURED_CVIZ_DRAW
COMMON IFURED_CVIZ_PROFILES
COMMON IFURED_CVIZ_WIDWIN
COMMON IFURED_CVIZ_WIDTXT
COMMON IFURED_CVIZ_WIDPAR
COMMON IFURED_CVIZ_PLOTPS
COMMON IFURED_CVIZ_IMFLIPS
COMMON IFURED_CVIZ_HICUBE

  if ((xi1-xi0) gt 25) or((yi1-yi0) gt 25)  then return
;  if ((xi1-xi0) le 2) or((yi1-yi0) le 2)  then return

if not(keyword_set(ps)) then begin
 widget_control,widdraw1,get_value=Id
 wset,id
endif
 rec=where(cub.uses eq 1,nc)

 loadct,27,/sil
 lx=Lxn/(xi1-xi0+1.)
 ly=Lyn/(yi1-yi0+1.)

 if keyword_set(ps) then begin
  lx=Lx_ps*1000./(xi1-xi0+1.)
  ly_ps=Lx_ps*float(lyn)/Lxn
  ly=Ly_ps*1000./(yi1-yi0+1.)
 endif

 pos=[0,0,lx,lx]
 recw=where((lda(*,rec(0)) lt lam2) and (lda(*,rec(0)) gt lam1 ))
 if flag21 then recw=where((lda(*,rec(0)) lt lam1) and (lda(*,rec(0)) gt lam2 ))

 ddy=0
 If FlipY eq 1 and Lyn gt Ywin then ddy=-1
 for x=xi0,xi1 do begin
  for y=yi0,yi1+ddy do begin
   x0=(x-xi0)*lx
    If FlipY eq 1 then   y0=(Ywin-(y-yi0+1)*lx)  else y0=((y-yi0)*lx)

   if keyword_set(ps) then begin
    If FlipY eq 1 then y0=(y0_ps+ly_ps)*1000-(y-yi0+1)*lx else y0=y0_ps*1000+(y-yi0)*lx
    x0=x0+x0_ps*1000
   endif
    num=y*xs+x ; element number
     rec=where(cub.uses eq 1,nc)
    if nc eq 0 then return
    if flag3D then  prf=cubes(x,y,0:zs(rec(0))-1,rec(0)) else    prf=cubes(0:zs(rec(0))-1,num,rec(0))
    yr=[min(prf(recw),/nan),max(prf(recw),/nan)]
    yr=yr+(yr(1)-yr(0))*[-0.05,0.05]
   plots,[x0,x0+lx,x0+lx,x0,x0],[y0,y0,y0+lx,y0+lx,y0],/dev,col=250
   plot,lda(0:zs(rec(0))-1,rec(0)),prf,xst=5,xr=[lam1,lam2],yr=yr,yst=5,pos=[x0,y0,x0+lx,y0+lx],$
      /dev,/noerase,col=color_val(cub(rec(0)).color)

    for i=1,nc-1 do begin
     if flag3D then  prf=cubes(x,y,0:zs(rec(i))-1,rec(i)) else    prf=cubes(0:zs(rec(i))-1,num,rec(i))
     oplot,lda(0:zs(rec(i))-1,rec(i)),prf,col=color_val(cub(rec(i)).color)
    endfor
  endfor
 endfor

END

PRO IFURED_CVIZ_DRAW_PRF,x,y ; plot profiles to large window
COMMON IFURED_CVIZ_IMCUB
COMMON IFURED_CVIZ_IMAGES
COMMON IFURED_CVIZ_DRAW
COMMON IFURED_CVIZ_PROFILES
COMMON IFURED_CVIZ_WIDWIN
COMMON IFURED_CVIZ_WIDTXT
COMMON IFURED_CVIZ_HICUBE
COMMON IFURED_CVIZ_ModeProfileNorm

 if Flag3D  then widget_control,prfnum,set_value=' ' else $
  widget_control,prfnum,set_value='Spectrum ('+string([X,Y],format='(2(I4,:, ", "))')+')'
 widget_control,widdraw2,get_value=Id
 wset,id

  num=y*xs+x ; element number
  if num lt 0 then return
  rec=where(cub.uses eq 1,nc)
  if nc eq 0 then return

  if flag3D then  prf=cubes(x,y,0:zs(rec(0))-1,rec(0)) else    prf=cubes(0:zs(rec(0))-1,num,rec(0))

  if flag21 then begin
    recw=where((lda(0:zs(rec(0))-1,rec(0)) lt lam1) and (lda(0:zs(rec(0))-1,rec(0)) gt lam2 ))
    xtit='km/s'
  endif else begin
  recw=where((lda(0:zs(rec(0))-1,rec(0)) lt lam2) and (lda(0:zs(rec(0))-1,rec(0)) gt lam1 ))
  xtit='Wavelength'
  endelse

y1=min(prf(recw),max=y2,/nan)

if profnorm eq 0 then begin
for i=1,nc-1 do begin
   if flag3D then  prf0=cubes(x,y,0:zs(rec(i))-1,rec(i)) else    prf0=cubes(*,num,rec(i))
    prf0=reform(prf0(recw))

    y1=min([prf0,y1],/nan)
    y2=max([prf0,y2],/nan)
  endfor
endif
yr=[y1,y2]

  ;yr=[min(prf(recw)),max(prf(recw))]
  yr=yr+(yr(1)-yr(0))*[-0.05,0.05]
loadct,0,/sil
  plot,lda(0:zs(rec(0))-1,rec(0)),prf,xst=1,xr=[lam1,lam2],yr=yr,yst=1,xtit=xtit,col=9,back=255
loadct,33,/sil
  ; stored the coordinate system:
   prf_clip=!p.clip(0:3)
   LimX=!x.crange
   LimY=!y.crange

  oplot,lda(0:zs(rec(0))-1,rec(0)),prf,col=color_val(cub(rec(0)).color)

  for i=1,nc-1 do begin
   if flag3D then  prf=cubes(x,y,0:zs(rec(i))-1,rec(i)) else    prf=cubes(*,num,rec(i))

   ; profnorm=1

   if  profnorm then begin
    recs=where(lda(0:zs(rec(i))-1,rec(i)) ge lam1 and lda(0:zs(rec(i))-1,rec(i)) le lam2,nnn)
    if nnn gt 0 then begin
    prf=prf-min(prf(recs),/nan)
    prf=prf/max(prf(recs),/nan)
    prf=prf*(y2-y1)+y1
    endif
   endif

   oplot,lda(0:zs(rec(i))-1,rec(i)),prf,col=color_val(cub(rec(i)).color)
  endfor

;  for i=0,nc-1 do begin
;   xyouts,0.2+0.2*i,0.96,cub(rec(i)).name,/norm,color=color_val(cub(rec(i)).color)
;  endfor
END

PRO IFURED_CVIZ_DRAW_IMAGE,PS=PS,testcolor=testcolor ; plot current image
COMMON IFURED_CVIZ_IMCUB
COMMON IFURED_CVIZ_IMAGES
COMMON IFURED_CVIZ_DRAW
COMMON IFURED_CVIZ_WIDWIN
COMMON IFURED_CVIZ_WIDPAR
COMMON IFURED_CVIZ_WIDTXT
COMMON IFURED_CVIZ_PLOTPS
common IFURED_CVIZ_widPS
common IFURED_CVIZ_cross
COMMON IFURED_CVIZ_IMFLIPS

 if not(keyword_set(ps)) then begin
  widget_control,widdraw1,get_value=Id
  wset,id
 endif

 prev_col=-1 ;
 j=im_id

 imax=im(j).max & imin=im(j).min
 widget_control,cw_min,set_value=Imin
 widget_control,cw_max,set_value=Imax


 image=images(*,*,j)
 if zoom ge 1 then begin ; Zoom >1
    lxn=round(xwin/zoom)
    lyn=round(ywin/zoom)
    xi1=(xi0+lxn-1)<(xs-1)
    yi1=(yi0+lyn-1)<(ys-1)
    xi1=(xi1+1)<(xs-1)
    lxn=(xi1-xi0+1)*zoom




    yi1=(yi1+1) <(ys-1)
    ;if yi1 lt ys-2 then yi1=yi1+1
    lyn=(yi1-yi0+1)*zoom

    if im(j).contour eq 0 then curimage=rebin(image(xi0:xi1,yi0:yi1),lxn,lyn,/sample)
 endif else begin   ; Zoom <1
    lxn=round(xwin/zoom)
    lyn=round(ywin/zoom)
    xi1=(xi0+lxn-1)<(xs-1)
    yi1=(yi0+lyn-1)<(ys-1)
    z1=round(1./zoom)
    lxn=(xi1-xi0+1)/z1
    lyn=(yi1-yi0+1)/z1
    if im(j).contour eq 0 then curimage=congrid(image(xi0:xi1,yi0:yi1),lxn,lyn)

 endelse


if not keyword_set(testcolor) then IFURED_CVIZ_Loadct_my;,im(j).ct
if not(keyword_set(ps)) then erase
ly_ps=float(lyn)*lx_ps/lxn
pos_ps=[x0_ps,y0_ps,x0_ps+lx_ps,y0_ps+ly_ps]*1000


  pallete=bytarr(3,!D.TABLE_SIZE)
  pallete[0,*]=color_tab.R
  pallete[1,*]=color_tab.G
  pallete[2,*]=color_tab.B
        



if im(j).contour eq 0 then begin ; Image mode
  if FlipY eq 1 then curimage=rotate(curimage,7)
  curimage=bytscl(curimage,min=Imin,max=Imax)
  rec=where(curimage eq 0,counts)
  if counts gt 0 then curimage(rec)=1
  if FlipY eq 1 then Ytmp=Ywin-lyn else Ytmp=0
  if not(keyword_set(ps)) then begin
    if not keyword_set(testcolor) then begin
;      IFURED_CVIZ_Loadct_my
          tv,curimage,0,Ytmp; cgimage,curimage,0,Ytmp,/tv,palette=pallete $
       endif else  tv,curimage,0,Ytmp ;cgimage,curimage,0,Ytmp,/tv
    
  endif else begin  
    ;cgimage,curimage,0,Ytmp,palette=pallete else begin
   ;                 keep_asp=keep_asp,oposition=opos,tv=tvpar,xrange=xrange,yrange=yrange,pos=pos ;tv,curimage,0,Ytmp else begin
    ; TV to PS
   ;cgimage,curimage,-xscr_cur[type],-yscr_cur[type],minval=minval,maxval=maxval,MISSING_VAL="NAN",MISSING_IND=0,$
   ;                 keep_asp=keep_asp,oposition=opos,tv=tvpar,xrange=xrange,yrange=yrange,pos=pos
    IFURED_CVIZ_Loadct_my
    tv,curimage, x0_ps,y0_ps,xs=lx_ps,ys=ly_ps,/centim

  endelse
endif else begin ; contour mode
 pos=[0, Ywin-lyn,lxn,Ywin]
  col=250 & back=0
  if  im(j).ct eq 1 then  back=5
 if not(keyword_set(ps)) then  erase,back else pos=pos_ps
 if flipy eq 1 then tmpcon=rotate(image(xi0:xi1,yi0:yi1),7) $
     else tmpcon=image(xi0:xi1,yi0:yi1)
    if (im(j).nlev gt 1)and (im(j).nlev lt 29) then $
    contour,tmpcon,pos=pos,/dev,col=col,$
    max_val=im(j).max,min_val=im(j).min,nlev=im(j).nlev,/noerase,xst=5,yst=5
 endelse

; overplot (if global set)

rec=where(im.overlap eq 1,nc)

k=j
IF nc gt 0 then begin
 for i=0,nc-1 do begin
  j=rec(i)
   image=images(xi0:xi1,yi0:yi1,j)
   pos=[0, Ywin-lyn,lxn,Ywin]
   if keyword_set(ps) then pos=pos_ps

  col=(230-i*100)>10
  if flipy eq 1 then tmpcon=rotate(image,7) $
    else tmpcon=image

   if (im(j).nlev gt 1)and (im(j).nlev lt 29) then begin
    IFURED_CVIZ_Loadct_my,1
    contour,tmpcon,pos=pos,/dev,col=col,follow=im(j).follow,$
    max_val=im(j).max,min_val=im(j).min,nlev=im(j).nlev,/noerase,xst=5,yst=5
   endif
 endfor
endif



if keyword_set(ps) then begin

 ; color scale box


 ;IFURED_CVIZ_Loadct_my,im(k).ct

 dh=0.5
 len=256
 line=fltarr(len,10)
 for j=0,9 do line(*,j)=findgen(len)*(Imax-imin)/(len-1)+Imin
if strcompress(units_ps,/remove) ne  '' then tv,bytscl(line(1:*,*),max=imax,min=imin),x0_ps,y0_ps+ly_ps+3*dh,xs=lx_ps,ys=dh,/centim

 loadct,0,/sil
if strcompress(units_ps,/remove) ne  '' then begin
 p=[x0_ps,y0_ps+ly_ps+3*dh,x0_ps+lx_ps,y0_ps+ly_ps+4*dh]*1000
 plots,[p(0),p(2),p(2),p(0),p(0)],[p(1),p(1),p(3),p(3),p(1)],/dev,col=0
endif
; coordinate system:
 xr=[xi0-0.5,xi1+0.5]-xc_ps*used
 If FlipY  then yr=[yi1+0.5,yi0-0.5] else yr=[yi0-0.5,yi1+0.5]-yc_ps*used

 if used and  FlipY  then  yr= [-yi1-0.5,-yi0+0.5]+yc_ps

 xr=xr*px_ps
 yr=yr*px_ps
 plot,[0],[0],pos=pos_ps,/dev,/noerase,xr=xr,yr=yr,xst=1,yst=1,$
 xtit=xtit_ps,ytit=ytit_ps,tit=title_ps,col=0
 oplot,[0],[0],psym=1,symsize=2,thick=2,col=0
 if strcompress(units_ps,/remove) ne  '' then axis,0,p(3),xaxis=1,XTICKlen=0,xr=[Imin,Imax],xst=1,/dev,xticks=1,xtit=units_ps,col=0

; PAOY
len=2000
xc=14500
yc=12000
if used then begin
 if paoy_ps gt -360 then begin
 one_arrow,xc,yc,-paoy_ps+90,'N',color=0,arrowsize=[len,len*0.2,30],thick=1.5,charsize=1.1
 one_arrow,xc,yc,-paoy_ps+180,'E',color=0,arrowsize=[len*0.6,len*0.2,30],thick=1.5,charsize=1.1
 endif
endif

ENDIF


widget_control,filetxt,set_value=cub.name
fin:
if cont_rems eq 1 then IFURED_CVIZ_OVERPLOT_PRF,xi1,yi1,lxn,lyn,PS=PS

END

PRO IFURED_CVIZ_EVENT,event
COMMON IFURED_CVIZ_IMCUB
COMMON IFURED_CVIZ_WIDTXT
COMMON IFURED_CVIZ_WIDWIN
COMMON IFURED_CVIZ_WIDPAR
COMMON IFURED_CVIZ_IMAGES
COMMON IFURED_CVIZ_DRAW
COMMON IFURED_CVIZ_PROFILES
COMMON IFURED_CVIZ_PLOTPS
common IFURED_CVIZ_noimage
common IFURED_CVIZ_cross
COMMON IFURED_CVIZ_IMFLIPS
COMMON IFURED_CVIZ_MASK
COMMON IFURED_CVIZ_HICUBE
COMMON IFURED_CVIZ_ModeProfileNorm
COMMON IFURED_CVIZ_NEWBLOCKforIFU
 WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

;   ; images/profiles properties
;   first=strmid(ev,0,1)
;   ; *** images ***
;   IF first EQ '#' THEN BEGIN
;     second=strmid(ev,1,2)
;     j=fix(strmid(ev,4,1))
;     j=j(0)-1
;     CASE  second OF
;        'lo': begin ; load image
;             file=dialog_pickfile(title='Load Image'+string(j+1),filter=['*.f*s'],get_path=w_dir)
;             if file eq '' then return
;             im_set(0)=1
;             im(j).file=file
;             fdecomp,file,disk,dir,name,qual
;             im(j).name=name
;             cd,w_dir
;             ; read FITS
;             new=readfits(file,head)
;             xsnew=sxpar(head,'naxis1')
;             ysnew=sxpar(head,'naxis2')
;
;             if not(((xsnew eq xs) or (xs eq -1)) and ((ysnew eq ys) or (ys eq -1))) then begin
;              res=widget_message(['New image differ from other ones!','Please do  File/Clean windows!'])
;              return
;             endif
;             if sxpar(head,'naxis3') gt 1 then begin
;              new=new(*,*,0)
;             endif
;
;             if (xs eq -1) then begin
;               images=fltarr(xsnew,ysnew,nim)
;               ; zoom determination
;               zoom=min([float(Xwin)/xsnew,float(Ywin)/ysnew])
;
;               if zoom gt 1 then zoom = round(zoom)
;               Xi0=0 & Yi0=0
;             endif
;
;
;             images(*,*,j)=new
;             xs=xsnew
;             ys=ysnew
;
;              im(j).header(0:n_elements(head)-1)=head
;              IFURED_CVIZ_IM_ONOFF,j,1
;              ; DRAW this image
;
;              widget_control,butim(im_id),set_button=0
;              im_id=j ; new current image
;              widget_control,butim(j),set_button=1
;
;             rec=where(new gt 0,cc)
;             ; visualisation cuts determination
;             cuts=minmax(new)
;             if cc gt 1 then  cuts(0)=min(new(rec))
;             ; cuts rounded
;             nsig=2 ; significant number
;             pow=exp((nsig-round(alog10(cuts)))*alog(10))
;             cuts=round(cuts*pow)/pow
;             im(j).min=cuts(0)
;             im(j).max=cuts(1)
;              IFURED_CVIZ_DRAW_IMAGE
;
;              end
;        'co': begin ; contour
;
;              im(j).contour=event.select
;              IFURED_CVIZ_IM_ONOFF,j,1
;              if j eq im_id then IFURED_CVIZ_draw_image
;              end
;        'ov': begin
;                 im(j).overlap=event.select
;                if j eq im_id then IFURED_CVIZ_draw_image
;              end
;
;        'fo': begin
;                 im(j).follow=event.select
;                 if j eq im_id then IFURED_CVIZ_draw_image
;              end
;        'ct': begin
;               im(j).ct=event.index
;               if j eq im_id then IFURED_CVIZ_draw_image
;              end
;         else:
;     ENDCASE
;   ENDIF
   ; *** CUBES ***
;   IF first EQ '@' THEN BEGIN
;     second=strmid(ev,1,2)
;     j=fix(strmid(ev,4,1))
;     j=j(0)-1
;     CASE  second OF
;        'lo': begin ; load cube
;             
;
;              end
;        'us': cub(j).uses=event.select
;        'co': cub(j).color=event.index
;         else:
;     ENDCASE
;   ENDIF

   ; *** Other events ***
   CASE Ev OF
   
   'load': begin
      
      file=dialog_pickfile(title='Load Cube',filter=['*.f*s']);,get_path=w_dir)
      if file eq ''  then return
      
      IFURED_CVIZ_LOADFILE,file=file
   
   END
   
   
   'min': begin
           widget_control,cw_min,get_value=Imin
            im(im_id).min=float(Imin(0))
           IFURED_CVIZ_DRAW_IMAGE
          end
   'max': begin
           widget_control,cw_max,get_value=Imax
           im(im_id).max=float(Imax(0))
           IFURED_CVIZ_DRAW_IMAGE
           end
           
    'brt_auto': auto_brt=1-auto_brt       
;   'nlev': begin
;           widget_control,cw_nlev,get_value=v
;           im(im_id).nlev=v(0)
;           IFURED_CVIZ_DRAW_IMAGE
;           end

   'l1': begin
           widget_control,cw_l1,get_value=l
           if l(0) ge lam2 then begin
            widget_control,cw_l1,set_value=lam1
            return
           endif
           lam1=l(0)
           IFURED_CVIZ_DRAW_PRF,Xold,Yold
           if cont_rems eq 1 then IFURED_CVIZ_DRAW_IMAGE
          end
   'l2': begin
           widget_control,cw_l2,get_value=l
           if l(0) le lam1 then begin
            widget_control,cw_l2,set_value=lam2
            return
           endif
           lam2=l(0)
           IFURED_CVIZ_DRAW_PRF,Xold,Yold
           if cont_rems eq 1 then IFURED_CVIZ_DRAW_IMAGE
          end

   'ReDraw':   if im_set(0)  then    IFURED_CVIZ_DRAW_IMAGE
      'win1': begin ; IMAGE WINDOW
           ; plot coordinates
           if im_set(0) eq 0 then return

           Xcur=floor(xi0+event.x/float(zoom))
           If FlipY eq 1 then Ycur=floor(yi0+(Ywin-event.y)/float(zoom)) $
                 else Ycur=floor(yi0+event.y/float(zoom))
           if (Xcur le (xs-1)) and (Ycur le (ys-1)) then begin
             Ival=string(images(xcur,ycur,im_id))
             Xval=string(Xcur,format='(I4)')
             Yval=string(Ycur,format='(I4)')
           endif else begin
            Ival='none'
            Xval='none'
            Yval='none'
           endelse

           widget_control,Xtxt,set_value='X:'+Xval
           widget_control,Ytxt,set_value='Y:'+Yval
           widget_control,Itxt,set_value='I:'+Ival

         ; *** Scan mode
          if  zoom_prf eq 2 then begin
           if event.press gt 0 then pressbut=1
           if event.release gt 0 then pressbut=0
          if pressbut eq 1 then return
          endif


           ; Zooming:

           if (event.press gt 0) and (zoom_prf eq 0 ) and (Yval ne 'none')then begin

           CASE event.press OF
            1:  begin ; Left button : Zoom(+)
                zoom=zoom*2 < max([float(Xwin),float(Ywin)])/2;(max([xs,ys])/10)
                ;if zoom ne max([float(Xwin),float(Ywin)])/2 then begin
                 if zoom gt 1 then zoom = round(zoom)
                 xi0=(Xcur-(Xwin/2)/zoom)>0
                 yi0=(Ycur-(Ywin/2)/zoom)>0
                 IFURED_CVIZ_DRAW_IMAGE
                ;endif
                end

            4:  begin ; Right button : Zoom(-)

                zoom=zoom*0.5 > min([float(Xwin)/Xs,float(Ywin)/Ys])
                ;if zoom ne min([float(Xwin)/Xs,float(Ywin)/Ys]) then begin
                 if zoom gt 1 then zoom = round(zoom)
                 xi0=(Xcur-(Xwin/2)/zoom)>0
                 yi0=(Ycur-(Ywin/2)/zoom)>0
                 IFURED_CVIZ_DRAW_IMAGE
                ;endif
                end
             else:
            endcase

           endif

           rec=where(cub.uses ne 0,nc)
           if (zoom_prf ne 1)and((Xold ne Xcur) or (Yold ne Ycur)) $
           and (nc gt 0) and (Ival ne 'none') and im_set(1)  then begin
             IFURED_CVIZ_DRAW_PRF,Xcur,Ycur
             Xold=Xcur
             Yold=Ycur
           endif


           ; PROFILE mode

         if (event.press gt 0) and (zoom_prf eq 1) and (Yval ne 'none') and im_set(1) then begin
          widget_control,widdraw1,get_value=Id
          wset,id

            X1=(Xcur+[0.,1.]-xi0)*zoom

            If FlipY eq 1 then y1=Ywin-(Ycur+[0.,1.]-yi0)*zoom $
                 else y1=(Ycur+[0.,1.]-yi0)*zoom

            ; erase previuos cross
            if prev_col ne -1 then begin
             IFURED_CVIZ_Loadct_my
             if  im(im_id).ct eq 1 and prev_col eq 0 then prev_col=1
             prev_col=bytscl(images((prev_x1[0]/zoom+xi0),(prev_y1[0]/zoom+yi0),im_id),im(im_id).min,im(im_id).max)
             plots,prev_x1+[1,-1],prev_y1+[1,-1],psym=0,/dev,col=prev_col,thick=2
             plots,prev_x1+[1,-1],[prev_y1(1),prev_y1(0)]+[-1,1],psym=0,/dev,col=prev_col,thick=2
             
            endif

            ; *** draw cross **
            im_col=bytscl(images(xcur,ycur,im_id),im(im_id).min,im(im_id).max)
            im_cross=im_col+126
            if im_cross gt 255 then im_cross-=256
            ;im_cross=255-im_col
;            if abs(im_cross-im_col) lt 30 then im_cross=im_cross+60


            plots,x1+[1,-1],y1+[1,-1],psym=0,/dev,col=im_cross,thick=2
            plots,x1+[1,-1],[y1(1),y1(0)]+[-1,1],psym=0,/dev,col=im_cross,thick=2
             Xold=Xcur
             Yold=Ycur
             IFURED_CVIZ_DRAW_PRF,Xold,Yold
             ; save values
             prev_x1=x1
             prev_y1=y1
             prev_col=im_col

          endif
;         ; masking mode
;         if (event.press gt 0) and (zoom_prf eq 2) and (Yval ne 'none') then begin
;             if winmask gt 1 then w=winmask/2 else w=0
;             x1=(xcur-w)>0 & x2=(xcur+w)<(xs-1)
;             y1=(ycur-w)>0 & y2=(ycur+w)<(ys-1)
;             images(x1:x2,y1:y2,im_id)=maskval
;
;             if extmask(0) then  x1=0
;             if extmask(1) then  x2=xs-1
;             if extmask(2) then if flipY then  y1=0 else y2=ys-1
;             if extmask(3) then if flipY then  y2=ys-1 else y1=0
;
;             images(x1:x2,y1:y2,im_id)=maskval
;
;
;
;            IFURED_CVIZ_draw_image
;         endif

           end

;   'Erase': begin ;  erase all data
;             xs=-1
;             ys=-1
;             zs=replicate(-1,nim)
;             im_set(*)=0
;             for i=0, nim-1 do IFURED_CVIZ_IM_ONOFF,i,0
;             for i=0, nim-1 do IFURED_CVIZ_CUB_ONOFF,i,0
;           widget_control,widdraw1,get_value=Id
;           wset,id & erase
;           widget_control,widdraw2,get_value=Id
;           wset,id & erase
;
;            end
   'PSmap': BEGIN ; plot maps to PS
             title_ps=im(im_id).name
             IFURED_CVIZ_PS_TITLE; SETTING Title of the figure
             nameps=dialog_pickfile(Title='Save Current Image as ',filter='*.ps')
             if strlen(nameps) eq 0 then return




;             set_plot,'PS'
;             device,file=nameps
             
             cgps_open,nameps
             IFURED_CVIZ_DRAW_IMAGE,/PS

             cgps_close
;             if  !VERSION.OS_family eq 'Windows' then set_plot,'win' else set_plot,'X'


            END
   'win2': begin ; PROFILE WINDOW
           ; plot coordinates
           rec=where(cub.uses ne 0,nc)
           if nc eq 0 then return
           widget_control,widdraw2,get_value=Id
           wset,id
           ; coordinate covertions:
           X=(float(event.x)-prf_clip(0))*(LimX(1)-LimX(0))/(prf_clip(2)-prf_clip(0))+limX(0)
           Y=(float(event.y)-prf_clip(1))*(LimY(1)-LimY(0))/(prf_clip(3)-prf_clip(1))+limY(0)
           res=[event.x,event.y]

           if flag3D then format='(F7.2)' else format='(F6.1)'
           if flag21 then begin
             format='(F11.2)'
             wtit='Velocity:'
           endif else wtit='Wavelength:'
           widget_control,Ltxt,set_value=wtit+string(x,format=format)
           widget_control,Iprf,set_value='I:'+string(y)

           ; *** Scan mode
           if event.press gt 0 then pressbut=1
           if event.release gt 0 then pressbut=0

           if zoom_prf eq 2  and pressbut then begin
           IFURED_CVIZ_DRAW_PRF,Xold,Yold
           plots,event.x*[1,1],prf_clip([1,3]),col=0,/dev,thick=2,linestyle=2

           ; wave-> X
           m=min(abs(x-lda),Z)
           ; new channel in Image N1
            J=0
            images(*,*,j)=reform(cubes(*,*, Z,J))
            
            if keyword_set(auto_brt) then begin
              new=images(*,*,j)
              ;Adjust brightness limits
                rec=where(new gt 0,cc)
               ; visualisation cuts determination
               if cc gt 1 then  Imin=min(new(rec)) else Imin=min(new)
               Imax=max(new)
               ; cuts rounded
               nsig=2 ; significant number
               pow=exp((nsig-round(alog10(Imax)))*alog(10))
               Imax=round(Imax*pow)/pow
               pow=exp((nsig-round(alog10(Imin)))*alog(10))
               Imin=round(Imin*pow)/pow
               im(j).min=Imin
               im(j).max=Imax
                widget_control,cw_min,set_value=Imin
                widget_control,cw_max,set_value=Imax
            endif   
            
            IFURED_CVIZ_DRAW_IMAGE

           endif

          end

;   'Image_1':begin
;                if event.select eq 1 then begin
;                 im_id=0
;                 IFURED_CVIZ_DRAW_IMAGE
;               endif
;              end
;   'Image_2':begin
;               if event.select eq 1 then begin
;                 im_id=1
;                 IFURED_CVIZ_DRAW_IMAGE
;               endif
;              end
;   'Image_3':begin
;               if event.select eq 1 then begin
;                 im_id=2
;                 IFURED_CVIZ_DRAW_IMAGE
;               endif
;             end
;   'Image_4':begin
;               if event.select eq 1 then begin
;                 im_id=3
;                 IFURED_CVIZ_DRAW_IMAGE
;               endif
;             end
;   'Image_5':begin
;               if event.select eq 1 then begin
;                 im_id=4
;                 IFURED_CVIZ_DRAW_IMAGE
;               endif
;              end
;   'Image_6':begin
;               if event.select eq 1 then begin
;                 im_id=5
;                 IFURED_CVIZ_DRAW_IMAGE
;               endif
;              end


   'zoomprof': zoom_prf=event.value
   'cont_prf': begin
      cont_rems=event.select
      IFURED_CVIZ_DRAW_IMAGE
    end
;   'Save': begin
;             image=reform(images(*,*,im_id))
;             ;image=rotate(image,7)
;             writefits,im(im_id).file,image,im(im_id).header(where(im(im_id).header ne ''))
;           end
  
   'ctab': begin
          
          if im_set[1] eq 1 then begin
          XCOLORS,NotifyPro='IFURED_CVIZ_DRAW_IMAGE',/drag,Group_Leader=BASE_G0,/block,$
            ColorInfo=colorInfo,/testcolor, brewer=color_tab.brewer,reverse=color_tab.reversed,$
            title="Load Color Table",index=color_tab.index 
         endif   else XCOLORS,Group_Leader=BASE_G0,/block,ColorInfo=colorInfo, brewer=color_tab.brewer,$
            reverse=color_tab.reversed, title="Load Color Table",index=color_tab.index
            
            ctmp={colors,$
              R: BytArr(!D.Table_Size), $ ; The current R color vector.
              G: BytArr(!D.Table_Size), $ ; The current G color vector.
              B: BytArr(!D.Table_Size), $ ; The current B color vector.
              NAME: "", $                 ; The name of the current color table.
              INDEX: 0, $                 ; The index number of the current color table.
              TYPE: "", $                 ; The type of color table (e.g, BREWER or IDL).
              BREWER: 0, $                ; Set to 1 if using BREWER color tables, else to 0.
              REVERSED: 0B}
             copy_struct,colorInfo,ctmp
             color_tab=ctmp
             if im_set[1] or im_set[0] then IFURED_CVIZ_DRAW_IMAGE
            end
   
  
   'Save': begin
             file=dialog_pickfile(title='Save current image',filter=['*.fts','*.fits'],get_path=w_dir)
             if file eq '' then return
             image=reform(images(*,*,im_id))
             ;image=rotate(image,7)
             writefits,file,image;,im(im_id).header(where(im(im_id).header ne ''))
           end
;   'newval': begin
;             widget_control,event.id,get_value=v
;             maskval=v(0)
;             end
;
;   'winmask': winmask=event.value
;   'extend': begin
;               id=event.value
;               extmask(id)=event.select
;               if event.select eq 1 then begin
;                case id of
;                0: extmask(1)=0
;                1: extmask(0)=0
;                2: extmask(3)=0
;                3: extmask(2)=0
;                endcase
;               widget_control,event.id,set_value=extmask
;               endif
;
;              end
;   'ReLoad': begin ; reload current FITS
;             if im_set(0) eq 0 then return
;                 new=readfits(im(im_id).file,head)
;                 if n_elements(new) eq 1 then begin
;                  res=widget_message('No file '+im(im_id).file+'!')
;                 endif
;                 ;images(*,*,im_id-1)=rotate(new,7)
;                 images(*,*,im_id)=new(*,*,0)
;                 IFURED_CVIZ_draw_image
;               if im_set(1) eq 0 then return
;               ; reload profiles
;                rec=where(cub.uses eq 1,nc)
;                for i=0,nc-1 do begin
;                  j=rec(i)
;                  tmp=readfits(cub(j).file,/sil)
;                  if n_elements(tmp) gt 1 then  if flag3D then cubes(0,0,0,j)=tmp else cubes(0,0,j)=tmp
;                endfor
;             end

   'QUIT': WIDGET_CONTROL,/DESTROY,event.top
;   'prfnorm': profnorm=event.value
;   'FlipY':begin    ;FlipY=1 for MPFS-style
;        FlipY=FlipY xor 1
;        WIDGET_CONTROL,W_MENU_11,set_button=FlipY
;        if  total(im_set(0)) gt 0 then IFURED_CVIZ_draw_image
;           end
      else:

  ENDCASE

END


; create interface for IFP gauss-analysis

PRO IFURED_CVIZ
COMMON IFURED_CVIZ_IMCUB
COMMON IFURED_CVIZ_WIDTXT
COMMON IFURED_CVIZ_WIDWIN
COMMON IFURED_CVIZ_WIDPAR
COMMON IFURED_CVIZ_IMAGES
COMMON IFURED_CVIZ_DRAW
COMMON IFURED_CVIZ_PROFILES
COMMON IFURED_CVIZ_PLOTPS
common IFURED_CVIZ_widPS
common IFURED_CVIZ_noimage,im_set
common IFURED_CVIZ_cross
COMMON IFURED_CVIZ_IMFLIPS
COMMON IFURED_CVIZ_MASK
COMMON IFURED_CVIZ_HICUBE
COMMON IFURED_CVIZ_ModeProfileNorm,profnorm
COMMON IFURED_CVIZ_NEWBLOCKforIFU

flag3D=-1
im_set=[0,0] ; [images,cubes]
flag21=0 ; 21 cm data cubes
nim=1 ; number of images/profiles
profnorm=0
device,decompose=0
;loadct,27,/sil
; set interface parameters
Xwin=462  & Ywin=504
; preliminary data parameters
Xs=-1 & Ys=-1 & zs=replicate(-1,nim)
im_id=0 ; current image identification
Xi0 =0 & Yi=0
; set default parameters
CD,current=w_dir
zoom=1
zoom_prf=0
cont_rems=0
maskval=0.0
winmask=1
Extmask=[0,0,0,0]
prf_clip=!p.clip(0:3)
LimX=!x.crange
LimY=!y.crange
Xold=-1 & Yold=-1
nlev=10
prev_col=-1
FlipY=0
pressbut =0
; PS-file
x0_ps=2 & y0_ps=5 & lx_ps=12 & title_ps=''
xtit_ps='X, (px)' & ytit_ps='Y, (px)' & units_ps='counts'
xc_ps=0 & yc_ps=0 & paoy_ps=0 & px_ps=1. & used=0


; *** images'/profiles  properties
ct_list=['gray','invert','blue-red','blue-red 2']
color_list=['black','red','yellow','green','blue']
color_val=[255,      200,     95  , 70,   10]

color_tab = {colors,$
              R: BytArr(!D.Table_Size), $ ; The current R color vector.
              G: BytArr(!D.Table_Size), $ ; The current G color vector.
              B: BytArr(!D.Table_Size), $ ; The current B color vector.
              NAME: "", $                 ; The name of the current color table.
              INDEX: 0, $                 ; The index number of the current color table.
              TYPE: "", $                 ; The type of color table (e.g, BREWER or IDL).
              BREWER: 0, $                ; Set to 1 if using BREWER color tables, else to 0.
              REVERSED: 0B }
   
   loadct,0,rgb_table=ctab,/sil
   color_tab.R=ctab[*,0]
   color_tab.G=ctab[*,1]
   color_tab.B=ctab[*,2]


;; images' parameters
im=replicate({ims,min:0.0,max:0.0,nlev:10B,name:'',file:'none',contour:0B,overlap:0B,follow:0B,ct:0B,header:strarr(1000)},nim)
;; widget numbers:
;imwid=replicate({imagesw,ct:0L,contour:0L,overlap:0L,follow:0L},nim)

; profiles' parameters
;cub=replicate({cubs,uses:0L,name:'',file:'none',color:0B},nim)
cub=replicate({cubs,uses:0L,name:'',file:'none',color:0B},nim)
;cc=[0,1,2,3,4,5]
;cub.color=cc(0:nim-1)

; widget numbers:
;cubwid=replicate({cubw,uses:0L,name:0L,color:0L},nim)
;cubwid={cubw,uses:0L,name:0L,color:0L}



if (!VERSION.OS_FAMILY eq "Windows") then titfont="Sans Serif" else titfont="-misc-fixed-bold-r-normal--13-120-75-75-c-70-iso8859-1"
;***  Interface creation
BASE_G0 = WIDGET_BASE(row=1, SPACE=3,XPAD=3,YPAD=3,TITLE='IFU data cubes viewer');,MBAR=MAINBASE_MBAR)


;; Menu interface
;  ; ** File
;  W_MENU_0 = Widget_Button(MAINBASE_mbar ,/MENU  ,VALUE='File')
;  W_MENU_01= Widget_Button(W_MENU_0, UVALUE='Save',VALUE='Re-Save FITS')
;  W_MENU_02= Widget_Button(W_MENU_0, UVALUE='Save as'  ,VALUE='Save current image as ..')
;  W_MENU_02= Widget_Button(W_MENU_0, UVALUE='Erase'  ,VALUE='Clean windows')
;  W_MENU_02= Widget_Button(W_MENU_0, UVALUE='PSmap'  ,VALUE='Plot map to PS')
;  W_MENU_04= Widget_Button(W_MENU_0, UVALUE='QUIT',separator=1,VALUE='EXIT')
;  W_MENU_1 = Widget_Button(MAINBASE_mbar ,/MENU  ,VALUE='Settings')
;  W_MENU_11= Widget_Button(W_MENU_1, /CHECKED_MENU ,UVALUE='FlipY',VALUE='Flip Y')




; ***  window 1 ***
BASE_w1=WIDGET_BASE(BASE_g0,col=1,/frame)
BASE01=WIDGET_BASE(BASE_w1,row=1,space=10)
;FileLoaded=widget_label(base01,val='File: None    ', xs=180)
Xtxt=widget_label(base01,val='X:'+string(0,format='(I6)')+'  ')
Ytxt=widget_label(base01,val='Y:'+string(0,format='(I6)')+'  ')
Itxt=widget_label(base01,val='I:'+string(0.D)+'  ')
filetxt=widget_label(base01,val='                             ',/align_right)

widdraw1=WIDGET_DRAW(base_w1,xsize=Xwin,ysize=ywin,uvalue='win1',$
         /BUTTON_EVENTS,/MOTION_EVENTS)
BASE03=WIDGET_BASE(BASE_w1,row=1,space=3)

; vizualization parameters
; first line
cw_min=cw_field(base03,title='Min:',val=0., /RETURN_EVENTS,xsize=10,uvalue='min')
cw_max=cw_field(base03,title='Max:',val=0., /RETURN_EVENTS,xsize=10,uvalue='max')
;cw_nlev=cw_field(base03,title='Nlev:',val=10, /RETURN_EVENTS,xsize=2,uvalue='nlev',/integer)

butredraw=widget_button(base03,value='ReDraw',uvalue='ReDraw',xoff=0)
tmpbase=widget_base(base03,/nonex,/col)
tmpobj=widget_button(tmpbase,value="Auto",uvalue="brt_auto",/align_left)
widget_control,tmpobj,set_button=1
auto_brt=1
;butreload=widget_button(base03,value='ReLoad',uvalue='ReLoad')

; second line
;BASE031=WIDGET_BASE(BASE_w1,row=1,space=0,frame=0)
;cw_zp=cw_bgroup(base031,['Zoom ','Prof', ' Mask','Scan'],/exclusive,uvalue='zoomprof',row=1,space=0,frame=0)
;BASE032=WIDGET_BASE(BASE031,row=1,space=0,/exclusive,frame=0)

;butim=lonarr(nim)
;for i=0,nim-1 do $
; butim(i)=widget_button(base032,val='Im '+string(i+1,format='(I1)'),uvalue='Image_'+string(i+1,format='(I1)'))


; ***  window 2 ***
BASE_w2=WIDGET_BASE(BASE_g0,col=1,/frame)
BASE02=WIDGET_BASE(BASE_w2,row=1,space=20)
PrfNum=widget_label(base02,val='Spectrum ('+string([0,0],format='(2(I5,:, ", "))')+')')
Ltxt=widget_label(base02,val='Wavelength:'+string(6562.78,format='(F9.2)'))
Iprf=widget_label(base02,val='I:'+string(0.D)+'      ')

widdraw2=WIDGET_DRAW(base_w2,xsize=1.2*Xwin,ysize=ywin,uvalue='win2',$
         /BUTTON_EVENTS,/MOTION_EVENTS)
; visualisation parameters:
BASE040=WIDGET_BASE(BASE_w2,col=1,space=0)

BASE04=WIDGET_BASE(BASE040,row=1,space=0)
bb=widget_base(base04,/nonexclusive)
cw_l1=cw_field(base04,title='Min:',val=0, /RETURN_EVENTS,xsize=8,uvalue='l1',/integer)
cw_l2=cw_field(base04,title='Max:',val=0, /RETURN_EVENTS,xsize=8,uvalue='l2',/integer)
butredraw=widget_button(base04,value='ReDraw',uvalue='ReDraw',xoff=0)



;*** Buttons area ***
BASE_control=WIDGET_BASE(BASE_g0,col=1,/frame)
butload=widget_button(base_control,uvalue='load',value='Load cube',xs=120)
butload=widget_button(base_control,uvalue='ctab',value='Color Table',xs=120)


base_mode=WIDGET_BASE(base_control,/col,/frame)
lab=WIDGET_LABEL(base_mode,val="Mode:",font=titfont)
cw_zp=cw_bgroup(base_mode,['Zoom ','Prof', 'Scan'],/exclusive,uvalue='zoomprof',col=1,frame=0)


BASE_tmp=WIDGET_BASE(base_control,space=0,/nonexclusive,frame=0)
cw_cont=widget_button(BASE_tmp,value='Overplot profiles',uvalue='cont_prf')

butsave=widget_button(base_control,uvalue='Save',value='Save FITS',xs=120)
butps=widget_button(base_control,uvalue='PSmap',value='Save PS',xs=120)
butquit=widget_button(base_control,uvalue='QUIT',value='Quit',xs=120)


;BASE05=WIDGET_BASE(BASE040,row=1,space=0)
;cw_prfnorm=cw_BGROUP(base05,['real','normalise'],uval='prfnorm', /RETURN_InDex,/excl,row=1)



;; Images and profiles parameters
;BASE_C=WIDGET_BASE(BASE_g0,row=1,frame=0,space=10)
;BASE_im=WIDGET_BASE(BASE_c,col=1,frame=1)
;lab1=widget_label(base_im,val='IMAGES:      Palette      Contours Overlap',/align_right)
;
;; create widget for images
;for i=0,nim-1 do begin
; ns=string(i+1,format='(I1)')
; bcomp=WIDGET_BASE(BASE_im,row=1,space=5)
; lab=widget_label(bcomp,value=ns)
; load=widget_button(bcomp,uvalue='#lo_'+ns,value='Load',ys=10)
; imwid(i).ct=Widget_droplist(bcomp,UVALUE='#ct_'+ns,VALUE=ct_list)
; us1=widget_base(bcomp,/nonexclusive)
; imwid(i).contour=widget_button(us1,uvalue='#co_'+ns,value='')
; us2=widget_base(bcomp,/nonexclusive)
; imwid(i).overlap=widget_button(us2,uvalue='#ov_'+ns,value='')
;endfor
;
;BASE_prf=WIDGET_BASE(BASE_c,col=1,frame=1)
;lab2=widget_label(base_prf,val='** CUBES ** ',/align_left,font='Tahoma*13*Bold')
;
;; creat widget for profiles
;
;for i=0,nim-1 do begin
; ns=string(i+1,format='(I1)')
; bcomp=WIDGET_BASE(BASE_prf,row=1,space=5)
; us=widget_base(bcomp,/nonexclusive)
; cubwid(i).uses=widget_button(us,uvalue='@us_'+ns,value=ns)
; load=widget_button(bcomp,uvalue='@lo_'+ns,value='Load')
; cubwid(i).color=Widget_droplist(bcomp,UVALUE='@co_'+ns,VALUE=color_list)
;endfor
;
;;Masking parameters
;
;BASE08=WIDGET_BASE(BASE_c,col=1,space=5,/frame)
;lab_3=widget_label(base08,val='Masking options')
;wid_mask=cw_field(base08,tit='New value:',val=maskval,/float,xsize=5,uvalue='newval',/all)
;
;wid_ext=cw_bgroup(base08,/nonexcl, 'Extend to '+['Left','Right','Top','Bottom'],/col,uvalue='extend')
;wid_win=cw_field(base08,tit='Mask:',val=Winmask,/integer,xsize=2,uvalue='winmask',/all)
;




widget_control,cw_zp,set_value=zoom_prf
widget_control,cw_cont,set_button=cont_rems
;widget_control,butim(im_id),set_button=1
;for i=0,nim-1 do begin
; widget_control,cubwid(i).color,set_droplist_select=cub(i).color
; IFURED_CVIZ_CUB_ONOFF,i,0
; IFURED_CVIZ_IM_ONOFF,i,0
;endfor

;WIDGET_CONTROL,W_MENU_11,set_button=FlipY
;WIDGET_CONTROL,cw_prfnorm,set_val=prfnorm
;WIDGET_CONTROL,wid_ext,set_value=Extmask

cgCENTERTLB,base_G0
WIDGET_CONTROL, BASE_G0, /REALIZE



XMANAGER, 'IFURED_CVIZ', BASE_G0,no_block=1
END

; Sub-routine for setting title of the figures
PRO IFURED_CVIZ_PS_TITLE_Event,event
COMMON IFURED_CVIZ_PLOTPS
common IFURED_CVIZ_widPS
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

CASE ev OF
'title': title_ps=event.value
'xtit': xtit_ps=event.value
'ytit': ytit_ps=event.value
'units': units_ps=event.value
'xc': xc_ps=event.value
'yc': yc_ps=event.value
'pa': paoy_ps=event.value
'px': px_ps=event.value
'OK':  WIDGET_CONTROL,/DESTROY,event.top
'used':begin
        used=event.select
        widget_control,text5,sensit=used
        widget_control,text6,sensit=used
        widget_control,text7,sensit=used
        widget_control,text8,sensit=used
       end
endcase

END

PRO IFURED_CVIZ_PS_TITLE ; Creating widgets for setting PS-figure title
COMMON IFURED_CVIZ_PLOTPS
common IFURED_CVIZ_widPS
  SIZEBASE = WIDGET_BASE(COLUMN=1, SPACE=0,XPAD=10,YPAD=10,$
      MAP=1,TITLE='Parameters of the PS-figure',UVALUE='SIZEBASE')
  TEXT1 = cw_field(SIZEBASE,value=title_ps,tit='Title:  ',uvalue='title',xsize=40,/all,/string)
  TEXT2 = cw_field(SIZEBASE,value=xtit_ps,tit= 'X title:',uvalue='xtit',xsize=40,/all,/string)
  TEXT3 = cw_field(SIZEBASE,value=ytit_ps,tit= 'Y title:',uvalue='ytit',xsize=40,/all,/string)
  TEXT4 = cw_field(SIZEBASE,value=units_ps,tit= 'Units:',uvalue='units',xsize=40,/all,/string)
  TEXT5 = cw_field(SIZEBASE,value=xc_ps,tit= 'Xcen:',uvalue='xc',xsize=40,/all,/float)
  TEXT6 = cw_field(SIZEBASE,value=yc_ps,tit= 'Ycen:',uvalue='yc',xsize=40,/all,/float)
  TEXT7 = cw_field(SIZEBASE,value=paoy_ps,tit= 'PA(OY):',uvalue='pa',xsize=38,/all,/float)
  TEXT8 = cw_field(SIZEBASE,value=px_ps,tit= 'px("):',uvalue='px',xsize=38,/all,/float)
  base1 = widget_base(SIZEBASE,/nonexclusive)
  butallow=widget_button(base1,value='Coordinates (Xc,Yc,PA,px)',uvalue='used')
  DONE = WIDGET_BUTTON(SIZEBASE,FRAME=5,UVALUE='OK',VALUE='OK',xsize=6)


  WIDGET_CONTROL, SIZEBASE, /REALIZE
  widget_control,butallow,set_button=used
        widget_control,text5,sensit=used
        widget_control,text6,sensit=used
        widget_control,text7,sensit=used

  XMANAGER, 'IFURED_CVIZ_PS_TITLE', SIZEBASE,no_block=1,/modal
END
