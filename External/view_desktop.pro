
pro REGIONparam
COMMON IMAblock,file,h,frame,ima,wdir,Nx,Ny,Nz,sc,bin,plane,d_lambda,lambda_0,over,x_size,y_size,range,level
COMMON REG,x_min,y_min,x_max,y_max,region,x,y,a,values
COMMON BOXblock,map,x1,x2,y1,y2,x_2,y_2,sw
; calculation region coordinates
ScX=float(x_size)/FLOAT(Nx) & ScY=float(y_size)/FLOAT(Ny)
x_min=min([x1,x2])/scX & x_max=max([x1,x2])/scX
y_min=min([y1,y2])/scY & y_max=max([y1,y2])/scY
if x_min lt 0 then x_min=0
if x_max gt Nx-1 then x_max=Nx-1
if y_min lt 0 then y_min=0
if y_max gt Ny-1 then y_max=Ny-1
region=frame(x_min:x_max,y_min:y_max,plane)

a=size(region)
x=findgen(a(1))+x_min
y=findgen(a(2))+y_min
END

pro BOX
COMMON IMAblock,file,h,frame,ima,wdir,Nx,Ny,Nz,sc,bin,plane,d_lambda,lambda_0,over,x_size,y_size,range,level
COMMON WINblock,win_num,swin,planeW,draw,micro
COMMON WIN,num_win,s_win,xs,ys,xgau,sw_gau,x_pre,y_pre
COMMON BOXblock,map,x1,x2,y1,y2,x_2,y_2,sw
WSET, win_num

slitpos=500
TV,map
plot,[0,x_size],[0,y_size],/noerase,/nodata,position=[0,0,1,1],$
;plot,[x_min,x_max],[y_min,y_max],/noerase,/nodata,position=[0,0,1,1],$
	xtickinterval=x_size,ytickinterval=y_size,xst=1,yst=1,$
	xminor=1,yminor=1,/norm
;if file eq 'map_i' then oplot,[1,1]*slitpos,[0,y_size],color=255
oplot,[x1,x_2,x_2,x1,x1],[y1,y1,y_2,y_2,y1],color=255
;oplot,[x1,x2,x2,x1,x1],[y1,y1,y2,y2,y1],color=255
end


pro DRAWIMAGE
COMMON IMAblock,file,h,frame,ima,wdir,Nx,Ny,Nz,sc,bin,plane,d_lambda,lambda_0,over,x_size,y_size
COMMON RANGEblock,range,level,avg_ima,rms_ima
COMMON WINblock,win_num,swin,planeW,draw,micro
COMMON BOXblock,map,x1,x2,y1,y2,x_2,y_2,sw
loadct,0,/sil
WSET, win_num
i=50./range
j=50./level
map=255-bytscl(ima,avg_ima*j-rms_ima*i,avg_ima*j+i*3*rms_ima)
TV,map
plot,[0,x_size],[0,y_size],/noerase,/nodata,position=[0,0,1,1],$
	xtickinterval=x_size,ytickinterval=y_size,xst=1,yst=1,$
	xminor=1,yminor=1,/norm
oplot,[x1,x2,x2,x1,x1],[y1,y1,y2,y2,y1],color=255
WSET, swin			; Restore the original window
END

pro MICRODRAW
COMMON WINblock,win_num,swin,planeW,draw,micro
COMMON IMAblock,file,h,frame,ima,wdir,Nx,Ny,Nz,sc,bin,plane,d_lambda,lambda_0,over,x_size,y_size
COMMON BOXblock,map,x1,x2,y1,y2,x_2,y_2,sw
COMMON RANGEblock,range,level,avg_ima,rms_ima
COMMON Wbutton,button3,button4,button5,pln_group
WIDGET_CONTROL, micro,GET_VALUE=win_micro
WSET, win_micro
i=50./range
j=50./level
w=20
ScX=float(x_size)/FLOAT(Nx) & ScY=float(y_size)/FLOAT(Ny)
if x_2/scX gt w and x_2/scX lt Nx-1-w and $
	y_2/scY gt w and y_2/scY lt Ny-1-w then  begin
micromap=congrid(frame(x_2/scX-w:x_2/scX+w,y_2/scY-w:y_2/scY+w),100,100)
micromap=255-bytscl(micromap,avg_ima*j-rms_ima*i,avg_ima*j+i*3*rms_ima)
TV,micromap
;plot,[0,2*w+1],[0,2*w+1],xst=1,yst=1,/nodata,/noerase,position=[0,0,1,1],/norm
arrow,0.5,0,0.5,1,hsize=0,color=1e5,/norm
arrow,0,0.5,1,0.5,hsize=0,color=1e5,/norm
endif
WSET, swin
END

pro  LOADFILE
COMMON Wbase,base_main,base_ccd,base_platform,base_scorpio,base_view,base_plot
COMMON IMAblock,file,h,frame,ima,wdir,Nx,Ny,Nz,sc,bin,plane,d_lambda,lambda_0,over,x_size,y_size
COMMON WINblock,win_num,swin,planeW,draw,micro
COMMON BOXblock,map,x1,x2,y1,y2,x_2,y_2,sw
COMMON Wbutton,button3,button4,button5,pln_group
COMMON RANGEblock,range,level,avg_ima,rms_ima

CD,wdir,CURRENT=scorp_dir
file_curr=file
file=pickfile(/read,filter='*.fts')
CD,scorp_dir
if file eq '' then file=file_curr
frame=readfits(file,h)
Nx=sxpar(h,'NAXIS1') & Ny=sxpar(h,'NAXIS2') & Nz=sxpar(h,'NAXIS3')
;print,Nx,Ny
if Nx lt Ny then begin
if Nz eq 0 then begin
tmp=fltarr(Ny,Ny)
tmp(0:Nx-1,*)=frame
frame=tmp
endif
Nx=Ny
endif

d_lambda=sxpar(h,'CDELT1')
lambda_0=sxpar(h,'CRVAL1')
bin=sxpar(h,'binning')
bin=fix(str_sep(bin,'x')) & bin=float(bin(0))/bin(1)
sc=float(y_size)/float(Ny)
Naxis=sxpar(h,'NAXIS')
Nz=sxpar(h,'NAXIS3')
ima=congrid(frame(*,*,plane),x_size,y_size)
rms_ima=stdev(median(congrid(ima,50,50),5),avg_ima)

WIDGET_CONTROL,base_view,BASE_SET_TITLE= 'VIEW '+file
WIDGET_CONTROL, draw, GET_VALUE=win_num,DRAW_XSIZE=x_size
WIDGET_CONTROL, draw, GET_VALUE=win_num,DRAW_YSIZE=y_size
WIDGET_CONTROL, draw, GET_VALUE=win_num,SET_DRAW_VIEW=[(x_size-y_size)/2,0]
if not (x1 eq x2 and y1 eq y2) then begin
 ;WIDGET_CONTROL,button3,SENSITIVE=0
 WIDGET_CONTROL,button4,SENSITIVE=0
 WIDGET_CONTROL,button5,SENSITIVE=0

 endif
 WIDGET_CONTROL,pln_group,SENSITIVE=Nz/2
END



PRO viewfits_event, event

COMMON Wbase,base_main,base_ccd,base_platform,base_scorpio,base_view,base_plot
COMMON LABELblock, pixel_label, value_label,mean_label,rms_label,POS_label,FWHM_label,FLUX_label,SEE_label,wave_label
COMMON SLIDEdlock,viewrange,viewbackground
COMMON Wbutton,button3,button4,button5,pln_group
COMMON REG,x_min,y_min,x_max,y_max,region,x,y,a,values
COMMON IMAblock,file,h,frame,ima,wdir,Nx,Ny,Nz,sc,bin,plane,d_lambda,lambda_0,over,x_size,y_size
COMMON RANGEblock,range,level,avg_ima,rms_ima
COMMON WINblock,win_num,swin,planeW,draw,micro
COMMON WIN,num_win,s_win,xs,ys,xgau,sw_gau,x_pre,y_pre
COMMON BOXblock,map,x1,x2,y1,y2,x_2,y_2,sw

WIDGET_CONTROL, event.id, GET_UVALUE = eventval

CASE eventval OF
'pln':	BEGIN
		WIDGET_CONTROL,event.id,GET_VALUE=a
		plane=a
		ima=congrid(frame(*,*,plane),x_size,y_size)
		rms_ima=stdev(median(congrid(ima,50,50),5),avg_ima)
		DRAWIMAGE
		END

'DRAW_WIN_EVENT': BEGIN
			ScX=float(x_size)/FLOAT(Nx) & ScY=float(y_size)/FLOAT(Ny)
			if event.press EQ 1  then begin
            sw=1
            x1=event.x  & y1=event.y

			endif

            if event.release EQ 1 and event.clicks EQ 1 then begin
             sw=0
             x2=event.x  & y2=event.y

			 REGIONparam
			 if x1 eq x2 and y1 eq y2 then begin
			  WIDGET_CONTROL,button3,SENSITIVE=0
			  WIDGET_CONTROL,button4,SENSITIVE=0
			  WIDGET_CONTROL,button5,SENSITIVE=0
			  DRAWIMAGE
			  if XREGISTERED('Wplot' ) gt 0 then  WIDGET_CONTROL,base_plot,/destroy
			 endif else begin
			  WIDGET_CONTROL,button3,SENSITIVE=1
			  WIDGET_CONTROL,button4,SENSITIVE=1
			  WIDGET_CONTROL,button5,SENSITIVE=1
			 endelse
            endif

            IF (event.press EQ 0) AND (event.release EQ 0) THEN BEGIN
            x_2=event.x  & y_2=event.y

            MICRODRAW
			if sw eq 1 then BOX

         	WIDGET_CONTROL,pixel_label, $
				   SET_VALUE='Pixel' + STRCOMPRESS('('+STRING(FIX(event.x/scX))+','+$
				   STRING(FIX(event.y/scY))+')')

			 if d_lambda gt 0 then $
				WIDGET_CONTROL,wave_label, $
				   SET_VALUE='wavelength' + STRCOMPRESS(STRING(FIX(event.x/scX*d_lambda+lambda_0))+'A') $
				   else WIDGET_CONTROL,wave_label,SET_VALUE=' '
				if event.x ge 0 and event.x le x_size-1 $
				and event.y ge 0 and event.y le y_size-1 then $
				WIDGET_CONTROL,value_label,$
				SET_VALUE='Value=' + STRING(ima(event.x,event.y))
			ENDIF
			if event.press eq 4 then $
			 if not (x1 eq x2 and y1 eq y2) then begin
			 rms_ima=stdev(median(region,3),avg_ima)
			  DRAWIMAGE
			endif

		    END
'range': BEGIN
			 WIDGET_CONTROL, viewrange, GET_VALUE=range
			 DRAWIMAGE
			 END
'background': BEGIN
			 WIDGET_CONTROL, viewbackground, GET_VALUE=level

			 DRAWIMAGE
			 END
'plot_region': BEGIN
			if XREGISTERED('Wplot' ) gt 0 then  WIDGET_CONTROL,base_plot,/destroy
 			Wplot
			end
'region_statistic':BEGIN
		;robomean,region,3,0.5,avg_region,rms_region
		rms_region=stdev(region,avg_region)
		WIDGET_CONTROL,mean_label,SET_VALUE='mean ' + STRING(avg_region)
		WIDGET_CONTROL,rms_label,SET_VALUE='rms ' + STRING(rms_region)
		end
'region_fwhm': BEGIN
			SKY,region,skylevel,skyrms,/silent
			res=GAUSS2DFIT(region-skylevel,G)

			FWHM=(sqrt(G(2)^2+G(3)^2))*2.345/sqrt(2)
			flux=total(region-skylevel)
			WIDGET_CONTROL,POS_label,SET_VALUE='x=' + STRING(G(4)+x_min,format='(F6.1)')+' y='+ STRING(G(5)+y_min,format='(F6.1)')
			WIDGET_CONTROL,FWHM_label,SET_VALUE='FWHM='+STRING(FWHM,format='(F6.1)')+' px'
			WIDGET_CONTROL,FLUX_label,SET_VALUE='flux='+STRING(flux,format='(I9)')
			if file eq 'map_i' then begin
			imascale=sxpar(h,'IMSCALE')
			imascale=str_sep(imascale,'x') & imascale=float(imascale(1))
			WIDGET_CONTROL,SEE_label,SET_VALUE='seeing='+STRING(FWHM*imascale,format='(F4.1)')+'"'
			endif
				END
'FILE':     BEGIN
			LOADFILE
			DRAWIMAGE
			END
   'DONE': BEGIN
   			if XREGISTERED('Wplot' ) gt 0 then  WIDGET_CONTROL,base_plot,/destroy
   			WIDGET_CONTROL, event.top, /DESTROY
   			;exit
   			END
ENDCASE

END



PRO viewfits, GROUP=GROUP

; This is the procedure that creates a draw widget that returns
; motion events.

; The COMMON block is used because the event handler needs
; widget ID's of the labels:
COMMON Wbase,base_main,base_ccd,base_platform,base_scorpio,base_view,base_plot
COMMON LABELblock,pixel_label,value_label,mean_label,rms_label,POS_label,FWHM_label,FLUX_label,SEE_label,wave_label
COMMON SLIDEdlock,viewrange,viewbackground
COMMON Wbutton,button3,button4,button5,pln_group
COMMON IMAblock,file,h,frame,ima,wdir,Nx,Ny,Nz,sc,bin,plane,d_lambda,lambda_0,over,x_size,y_size
COMMON RANGEblock,range,level,avg_ima,rms_ima
COMMON WINblock,win_num,swin,planeW,draw,micro
COMMON WIN,num_win,s_win,xs,ys,xgau,sw_gau,x_pre,y_pre
COMMON BOXblock,map,x1,x2,y1,y2,x_2,y_2,sw


swin = !D.WINDOW	; Remember the current window so it can be restored


; This example uses keywords to define the size of the draw area:


range=50. & level=50.
plane=0 ;set plane 1
sw=0 & x1=0& y1=0 & x2=0 & y2=0 & x_2=0 &y_2=0
;head=headfits(file)
;Texp=sxpar(head,'EXPTIME')
;Texp='   Texp='+strcompress(string(Texp,format='(F6.1)'))
;ima_size=strcompress(string(sxpar(head,'NAXIS1'),format='(I5)')+' x'+$
;					 string(sxpar(head,'NAXIS2'),format='(I5)')+' px')
base_view = WIDGET_BASE(TITLE = 'VIEW '+file,xsize=x_size+125,ysize=y_size+30,/ROW,xoffset=520,yoffset=400)

BASE1 = WIDGET_BASE(base_view,col=1,MAP=1)

;draw = WIDGET_DRAW(base1, X_SCROLL_SIZE=y_size, Y_SCROLL_SIZE=y_size,$
;    XSIZE=x_size , YSIZE=y_size,/SCROLL,/FRAME, $
draw = WIDGET_DRAW(base1,    XSIZE=x_size , YSIZE=y_size,/FRAME, $
	/MOTION_EVENTS, /BUTTON_EVENTS ,$		;generate LOTS of events
	UVALUE = 'DRAW_WIN_EVENT', 	RETAIN = 1)

BASE2 = WIDGET_BASE(base1,row=1,MAP=1)

base3=WIDGET_BASE(base_view,col=1,MAP=1)
button1 = WIDGET_BUTTON(base3, $
		UVALUE = 'FILE', $
		VALUE = 'LOAD FILE',ysize=20)
pln_group = CW_BGROUP(base3,['1','2'],/EXCLUSIVE,uvalue='pln',set_value=plane,/row,ysize=40,/return_index)
viewrange = WIDGET_SLIDER(base3,$ TITLE = 'contrast', $
                        MINIMUM = 1, MAXIMUM = 100, VALUE = 50, $
			UVALUE = 'range',ysize=32 )
contrast_label = WIDGET_LABEL(base3, $
	VALUE='contrast',ysize=17)
viewbackground = WIDGET_SLIDER(base3,$ TITLE = 'brigthness', $
                        MINIMUM = 1, MAXIMUM = 100, VALUE = 50, $
			UVALUE = 'background',ysize=32)
background_label = WIDGET_LABEL(base3,  $
	VALUE='background',ysize=17)
;label1=WIDGET_LABEL(base3,  $
;	VALUE='  ')
pixel_label = WIDGET_LABEL(base3, /DYNAMIC_RESIZE, $
		VALUE='',ysize=17)

value_label = WIDGET_LABEL(base3, /DYNAMIC_RESIZE, $
		VALUE='',ysize=17)
wave_label=WIDGET_LABEL(base3, /DYNAMIC_RESIZE, $
		VALUE=' ')
button3 = WIDGET_BUTTON(base3, $
		UVALUE = 'plot_region', $
		VALUE = 'plot region',ysize=20)
button4 = WIDGET_BUTTON(base3, $
		UVALUE = 'region_statistic', $
		VALUE = 'region statistic',ysize=20)
mean_label = WIDGET_LABEL(base3, /DYNAMIC_RESIZE, $
		VALUE='mean value',ysize=17)
rms_label = WIDGET_LABEL(base3, /DYNAMIC_RESIZE, $
		VALUE='rms value',ysize=17)

button5 = WIDGET_BUTTON(base3, $
		UVALUE = 'region_fwhm', $*
		VALUE = 'region centroid',ysize=20)
POS_label = WIDGET_LABEL(base3, /DYNAMIC_RESIZE, $
		VALUE='x    y')
FWHM_label = WIDGET_LABEL(base3, /DYNAMIC_RESIZE, $
		VALUE='FWHM',ysize=15)
FLUX_label=WIDGET_LABEL(base3, /DYNAMIC_RESIZE, $
		VALUE='flux',ysize=15)
button2 = WIDGET_BUTTON(base3, $
		UVALUE = 'DONE', $
		VALUE = 'DONE',ysize=20)
micro = WIDGET_DRAW(base3, XSIZE=100 , YSIZE=100,/FRAME, $
		UVALUE = 'micro', 	RETAIN = 1)

; Realize the widgets:


WIDGET_CONTROL, base_view,/realize

WIDGET_CONTROL, draw, GET_VALUE=win_num,SET_DRAW_VIEW=[(x_size-y_size)/2,0]
DRAWIMAGE
if x1+x2+y1+y2 eq 0 then begin
WIDGET_CONTROL,button3,SENSITIVE=0
WIDGET_CONTROL,button4,SENSITIVE=0
WIDGET_CONTROL,button5,SENSITIVE=0
WIDGET_CONTROL,pln_group,SENSITIVE=Nz/2
endif
; Use TVSCL to display an image in the draw widget.  Set the window for
; the TVSCL command since there may be other draw windows.


; Hand off control of the widget to the XMANAGER:
XMANAGER, "viewfits", base_view, GROUP_LEADER=GROUP, /NO_BLOCK
END

pro view_desktop,filename
COMMON Wbase,base_main,base_ccd,base_platform,base_scorpio,base_view
COMMON IMAblock,file,h,frame,ima,wdir,Nx,Ny,Nz,sc,bin,plane,d_lambda,lambda_0,over,x_size,y_size
COMMON RANGEblock,range,level,avg_ima,rms_ima

file=filename
wdir=str_sep(file,'\') & wdir=strlen(wdir(N_elements(wdir)-1))
wdir=strmid(file,0,strlen(file)-wdir)

frame=readfits(filename,h)
if file eq '' then exit
Nx=sxpar(h,'NAXIS1') & Ny=sxpar(h,'NAXIS2') & Nz=sxpar(h,'NAXIS3')
;print,Nx,Ny,Nz
if Nx lt Ny then begin
if Nz eq 0 then begin
tmp=fltarr(Ny,Ny)
tmp(0:Nx-1,*)=frame
frame=tmp
endif
Nx=Ny
endif
y_size=500 & x_size=1000
sc=float(y_size)/float(Ny)
d_lambda=sxpar(h,'CDELT1')
lambda_0=sxpar(h,'CRVAL1')
bin=sxpar(h,'binning')
bin=fix(str_sep(bin,'x')) & bin=float(bin(0))/bin(1)
XY=float(Nx)/float(Ny)

Naxis=sxpar(h,'NAXIS')
plane=0
ima=fltarr(x_size,y_size)
ima(*,*)=congrid(frame(*,*,plane),x_size,y_size)
rms_ima=stdev(median(congrid(ima,50,50),5),avg_ima)
viewfits
end
