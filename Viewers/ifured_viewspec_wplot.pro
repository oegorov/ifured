pro IFURED_viewspec_DRAWPLOT,OPLOT=oplot
COMMON IFURED_viewspec_IMAblock
COMMON IFURED_viewspec_WIN
COMMON IFURED_viewspec_BOXblock
COMMON IFURED_viewspec_REG
WSET, num_win
imascale=sxpar(h,'IMSCALE')
imascale=str_sep(imascale,'x')
imascale=float(imascale(1))

limit=[x_min,y_min,x_max,y_max]

!P.color=0 & !P.background=2^24-1
R=FLOAT(x_max-x_min)/float(y_max-y_min)
if R ge 1 then begin
S=x & index=1 & k=2
endif
if R lt 1 then begin
S=y & index=2 & k=1
endif
x_tmp=FIX((limit(index-1)+(limit(index+1)-limit(index-1))/530.*xs))
x_tmp_pre=FIX((limit(index-1)+(limit(index+1)-limit(index-1))/530.*x_pre))
if a(0) eq 2 then begin
vector=total(region,k,/nan)/a(k)
if not(keyword_set(oplot)) then $
plot,S,vector,XST=1,yst=1,position=[0,0,1,1],xmin=1,ymin=1,$
		xtickinterval=1E5,ytickinterval=1E8,psym=10
if x_tmp eq limit(index-1) THEN col=2^24-1 ELSE col=0
oplot,x_tmp*[1,1],color=col,$
[min(vector),max(vector)],linestyle=2
if x_tmp_pre ne x_tmp then $
oplot,x_tmp_pre*[1,1],color=2^24-1,$
[min(vector),max(vector)],linestyle=2
oplot,S,vector,color=0,psym=10
if sw_gau ne 1 then begin
str_out=strcompress('pixel '+string(x_tmp))
if d_lambda ne 0 then str_out=str_out+'   '+strcompress('wavelength '+string(FIX(d_lambda*x_tmp+lambda_0))+' A')

WIDGET_CONTROL,values,SET_VALUE=str_out+ $
'    value '+string(vector(x_tmp-limit(index-1)))
endif
if sw_gau eq 1 then begin
;����������� ������ ����
fon=(max(vector)-min(vector))*ys/300.+min(vector)
res=MULTIGAUS ( S, vector-fon, x_tmp,FWHM=10,YFIT=YFIT)
MM=max(abs(Yfit),Nmax)
ww=res.FWHM*3
wmin=Nmax-ww & if wmin lt 0 then wmin=0
wmax=Nmax+ww & if wmax gt N_elements(s)-1 then wmax=N_elements(s)-1
oplot,S(wmin:wmax),Yfit(wmin:wmax)+fon,color=1e5

lamb_gau=''
FWHM_gau=''
flux_gau=''
if index eq 1 and d_lambda ne 0 then begin
lamb_gau=' ('+STRCOMPRESS(string(res.center*d_lambda+lambda_0,format='(F7.1)'),/remove_all)+' A)'
FWHM_gau=' ('+STRCOMPRESS(string(res.FWHM*d_lambda,format='(F7.1)'),/remove_all)+' A)'
flux_gau='  flux '+STRCOMPRESS(string(res.flux,format='(I9)'),/remove_all)
endif
if index eq 2 then see='  seeing '+$
	string(res.FWHM*imascale,format='(F7.1)')+'"' ELSE see=''
WIDGET_CONTROL,values,SET_VALUE='pixel'+string(x_tmp,format='(I5)')+$
'   value '+STRCOMPRESS(string(vector(x_tmp-limit(index-1))))+$
'   center'+string(res.center,format='(F7.1)')+'px '+lamb_gau+$
'   FWHM '+STRCOMPRESS(string(res.FWHM,format='(F7.1)'))+' px '+FWHM_gau+flux_gau+see

endif
	endif
;!P.color=255 & !P.background=0
END




Pro IFURED_viewspec_Wplot_event, event
COMMON IFURED_viewspec_Wbase
COMMON IFURED_viewspec_IMAblock
COMMON IFURED_viewspec_WIN
COMMON IFURED_viewspec_BOXblock
COMMON IFURED_viewspec_REG
; When a widget is selected, put its User Value into 'eventval':

WIDGET_CONTROL, event.id, GET_UVALUE = eventval

; Perform actions based on the User Value of the event:

CASE eventval OF
   'DRAW_EVENT': BEGIN

			IF (event.press EQ 0) AND (event.release EQ 0) THEN BEGIN

			xs=event.x & ys=event.y
			IFURED_viewspec_DRAWPLOT,/oplot
			x_pre=xs & y_pre=xs
			ENDIF
			sw_gau=0
			if event.press EQ 1 then begin
			sw_gau=1 & xgau=event.x
			IFURED_viewspec_DRAWPLOT,/oplot
			endif


		     END
   'DONE': WIDGET_CONTROL, event.top, /DESTROY
ENDCASE

END



PRO IFURED_viewspec_Wplot, GROUP=GROUP
COMMON IFURED_viewspec_Wbase
COMMON IFURED_viewspec_IMAblock
COMMON IFURED_viewspec_WIN
COMMON IFURED_viewspec_BOXblock
COMMON IFURED_viewspec_REG
s_win = !D.WINDOW	; Remember the current window so it can be restored
sw_gau=0
xs=0 & ys=0
x_pre=1 & y_pre=1
; This example uses keywords to define the size of the draw area:
base_plot = WIDGET_BASE(TITLE = 'Plot region',/COLUMN,xoffset=720,yoffset=490)

WIDGET_CONTROL, /MANAGED, base_plot


;
draw_plot = WIDGET_DRAW(base_plot , XSIZE=530, YSIZE=300,/FRAME, $
	/MOTION_EVENTS,  /BUTTON_EVENTS ,$		;generate LOTS of events
	UVALUE = 'DRAW_EVENT', 	RETAIN = 2)


base_inform=WIDGET_BASE(base_plot,/row,xsize=530,ysize=30)
values=WIDGET_LABEL(base_inform,value=' ',xsize=465)
button = WIDGET_BUTTON(base_inform , $
		UVALUE = 'DONE', $
		VALUE = 'DONE')

WIDGET_CONTROL, base_plot , /REALIZE



WIDGET_CONTROL, draw_plot, GET_VALUE=num_win


IFURED_viewspec_DRAWPLOT
; Hand off control of the widget to the XMANAGER:
XMANAGER, "IFURED_viewspec_Wplot", base_plot , GROUP_LEADER=GROUP, /NO_BLOCK

END
