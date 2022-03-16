;**********************************
;* atmospher dispersion determinaion
;* file atm_dis.txt will be constracted
;* Format : <channel>   <dX>    <dY>
;* dX, dY -- barycenter shift
;* using 2D-gauss-fiting of star image
;***********************************

PRO IFURED_ATM_DIS,name,ext=ext,border=border,Ndeg=Ndeg,rad=rad, w_dir=w_dir,show=show




if not(keyword_set(rad)) then rad=4.5 ;radius of window

if not(keyword_set(Ndeg)) then Ndeg=2  ; polynom degree
if not(keyword_set(border)) then border=5
if not(keyword_set(ext)) then ext='_abs'

;name='obj'
filein=name+ext+'.fts'   ; input file
fileout='atmdis_'+name+'.txt' ;output file
fileps='atmdis_'+name+'.eps' ;output PS-file
if ext eq '_cor' then begin
 fileout='atmdis_'+name+'_cor.txt' ;output file
 fileps='atmdis_'+name+'_cor.eps' ;output PS-file
endif
sig=2 ; rms for goodpoly
bin=20. ; rebinning



if file_test(w_dir+filein) eq 0 then begin
 res=dialog_message('File not found: '+w_dir+filein)
 return
endif

obj=readfits(w_dir+filein,header) ; read OBJ

nx=sxpar(header,'NAXIS1')
ny=sxpar(header,'NAXIS2')
nz=sxpar(header,'NAXIS3')

ld0 =sxpar(header,'CRVAL3',comment=c1)
dld0=sxpar(header,'CDELT3',comment=c2)

lda=ld0+dld0*findgen(nz); wavelength grid
ch=findgen(nz)

; array creation
im=fltarr(nx,ny)

Nnew=ceil(nz/bin)
lda_bin=congrid(lda,Nnew)
obj_bin=congrid(obj,nx,ny,Nnew)
x_bar=fltarr(nz)
y_bar=fltarr(nz)
x1=ceil(border/bin)  ;\skip borders
x2=floor((nz-border)/bin);/

FOR i=x1,x2 do BEGIN
	im=obj_bin[*,*,i]
	m=MAX(im,mp)
	xc=mp mod nx
	yc=mp/nx

	res=STAR_2D(im,xc,yc,win=rad)
	x_bar(i)=res.xc
	y_bar(i)=res.yc
ENDFOR



rec=where( (ch ge x1)and(ch le x2)and(x_bar gt 0),nc)

if nc lt Ndeg+4 then begin
 res=dialog_message('ATM_DIS: need more points in '+filein+' !')
 return
endif
;fx=goodpoly(median(lda(rec),21),x_bar(rec),Ndeg,sig)
fx=goodpoly(lda_bin(rec),x_bar(rec),Ndeg,sig)

x_pos=poly(lda,fx)


;fy=goodpoly(median(lda(rec),7),y_bar(rec),Ndeg,sig)
fy=goodpoly(lda_bin(rec),y_bar(rec),Ndeg,sig)
y_pos=poly(lda,fy)

if keyword_set(show) then begin
cgdisplay,600,600,wid=7
cgplot,lda_bin(rec),x_bar(rec),xst=1,yst=1,xtit='WAVELENGTH',YTIT='X bar',tit='Barycenter for '+filein,psym=4,layout=[1,2,1];,yr=minmax([x_pos,x_bar])
cgoplot,lda,x_pos,thick=3
cgplot,lda_bin(rec),y_bar(rec),xst=1,yst=1,xtit='WAVELENGTH',YTIT='Y bar',tit='Barycenter for '+filein,psym=4,layout=[1,2,2];,yr=minmax([y_pos,y_bar])
cgoplot,lda,y_pos,thick=3
endif


openw,u,w_dir+fileout,/get_lun
printf,u,'Barycenter positions from file: '+filein
printf,u,'Rwin=',rad
printf,u,'wavelength  Xbar     Ybar'
for i=0,nz-1 do printf,u,lda(i),x_pos(i),y_pos(i),format='(3F10.4)'
close,u
free_lun,u



; PS-file
plotsym,0,0.5,/fill

cgps_open,w_dir+fileps,xs=9,ys=9,/encaps

cgplot,lda_bin(rec),x_bar(rec),xst=1,yst=1,psym=8,xtit='Wavelength',YTIT='X bar',yr=minmax(x_pos),layout=[1,2,1];,tit='Barycenter for '+filein
cgoplot,lda,x_pos,thick=3

cgplot,lda_bin(rec),y_bar(rec),xst=1,psym=8,yst=1,xtit='Wavelength',YTIT='Y bar',yr=minmax(y_pos),layout=[1,2,2];,tit='Barycenter for '+filein
cgoplot,lda,y_pos,thick=3

cgps_close

END