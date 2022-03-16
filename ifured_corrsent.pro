; SCORPIO-2 IFU data reduction
; correction to true flux
pro IFURED_CORRSENT,filein,fileout,w_dir=w_dir

if keyword_set(w_dir) then w_dir=slash(w_dir) else w_dir=''
sent=READFITS(w_dir+'sent.fts',h_sent)
spectr=READFITS(w_dir+filein,h)

sent=reform(sent[*,0])

N_x=sxpar(h,'NAXIS1')	; Number of elements along X-axis
N_y=sxpar(h,'NAXIS2')    ;Number of elements along Y-axis
N_z=sxpar(h,'NAXIS3')  ;Number of spectral
obj_red=fltarr(N_x,N_y,N_z)
param=[sxpar(h,'CRVAL3'),sxpar(h,'CDELT3'),N_z]


exp_obj=sxpar(h,'EXPTIME')
z_obj=float(sxpar(h,'Z'))
object_name=STRCOMPRESS(sxpar(h,'OBJECT'))
date_obs=STRCOMPRESS(sxpar(h,'DATE-OBS'))
;calculation mean value extintion
a=0.012
c=0.12
lambda=findgen(N_z)*param(1)+param(0)
secz=1/cos(z_obj*!DTOR)
ext=(a*1/((lambda/10000.)^4)+c)*2.5*alog10(exp(1))
ext_obj=10.^(0.4*ext*secz)
 print,'Z=',z_obj, 'Ext_obj=',median(ext_obj)

;definition  unit
sent=sent/(exp_obj*param[1])
order=fix(min(ALOG10(sent)))
if order lt 0 then order=order-1
out_units=10.^order		;output units in erg/cm^2/sec/A


for k=0,N_x-1 do begin
  for m=0,N_y-1 do begin
    obj_red[k,m,*]=spectr[k,m,*]*sent[*]*ext_obj
 endfor
endfor

sxaddpar,h,'BUNIT','erg/cm^2/sec/A',before='COMMENT'
sxaddhist,"IFU_CORRSENT: Flux calibration using standard star "+sxpar(h_sent,"OBJECT"),h
writefits,w_dir+fileout,obj_red,h

END
