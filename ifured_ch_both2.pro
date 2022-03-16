; removing Cosmic Hits from two images

PRO IFURED_CH_BOTH2,im1,im2,T=T,lim=lim,gain=gain,nonorm=nonorm
 if not(keyword_set(T)) then T=5
  if not(keyword_set(lim)) then lim=10
 if not(keyword_set(gain)) then gain=1
 
 recfin1=where(finite(im1))
 recfin2=where(finite(im2))
 
  if not(keyword_set(nonorm)) then norm=median(im1[recfin1])/median(im2[recfin2]) else norm=1
  message,'Norm factor:'+string(norm),/cont
  dif=(im1-im2*norm)
  r1=where(dif ge 0,counts1)
  med=im1
  if counts1 gt 0 then  med[r1]=im2[r1]*norm
  MED=ABS(MED)>1
  med=med;+0.2*median(med)
  dif=dif/sqrt(gain*med)
  recfin=where(finite(dif))
  s=float(size(im1))
  sig=total(abs(median(dif[recfin],3)),/nan)/(s(1)*s(2))

  rec1=where( dif GT t*sig,count1)
  rec2=where( dif LT -t*sig,count2)


  ; mask creating
  ;if keyword_set(lim) then begin
    mask=im1
    mask(*)=0
   if count1 gt 0 then  mask(rec1)=100.
   if count2 gt 0 then  mask(rec2)=-100.
   mask=smooth(mask,3)
   if count1 gt 0 then rec1=where( mask GT lim,count1)
   if count2 gt 0 then rec2=where( mask LT -lim,count2)
 ;endif

   message,string(count1+count2)+' points was replaced',/cont
   if count1 gt 0 then  im1(rec1)=im2(rec1)*norm
   if count2 gt 0 then im2(rec2)=im1(rec2)/norm

END