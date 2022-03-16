Function DEF_VECSHIFT,vec0,vec,bin=bin,plot=plot,m=m,gaus=gaus
;
; determination of the shift between two vectors
; from cross-correlation (peak-maximum)
; vec0 - input template (reference) vector
; vec - shifted vector
; M -- amplitude of the shift (DEFAULT=20)
; bin - factor for rebinning (default=10)

if n_params() lt 2 then begin
 message,/cont,'DEF_VECSHIFT,vec0,vec,bin=bin,/plot,m=m,/gaus'
 return,0
endif

;if not(keyword_set(M)) then M=10
if not(keyword_set(bin)) then bin=10
 num=n_elements(vec0)

 if n_elements(vec) ne num then begin
  message,/cont,'vec0 and vec must have same lengths'
  return,0
 endif

 nmax=bin*num
 mul=cosin_apod(num,10); apodisation function
 vec1=rebin(vec0*mul,nmax)
 vec2=rebin(vec*mul,nmax)

  f0=nmax/2+(nmax mod 2)
 f=findgen(nmax)-f0
 f=f/bin
 cross=cross_c(vec1,vec2)

 ma=max(cross,mmp)
 if keyword_set(m) then begin
  rec=where( f gt -abs(2*m) and f lt abs(2*m))
  ma=max(cross(rec),mmp)
  mmp=mmp+rec(0)
 endif  else ma=max(cross,mmp)

 if keyword_set(gaus) then begin
   ;bar=f(mmp) ; position of the maximum
   w=bin*5
   w2=(mmp+w)<(nmax-1)
   w1=(mmp-w)>0
   ;g=gaus_fit(f,cross,w,limmax=bar+w,limmin=bar-w,plot=0)
   res=gaussfit(f(w1:w2),cross(w1:w2),g,nterms=3)
   dx=-g(1)
  endif else   dx=- (mmp-f0)/float(bin)

 if keyword_set(plot) then begin
 window,2,tit='cross-correlation'
  plot,f,cross,yst=1,xst=1,xr=-dx+[-num,num]/50
  oplot,-[dx,dx],[0,1],linestyle=1
  endif
 return,dx

END