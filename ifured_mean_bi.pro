PRO IFURED_MEAN_BI,inp_arr,out_arr,nosub=nosub
  s=size(inp_arr)
  Xs=s[1]; \
  Ys=s[2];  > Array size
  n=s[3] ; /
  p=n/2
  w=50
  
  
  if not(keyword_set(nosub)) then begin
  for i=0,n-1 do begin
      ;box=inp_arr(xs/2-w:xs/2+w,ys/2-w:ys/2+w,i)
      box=inp_arr[*,*,i]
      box=median(box,5)
      a=stdev(box,mean)
      if i eq 0 then mean0=mean else inp_arr[*,*,i]=inp_arr[*,*,i]-(mean-mean0)
      message,'Mean value in frame '+string(i,format='(I3)')+': '+string(mean),/cont
  endfor
  endif
  
   out_arr=fltarr(xs,ys)
   for x=0,xs-1 do BEGIN
     for y=0,ys-1 do BEGIN
      
      meanclip, inp_arr[x,y,*], mn
      out_arr[x,y]=mn*n
      
;      v1=float(inp_arr[x,y,*])
;      v1=v1(sort(v1))
;      n1=n
;      med=v1[p]
;      REPEAT BEGIN
;       v1=v1(0:n1-1)
;       v2=v1(0:n1-2)
;       s1=sqrt(total((v1-med)^2)/n1)
;       s2=sqrt(total((v2-med)^2)/(n1-1))
;       n1=n1-1
;       ENDREP UNTIL (n1 LT 2) or (s1-s2 lt s2)
;      out_arr[x,y]=total(v1)*n/(n1+1)
     ENDFOR
    if x  mod 200 eq 0 then begin
      print,x
    endif 
   
   ENDFOR
 
END
;
;
;
;
;s=size(inp_arr)
;Xs=s(1); \
;Ys=s(2);  > Array size
;n=s(3) ; /
;p=n/2
; image=fltarr(xs,ys)
;print,'MEAN BIAS creating:'
; for x=0,xs-1 do BEGIN
;   for y=0,ys-1 do BEGIN
;    v1=float(inp_arr(x,y,*))
;    v1=v1(sort(v1))
;    n1=n
;    med=v1(p)
;    REPEAT BEGIN
;     v1=v1(0:n1-1)
;     v2=v1(0:n1-2)
;     s1=sqrt(total((v1-med)^2)/n1)
;     s2=sqrt(total((v2-med)^2)/(n1-1))
;     n1=n1-1
;     ENDREP UNTIL (n1 LT 2) or (s1-s2 lt s2)
;    image(x,y)=total(v1)*n/(n1+1)
;   ENDFOR
;  if x/200 eq x/200. then  print,'X=',x
; ENDFOR
;END
