;**************************************
;* Function VECSHIFT
;* shifting vector in X direction
; if CONTIN=1 then cycle resetting ( CIGALE-mode)
;*******************************
FUNCTION VECSHIFT,vector,DX=dx

; cycles resetting
;if keyword_set(contin) then  image=[vector,vector,vector] else image=vector
;xs=n_elements(image)
;if keyword_set(contin)  and (dx GT xs/3) then dx=dx-xs/3*floor(3*dx/xs)
;if keyword_set(contin)  and (dx LT -xs/3) then dx=dx-xs/3*floor(3*dx/xs)

; creating shifting image
ax=0.
res=shift(vector,round(dx))
dx=dx-round(dx)
if dx lt 0 then ax=res-shift(res,-1)
if dx gt 0 then ax=shift(res,1)-res
res=res+ax*dX

;x=findgen(xs)-dx
;Res = INTERPOLATE(image,X,/cubic)
;rec=where((x LE 0)or(x GE xs-1),count)
;if count gt 0 then res(rec)=0
;if keyword_set(contin) then  res=res(xs/3:2*xs/3-1)

return,res

END


