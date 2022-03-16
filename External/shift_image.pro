function shift_image,im,dx0,dy0
;+
; NAME:        
;        SHIFT_IMAGE
; PURPOSE:      Remap image by linear interpolation
; CATEGORY:
; CALLING SEQUENCE:
;       In = shift_image(im,dx,dy)
; INPUTS:
;       Im      = Image to be shifted
;       Dx      = Shift in X-direction
;       Dy      = Shift in Y-direction
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;       In
; COMMON BLOCKS:
;       None.
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;       Z. Yi, UiO, June, 1992.
;       modified for MPFS-data by Victor Afanasiev, Jul 1999
;-
on_error,2
dx=dx0
dy=dy0

 ax=0 & ay=0
in=shift(im,round(dx),round(dy))
dx=dx-round(dx)  &       dy=dy-round(dy)
if dx lt 0 then ax=in -shift(in,-1,0)
if dx gt 0 then ax=shift(in,1,0)-in
if dy lt 0 then ay=in -shift(in,0,-1)
if dy gt 0 then ay=shift(in,0,1)-in

in=IN+ax*DX+ay*Dy


return,IN
END
