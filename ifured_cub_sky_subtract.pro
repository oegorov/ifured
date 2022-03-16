PRO IFURED_CUB_SKY_SUBTRACT, filename,  w_dir=w_dir, show=show;,ndeg=ndeg

  n_files=n_elements(filename)
  if n_files eq 0 then return
;  if n_elements(ndeg) eq 0 then ndeg=3
  
  
  
  
  FOR current=0,n_files-1 DO BEGIN
    cub_all=readfits(w_dir+filename[current]+"_cub_n.fts",h)
    cub_out=cub_all*0
    nx=sxpar(h,"NAXIS1")
    ny=sxpar(h,"NAXIS2")
    nz=sxpar(h,"NAXIS3")
    
    
;    
;    sky1=reform(cub_all[*,ny-1,*])
;    sky0=reform(cub_all[*,0,*])
;    xx=findgen(nx)
;    win=5
;    for z=0,nz-1 do begin
;      lowess, xx,reform(sky0[*,z]),win,sky0_smo
;;      sky0_smo=smooth(reform(sky0[i,*]),win,/edge,/nan)
;;      sky1_smo=smooth(reform(sky1[i,*]),win,/edge,/nan)
;      lowess, xx,reform(sky1[*,z]),win,sky1_smo
;      for j=0,11 do cub_out[*,j,z]=cub_all[*,j,z]-sky0_smo
;      for j=12,ny-1 do cub_out[*,j,z]=cub_all[*,j,z]-sky1_smo
;    
;    endfor
;    
    
    
    
    FOR z=0l,nz-1 do begin
     FOR i=0,nx-1 do begin
      sky1=(cub_all[i,ny-1,z])
      sky0=(cub_all[i,0,z])
      cub_out[i,0:11,z]=cub_all[i,0:11,z]-sky0;+cub_all[i,ny-1,z])/2.
      cub_out[i,12:ny-1,z]=cub_all[i,12:ny-1,z]-sky1
     endfor
    ENDFOR
    
    
    
    sxaddhist,"IFURED_CUB_SKY_SUBTRACT: Night sky line removed",h;ing using polynom fit with NDEG="+string(ndeg,format="(I0)"),h
    writefits,w_dir+filename[current]+"_cub-sky.fts",cub_out,h
    
  ENDFOR

END