pro ifured_flatnorm,w_dir=w_dir, suff=suff, show=show
 if not keyword_set(w_dir) then w_dir="" 
 if not keyword_set(suff) then suff='_cub.fts'
    
    fileflat=w_dir+'flat'+suff
    
    flat=float(readfits(fileflat,hflat))
    
    nx=sxpar(hflat,'naxis1')
    ny=sxpar(hflat,'naxis2')
    nz=sxpar(hflat,'naxis3')
    
    
    ;#### Normalize Flat
    
    wz=20
    win=20
    
    
    
    flat_norm=flat
    x=get_num(nx,ny,/x)
    y=get_num(nx,ny,/y)
    
    
    norm0=mean(flat_norm[6:17,5:11,nz/2-wz:nz/2+wz],/nan)
    norm1=mean(flat_norm[6:17,12:18,nz/2-wz:nz/2+wz],/nan)
    
    vec0=reform(total(total(flat_norm[6:17,5:11,*],1,/nan),1,/nan))/12./7.
    vec1=reform(total(total(flat_norm[6:17,12:18,*],1,/nan),1,/nan))/12./7.
    vec=(vec0/norm0+vec1/norm1)*(norm0+norm1)/2.
    vec=[reverse(vec[0:win-1]),vec,reverse(vec[nz-win:nz-1])]

    xx=findgen(n_elements(vec))
    lowess, xx,vec,win,vec0
;    vec0=smooth(vec,win,/edge,/nan)
    for ind=0,nx*ny-1 do begin
      vec=reform(flat_norm[x[ind],y[ind],*])
      vec=[reverse(vec[0:win-1]),vec,reverse(vec[nz-win:nz-1])]
      vec_smo=smooth(vec,win,/edge_m,/nan)
;      lowess,xx,vec,win,vec_smo
      flat_norm[x[ind],y[ind],*]=vec_smo[win:nz+win-1]/vec0[win:nz+win-1] 
    endfor
    
    ;norm=mean(flat_norm[8:15,6:17,nz/2-wz:nz/2+wz]);*3
;    flat_norm=flat_norm;/norm
    flat_out=flat/flat_norm
    
    
    
    
    
    
    
    
    sxaddhist,'IFURED_FLATNORM: Flat smoothed along Z.  win='+string(win),hflat
    writefits,w_dir+'flat_cub_n.fts',flat_out,hflat
    writefits,w_dir+'flat_sens.fits',flat_norm,hflat
    IF Keyword_set(show) then begin
    
      wave_0=sxpar(hflat,'CRVAL3') & d_wave=sxpar(hflat,'CDELT3')
      wave=findgen(Nz)*d_wave+wave_0
      
      flat_tot=fltarr(nz)
      for z=0,nz-1 do flat_tot[z]=mean(flat_out[*,*,z])
      cgdisplay,600,700,wid=2,title="Normalized Flat"
      cgplot,wave,flat_tot,xst=1,charsize=1.5,col="black",/yst,pos=[0.15,0.1,0.95,0.95],xtit="Wavelength, A", ytit="Flux, counts",thick=2;,yr=minmax(flat_tot[50:nz-50])
;      cgoplot,wave,vec0,col='cyan'
      cgoplot,wave,total(total(flat_out[*,0:11,*],2,/nan),1,/nan)/22./12,col="orange",thick=2
      cgoplot,wave,total(total(flat_out[*,12:23,*],2,/nan),1,/nan)/22./12,col="blue",thick=2
      al_legend,["Total","Right slit","Left slit"],linest=[0,0,0],col=["black","orange","blue"],thick=[2,2,2]
    ENDIF
    
END