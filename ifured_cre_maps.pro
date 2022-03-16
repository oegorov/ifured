FUNCTION IFURED_FITLINES,x,param
  res=gaussian(x,[param[1],param[0],param[2]/2.35482])+poly(x,[param[3],param[4],param[5]])
  if n_elements(param) gt 4+2 then res+=gaussian(x,[param[1]*param[5+2],param[0]+param[4+2],param[2]/2.35482])
  if n_elements(param) gt 6+2 then res+=gaussian(x,[param[7+2]*param[1],param[0]+param[6+2],param[2]/2.35482])
  RETURN,res

END


pro ifured_cre_maps,filein,w_dir=w_dir, sysvel=sysvel,error=error, sn_min=sn_min
  ; This procedure creates maps of flux, velocity, velocity dispersion and underlying continuum 
  ;    in each of observed emission line from following list: 
  ;      Hbeta, [OIII]5007, Ha+[NII]6548,6584 (triplet, return separately Ha and [NII]6584), [SII]6717,[SII]6731 (fit as dublet) 
  
  if keyword_set(w_dir) then cd,w_dir,current=olddir
  if not keyword_set(sysvel) then sysvel=0
  if n_elements(sn_min) eq 0 then sn_min=1.5
  maps_dir=slash('maps')
  error=0
  cub=readfits(filein,h)
  get_vhel,filein,helcor
  tmp={line_par,name:'',val:0E,dobl_len:0E,dobl_rat:0E,tri_len:0E,tri_rat:0E,left_bord:0E,right_bord:0E, $
       fix_drat: 0L, fix_trat: 0L, fix_dlen: 0L, fix_tlen: 0L, name_dob:'', name_tri:''}
  lines_fit=[{line_par,'Hb',4861.325,0,0,0,0,4790.,4920.,0,0,0,0,'',''},$
             {line_par,'[OIII] 5007',5006.82,4958.9-5006.82,1./3.,0,0,4935.,5070.,1,1,1,0,'[OIII] 4959',''},$
             {line_par,'HeI 5876',5875.62,0,0,0,0,5820.,5940.,0,0,0,0,'',''},$
             {line_par,'[OI] 6300',6300.20,6363.67-6300.20,1./3.,0,0,6230.,6430.,0,0,1,0,'[OI] 6363',''},$
             {line_par,'[NII] 6583',6583.4601,6548.04-6583.46,0.327,6562.78-6583.46,20.0,6460.,6640.,1,0,1,1,'[NII] 6548','Ha'},$
             {line_par,'[SII] 6717',6716.44,6730.81-6716.44,1./1.3,0,0,6698.,6790.,0,0,1,0,'[SII] 6731',''}]
                       
  
  lam0=sxpar(h,"CRVAL3")
  dl=sxpar(h,"CDELT3")
  crpix=sxpar(h,"CRPIX3")
  if crpix eq 0 then crpix=1
  nz=sxpar(h,"NAXIS3")
  nx=sxpar(h,"NAXIS1")
  ny=sxpar(h,"NAXIS2")
  lam_obs=(findgen(nz)-(crpix-1))*dl+lam0
  
  c=2.99792458e5
  lam=lam_obs/(1.+(sysvel-helcor)/c)
  
  
  rec=where(lines_fit.val-10. gt lam[0] and lines_fit.val+10. lt lam[nz-1],nr)
  if nr eq 0 then begin
    error=1
    message,"No pre-defined emission lines were found whithin wavelenght scale of the current cube.",/cont
    return
  endif
  lines_fit=lines_fit[rec]
  nlines=nr
  nmaps=nlines
  
  for i=0,nr-1 do begin
    if lines_fit[i].dobl_len ne 0 and lines_fit[i].dobl_rat ne 0 then nmaps+=1
    if lines_fit[i].tri_len ne 0 and lines_fit[i].tri_rat ne 0 then nmaps+=1 
  endfor
  
  maps_flux=fltarr(nx,ny,nmaps)
  maps_vel=fltarr(nx,ny,nmaps)
  maps_disp=fltarr(nx,ny,nmaps)
  maps_cont=fltarr(nx,ny,nmaps)
  maps_sn=fltarr(nx,ny,nmaps)
  maps_name=strarr(nmaps)
  tmp={parinfo_struct,fixed:0L,limits:fltarr(2),limited:lonarr(2)}
  
  FOR i=0,nx-1 do begin
    FOR j=0,ny-1 do begin
      curmap=0
      FOR l=0,nlines-1 do begin
        curline=reform(lines_fit[l])
        rec=where(lam ge curline.left_bord and lam le curline.right_bord,nr)
          
          cont0=min(cub[i,j,rec])
          norm_factor=max(cub[i,j,rec])-cont0
          vec=reform(cub[i,j,rec]-cont0)/norm_factor
          rec1=where((lam ge curline.left_bord and lam le curline.val-40) or (lam ge curline.val+40 and lam le curline.right_bord),nr1)
          if nr1 ne 0 then noise=abs(stddev(cub[i,j,rec1])) else noise=abs(stddev(cub[i,j,rec]))
          ;      center0,                          intens0, fwhm, contin,  dlam,  drat,  tlam, trat]   
          param=[curline.val*(1.+(sysvel-helcor)/c),  1.,   3.,     0,0,0]
          npars=n_elements(param)
          parinfo=replicate(tmp,npars)
          parinfo[1].limits=[0.,1.]
          parinfo[1].limited=[1,1]
          parinfo[2].limits=[1.,30.]
          parinfo[2].limited=[1,1]
          ;parinfo[3].limits=[0.,median(vec)]
          ;parinfo[3].limited=[1,1]
          if curline.dobl_len ne 0 and curline.dobl_rat ne 0 then begin
            param=[param,curline.dobl_len,curline.dobl_rat]
            npars=n_elements(param)
            parinfo=[parinfo,replicate(tmp,2)]
            parinfo[npars-2].fixed = curline.fix_dlen
            parinfo[npars-1].fixed = curline.fix_drat
          endif
          if curline.tri_len ne 0 and curline.tri_rat ne 0 then begin
            param=[param,curline.tri_len,curline.tri_rat]
            npars=n_elements(param)
            parinfo=[parinfo,replicate(tmp,2)]
            parinfo[npars-2].fixed = curline.fix_tlen
            parinfo[npars-1].fixed = curline.fix_trat
          endif 
          
          fxpars=total(parinfo.fixed)
          err=sqrt(10.+abs(vec))
          weight=1./err
          x=lam_obs[rec]
          res=mpfitfun('IFURED_FITLINES',x,vec,err,param,weight=weight,/QUIET,yfit=yfit,parinfo=parinfo,perror=perror,bestnorm=bestnorm)
          dof=n_elements(x)-n_elements(param)+fxpars
          if n_elements(perror) eq 0 then perror=fltarr(10)+1e6 
          perror=perror*sqrt(BESTNORM / DOF)
          
          maps_flux[i,j,curmap]=sqrt(2*!pi)*res[2]*res[1]/2.35482*norm_factor
          maps_disp[i,j,curmap]=res[2]/2.35482/curline.val*c
          maps_vel[i,j,curmap]=(res[0]/curline.val-1)*c+helcor
          maps_cont[i,j,curmap]=poly(res[0],[res[3],res[4],res[5]])*norm_factor+cont0
          maps_sn[i,j,curmap]=(res[1]*norm_factor)/noise
          if i eq 0 and j eq 0 then maps_name[curmap]=curline.name
          curmap+=1
          if curline.dobl_len ne 0 and curline.dobl_rat ne 0 then begin
            maps_flux[i,j,curmap]=sqrt(2*!pi)*res[2]*res[1]*res[5+2]/2.35482*norm_factor
            maps_disp[i,j,curmap]=res[2]/2.35482/(curline.val+curline.dobl_len)*c
            maps_vel[i,j,curmap]=((res[0]+res[4+2])/(curline.val+curline.dobl_len)-1)*c+helcor
            maps_cont[i,j,curmap]=poly(res[0]+res[4+2],[res[3],res[4],res[5]])*norm_factor+cont0
            maps_sn[i,j,curmap]=(res[1]*res[5+2]*norm_factor)/noise
            if i eq 0 and j eq 0 then maps_name[curmap]=curline.name_dob
            curmap+=1
          endif
          if curline.tri_len ne 0 and curline.tri_rat ne 0 then begin
            maps_flux[i,j,curmap]=sqrt(2*!pi)*res[2]*res[1]*res[7+2]/2.35482*norm_factor
            maps_disp[i,j,curmap]=res[2]/2.35482/(curline.val+curline.tri_len)*c
            maps_vel[i,j,curmap]=((res[0]+res[6+2])/(curline.val+curline.tri_len)-1)*c+helcor
            maps_cont[i,j,curmap]=poly(res[0]+res[6+2],[res[3],res[4],res[5]])*norm_factor+cont0
            maps_sn[i,j,curmap]=(res[1]*res[7+2]*norm_factor)/noise
            if i eq 0 and j eq 0 then maps_name[curmap]=curline.name_tri
            curmap+=1
          endif
      endfor
    endfor
  endfor
  
  hmap=h
  sxaddpar,hmap,"NAXIS",2
  sxdelpar,hmap,"NAXIS3"
  sxdelpar,hmap,"CRVAL3"
  sxdelpar,hmap,"CDELT3"
  sxdelpar,hmap,"CRPIX3"
  sxaddhist,"IFURED: Map created from "+filein,hmap
  
  r=file_test(maps_dir,/dir)
  if r eq 0 then file_mkdir,maps_dir
  
  FOR i=0,nmaps-1 do begin
    line_name=strcompress(maps_name[i],/remove_all)
    remchar,line_name,'['
    remchar,line_name,']'
    writefits,maps_dir+"obj_"+line_name+"_flux.fits",reform(maps_flux[*,*,i]),hmap
    writefits,maps_dir+"obj_"+line_name+"_vel.fits",reform(maps_vel[*,*,i]),hmap
    writefits,maps_dir+"obj_"+line_name+"_disp.fits",reform(maps_disp[*,*,i]),hmap
    writefits,maps_dir+"obj_"+line_name+"_cont.fits",reform(maps_cont[*,*,i]),hmap
    writefits,maps_dir+"obj_"+line_name+"_sn.fits",reform(maps_sn[*,*,i]),hmap
  ENDFOR
  
  grid=[3,ceil(nmaps/3.)]
  xs=14.
  dx=0.05
  dy=0.06
  x0=0.05
  y0=0.05
  yscb=0.01
  dycb=0.01
  xs_im=(1.-(dx*(grid[0]-1)+x0+0.01))/grid[0]
  ys_im=(1.-(dy*(grid[1]-1)+(dycb+yscb)*grid[1]+y0+0.1))/grid[1]
  ys=xs*xs_im*ny/nx/ys_im
  
  
  ;Fluxes in emission lines
  cgps_open,maps_dir+'fluxes.eps',xs=xs,ys=ys,/encaps
    for i=0,nmaps-1 do begin
      pos=[x0+(dx+xs_im)*(i mod grid[0]),y0+(dy+ys_im+dycb+yscb)*(grid[1]-floor(i/grid[0])-1),$
              x0+(dx+xs_im)*(i mod grid[0])+xs_im,y0+(dy+ys_im+dycb+yscb)*(grid[1]-floor(i/grid[0])-1)+ys_im]
      pos_cb=[pos[0],pos[3]+dycb,pos[2],pos[3]+dycb+yscb]
      sn=reform(maps_sn[*,*,i])
      rec=where(sn lt sn_min,nr)
      if nr eq n_elements(sn) then continue
      im=reform(maps_flux[*,*,i])
      if nr gt 0 then im[rec]=!Values.F_NAN
      
      cur_im=cgresizeimage(im,256,256.*ny/nx)
      im_flux=reform(maps_flux[*,*,i])
      if nr gt 0 then im_flux[rec]=!Values.F_NAN
      cont=reform(im_flux)
      med=median(im > 0)
      rms=stddev(im > 0,/nan)
      minval=(med-1.*rms)>0
      maxval=med+10.*rms
      ctind=27
      cgimage,cur_im,maxval=maxval,minval=minval,/axes,xr=[0,nx],yr=[0,ny],xtit="X, pix", ytit="Y, pix",ctind=ctind,pos=pos,/norm,/noer,charsize=1.0,missing_col="white",missing_val=!Values.F_NAN
      cgcontour,cont,/onima,pos=pos,label=0
      cgcolorbar,pos=pos_cb,range=[minval,maxval],/norm,title="Flux ("+maps_name[i]+"), erg/s/cm$\up2$",/top,ctind=ctind,charsize=1.2
    endfor
  cgps_close
  
  
  ;Velocity fields
  cgps_open,maps_dir+'velocities.eps',xs=xs,ys=ys,/encaps
    for i=0,nmaps-1 do begin
      pos=[x0+(dx+xs_im)*(i mod grid[0]),y0+(dy+ys_im+dycb+yscb)*(grid[1]-floor(i/grid[0])-1),$
              x0+(dx+xs_im)*(i mod grid[0])+xs_im,y0+(dy+ys_im+dycb+yscb)*(grid[1]-floor(i/grid[0])-1)+ys_im]
      pos_cb=[pos[0],pos[3]+dycb,pos[2],pos[3]+dycb+yscb]
      sn=reform(maps_sn[*,*,i])
      rec=where(sn lt sn_min,nr)
      if nr eq n_elements(sn) then continue
      im=reform(maps_vel[*,*,i])
      if nr gt 0 then im[rec]=!Values.F_NAN
      cur_im=cgresizeimage(im,256,256.*ny/nx)
      im_flux=reform(maps_flux[*,*,i])
      if nr gt 0 then im_flux[rec]=!Values.F_NAN
      cont=reform(im_flux)
      med=median(im)
      rms=stddev(im,/nan)
      med=sysvel
      minval=med-3.*rms
      maxval=med+3.*rms
      ctind=27
      cgimage,cur_im,maxval=maxval,minval=minval,/axes,xr=[0,nx],yr=[0,ny],xtit="X, pix", ytit="Y, pix",ctind=ctind,pos=pos,/norm,/noer,charsize=1.0,missing_col="white",missing_val=!Values.F_NAN
      cgcontour,cont,/onima,pos=pos,label=0
      cgcolorbar,pos=pos_cb,range=[minval,maxval],/norm,title="Velocity ("+maps_name[i]+"), km/s",/top,ctind=ctind,charsize=1.2,format="(I0)"
    endfor
  cgps_close
  
  
  ;Velocity Dispersion
  cgps_open,maps_dir+'dispersions.eps',xs=xs,ys=ys,/encaps
    for i=0,nmaps-1 do begin
      pos=[x0+(dx+xs_im)*(i mod grid[0]),y0+(dy+ys_im+dycb+yscb)*(grid[1]-floor(i/grid[0])-1),$
              x0+(dx+xs_im)*(i mod grid[0])+xs_im,y0+(dy+ys_im+dycb+yscb)*(grid[1]-floor(i/grid[0])-1)+ys_im]
      pos_cb=[pos[0],pos[3]+dycb,pos[2],pos[3]+dycb+yscb]
      sn=reform(maps_sn[*,*,i])
      rec=where(sn lt sn_min,nr)
      if nr eq n_elements(sn) then continue
      im=reform(maps_disp[*,*,i])
      if nr gt 0 then im[rec]=!Values.F_NAN
      cur_im=cgresizeimage(im,256,256.*ny/nx)
      im_flux=reform(maps_flux[*,*,i])
      if nr gt 0 then im_flux[rec]=!Values.F_NAN
      cont=reform(im_flux)
      med=median(im)
      rms=stddev(im,/nan)
      minval=med-1.5*rms
      maxval=med+1.5*rms
      ctind=27
      cgimage,cur_im,maxval=maxval,minval=minval,/axes,xr=[0,nx],yr=[0,ny],xtit="X, pix", ytit="Y, pix",ctind=ctind,pos=pos,/norm,/noer,charsize=1.0,missing_col="white",missing_val=!Values.F_NAN
      cgcontour,cont,/onima,pos=pos,label=0
      cgcolorbar,pos=pos_cb,range=[minval,maxval],/norm,title="Velocity dispersion ("+maps_name[i]+"), km/s",/top,ctind=ctind,charsize=1.2,format="(I0)"
    endfor
  cgps_close
  
  ;Underlying Continuum
  cgps_open,maps_dir+'continuum.eps',xs=xs,ys=ys,/encaps
    for i=0,nmaps-1 do begin
      pos=[x0+(dx+xs_im)*(i mod grid[0]),y0+(dy+ys_im+dycb+yscb)*(grid[1]-floor(i/grid[0])-1),$
              x0+(dx+xs_im)*(i mod grid[0])+xs_im,y0+(dy+ys_im+dycb+yscb)*(grid[1]-floor(i/grid[0])-1)+ys_im]
      pos_cb=[pos[0],pos[3]+dycb,pos[2],pos[3]+dycb+yscb]
      sn=reform(maps_sn[*,*,i])
      rec=where(sn lt sn_min,nr)
      if nr eq n_elements(sn) then continue
      im=reform(maps_cont[*,*,i])
      if nr gt 0 then im[rec]=!Values.F_NAN
      cur_im=cgresizeimage(im,256,256.*ny/nx)
      im_flux=reform(maps_flux[*,*,i])
      if nr gt 0 then im_flux[rec]=!Values.F_NAN
      cont=reform(im_flux)
      med=median(im)
      rms=stddev(im,/nan)
      minval=(med-1.*rms)>0
      maxval=med+10.*rms
      ctind=27
      cgimage,cur_im,maxval=maxval,minval=minval,/axes,xr=[0,nx],yr=[0,ny],xtit="X, pix", ytit="Y, pix",ctind=ctind,pos=pos,/norm,/noer,charsize=1.0,missing_col="white",missing_val=!Values.F_NAN
      cgcontour,cont,/onima,pos=pos,label=0
      cgcolorbar,pos=pos_cb,range=[minval,maxval],/norm,title="Intensity ("+maps_name[i]+"), erg/s/cm$\up2$",/top,ctind=ctind,charsize=1.2
    endfor
  cgps_close
  
  
  
  if keyword_set(w_dir) then cd,olddir

END