PRO IFURED_DO_WARP, filenames=filenames, in_suff=in_suff, out_suff=out_suff, w_dir=w_dir, filewarp=filewarp
  
  if not keyword_set(out_suff) then out_suff='_warp.fts'
  if not keyword_set(in_suff) then in_suff='_i.fts'
  if not keyword_set(w_dir) then w_dir=''
  if not keyword_set(filewarp) then filewarp='warp.fit'
  
  warp_param=read_asc(w_dir+filewarp)
  s=size(warp_param)
  shift_x=reform(warp_param[0,0])
  shift_y=reform(warp_param[1,0])
  new_nx_lt=reform(warp_param[2,0])
  new_nx_rt=reform(warp_param[3,0])
  new_ny=reform(warp_param[4,0])
  nx_out=max([new_nx_rt,new_nx_lt])
  
  warp_param=warp_param[*,1:s[2]-1]
  rec=where(warp_param[4,*] eq 0)
  warp_param_lt=warp_param[0:3,rec]
  rec=where(warp_param[4,*] eq 1)
  warp_param_rt=warp_param[0:3,rec]
  
  
  im_out=fltarr(nx_out,new_ny,2)
  
  FOR f=0,n_elements(filenames)-1 do begin
    im=readfits(w_dir+filenames[f]+in_suff,h)
    im_out[*]=!VALUES.F_NAN
    
    if f eq 0 then begin 
      s=size(im)
      nx=s[1]
      ny=s[2]
      x=indgen(nx)
      y=indgen(ny)
      break_lt=max(where(finite(reform(im[*,ny/2,0]))))
      break_rt=max(where(finite(reform(im[*,ny/2,1]))))
      x_lt_rec=where(x le break_lt,nx_lt)
      x_lt=x[x_lt_rec]
      x_rt_rec=where(x le break_rt,nx_rt)
      x_rt=x[x_rt_rec]
    endif
    
    im_lt=fltarr(nx_lt+shift_x*2,ny+shift_y*2)
    im_lt[shift_x:nx_lt-1+shift_x,shift_y:ny-1+shift_y]=reform(im[x_lt,*,0])
    im_rt=fltarr(nx_rt+shift_x*2,ny+shift_y*2)
    im_rt[shift_x:nx_rt-1+shift_x,shift_y:ny-1+shift_y]=reform(im[x_rt,*,1])
       
  
    
    res_im_lt=warp_tri(warp_param_lt[2,*],warp_param_lt[3,*],warp_param_lt[0,*],warp_param_lt[1,*],im_lt,output_size=[new_nx_lt,new_ny])
      rec=where(total(res_im_lt,2) eq 0,nr)
      if nr gt 0 then res_im_lt[rec,*]=!VALUES.F_NAN
    res_im_rt=warp_tri(warp_param_rt[2,*],warp_param_rt[3,*],warp_param_rt[0,*],warp_param_rt[1,*],im_rt,output_size=[new_nx_rt,new_ny])
      rec=where(total(res_im_rt,2) eq 0,nr)
      if nr gt 0 then res_im_rt[rec,*]=!VALUES.F_NAN
    
    
    
    im_out[0:new_nx_lt-1,*,0]=res_im_lt
    im_out[0:new_nx_rt-1,*,1]=res_im_rt
    
    writefits,w_dir+filenames[f]+out_suff,im_out,h
    
    
  ENDFOR
  
    
  
END