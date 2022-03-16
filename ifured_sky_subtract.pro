PRO IFURED_SKY_SUBTRACT, filename,  w_dir=w_dir, show=show,ndeg=ndeg

  n_files=n_elements(filename)
  if n_files eq 0 then return
  if n_elements(ndeg) eq 0 then ndeg=3
  
  fibers_all=read_asc(w_dir+"fibers_pos.fit")
  
  
  
  FOR current=0,n_files-1 DO BEGIN
    im_all=readfits(w_dir+filename[current]+"_cub_n.fts",h)
    im_out=im_all*0
    nx=sxpar(h,"NAXIS1")
    ny=sxpar(h,"NAXIS2")
    cpix=sxpar(h,"CRPIX1")
    if cpix eq 0 then cpix=1
    
    lam=sxpar(h,"CDELT1")*findgen(nx)+sxpar(h,"CRVAL1")-(cpix-1)*sxpar(h,"CDELT1")
    yy=findgen(ny)
    sky_lines=fltarr(nx,ny)
    FOR slitnum=0,1 do begin
    
      rec=where(fibers_all[2,*] eq slitnum)
      fibers=fibers_all[0:1,rec]
      sorted=sort(fibers[0,*])
      fib_type=reform(fibers[1,sorted])
      rec_sky=where(fib_type eq 1, nsky)
      
    
      im=reform(im_all[*,*,slitnum])
      
      for i=0,nx-1 do begin
        res=goodpoly(yy[rec_sky],im[i,rec_sky],ndeg,3)
        sky_lines[i,*]=poly(yy,res)
      endfor
      
      im_out[*,*,slitnum]=im-sky_lines
      
      if keyword_set(show) then begin
        if slitnum eq 0 then cgdisplay,wid=5+current,title="Sky subtruction: "+filename[current]
        cgimage,sigrange(im, frac=0.95),layout=[2,2,1+2*slitnum],/nega,ctind=0
        cgimage,sigrange(im-sky_lines, frac=0.95),layout=[2,2,2+2*slitnum],/nega,ctind=0
      endif
      
    ENDFOR
    
    sxaddhist,"IFURED_SKY_SUBTRACT: Night sky line removing using polynom fit with NDEG="+string(ndeg,format="(I0)"),h
    writefits,w_dir+filename[current]+"_cub-sky.fts",im_out,h
    
  ENDFOR

END