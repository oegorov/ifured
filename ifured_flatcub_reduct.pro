PRO ifured_flatcub_reduct, image_type,suff=suff, w_dir=w_dir


    if not keyword_set(w_dir) then w_dir="" 
    if not keyword_set(suff) then suff='_cub.fts'
    flat_norm=readfits(w_dir+'flat_sens.fits',hflat)
    
    
    
    ;#### Normalize all other files
    for i=0,n_elements(image_type)-1 do begin
      if image_type[i] eq 'flat' then continue
          file=image_type[i]+suff
          im=readfits(w_dir+file,h)
          im=im/flat_norm
          sxaddhist,'IFURED_FLATCUB_REDUCT: original file:'+file,h
          writefits,w_dir+image_type[i]+'_cub_n.fts',im,h
    endfor
        
    
END