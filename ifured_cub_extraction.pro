PRO IFURED_CUB_Extraction, filename, w_dir=w_dir, in_suffix=in_suffix, lambda_test=lambda_test,out_suffix=out_suffix, show=show
  
  if not KEYWORD_SET(w_dir) then w_dir=""
  if not KEYWORD_SET(in_suffix) then in_suffix="_lin.fts"
  if not KEYWORD_SET(out_suffix) then out_suffix="_cub.fts"
  
  
  nfiles=n_elements(filename)
  if nfiles eq 0 then return

    
  
  
  
  FOR current=0,nfiles-1 do begin
    if filename[current] ne "obj" and filename[current] ne "star" then used_suffix="_lin.fts" else used_suffix=in_suffix
    image=readfits(w_dir+filename[current]+used_suffix,h)
    image0=reform(image[*,*,0])
    image1=reform(image[*,*,1])
    
  
  
    
      ;### Now it is time to extract similar wavelength axes for both slits
    nx=sxpar(h,"NAXIS1")
    l=sxpar(h,"CRVAL1")
    dl=sxpar(h,"CDELT1")
    pix=sxpar(h,"CRPIX1")
    
    if pix eq 0 then pix=1
    
    lam=findgen(nx)*dl+l-(pix-1)*dl
    
    
    out_im=IFURED_PACK_FIBERS(in_left=image0, in_right=image1, Nside_x=Nx_cub, Nside_y=Ny_cub, nz=nz, start=start, file_fiberpos=w_dir+"fibers_pos.fit")
    
    
    
    sxaddpar,h,"NAXIS",3
    sxaddpar,h,"NAXIS3",nz,after="NAXIS2"
    sxaddpar,h,"NAXIS1",nx_cub
    sxaddpar,h,"NAXIS2",ny_cub
    
    sxaddpar,h,"CRVAL1",0
    sxaddpar,h,"CDELT1",1
    sxaddpar,h,"CRPIX1",1
    sxaddpar,h,"CRVAL2",0
    sxaddpar,h,"CDELT2",1
    sxaddpar,h,"CRPIX2",1
    sxaddpar,h,"CRVAL3",lam[start[0]],after="CRPIX2"
    sxaddpar,h,"CDELT3",dl,after="CRVAL3"
    sxaddpar,h,"CRPIX3",1,after="CDELT3"
    sxaddpar,h,"CTYPE3","AWAV",after="CRPIX3"
    
    sxaddhist,"IFURED_CUB_EXTRACTION: IFU data cub created from "+filename[current]+used_suffix,h
    
    writefits,w_dir+filename[current]+out_suffix,out_im,h

  IF filename[current] eq "obj" or filename[current] eq "star" and KEYWORD_SET(lambda_test) THEN BEGIN
      loadct,0,/sil
      w=2
      lam_cur=lam[start[0]]+dl*findgen(nz)
      
      if lambda_test lt lam_cur[0] or lambda_test gt lam_cur[nz-1] then lambda_test=(lam_cur[nz-1]+lam_cur[0])/2. 
      
      m=min(abs(lam_cur-lambda_test),mpos)
      im=total(out_im[*,*,mpos-w:mpos+2],3)/(2*w+1)
      
      objname=sxpar(h,"OBJECT")
      disperser=sxpar(h,"DISPERSE")
      if keyword_set(night) then add="Night "+night+", " else add=""
      cgps_open,w_dir+filename[current]+"_map.ps",charsize=0.8
        cgimage,(im),/negative,/axes,/keep_asp,title="Map of "+objname+" at "+string(lambda_test,format="(I4)")+"A, "+add+"Disperser "+disperser,xtit="x, pix",ytit="y, pix"
      cgps_close
  ENDIF

  ENDFOR
  
END

