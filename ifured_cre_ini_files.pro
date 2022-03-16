; Combining several exposures
PRO IFURED_CRE_INI_FILES,type,d_dir=d_dir, w_dir=w_dir, night=night, cub=cub, zip=zip, file_in=file_in, crop=crop, logfield=logfield, use_lacosmic=use_lacosmic

  cd, current=old_dir
  if keyword_set(w_dir) then begin
    tst=file_test(w_dir)
    if tst eq 0 then begin
      res=dialog_message(['No such directory:',w_dir])
      cd, old_dir
      return
    endif
  cd,w_dir
  endif
  
imbias=READFITS('meanbias.fts',/SILENT)
if n_elements(imbias) eq 1 then begin
  res=dialog_message(['File with BIAS is not found: ',bias])
  cd, old_dir
  return
endif
  a=size(imbias)
  nx=a[1] & ny=a[2]
  
  readnoise = stddev(imbias[300:nx-300,300:ny-300])
  
  ; Crop overscan  
  IF KEYWORD_SET(crop) THEN BEGIN
      cut=fix(crop)

      cut[0]=(cut[0]) > 0
      cut[2]=(cut[2]) < (nx-1)
      
      if cut[2] eq 0 then cut[2]=nx-1
      if cut[0] eq cut[2] then begin
        cut[0]=0
        cut[2]=nx-1
      endif
      cut[1]=(cut[1]) > 0
      cut[3]=(cut[3]) < (ny-1)
      if cut[3] eq 0 then cut[3]=ny-1
      if cut[1] eq cut[3] then begin
        cut[1]=0
        cut[3]=[ny-1]
      endif 
 ENDIF else cut=[0,0,nx-1,ny-1]
 
 file=file_in
 
 names=ifured_filename(night=night, cub=cub, file=file, ext=".fts")
 dir=slash(d_dir)+slash(night+cub)
 n_image=n_elements(file)
 if keyword_set(zip) and not file_test(dir,/dir) then IFURED_UNZIP_CUB, d_dir, night, cub

  
  fileout=type+'_obs.fts'
  
;  IF keyword_set(logfield) then IFURED_LOG_UPD, "  *** Working on "+strupcase(type)+" frames ***"
  
  
  header=headfits(dir+names[0])
  if n_elements(header) eq 1 then begin
    res=dialog_message(['File is not found: ',names[0]])
    return
  endif
  nx = cut[2]-cut[0]+1
  ny = cut[3]-cut[1]+1
  im_out=fltarr(nx,ny,n_image)
  im_out[*]=!Values.F_NAN
  
  
  
  ; #### start reducing individual frames
  
  
  Texp=0
  for i=0,n_image-1 do begin
    im=readfits(dir+names[i],h)
    if n_elements(im) eq 1 then begin
      res=dialog_message(['File is not found: ',names[i]])
      return
    endif
    
    im-=imbias 
    over=IFURED_overscan(im)
    if abs(over) gt 20 then over=0
    im=float(im)-over
    
    im=im[cut[0]:cut[2],cut[1]:cut[3]]
    ;#### subtract scattering light in eta frames
    if type eq 'eta' then im=ifured_remove_bgr(im)
    
    Texp+=sxpar(h,'EXPTIME')
    
    if use_lacosmic then begin
      sxaddpar,h,"BSCALE",1.
      sxaddpar,h,"BZERO",0
      writefits,"temp_i.fits",im,h
      gain = sxpar(h,"GAIN")
      if not gain then begin
        gain = -1
        readnoise=0
      endif
      la_cosmic,"temp_i.fits", outlist="temp_i_lc.fits", masklist = 'temp_i_mask.fits', gain = gain, readn = readnoise*gain, niter=1, sigclip=10, sigfrac=0.8
    
    im_out[*,*,i]=readfits("temp_i_lc.fits",h)
    file_delete, "temp_i.fits","temp_i_lc.fits",'temp_i_mask.fits'
    endif else im_out[*,*,i]=im
    
    
    
  endfor
  sxaddpar,header,"BSCALE",1.
  sxaddpar,header,"BZERO",0
  sxaddpar,header,"EXPTIME", Texp
  sxaddhist,'IFURED: Original frames: '+strjoin(names,','),header
  sxaddhist,'IFURED: bias was removed',header
  if KEYWORD_SET(crop) then sxaddhist,'IFURED: Frames cutted',header
  if use_lacosmic then sxaddhist,'LA_Cosmic: CR hits cleaning done',header
  
  writefits,fileout,im_out,header
  cd, old_dir
;  if keyword_set(logfield) then IFURED_LOG_UPD, " "
 
 
 
 
 



end

