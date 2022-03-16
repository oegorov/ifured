
;################ WORKING WITH FILES IN OS #################################

PRO IFURED_UNZIP_CUB, d_dir, night, cub
;Unzip NIGHT+CUB+.ZIP file in d_dir directory 
  COMMON IFURED_EXTERNAL
    file_mkdir,slash(d_dir)+slash(night+cub)
    clean_dir=[clean_dir,slash(d_dir)+slash(night+cub)]
    cd,slash(d_dir)+slash(night+cub),current=old_dir
    spawn,unzipper+' '+slash(d_dir)+night+cub+'.zip' 
    cd,old_dir
END


PRO IFURED_CLEAN_DIRS
  ;Delete all previously unzipped directories
  COMMON IFURED_EXTERNAL
  ncd=n_elements(clean_dir)
  if ncd gt 1 then begin
     for zz=1,ncd-1 do file_delete,clean_dir[zz],/rec,/allow_nonex
     clean_dir=['']
  endif
END



;################ PARSE AND CHECK LOG FILE #################################


PRO IFURED_SETFIELDS
    COMMON IFURED_PARAMS
    COMMON IFURED_WIDGET_ELEMENTS
    
    crop_vals=strsplit(crop,',',/ext)
    for i=0,3 do WIDGET_CONTROL, ovscan[i],set_val=crop_vals[i]
    
    WIDGET_CONTROL, slit_bord[0],set_val=slit_break_left
    WIDGET_CONTROL, slit_bord[1],set_val=slit_break_right
    
    for i=0,5 do WIDGET_CONTROL, tr_cr_wid[i],set_val=tr_cr[i]
      
    WIDGET_CONTROL, tra_params[0],set_val=tra_dspec
    WIDGET_CONTROL, tra_params[1],set_val=tra_win
  ;  WIDGET_CONTROL, ndeg_geometry,get_val=Geometry_Lines_ndeg
  ;  WIDGET_CONTROL, win_geometry,get_val=Geometry_Lines_win
  
    WIDGET_CONTROL,lin_params[0],set_val=lin_deg[0]
    WIDGET_CONTROL,lin_params[1],set_val=lin_yfit[0]
    WIDGET_CONTROL,lin_params[2],set_val=fw_lin[0]
    
    for i=0,2 do WIDGET_CONTROL,wlout_params[i],set_val=wl_out[i]
    
    ;WIDGET_CONTROL,sky_params,set_val=sky_deg


END




FUNCTION ifured_find_data, cub, type, d_dir,night, zip=zip
  ;Find all files of necessary type in current cube
  ;  also unzip if necessary
   
  if keyword_set(zip) and not file_test(slash(d_dir)+slash(night+cub),/dir)  then begin
    f=slash(d_dir)+night+cub+'.zip'
    if not file_test(f,/regular) then begin
      mes=dialog_message(['File not exists:',f])
      return,0
    endif
    IFURED_UNZIP_CUB, d_dir, night, cub
  endif
  
  pattern='*.f*ts'
  pat=slash(d_dir)+slash(night+cub)
  list=file_search(pat,pattern,count=count,/fold_case)
  if count eq 0 then begin
   mes=dialog_message(['Files  have not found!','Pattern:'+pat+pattern])
   return,0
  endif

  names=['']
  for i=0, count-1 do begin
   h=headfits(list[i])
   tt=strlowcase(strtrim(sxpar(h,'IMAGETYP'),2))
   mode=strlowcase(strtrim(sxpar(h,'mode'),2))
   objn=strlowcase(strcompress(sxpar(h,'OBJECT'),/remove))
   mask=strlowcase(strcompress(sxpar(h,'SLITMASK'),/remove))
   dispers=strlowcase(strcompress(sxpar(h,'DISPERSE'),/remove))
   if type eq 'eta' then begin
     if not ((tt eq 'eta' or (tt eq 'flat' and mask eq 'hmin') ) and mode eq 'ifu' and objn ne 'slitpos'  and dispers ne "hole") then continue ;and mask eq 'hmin'
   endif else begin 
     if not (tt eq type and mode eq 'ifu' and objn ne 'slitpos' and mask eq '' and dispers ne "hole") then continue
   endelse
   FDECOMP, list[i], disk, dir, name, qual
   name=strsplit(name,night+cub,/extract,/regex)
   names=[names, name]
  endfor
  
  if n_elements(names) eq 1 then begin
    mes=dialog_message(['Files type='+type+' have not found!','Pattern:'+pat+pattern])
     return,0
  endif
  names=names[1:*]
  return,strjoin(names,",")

END


FUNCTION IFURED_TEST_PAR, param, default=default
  ;Change blank parameter obtained from log-file to default one
  if param eq "" then param=default
  return, param
END

FUNCTION IFURED_FILENAME, night=night, cub=cub, file=file, ext=ext
  ;Return full path to the file 
  if not keyword_set(ext) then ext=".fts"
  FILE=strsplit(file,',',/extract)
  n_files=n_elements(file)
  path=night+cub+file+ext
return, path
end


Function IFURED_READ_LOG,FILE
  ;  READ ASCII-file with IFU readuction log
  ;  into string array
  tst=file_test(file)
  if tst eq 0 then begin
   res=dialog_message('LOG-file is not found: '+file)
   return,''
  endif
  s='' & par=''
  openr,u,file,/get_lun
   while not(eof(u)) do begin
    readf,u,s
    if strcompress(s,/remove) ne '' then par=[par,s]
   endwhile
  free_lun,u
  if n_elements(par) gt 1 then par=par[1:*]
  return,par
END



FUNCTION IFURED_PARSE_LOG, fileparam,nowl_upd=nowl_upd
  ; Parse log-file and extract all necessary parameters
  COMMON IFURED_PARAMS
  COMMON IFURED_WIDGET_ELEMENTS
  
  filepar=fileparam
  par=ifured_read_log(filepar)
  fdecomp,filepar,disk,file_dir,name_filepar,qual
  IF n_elements(par) le 1 then begin
    filepar=''
    return,0
  endif
  if not keyword_set(rawdata_dir) then begin
    cd,current=curdir
    rawdata_dir=slash(curdir)+slash(Raw)
  endif
  if not keyword_set(reddata_dir) then begin
    cd,current=curdir
    rawdata_dir=slash(curdir)
  endif
  
  
  ifu_dir=slash(ifured_test_par(sxpar(par,"PRO_DIR"), default=ifu_dir))
  
  w_dir=slash(ifured_test_par(sxpar(par,"W_DIR"), default=reddata_dir+slash(name_filepar)))
  d_dir=slash(ifured_test_par(sxpar(par,"R_DIR"), default=rawdata_dir))
  
  night=sxpar(par,"NIGHT")
  obj_cub=sxpar(par,"CUB_OBJ")
  if not keyword_set(obj_cub) then begin
    mes=dialog_message("'CUB_OBJ' has to be set!!!")
    return,0
  endif
  neon_cub=ifured_test_par(sxpar(par,"CUB_NEON"), default=obj_cub)
  flat_cub=ifured_test_par(sxpar(par,"CUB_FLAT"), default=obj_cub)
  eta_cub=ifured_test_par(sxpar(par,"CUB_ETA"), default=obj_cub)
  star_cub=sxpar(par,"CUB_STAR")
  sky_cub=sxpar(par,"CUB_SKY")
  if not keyword_set(star_cub) then begin
  ;  mes=dialog_message("'CUB_STAR' is not set! Continue without STAR reduction.")
    no_star=1
  endif else no_star=0
  if not keyword_set(sky_cub) then begin
   ; mes=dialog_message("'CUB_SKY' is not set! Continue without SKY reduction.")
    no_sky=1
  endif else no_sky=0
  obj_file=sxpar(par,"OBJ")
  neon_file=sxpar(par,"NEON")
  flat_file=sxpar(par,"FLAT")
  eta_file=sxpar(par,"ETA")
  sky_file=sxpar(par,"SKY")
  star_file=sxpar(par,"STAR")
  
  zip=sxpar(par,"ZIP")
  
;  if not keyword_set(obj_file) then obj_file=ifured_find_data(obj_cub,"obj",d_dir,night,zip=zip)
;  if not keyword_set(neon_file) then neon_file=ifured_find_data(neon_cub,"neon",d_dir,night,zip=zip)
;  if not keyword_set(flat_file) then flat_file=ifured_find_data(flat_cub,"flat",d_dir,night,zip=zip)
;  if not keyword_set(eta_file) then eta_file=ifured_find_data(eta_cub,"eta",d_dir,night,zip=zip)
;  if not (keyword_set(obj_file) and keyword_set(neon_file) and keyword_set(flat_file) and keyword_set(eta_file)) then begin
;    mes=dialog_message( "Incomplete set of files (OBJ: "+string(obj_file)+"; NEON: "+string(neon_file)+"; FLAT: "+string(flat_file)+"; ETA: "+string(eta_file)+$
;              "). Please, resolve this problem before continue!")
;    return,0
;  endif
;  if not keyword_set(star_file) and keyword_set(star_cub) then star_file=ifured_find_data(star_cub,"obj",d_dir,night,zip=zip)
;  if not keyword_set(sky_file) and keyword_set(sky_cub) then sky_file=ifured_find_data(sky_cub,"obj",d_dir,night,zip=zip)
  star_table=sxpar(par,"ST_TABLE")
  
  
  
  ;if not file_test(w_dir+name_filepar) then file_mkdir,w_dir+name_filepar
  w_dir=slash(w_dir+name_filepar)
  
  
  ;Get default values from widgets
  WIDGET_CONTROL, ovscan[0],get_val=x0
  WIDGET_CONTROL, ovscan[1],get_val=y0
  WIDGET_CONTROL, ovscan[2],get_val=x1
  WIDGET_CONTROL, ovscan[3],get_val=y1
  
  crop_def=strjoin([x0,y0,x1,y1],",")
  WIDGET_CONTROL, slit_bord[0],get_val=sbl_def
  WIDGET_CONTROL, slit_bord[1],get_val=sbr_def
  
  trdef=strarr(6)
  for i=0,5 do begin
    WIDGET_CONTROL, tr_cr_wid[i],get_val=tr
    trdef[i]=tr
  endfor
  trdef=strjoin(trdef,",")
  
  WIDGET_CONTROL, tra_params[0],get_val=def_tstep
  WIDGET_CONTROL, tra_params[1],get_val=def_twin
  
;  WIDGET_CONTROL, ndeg_geometry,get_val=Geometry_Lines_ndeg
;  WIDGET_CONTROL, win_geometry,get_val=Geometry_Lines_win

  WIDGET_CONTROL,lin_params[0],get_val=def_ldeg
  WIDGET_CONTROL,lin_params[1],get_val=def_lyfit
  WIDGET_CONTROL,lin_params[2],get_val=def_fw
  
  
  
  if not keyword_set(nowl_upd) then IFURED_UPDATE_WLOUT
  
  WIDGET_CONTROL,wlout_params[0],get_val=v0
  WIDGET_CONTROL,wlout_params[1],get_val=v1
  WIDGET_CONTROL,wlout_params[2],get_val=v2
  def_wlout=strjoin([v0,v1,v2],",")
  
  ;WIDGET_CONTROL,sky_params,get_val=def_skydeg
  def_skydeg=3
  
  slit_break_left=fix(ifured_test_par(sxpar(par,"SLIT_0"),default=sbl_def))
  slit_break_right=fix(ifured_test_par(sxpar(par,"SLIT_1"),default=sbr_def))
  tr_cr=(ifured_test_par(sxpar(par,"TRESH_CR"),default=trdef))
  tr_cr=fix(strsplit(tr_cr,',',/extract))
  tra_dspec=fix(ifured_test_par(sxpar(par,"TRA_STEP"),default=def_tstep))
  tra_win=fix(ifured_test_par(sxpar(par,"TRA_WIN"),default=def_twin))
  first_eta=fix(ifured_test_par(sxpar(par,"ETA_0"),default=0))
  last_eta=fix(ifured_test_par(sxpar(par,"ETA_1"),default=11))

  crop=ifured_test_par(sxpar(par,"CCD_CUT"),default=crop_def)
  wl_out_str=ifured_test_par(sxpar(par,"OUT_WAVE"),default=def_wlout)
  
  wl_out_str=strsplit(wl_out_str,',',/extract)
  wl_out=fltarr(3)
  wl_out[*]=float(wl_out_str[*])
  
  sky_deg=fix(ifured_test_par(sxpar(par,"SKY_DEG"),default=def_skydeg))
  
  lin_deg=replicate(fix(ifured_test_par(sxpar(par,"LIN_DEG"),default=def_ldeg)),2)
  fw_lin=replicate(fix(ifured_test_par(sxpar(par,"LINEFWHM"),default=def_fw)),2)
  lin_yfit=fix(ifured_test_par(sxpar(par,"LIN_YFIT"),default=def_lyfit))
  if lin_yfit ne 0 then lin_yfit=replicate(lin_yfit,2)
  
  lam_test=fix(ifured_test_par(sxpar(par,"LAM_TEST"),default='5500'))
  
  IFURED_SETFIELDS
  
  RETURN,1
END


Function IFURED_TEST_BINNING, obj,neon,flat,eta,star,sky, dir=dir, bin=bin
  ;Check whether all files have the same binning 
  ;      and return the value of bin
  
  files=[""]
  type=[""]
  if keyword_set(obj) then begin
    n_obj=n_elements(obj)
    files=[files,obj]
    type=[type,replicate("obj",n_obj)]
  endif else n_obj=0 
  if keyword_set(neon) then begin
    n_neon=n_elements(neon)
    files=[files,neon]
    type=[type,replicate("neon",n_neon)]
  endif else n_neon=0
  
  if keyword_set(flat) then begin
    n_flat=n_elements(flat)
    files=[files,flat]
    type=[type,replicate("flat",n_flat)]
  endif else n_flat=0
  if keyword_set(eta) then begin
    n_eta=n_elements(eta)
    files=[files,eta]
    type=[type,replicate("eta",n_eta)]
  endif else n_eta=0
  if keyword_set(star) then begin
    n_star=n_elements(star)
    files=[files,star]
    type=[type,replicate("star",n_star)]
  endif else n_star=0
  if keyword_set(sky) then begin
    n_sky=n_elements(sky)
    files=[files,sky]
    type=[type,replicate("sky",n_sky)]
  endif else n_sky=0
  
  n_files=n_obj+n_neon+n_flat+n_eta+n_star+n_sky
  if n_files eq 0 then return,0
  
  files=files[1:n_files]
  type=type[1:n_files]
  
  
  n_files_exist=0
  bin=['']
  notfiles=['']
  for i=0,n_files-1 do begin
    
    r=file_test(dir+files[i])
    if r eq 0 then begin
      notfiles=[notfiles,files[i]]
      continue
    endif else n_files_exist++
    bin=[bin,sxpar(headfits(dir+files[i]),"BINNING")]
  endfor
  if n_files_exist eq 0 then begin
    message,"None of the fits-files exist. Check R_DIR, NIGHT and CUBs parameters",/cont
    return,0
  endif
  if n_elements(notfiles) gt 1 then message,"Some fits-files do not exist: "+notfiles[1:n_elements(notfiles)-1]+". Check R_DIR, NIGHT and CUBs parameters",/cont
  bin=bin[1:n_files_exist]
  i=0
  weight_uniq=[0]
  uniq_bin=[""]
  while i lt n_files do begin  
    rec=where(uniq_bin eq bin[i],nr)
    if nr eq 0 then begin
      uniq_bin=[uniq_bin,bin[i]]
      weight_uniq=[weight_uniq,1]
    endif else weight_uniq[rec]+=1
    i+=1
  endwhile
  
  n_uniq=n_elements(uniq_bin)-1
  uniq_bin=uniq_bin[1:n_uniq]
  weight_uniq=weight_uniq[1:n_uniq]
  
  if n_uniq eq 1 then bin=uniq_bin[0] else begin
  
    s=sort(weight_uniq)
    uniq_bin=uniq_bin[s]
    
    
    types_bad=[""]
    files_bad=[""]
    for i=1,n_uniq-1 do begin
      rec=where(bin eq uniq_bin[i])
      files_bad=[files_bad,files[rec]]
    endfor
    n_bad=n_elements(files_bad)-1
    files_bad=files_bad[1:n_bad]
    r=dialog_message("There are some files with incorrect binning:" + files_bad)
    bin=uniq_bin[0]
    return,0
  endelse 
  return,1
END





FUNCTION IFURED_SEARCH_FIBPOS_FILE,logfield=logfield
  COMMON IFURED_PARAMS
    ;#### Search for fiber_pos.fts
    pattern='fiber_pos.fts'
    pat=slash(w_dir)
    list=file_search(pat,pattern,count=count,/fold_case)
    if count eq 0 then begin
      pat=slash(d_dir)
      list=file_search(pat,pattern,count=count,/fold_case)
      if count eq 0 then begin 
       mes=dialog_message(['Files fiber_pos.fts was not found in work_dir or data_dir!'])
       if keyword_set(logfield) then IFURED_LOG_UPD, ["Fiber_pos.fts is not found!"," "]
       return,0
     endif
     file_copy,list[0],w_dir,/over
    endif
    return,1      
END





FUNCTION IFURED_CHECK_INI_FILES,logfield=logfield
  COMMON IFURED_PARAMS
     ;#### Search for raw files
      if not keyword_set(obj_file) then obj_file=ifured_find_data(obj_cub,"obj",d_dir,night,zip=zip)
      if not keyword_set(neon_file) then neon_file=ifured_find_data(neon_cub,"neon",d_dir,night,zip=zip)
      if not keyword_set(flat_file) then flat_file=ifured_find_data(flat_cub,"flat",d_dir,night,zip=zip)
      if not keyword_set(eta_file) then eta_file=ifured_find_data(eta_cub,"eta",d_dir,night,zip=zip)
      if not keyword_set(star_file) and keyword_set(star_cub) then star_file=ifured_find_data(star_cub,"obj",d_dir,night,zip=zip)
      if not keyword_set(sky_file) and keyword_set(sky_cub) then sky_file=ifured_find_data(sky_cub,"obj",d_dir,night,zip=zip)
      
      if not (keyword_set(obj_file) and keyword_set(neon_file) and keyword_set(flat_file) and keyword_set(eta_file)) then begin
         mes=dialog_message( "Incomplete set of files (OBJ: "+string(obj_file)+"; NEON: "+string(neon_file)+"; FLAT: "+string(flat_file)+"; ETA: "+string(eta_file)+$
              "). Please, resolve this problem before continue!")
         if keyword_set(logfield) then IFURED_LOG_UPD, ["Cannot proceed: Incomplete set of files!"," "]
         return,0
      endif
                      
      tmp=obj_file
      names_obj=slash(night+obj_cub)+ifured_filename(night=night, cub=obj_cub, file=tmp, ext=".fts")
      tmp=neon_file
      names_neon=slash(night+neon_cub)+ifured_filename(night=night, cub=neon_cub, file=tmp, ext=".fts")
      tmp=flat_file
      names_flat=slash(night+flat_cub)+ifured_filename(night=night, cub=flat_cub, file=tmp, ext=".fts")
      tmp=eta_file
      names_eta=slash(night+eta_cub)+ifured_filename(night=night, cub=eta_cub, file=tmp, ext=".fts")
      if not keyword_set(no_star) then begin
        tmp=star_file
        names_star=slash(night+star_cub)+ifured_filename(night=night, cub=star_cub, file=tmp, ext=".fts")
      endif  else names_star=''
      if not keyword_set(no_sky) then begin
        tmp=sky_file
        names_sky=slash(night+sky_cub)+ifured_filename(night=night, cub=sky_cub, file=tmp, ext=".fts") 
      endif else names_sky=''
      response=IFURED_TEST_BINNING(names_obj,names_neon,names_flat,names_eta,names_star,names_sky, dir=d_dir, bin=bin)
      if response eq 0 then begin
        if keyword_set(logfield) then IFURED_LOG_UPD, ["Some files have incorrect binning. Please, resolve it!"," "]
        return,0
      endif
      
       if keyword_set(logfield) then begin
          add_log=["Data set for analysis:"]
          add_log=[add_log,"    Night: "+night,'    =============']
          add_log=[add_log,"    OBJECT: "]
          add_log=[add_log,"        cube: "+string(obj_cub,format='(I02)')]
          add_log=[add_log,"        files: "+obj_file,'    =============']
          add_log=[add_log,"    NEON: "]
          add_log=[add_log,"        cube: "+string(neon_cub,format='(I02)')]
          add_log=[add_log,"        files: "+neon_file,'    =============']
          add_log=[add_log,"    FLAT: "]
          add_log=[add_log,"        cube: "+string(flat_cub,format='(I02)')]
          add_log=[add_log,"        files: "+flat_file,'    =============']
          add_log=[add_log,"    ETA: "]
          add_log=[add_log,"        cube: "+string(eta_cub,format='(I02)')]
          add_log=[add_log,"        files: "+eta_file,'    =============']
          
          add_log=[add_log,"    SKY: "]
          if keyword_set(sky_file) then begin 
            add_log=[add_log,"        cube: "+string(sky_cub,format='(I02)')]
            add_log=[add_log,"        files: "+sky_file,'    =============']
          endif  else add_log=[add_log,"        not loaded",'    =============']
          
          add_log=[add_log,"    STAR: "]
          if keyword_set(star_file) then begin 
            add_log=[add_log,"        cube: "+string(star_cub,format='(I02)')]
            add_log=[add_log,"        files: "+star_file,'    =============']
          endif  else add_log=[add_log,"        not loaded",'    =============']
          
          add_log=[add_log,"    BINNING: "+bin,"#######################################",'']
          IFURED_LOG_UPD, add_log
      endif
      return,1      
END





;########### WORKING WITH DATA #########################

FUNCTION IFURED_overscan,im
  
  s=size(im)
  x=get_num(s[1],s[2],/x)
  y=get_num(s[1],s[2],/y)
  rec_x=where(x lt 10)
  rec_y=where(y lt 10)
  
  over=median([im[rec_x],im[rec_y]])

  return, over

END












;################ GEOMETRY TRANSFORMATION OF SPECTRA #################################


Function IFURED_PEAKS_COMPARE, peaks,master,plot=plot,max_shift=max_shift,show_correct=show_correct

  
  res=goodpoly(master,peaks,1,2,yfit)
  
  if keyword_set(plot) then begin
    cgdisplay
    cgplot, master,peaks,ps=2
    cgoplot, [master[0],master[n_elements(master)-1]],[yfit[0],yfit[n_elements(master)-1]]
  endif
  
  if not keyword_set(show_correct) then begin
    rec=where(abs(peaks-yfit) gt max_shift,nrec)
    if nrec gt 0 then res=0
  ENDIF ELSE BEGIN
    rec=where(abs(peaks-yfit) le max_shift,nrec)
    if nrec ne 0 then res=peaks[rec]
  ENDELSE
  return,res
END


FUNCTION IFURED_FIND_PEAKS, vector, win=win,plot=plot,robo=robo,npeaks=npeaks, find=find
    if not keyword_set(npeaks) then npeaks=14
    if not keyword_set(win) then win=5
    s=size(vector)
    ny=s[1]
    if s[0] eq 2 THEN BEGIN
      vector_ini=vector
      vector=fltarr(ny)
      vector[*]=vector_ini[*,0]
    endif
    
    FOR k=0, s[0]-1 DO BEGIN
      IF k eq 1 THEN vector[*]=vector_ini[*,k]
      drv=deriv(vector)
      pks=[0.]
      IF KEYWORD_SET (ROBO) THEN BEGIN
        
        robomean,vector[where(finite(vector))],1.,.5,avg, avgdev, stddev, var, skew, kurt, nfinal,new   
        rec=where(vector gt avg)   
        robomean,vector[rec[where(finite(vector[rec]))]],10.,.5,avg, avgdev, stddev, var, skew, kurt, nfinal,new      
      ENDIF ELSE avg=min(vector)
      
      FOR i=0,ny-1 do begin
        
        win_arr=indgen(win)
        lbord = min((i-win_arr[*]) > 0)
        rbord = max((i+win_arr[*]) < (ny-1))
        
        if drv[i] le 0 then begin
          sm=0
          for j=lbord,rbord do begin
            if j lt i then sm+=round(drv[j]/abs(drv[j]))
            if j gt i then sm-=round(drv[j]/abs(drv[j]))
          endfor
          if sm eq rbord-lbord and vector[i] gt avg then pks=[pks,i]
        endif 
      ENDFOR 
      
      npks=n_elements(pks)-1
      if npks gt 0 then pks=pks[1:npks]
      
      ; ### Correction of peaks position
      if npks eq npeaks or keyword_set(find) then begin
      
        for i=0,npks-1 do begin
          rec=indgen(2*win+1)+pks[i]-win
          rr=where(rec gt 0 and rec lt ny-1,nrr)
          
          if nrr eq 0 then continue else rec=rec[rr]
          vbin=congrid(vector[rec],nrr*30.,/interp,/cent)
          recbin=congrid(rec,nrr*30.,/interp,/cent)
          c=poly_fit(recbin,vbin,2)
          mpos=-c[1]/2./c[2]
          pks[i]=mpos
        endfor
      endif
      
      pks_out=pks
      
      IF KEYWORD_SET(plot) THEN BEGIN          
        cgplot,vector,xs=1
        cgoplot,pks,vector[pks],psym=2
        wait,0.1
      ENDIF
    ENDFOR
    return,pks_out
END




function IFURED_cross_vector,ima,x,wx=wx,nosmooth=nosmooth

        nx=n_elements(x)
        if nx eq 0 then x=500
        if nx gt 2 then begin
          x=x[0:1]
          nx=2
        endif
        IF n_elements(wx) eq 0 THEN begin
          wx=intarr(nx)
          wx[*]=50
        ENDIF

        s=size(ima)

        vector=fltarr(s[2],nx)
        for j=0,nx-1 do vector[*,j]=total(ima[x[j]-wx[j]:x[j]+wx[j],*],1,/nan)/(2*wx[j]+1)

        y=findgen(s[2])

        IF NOT KEYWORD_SET(nosmooth) THEN BEGIN
          for i=0,nx-1 do begin
            lowess,y,vector[*,i],s[2]/10,v1,order=2
            vector[*,i]-=v1[*]
          endfor
        ENDIF

        if nx eq 1 then begin
          v_out=fltarr(s[2])
          v_out[*]=vector[*,0]
        endif else v_out=vector
        return,v_out
end


function ifured_intersection,lines,traectory,W,x=x,y=y
ext=0
Nx=N_elements(traectory)
Ny=N_elements(lines)
if not keyword_set(x) then begin 
  x0=0
  x=findgen(Nx)
endif  else x0=x[0]
if not keyword_set(y) then begin
  y0=0
  y=findgen(Ny)
endif else y0=y[0]
x_use=x-x0
y_use=y-y0




yc=total(traectory)/Nx-y0
yst=(yc-w+ext)>y_use[0];0
yfin=(yc+w+ext)<y_use[ny-1]
;ywid=yfin-yst+1

rec_y=where(y_use ge yst and y_use le yfin, ywid)
if ywid lt 1 then return, -1
xc=total(lines[rec_y])/ywid-x0
xst=(xc-w+ext)>x_use[0]
xfin=(xc+w+ext)<x_use[nx-1]
rec_x=where(x_use ge xst and x_use le xfin, xwid)

;if xwid eq 0 then stop
if xwid lt 5 then return, -1

T=robust_poly_fit(x[rec_x],traectory[rec_x],1,fit,3)
L=robust_poly_fit(lines[rec_y],y[rec_y],1,fit,3)


xc=-(L[0]-T[0])/(L[1]-T[1])
yc=T[0]+T[1]*xc

return,[xc,yc]
end



FUNCTION ifured_find_repers, lines, tra, w=w,x=x,y=y
   if not keyword_set(w) then w=5
  
   s=size(lines)
   n_lines=s[2]
   s=size(tra)
   n_tra=s[2]
   
   nx=n_elements(x)
   ny=n_elements(y)
   
   x_in=fltarr(n_lines,n_tra)-32000
   x_out=fltarr(n_lines,n_tra)
   y_in=fltarr(n_lines,n_tra)-32000
   y_out=fltarr(n_lines,n_tra)
   
   cent_track=poly(x,tra[*,(n_tra-2)/2])
   
   for i=0,n_lines-1 do begin
      lines_x=double(poly(double(y),lines[*,i]))
      
      if stddev(lines_x) eq 0 then lines_x=lines_x+randomn(seed,ny)*0.03
      for j=0,n_tra-1 do begin
       tra_y=poly(x,tra[*,j])
       pos=ifured_intersection(lines_x,tra_y,w,x=x,y=y)
       if n_elements(pos) eq 1 then pos=[x[0]-100,y[0]-100]
       if pos[0] gt x[0] and pos[0] lt x[nx-1] and pos[1] gt y[0] and pos[1] lt y[ny-1] then begin
        x_in[i,j]=pos[0]
        y_in[i,j]=pos[1]
       endif
        y_out[i,j]=total(tra_y)/n_elements(tra_y)
      endfor
      pos_out=ifured_intersection(lines_x,cent_track,w,x=x,y=y)
      x_out[i,*]=pos_out[0];total(lines_x)/n_elements(lines_x);pos_out[0]

   endfor
;   print,x_out[*,0]
;   stop
   ;rec=where(x_in gt 0 and y_in gt 0 and x_out gt 0 and y_out gt 0, nrec)
   res=fltarr(4,n_lines,n_tra);nrec)
   
   res[0,*,*]=x_in[*,*];[rec]
   res[1,*,*]=y_in[*,*];[rec]
   res[2,*,*]=x_out[*,*];[rec]
   res[3,*,*]=y_out[*,*];[rec]

    
   return,res
end


PRO IFURED_ADJUST_WARP_ETAPOS, w_dir=w_dir, flatname=flatname, posname=posname
  
  flat=readfits(w_dir+flatname,/sil)
  etapos=read_asc(w_dir+posname)
  etapos_ini=etapos
  IFURED_CHECK_MASTER,type="eta",mode="get_index",left=bad_fibers_lt,right=bad_fibers_rt
  
  s=size(flat)
  current_pos=[s[1]/2.,s[1]/2.]
  xx=findgen(s[1])
  win=300
  
  ;#### Write corrected eta-fibers positions
    openw,u,w_dir+posname,/get_lun
    
  FOR slit=0,1 DO BEGIN
    if slit eq 0 then bad_fibers=bad_fibers_lt else bad_fibers=bad_fibers_rt
    rec_slit=where(etapos[1,*] eq slit,nfib)
    r=where(bad_fibers gt -1,nbad)
    vector=reform(IFURED_CROSS_VECTOR(reform(flat[*,*,slit]),current_pos[slit],wx=win,/nosmooth))
    
    for j=0,nfib-1 do begin
        r=where(bad_fibers eq j,flag)
        if flag ne 0 then continue
        
        rec=where(xx ge etapos[0,rec_slit[j]]-3.5 and xx le etapos[0,rec_slit[j]]+3.5,nr)
        
        if nr eq 0 then continue
        
          
        sl=congrid(xx[rec],nr*100,/interp,/cent)
        nv=congrid(vector[rec],nr*100,/interp,/cent)
        
        rec1=where(sl ge etapos[0,rec_slit[j]]-1.5 and sl le etapos[0,rec_slit[j]]+1.5)
        
        res=poly_fit(sl[rec1],nv[rec1],2)
                       
        if abs(-res[1]/2./res[2]-etapos[0,rec_slit[j]])/6.78 lt 0.2 then etapos[0,rec_slit[j]]=-res[1]/2./res[2]
    endfor
    
    IF nbad ne 0 then begin
      FOR k=0,nbad-1 do begin
        if bad_fibers[k] eq nfib-1 then etapos[0,bad_fibers[k]]+=etapos[0,bad_fibers[k]-1]-etapos_ini[0,bad_fibers[k]-1]
        if bad_fibers[k] eq 0 then etapos[0,bad_fibers[k]]+=etapos[0,bad_fibers[k]+1]-etapos_ini[0,bad_fibers[k]+1]
        if bad_fibers[k] gt 0 and bad_fibers[k] lt (nfib-1) then $
            etapos[0,bad_fibers[k]]+=(etapos[0,bad_fibers[k]-1]-etapos_ini[0,bad_fibers[k]-1]+etapos[0,bad_fibers[k]+1]-etapos_ini[0,bad_fibers[k]+1])/2.
      endfor
    endif
    for j=0,nfib-1 do printf,u,etapos[0,rec_slit[j]],slit, format="(F9.4, 2X, I1)"
  ENDFOR
   
    close,u
    free_lun,u    
  

END

FUNCTION IFURED_FIND_FIBERS,flat,eta_fibers,master_eta,master_fib;,fiberpos_obs=fiberpos_obs
  ;Search all fibers on the warped flat.
  
  
  s1=size(master_eta)
  s2=size(master_fib)
  
  nfib=s1[1]+s2[1]
  
  fibers=fltarr(3,nfib*2)
  s=size(flat)
  nx=s[1]
  
  FOR slit=0,1 DO BEGIN
    rec_eta=where(eta_fibers[1,*] eq slit, neta)
    fibers[0,slit*nfib:slit*nfib+neta-1]=eta_fibers[0,rec_eta]
    fibers[1,slit*nfib:slit*nfib+neta-1]=1
    fibers[2,slit*nfib:slit*nfib+neta-1]=slit
    
    pks_eta=reform(eta_fibers[0,rec_eta])
    
    if slit eq 0 then cur_pos=4*nx/5 else cur_pos=nx/5
      IFURED_PEAKS_FIBERS,reform(flat[*,*,slit]), reform(master_eta[*,slit]),reform(master_fib[*,slit]), pks_eta, $
                                 current_pos=cur_pos, current_win=50, peaks_out=pks_fib, fiber_num=nm_fib;,peaks_obs=reverse(reform(fiberpos_obs[*,slit]))
    fibers[0,slit*nfib+neta:slit*nfib+nfib-1]=pks_fib[*]
    fibers[1,slit*nfib+neta:slit*nfib+nfib-1]=0
    fibers[2,slit*nfib+neta:slit*nfib+nfib-1]=slit       
    
  ENDFOR
  
  

  Return, fibers
END

PRO IFURED_READ_MASTER, master_eta, master_fib, get_xshift=get_xshift
  COMMON IFURED_PARAMS
  ;#### Read master file
  if not keyword_set(get_xshift) then get_xshift=0
  file_master=ifu_dir+"master_pos.txt"
  
  total_Neta=12 ; Number of spectra in file_eta
  total_NFib=242; Number of spectra from science fibers in file_flat
  
  
  master_eta=fltarr(2+get_xshift,total_Neta,2)
  master_fib=fltarr(2+get_xshift,total_Nfib,2)
  
  f=fltarr(7,total_Neta+total_NFib)
  openr,u,file_master,/get_lun
     readf,u,f
  close,u
  free_lun,u

  ; Read eta-fibers positions
  R=where(f[0,*] lt total_Neta+1) & R=reverse(R)
  master_eta[0,*,0]=f[4,R] & master_eta[0,*,1]=f[1,R]
  master_eta[1,*,0]=f[6,R] & master_eta[1,*,1]=f[3,R]
  if get_xshift then begin
    master_eta[2,*,0]=f[5,R] & master_eta[2,*,1]=f[2,R]
  endif
   
  ; Read science-fibers positions
  R=where(f[0,*] gt total_Neta+1,ind) & R=reverse(R)
  master_fib[0,*,0]=f[4,R] & master_fib[0,*,1]=f[1,R]
  master_fib[1,*,0]=f[6,R] & master_fib[1,*,1]=f[3,R]
  if get_xshift then begin
    master_fib[2,*,0]=f[5,R] & master_fib[2,*,1]=f[2,R]
  endif
  
  return
END


FUNCTION IFURED_REMOVE_BGR, image
  
  bgr=estimate_background(image,150)
  bgr+=estimate_background(image-bgr,50)
  bgr+=estimate_background(image-bgr,20)
  ;writefits,"/Volumes/iData/Science/IFU/Reduction/mkn78_nov17_v940@600/masked_ini/bgr.fits",bgr
  image_corr=image-bgr

RETURN, image_corr
END


pro ifured_change_fiber, arr, slit, rec_all, rec_bad, x, y, y1, y2
   ;## вспомогательная функция для ifured_check_master
   if x ne -1 then begin
     vec=reform(arr[x,rec_all,slit])
     vec1=vecshift(vec,dx=y-y1)
     vec2=vecshift(vec,dx=y-y2)
     arr[x,rec_all[rec_bad],slit]=(vec1[rec_bad]+vec2[rec_bad])/2.
   endif else begin
    ; perform 2D shift
    im=reform(arr[*,rec_all,slit])
    im1=shift_image(im,0,y-y1)
    im2=shift_image(im,0,y-y2)
    arr[*,rec_all[rec_bad],slit]=(im1[*,rec_bad]+im2[*,rec_bad])/2.
   endelse 
   return 
end






PRO ifured_check_master, type=type, mode=mode, left=left, right=right
  
  ;*** Check master_pos.txt for the flags of bad fibers
  
  COMMON IFURED_PARAMS
  COMMON IFURED_WIDGET_ELEMENTS
  
  if not keyword_set(mode) then mode="change"
  npeaks_eta=12
  IFURED_READ_MASTER,master_eta, master_fib
  s=size(master_eta)
  neta=s[1]
    IF mode eq "get_index" and TYPE eq "eta" then begin
      left=where(master_eta[1,*,0] eq -1)
      right=where(master_eta[1,*,1] eq -1)
      return
    ENDIF
    
    IF TYPE eq "eta" and mode eq "change" THEN BEGIN
          ;CHANGE ETA BAD FIBERS
      ; ### Borders for processing (relative from 0 and nx (ny) )
        x0_cut=100
        x1_cut=-50
        y0_cut=0
        y1_cut=0
      
      file_eta="eta_i.fts"
      
      
      WIDGET_CONTROL, tra_params[0],get_val=dspec
      WIDGET_CONTROL, tra_params[1],get_val=ncolumns
      
      if not keyword_set(dspec) then dspec=70 else dspec=fix(dspec[0])   ; step along dispersion axis
      if not keyword_set(ncolumns) then ncolumns=10 else ncolumns=fix(ncolumns[0]) ; window along dispersion axis for vector extraction
         
      eta=readfits(w_dir+file_eta,h_eta,/sil)
      
      s=size(eta)
      nx=s[1]
      ny=s[2]
      spec0=x0_cut
      nspec=floor((nx-1+x1_cut-x0_cut)/dspec)
      xsc=spec0+findgen(nspec)*dspec
      cross_scale=findgen(ny)
      xscale=findgen(nx)
      
      change=0
      FOR slit=0,1 do begin  
          rec_bad=where(master_eta[1,*,slit] eq -1, nr_bad)
          if nr_bad eq 0 then continue
          rec_good=where(master_eta[1,*,slit] eq 0, nr_good)
          change=1
          
          neta=npeaks_eta-nr_bad
          
          slit_break=max(where(finite(reform(eta[*,ny/2,slit]))))
          npks_eta=intarr(nspec)
          badpeaks_eta=fltarr(nspec,nr_bad)
          goodpeaks_eta=fltarr(nspec,2,nr_bad)
          for i=0,nspec-1 do begin
                 if xsc[i] gt slit_break then break
                  nosmooth=1
                  for j=0,1 do begin
                    ; ##### Extraction of the vector along cross-dispersion with window along dispersion = wx ###
                    vector=IFURED_CROSS_VECTOR(eta[*,*,slit],spec0+i*dspec,wx=ncolumns,nosmooth=nosmooth)
                    ;vector=(vector-0.2*(max(vector)-min(vector))) > 0
                    ; ##### Trying to find NPEAKS peaks on vector ###
                    pks_eta=IFURED_FIND_PEAKS(vector,win=6, npeaks=neta,/robo,/find)
                    npks_eta[i]=n_elements(pks_eta)
                    if j eq 0 and npks_eta[i] ne neta then nosmooth=0 else break
                  endfor
                  
                  if npks_eta[i] eq neta then begin
                      ;vec_flat=IFURED_CROSS_VECTOR(flat[*,*,slit],spec0+i*dspec,wx=ncolumns,nosmooth=nosmooth)
                      
                      ; Get bad fibers on eta and on flat
                      res=goodpoly(master_eta[0,rec_good,slit],pks_eta,5,3,fit)
                      badpeaks_eta[i,*]=poly(master_eta[0,rec_bad,slit],res)
                      for zz=0,nr_bad-1 do begin
                        if rec_bad[zz] ne 0 then goodpeaks_eta[i,0,zz]=pks_eta[rec_bad[zz]-1-zz] else goodpeaks_eta[i,0,zz]=pks_eta[rec_bad[zz]-zz] 
                        if rec_bad[zz] ne neta+nr_bad then goodpeaks_eta[i,1,zz]=pks_eta[rec_bad[zz]-zz] else goodpeaks_eta[i,1,zz]=pks_eta[rec_bad[zz]-zz-1]
                      endfor
                  endif    
          endfor            
          
          rec=where(npks_eta eq neta,nr)
          win_eta=15.
    ;      win_flat=5.
          For zz=0,nr_bad-1 do begin
            res_bad_eta=robust_poly_fit(xsc[rec],badpeaks_eta[rec,zz],2,yfit,sig)
            res_good1_eta=robust_poly_fit(xsc[rec],goodpeaks_eta[rec,0,zz],2,yfit,sig)
            res_good2_eta=robust_poly_fit(xsc[rec],goodpeaks_eta[rec,1,zz],2,yfit,sig)
            
            for x=0,nx-1 do begin
              y=poly(x,res_bad_eta)
              y1=poly(x,res_good1_eta)
              y2=poly(x,res_good2_eta)
              
              rec=where(cross_scale ge (min([y1,y])-2*win_eta) and cross_scale le (max([y2,y])+2*win_eta))
              rec_eta=where(cross_scale[rec] ge (y-win_eta) and cross_scale[rec] le (y+win_eta))  
    ;          rec_flat=where(cross_scale[rec] ge (y-win_flat) and cross_scale[rec] le (y+win_flat))  
              
              ifured_change_fiber, eta, slit, rec, rec_eta, x, y, y1, y2
            endfor
          endfor  
      ENDFOR
      
      if change eq 0 then return
      
      ;file_copy,w_dir+file_eta,w_dir+"_eta_i.fts",/over
      writefits,w_dir+file_eta,eta,h_eta

  
  ENDIF
  IF TYPE EQ "other" and mode eq "change" THEN BEGIN
    ;**** CHANGE BAD FIBERS IN ALL FRAMES EXCEPT ETA
    file_obj="obj_warp.fts"
    file_flat="flat_warp.fts"
    file_neon="neon_warp.fts"
    file_star="star_warp.fts"
    file_sky="sky_warp.fts"  
    
     win_flat=6.
    
    fibpos=read_asc(w_dir+"eta_pos_warp.fit")
    
    flat=readfits(w_dir+file_flat,h_flat,/sil)
    obj=readfits(w_dir+file_obj,h_obj,/sil)
    neon=readfits(w_dir+file_neon,h_neon,/sil)
    if not keyword_set(no_sky) then sky=readfits(w_dir+file_sky,h_sky,/sil)
    if not keyword_set(no_star) then star=readfits(w_dir+file_star,h_star,/sil)
    
    s=size(flat)
    nx=s[1]
    ny=s[2]
    cross_scale=findgen(ny)
    xscale=findgen(nx)
      
    change=0
    for slit=0,1 do begin
        rec_bad=where(master_eta[1,*,slit] eq -1, nr_bad)
        if nr_bad eq 0 then continue
        rec_good=where(master_eta[1,*,slit] eq 0, nr_good)
        change=1
        
        rec_pos=where(fibpos[1,*] eq slit)
        
        For zz=0,nr_bad-1 do begin
          y=fibpos[0,rec_pos[rec_bad[zz]]]
          if rec_bad[zz] ne 0 then y1=fibpos[0,rec_pos[rec_bad[zz]-1]] else y1=fibpos[0,rec_pos[rec_bad[zz]+1]] 
          if rec_bad[zz] ne neta-1 then y2=fibpos[0,rec_pos[rec_bad[zz]+1]] else y2=fibpos[0,rec_pos[rec_bad[zz]-1]]
          y=y[0]
          y1=y1[0]
          y2=y2[0]    
          rec=where(cross_scale ge (min([y1,y])-2*win_flat) and cross_scale le (max([y2,y])+2*win_flat))
          rec_flat=where(cross_scale[rec] ge (y-win_flat) and cross_scale[rec] le (y+win_flat))  
          
          
          ifured_change_fiber, flat, slit, rec, rec_flat, -1, y, y1, y2
          ifured_change_fiber, neon, slit, rec, rec_flat, -1, y, y1, y2
          ifured_change_fiber, obj, slit, rec, rec_flat, -1, y, y1, y2
          if not keyword_set(no_star) then ifured_change_fiber, star, slit, rec, rec_flat, -1, y, y1, y2
          if not keyword_set(no_sky) then ifured_change_fiber, sky, slit, rec, rec_flat, -1, y, y1, y2
       endfor
    endfor
    if change eq 0 then return
    ;file_copy,w_dir+file_flat,w_dir+"_flat_warp.fts",/over
    ;file_copy,w_dir+file_obj,w_dir+"_obj_warp.fts",/over
    ;file_copy,w_dir+file_neon,w_dir+"_neon_warp.fts",/over
    ;if not keyword_set(no_star) then file_copy,w_dir+file_star,w_dir+"_star_warp.fts",/over
    ;if not keyword_set(no_sky) then file_copy,w_dir+file_sky,w_dir+"_sky_warp.fts",/over
    writefits,w_dir+file_flat,flat,h_flat
    writefits,w_dir+file_neon,neon,h_neon
    writefits,w_dir+file_obj,obj,h_obj
    if not keyword_set(no_star) then writefits,w_dir+file_star,star,h_star
    if not keyword_set(no_sky) then writefits,w_dir+file_sky,sky,h_sky
    
  ENDIF
END



PRO IFURED_CUTNANS, w_dir, filelist

  nfiles=n_elements(filelist)
  
  
  
  FOR nf=0,nfiles-1 do begin
    im=readfits(w_dir+filelist[nf],/sil)
    s=size(im)
    if nf eq 0 then xborder=[0,s[1]-1,0,s[1]-1]
    for slit=0,1 do begin
      nr=0
      i=xborder[slit*2]
      while nr eq 0 do begin
        rec=where(finite(reform(im[i,*,slit])),nr)
        i+=1
      endwhile
      if xborder[slit*2] lt (i-1) then xborder[slit*2]=i-1
      nr=0
      i=xborder[slit*2+1]
      while nr eq 0 do begin
        rec=where(finite(reform(im[i,*,slit])),nr)
        i-=1
      endwhile
      if xborder[slit*2+1] gt (i+1) then xborder[slit*2+1]=i+1
    endfor
  ENDFOR
  
  mn=min([xborder[0],xborder[2]])
  mx=max([xborder[1],xborder[3]])
  
  if mn gt 0 or mx lt s[2]-1 then begin
    FOR nf=0,nfiles-1 do begin
      im=readfits(w_dir+filelist[nf],h,/sil)
      
      ss=size(im)
      lt_part=reform(im[xborder[0]:xborder[1],*,0])
      rt_part=reform(im[xborder[2]:xborder[3],*,1])
      
      slt=size(lt_part)
      srt=size(rt_part)
      nx_lt=slt[1]
      nx_rt=srt[1]
      
      nx=max([nx_lt,nx_rt])
      
      im=fltarr(nx,ss[2],2)
      im[*]=!VALUES.F_NAN
      im[0:nx_lt-1,*,0]=lt_part[*,*]
      im[0:nx_rt-1,*,1]=rt_part[*,*]
      
      writefits,w_dir+filelist[nf],im,h
    ENDFOR
  endif

END




;############# Working with WIDGETS ###################

PRO IFURED_UPDATE_WLOUT
  COMMON IFURED_PARAMS
  COMMON IFURED_WIDGET_ELEMENTS

  if file_test(w_dir+'neon_i.fts') eq 0 then return
  h=headfits(w_dir+'neon_i.fts')
  dispname=sxpar(h,'disperse')
  s=strsplit(dispname,' ',/extr)
  dispname=strupcase(s[0])
  filedis=ifu_dir+slash("Grisms")+dispname+'.txt'
  disperser=read_asc(filedis,1)
  recommend_wlout=disperser[*,0]
  WIDGET_CONTROL,wlout_params[0],set_val=string(recommend_wlout[0],format="(I0)")
  WIDGET_CONTROL,wlout_params[2],set_val=string(recommend_wlout[2],format="(F0.1)")
  WIDGET_CONTROL,wlout_params[1],set_val=string(recommend_wlout[1],format="(I0)")
END