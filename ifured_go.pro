PRO IFURED_LOG_UPD, add_text,new=new
   COMMON IFURED_WIDGET_ELEMENTS
   WIDGET_CONTROL,log_field,get_val=cur_text
   if not keyword_set(new) then new_text=[cur_text,add_text] else new_text=[add_text]
   WIDGET_CONTROL,log_field,set_val=new_text
END                 





PRO IFURED_GO_EVENT, event

  COMMON IFURED_PARAMS
  COMMON IFURED_WIDGET_ELEMENTS
  COMMON IFURED_REDUCTION_PROCESS
  ;COMMON IFURED_DISPLAY
  WIDGET_CONTROL,event.ID,get_uvalue=ev

  CASE ev OF
  
                    
    ;'wlout_auto_upd': auto_wlout=1-auto_wlout
     
  
    "load_log": BEGIN
        if keyword_set(log_dir) then cd,log_dir  
        filepar=DIALOG_PICKFILE(title='Load parameters',filter=['*.txt', '*'],get_path=par_dir)
        inf=FILE_INFO(filepar)
       
        IF NOT (inf.exists) OR (filepar EQ '')  THEN filepar='' ELSE BEGIN
            if not keyword_set(log_dir) then begin
              log_dir=par_dir
              cd,log_dir
            endif
            fdecomp,filepar,disk,file_dir,file_name,qual
            file_name+='.'+qual
            IF (file_name eq '.') then break
            WIDGET_CONTROL, status_field, set_value="STATE:  Checking log-file"
            response=ifured_parse_log(filepar)
            if response eq 1 then begin
              widget_control,ifu_log_text,set_value=filepar
              
              
              add_log=['LOG OF DATA REDUCTION:','**********************','',systime()+":","######## New log file loaded. ########"," "]
              
              
              WIDGET_CONTROL, status_field, set_value="STATE:  Ready!"
            endif else begin
              add_log=['LOG OF DATA REDUCTION:','**********************']
              filepar=''
              WIDGET_CONTROL, status_field, set_value="STATE:  Please, load log-file!"
            endelse
            IFURED_LOG_UPD, add_log,/new
        ENDELSE
    END
    
    "set_log": BEGIN
        
        fprev=filepar
        widget_control,ifu_log_text,get_value=filepar
        
        inf=FILE_INFO(filepar)
        IF NOT (inf.exists) OR (filepar EQ '')  THEN filepar='' else begin
          if filepar ne fprev then begin
            fdecomp,filepar,disk,file_dir,file_name,qual
            file_name+='.'+qual
            IF (file_name eq '.') then break
            WIDGET_CONTROL, status_field, set_value="STATE:  Checking log-file"
            response=ifured_parse_log(filepar)
            if response eq 1 then begin
              
             add_log=['LOG OF DATA REDUCTION:','**********************','',systime()+":","######## New log file loaded. ########"," "]
              
              
              WIDGET_CONTROL, status_field, set_value="STATE:  Ready!"
            endif else begin
              add_log=['LOG OF DATA REDUCTION:','**********************']
              filepar=''
              WIDGET_CONTROL, status_field, set_value="STATE:  Please, load log-file!"
            endelse
            IFURED_LOG_UPD, add_log,/new
          endif
        endelse
     END
    
    "edit_log": if filepar ne '' then begin
      XDISPLAYFILE,filepar,TITLE='Log file',/editable,done_button="Done", group=ifu_mb,/block
      WIDGET_CONTROL, status_field, set_value="STATE:  Checking log-file"
      response=ifured_parse_log(filepar,/nowl_upd)
              IFURED_LOG_UPD, [systime()+':','Log file updated','']
         WIDGET_CONTROL, status_field, set_value="STATE:  Ready!"
     end
    
    
  
  
    "quit": BEGIN
      IFURED_CLEAN_DIRS
      WIDGET_CONTROL,/DESTROY,event.top
    END
    
    'view_file': BEGIN
      if keyword_set(w_dir) and file_test(w_dir) then cd,w_dir
      fileview=DIALOG_PICKFILE(title='Load parameters',filter=['*.f*ts'])
        inf=FILE_INFO(fileview)
       
        IF NOT (inf.exists) OR (fileview EQ '')  THEN fileview='' ELSE BEGIN
            fdecomp,fileview,disk,file_dir,file_name,qual
            file_name+='.'+qual
            IF (file_name eq '.') then break
            
            hview=headfits(fileview)
            nz=sxpar(hview,"NAXIS3")
            
            ;*** If it is cube => show with cviz
            if nz gt 2 then begin
              IFURED_cviz
              IFURED_cviz_loadfile,file=fileview
            endif
            
            ;*** If it 3D file with 2 2D spectra of left and right slits or just 2d file =>show in view_desktop
            if nz le 2 then IFURED_viewspec_load,fileview
            
            
        
        ENDELSE
    
    END
    
    'show_exract': spec_extra_show=1-spec_extra_show
    'auto_neon_search': search_geometry=1-search_geometry
    'optimal_extr': optimal=1-optimal
    
    'cub_flat_norm': norm_cub_flat=1-norm_cub_flat
    
    'cub_sky_norm': norm_cub_sky=1-norm_cub_sky
    
    "run": BEGIN
      rec_red_steps=where(markers[*].set eq 1, n_red_steps)
      IF n_red_steps eq 0 or filepar eq '' then RETURN
      show_all=markers[rec_red_steps].showres
      if markers[1].set eq 1 then nowl_upd=0 else nowl_upd=1 
      response=ifured_parse_log(filepar,nowl_upd=nowl_upd)
      FOR cstep_ind=0,n_red_steps-1 DO BEGIN
          cur_step=reduct_steps[rec_red_steps[cstep_ind]]
          show=show_all[cstep_ind]
              CASE cur_step OF
              
                  "meanbias": BEGIN
                    ; (1) - Create meanbias
                    step=1
                    WIDGET_CONTROL,status_field,set_val="STATE:  Processing (Step # "+string(step,format="(I0)")+")"
                    
                    if not file_test(w_dir) then file_mkdir,w_dir
                    
                    IFURED_LOG_UPD, [systime()+":","##### Meanbias creation #####"]
                    
                    IFURED_CRE_BIAS,w_dir=w_dir,dir=d_dir, binning=bin,/logfield,zip=zip
                    

                  END
                  
                  
                  "cre_ini": BEGIN
                    ; (2) - create initial cubes subtracting bias and applying la_cosmic for CR hits removal 
                    step=2
                    WIDGET_CONTROL,status_field,set_val="STATE:  Processing (Step # "+string(step,format="(I0)")+")"
                    
                    IFURED_LOG_UPD, [systime()+":","##### Create initial frames #####"]
                    
                    response=IFURED_CHECK_INI_FILES(logfield=1)
                    if response eq 1 then begin
                      WIDGET_CONTROL, ovscan[0],get_val=x0
                      WIDGET_CONTROL, ovscan[1],get_val=y0
                      WIDGET_CONTROL, ovscan[2],get_val=x1
                      WIDGET_CONTROL, ovscan[3],get_val=y1
                      crop=[fix(x0[0]),fix(y0[0]),fix(x1[0]),fix(y1[0])]
                      if x1 eq 0 then xlast="end" else xlast=string(x1,format="(I0)")
                      if y1 eq 0 then ylast="end" else ylast=string(y1,format="(I0)")
                      IFURED_LOG_UPD,["Cut all frames: X = "+string(x0,format="(I0)")+" to "+xlast+"; Y = "+string(y0,format="(I0)")+" to "+ylast]
                      
                      IFURED_CRE_INI_FILES,"obj",d_dir=d_dir,night=night,cub=obj_cub,file_in=obj_file,$
                        w_dir=w_dir,crop=crop,zip=zip,/logfield,use_lacosmic=0
                      IFURED_CRE_INI_FILES,"flat",d_dir=d_dir,night=night,cub=flat_cub,file_in=flat_file,$
                        w_dir=w_dir,crop=crop,zip=zip,/logfield,use_lacosmic=0
                      IFURED_CRE_INI_FILES,"neon",d_dir=d_dir,night=night,cub=neon_cub,file_in=neon_file,$
                        w_dir=w_dir,crop=crop,zip=zip,/logfield,use_lacosmic=0
                      IFURED_CRE_INI_FILES,"eta",d_dir=d_dir,night=night,cub=eta_cub,file_in=eta_file,$
                        w_dir=w_dir,crop=crop,zip=zip,/logfield,use_lacosmic=0
                       if not keyword_set(no_star) then IFURED_CRE_INI_FILES,"star",d_dir=d_dir,night=night,cub=star_cub,file_in=star_file,$
                        w_dir=w_dir,crop=crop,zip=zip,/logfield,use_lacosmic=0
                       if not keyword_set(no_sky) then IFURED_CRE_INI_FILES,"sky",d_dir=d_dir,night=night,cub=sky_cub,file_in=sky_file,$
                        w_dir=w_dir,crop=crop,zip=zip,/logfield,use_lacosmic=0
                    endif
                  END
                  
                  "coadding": BEGIN
                    
                    ; (3) - Combine spectra
                    step=3
                    WIDGET_CONTROL,status_field,set_val="STATE:  Processing (Step # "+string(step,format="(I0)")+")"
                    
                    IFURED_LOG_UPD, [systime()+":","##### Co-adding of initial frames #####"]
                    
                    WIDGET_CONTROL, slit_bord[0],get_val=slit_break_left
                    WIDGET_CONTROL, slit_bord[1],get_val=slit_break_right
                    slit_break_left=fix(slit_break_left[0])
                    slit_break_right=fix(slit_break_right[0])
                    
                    IFURED_LOG_UPD, ["Left slit: till "+string(slit_break_left,format="(I0)")+" pix.;",$
                    "Right slit: from "+string(slit_break_right,format="(I0)")+" pix."]
                    
;;                    response=IFURED_CHECK_INI_FILES(logfield=1)
;                    
;;                    if response eq 1 then begin
;                       ;response=IFURED_SEARCH_FIBPOS_FILE(logfield=1) 
;                        ;if response eq 1 then begin
;                        
;;                          WIDGET_CONTROL, ovscan[0],get_val=x0
;;                          WIDGET_CONTROL, ovscan[1],get_val=y0
;;                          WIDGET_CONTROL, ovscan[2],get_val=x1
;;                          WIDGET_CONTROL, ovscan[3],get_val=y1
;;                          crop=[fix(x0[0]),fix(y0[0]),fix(x1[0]),fix(y1[0])]
;                          
;                          
;                          if x1 eq 0 then last="last" else last=string(x1,format="(I0)")
;                          IFURED_LOG_UPD, ["Left slit: from "+string(x0,format="(I0)")+$
;                                                          " to "+string(slit_break_left,format="(I0)")+" pix.;",$
;                                                          "Right slit: from "+string(slit_break_right,format="(I0)")+$
;                                                          " to "+last+" pix.;"]
;                          
;                          
                          immask=0
                          
                          WIDGET_CONTROL, tr_cr_wid[0],get_val=tr
                          tr=float(tr[0])
                          
                          
                          ; USE FOR IFU-polarisation
;                          ref_frame=readfits("/Users/mors/Science/IFU/Reduction/Mrk938_0-1/obj_i.fts")
                          xlim=[1400,1800,250,900]
                          mask_y=0; [950,1100]
                          types=['obj','flat','star','sky','eta','neon']
                          tr_id=[0,2,5,4,3,1]
                          for type_id = 0,n_elements(types)-1 do begin
                            if type_id eq 1 then ref_frame=readfits(w_dir+"obj_i.fts")
                            if type_id gt 1 then ref_frame=readfits(w_dir+"flat_i.fts")
                            if keyword_set(no_star) and types[type_id] eq 'star' then continue
                            if keyword_set(no_sky) and types[type_id] eq 'sky' then continue
                            IFURED_LOG_UPD, "  *** Working on "+strupcase(types[type_id])+" frames ***"
                            
                            WIDGET_CONTROL, tr_cr_wid[tr_id[type_id]],get_val=tr
                            tr=float(tr[0])
                            if types[type_id] eq 'neon' or types[type_id] eq 'obj' then shiftX=1 else shiftX=0
                            IFURED_COADD,types[type_id]+"_obs.fts",types[type_id]+'_i.fts',w_dir=w_dir,t=t,ref_frame=ref_frame,/logfield,mask_y=mask_y,filetest="ch_test_"+types[type_id]+".fts",immask=immask,$
                              nonor=0,shiftx=shiftX,shiftY=1,ylim=0,xlim=xlim,win=5,separate=[slit_break_left,slit_break_right],show=show,objtype=types[type_id], no_cr=no_cr
                            
                            IFURED_LOG_UPD, " "
                          endfor
                          
                          
;                          IFURED_COMBINE,"obj",d_dir=d_dir,night=night,cub=obj_cub,file_in=obj_file,$
;                                        w_dir=w_dir,crop=crop,t=tr,zip=zip, ref_frame=ref_frame,show=show,xlim=xlim,$
;                                        slit_break_left=slit_break_left,slit_break_right=slit_break_right,/logfield,mask_y=mask_y; /no_cr,
;                          stop
                          
;                          ref_frame=readfits(w_dir+"obj_i.fts")
                          
;                          WIDGET_CONTROL, tr_cr_wid[2],get_val=tr
;                          tr=float(tr[0])
;                          IFURED_COMBINE,"flat",d_dir=d_dir,night=night,cub=flat_cub,file_in=flat_file,$
;                                        w_dir=w_dir,crop=crop,t=tr,zip=zip, ref_frame=ref_frame,show=show,xlim=xlim,$
;                                        slit_break_left=slit_break_left,slit_break_right=slit_break_right,/logfield;,mask_y=mask_y
                          
;                          ref_frame=readfits(w_dir+"flat_i.fts")
                          
;                          WIDGET_CONTROL, tr_cr_wid[5],get_val=tr
;                          tr=float(tr[0])
;                          if not keyword_set(no_star) then IFURED_COMBINE,"star",d_dir=d_dir,night=night,cub=star_cub,xlim=xlim,$
;                                                      file_in=star_file,w_dir=w_dir,crop=crop,t=tr,zip=zip, ref_frame=ref_frame,show=show,$
;                                                      slit_break_left=slit_break_left,slit_break_right=slit_break_right,/logfield;, /no_cr;,mask_y=mask_y
;                          
;                          
;                          
;                          WIDGET_CONTROL, tr_cr_wid[4],get_val=tr
;                          tr=float(tr[0])
;                          if not keyword_set(no_sky) then IFURED_COMBINE,"sky",d_dir=d_dir,night=night,cub=sky_cub,xlim=xlim,$
;                                                      file_in=sky_file,w_dir=w_dir,crop=crop,t=tr,show=show,zip=zip, ref_frame=ref_frame,$
;                                        slit_break_left=slit_break_left,slit_break_right=slit_break_right,/logfield
;                          
;                           WIDGET_CONTROL, tr_cr_wid[3],get_val=tr
;                          tr=float(tr[0])
;                          IFURED_COMBINE,"eta",d_dir=d_dir,night=night,cub=eta_cub,file_in=eta_file,immask=immask,$
;                                        w_dir=w_dir,crop=crop,t=tr,zip=zip, show=show,xlim=xlim,$
;                                        slit_break_left=slit_break_left,slit_break_right=slit_break_right, ref_frame=ref_frame,/logfield
;                         
;                         WIDGET_CONTROL, tr_cr_wid[1],get_val=tr
;                          tr=float(tr[0])
;                          IFURED_COMBINE,"neon",d_dir=d_dir,night=night,cub=neon_cub,file_in=neon_file,immask=immask,$
;                                        w_dir=w_dir,crop=crop,t=tr,zip=zip, ref_frame=ref_frame,show=show,xlim=xlim,$
;                                        slit_break_left=slit_break_left,slit_break_right=slit_break_right,/logfield
;;                    
                    
;                          IFURED_UPDATE_WLOUT
                          IFURED_CHECK_MASTER,type="eta"
                          
                    ;endif      
;                    endif
                  END
              
              
                  "traectory": BEGIN 
                   ; (4) - Traectory calculation
                    step=4
                    WIDGET_CONTROL,status_field,set_val="STATE:  Processing (Step # "+string(step,format="(I0)")+")"
                    
                    IFURED_LOG_UPD, [systime()+":","##### Compute spectra traectory #####"]
                    
                    WIDGET_CONTROL, tra_params[0],get_val=tra_dspec
                    WIDGET_CONTROL, tra_params[1],get_val=tra_win
                    
                    ;first_eta=0
                    ;last_eta=12
                    tra_win=fix(tra_win[0])
                    tra_dspec=fix(tra_dspec[0])
                    IFURED_TRAECTORY, w_dir=w_dir,first_eta=first_eta, last_eta=last_eta,show=show,$
                                    ifu_dir=ifu_dir, night=night,dspec=tra_dspec,win=tra_win,/logfield
                  
            
                  END
                  
                  "geometry_calc": BEGIN 
                    ;######## (5) Geometry calculation
                    step=5
                    WIDGET_CONTROL,status_field,set_val="STATE:  Processing (Step # "+string(step,format="(I0)")+")"
                    
                    
                    file_eta="eta_i.fts"
                    file_flat="flat_i.fts"
                    file_neon="neon_i.fts"
                    file_obj="obj_i.fts"
                    if not keyword_set(no_sky) then file_sky="sky_i.fts" else file_sky=""
                    if not keyword_set(no_star) then file_star="star_i.fts" else file_star=""     
                    lines=read_asc(ifu_dir+"lines.tab")
                    rec=where(lines[1,*] ne -1,nrec)
                    neon_lines=fltarr(2,nrec)
                    for i=0,1 do neon_lines[i,*]=lines[2*i,rec]
                    
                    file_traectory=w_dir+"traectory.fit"
                    traectory=read_asc(file_traectory)
                    
                    WIDGET_CONTROL, win_geometry_x,get_val=Geometry_Lines_win_x
                    Geometry_Lines_win_x=fix(Geometry_Lines_win_x[0])
                    WIDGET_CONTROL, win_geometry_y,get_val=Geometry_Lines_win_y
                    Geometry_Lines_win_y=fix(Geometry_Lines_win_y[0])
                    WIDGET_CONTROL,ovscan[0],get_val=shi_tab
                    shi_tab=fix(shi_tab[0])
                    
                    IFURED_LOG_UPD, [systime()+":","##### Fing geometry #####","Win X = "+string(Geometry_Lines_win_x,format="(I2)")+"; Win Y = "+$
                                  string(Geometry_Lines_win_y,format="(I0)")]
                                  
                    IFURED_CHECK_MASTER,type="eta",mode="get_index",left=bad_fibers_lt,right=bad_fibers_rt
                                  
                    IFURED_GEOMETRY, fileneon=file_neon, night=night,filestar=file_star,$
                                    lines=neon_lines,tra=traectory,fileflat=file_flat,fileeta=file_eta,w_dir=w_dir,dirforifu=ifu_dir, fileobj=file_obj,$
                                    filesky=file_sky, show=show,/logfield,fit_deg=2,wx=Geometry_Lines_win_x,wy=Geometry_Lines_win_y,win=30,$
                                    bad_fibers_lt=bad_fibers_lt,bad_fibers_rt=bad_fibers_rt,set_center=1-search_geometry, shi_tab=shi_tab
                    IFURED_LOG_UPD, [" "]
                   
                  END
                   
                  
                  "geometry_corr": BEGIN
                  ;######## (6) Geometry correction
                  step=6
                  WIDGET_CONTROL,status_field,set_val="STATE:  Processing (Step # "+string(step,format="(I0)")+")"
                  IFURED_LOG_UPD, [systime()+":","##### Geometry correction #####"]
                  filenames=['neon','flat','obj']
                  if not keyword_set(no_star) then filenames=[filenames,"star"]
                    if not keyword_set(no_sky) then filenames=[filenames,"sky"]
                    IFURED_DO_WARP, w_dir=w_dir,filenames=filenames
                    
                    IFURED_ADJUST_WARP_ETAPOS, w_dir=w_dir, flatname='flat_warp.fts', posname="eta_pos_warp.fit"
                    
                    IFURED_CHECK_MASTER,type="other"
                  
                   IFURED_LOG_UPD, [" "]
                  END
                   
                   
                  "cross_talk_rem": BEGIN
                     ; (7) - Spectra extraction
                    step=7
                    WIDGET_CONTROL,status_field,set_val="STATE:  Processing (Step # "+string(step,format="(I0)")+")"
                    
                    IFURED_LOG_UPD, [systime()+":","##### Spectra extraction #####"]
                    if not keyword_set(optimal) then IFURED_LOG_UPD,["Integration in boxes"] $
                    else IFURED_LOG_UPD,["Note: Optimal extraction now is not able to take into account gradiens."]
                    
                    USE_OPTIMAL=optimal
                    
                    neon=readfits(w_dir+"neon_warp.fts",hneon)
                    flat=readfits(w_dir+"flat_warp.fts",hflat)
                    obj=readfits(w_dir+"obj_warp.fts",hobj)
                    
                    
                    if not keyword_set(no_star) then star=readfits(w_dir+"star_warp.fts",hstar)
                    if not keyword_set(no_sky) then star=readfits(w_dir+"sky_warp.fts",hsky)
                     
                    IFURED_TEST_WARP,show=show,/logfield;=logfield

;                    
                    type=['flat','neon','obj']
                    images0=[ptr_new(reform(flat[*,*,0])),ptr_new(reform(neon[*,*,0])),ptr_new(reform(obj[*,*,0]))]
                    images1=[ptr_new(reform(flat[*,*,1])),ptr_new(reform(neon[*,*,1])),ptr_new(reform(obj[*,*,1]))]
                    if not keyword_set(no_star) then begin
                      type=[type,'star']
                      images0=[images0,ptr_new(reform(star[*,*,0]))]
                      images1=[images1,ptr_new(reform(star[*,*,1]))]
                    endif
                    if not keyword_set(no_sky) then begin
                      type=[type,'sky']
                      images0=[images0,ptr_new(reform(sky[*,*,0]))]
                      images1=[images1,ptr_new(reform(sky[*,*,1]))]
                    endif
                  
                    
                    ctalk_rem0=IFURED_CROSS_TALK_REM(images=images0,slitnum=0,w_dir=w_dir,$
                                  /logfield,show=spec_extra_show, tit="left slit", type=type, optimal=use_optimal)
                    ctalk_rem1=IFURED_CROSS_TALK_REM(images=images1,slitnum=1,w_dir=w_dir,$
                                  show=spec_extra_show,/logfield, tit="right slit", type=type, optimal=use_optimal)
                    
                    neon0=*ctalk_rem0[1]
                    neon1=*ctalk_rem1[1]
                    flat0=*ctalk_rem0[0]
                    flat1=*ctalk_rem1[0]
                    obj0=*ctalk_rem0[2]
                    obj1=*ctalk_rem1[2]
                    if not keyword_set(no_star) then begin
                      star0=*ctalk_rem0[3]
                      star1=*ctalk_rem1[3]
                   endif
                    if not keyword_set(no_sky) then begin
                      sky0=*ctalk_rem0[4]
                      skyr1=*ctalk_rem1[4]
                   endif
                    ptr_free,images0,images1
                    ptr_free,ctalk_rem0,ctalk_rem1
                    
                    s0=size(neon0)
                    s1=size(neon1)
                    nx=max([s0[1],s1[1]])
                    ny=max([s0[2],s1[2]])
                    neon=fltarr(nx,ny,2)
                    flat=fltarr(nx,ny,2)
                    obj=fltarr(nx,ny,2)
                    neon[*]=!VALUES.F_NAN
                    flat[*]=!VALUES.F_NAN
                    obj[*]=!VALUES.F_NAN
                    neon[0:s0[1]-1,0:s0[2]-1,0]=neon0
                    neon[0:s1[1]-1,0:s1[2]-1,1]=neon1
                    flat[0:s0[1]-1,0:s0[2]-1,0]=flat0
                    flat[0:s1[1]-1,0:s1[2]-1,1]=flat1
                    obj[0:s0[1]-1,0:s0[2]-1,0]=obj0
                    obj[0:s1[1]-1,0:s1[2]-1,1]=obj1
                    if not keyword_set(no_star) then begin
                      star=fltarr(nx,ny,2)
                      star[*]=!VALUES.F_NAN
                      star[0:s0[1]-1,0:s0[2]-1,0]=star0
                      star[0:s1[1]-1,0:s1[2]-1,1]=star1
                    endif
                    if not keyword_set(no_sky) then begin
                      sky=fltarr(nx,ny,2)
                      sky[*]=!VALUES.F_NAN
                      sky[0:s0[1]-1,0:s0[2]-1,0]=sky0
                      sky[0:s1[1]-1,0:s1[2]-1,1]=sky1
                    endif
                    writefits,w_dir+"neon_s.fts",neon,hneon
                    writefits,w_dir+"flat_s.fts",flat,hflat
                    writefits,w_dir+"obj_s.fts",obj,hobj
                    if not keyword_set(no_star) then  writefits,w_dir+"star_s.fts",star,hstar
                    if not keyword_set(no_sky) then  writefits,w_dir+"sky_s.fts",sky,hsky
                    
                    
                    IFURED_CORRECT_FIBERS_XSHIFT
                    filelist=["neon_s","flat_s","obj_s"]
                    if not keyword_set(no_star) then filelist=[filelist,"star_s"]
                    if not keyword_set(no_sky) then filelist=[filelist,"sky_s"]
                    
                    IFURED_CUTNANS,w_dir, filelist+'.fts'
                    
                    IFURED_LOG_UPD, [" "]
                    
                  END 
                  
                  'disp_curve':BEGIN
                  ;(8) - Dispersion curve calculation
                    step=8
                    WIDGET_CONTROL,status_field,set_val="STATE:  Processing (Step # "+string(step,format="(I0)")+")"
                    
                    WIDGET_CONTROL,lin_params[0],get_val=lin_deg
                    WIDGET_CONTROL,lin_params[1],get_val=lin_yfit
                    WIDGET_CONTROL,lin_params[2],get_val=fw_lin
                    fw_lin=[float(fw_lin[0]),float(fw_lin[0])]
                    lin_yfit=[fix(lin_yfit[0]),fix(lin_yfit[0])]
                    lin_deg=[fix(lin_deg[0]),fix(lin_deg[0])]
                    
                    TEST_LIN_MODE=0
                    IF (TEST_LIN_MODE) THEN BEGIN   
                      im_ini=readfits(w_dir+"neon_s.fts")
                      for i=0,1 do begin 
                        if i eq 0 then im=reform(im_ini[*,*,0]) else im=reform(im_ini[*,*,1])
                        vector=total(im[*,128-3:128+3],2)/7.
                        s=size(vector)
                        x=findgen(s[1])
                        pks=IFURED_FIND_PEAKS(vector,win=5, /find,/robo)
                        pks_val=vector[round(pks)]
                        
                        if i eq 1 then begin
                          x+=slit_break_right
                          pks+=slit_break_right
                        endif
                        
                        cgdisplay,wid=7+i,xs=1200
                        cgplot,x,vector,xst=1,yr=[-200,15000]
                        cgoplot,pks,pks_val,psym=4,color="red"
                        cgtext,pks,1.05*pks_val,string(pks,format="(F7.2)"),charsize=0.9,orientation=90
                      endfor
                      
                      ;   ### NEON lines
                        neon_lines=read_asc(ifu_dir+"lines.tab")
                        rec=where(neon_lines[1,*] ne -1,nrec)
                        lines=fltarr(2,nrec)
                        for i=0,1 do lines[i,*]=neon_lines[2*i,rec]
                        cgdisplay,wid=3,xs=1400,ys=850
                        lam=findgen(6000)+3000
                        gau=lam*0
                        for i=0,nrec-1 do gau+=gaussian(lam,[lines[1,i],lines[0,i],2.2])
                        cgplot,lam,gau,yr=[-0.1,max(gau)+1],xst=1,yst=1,xr=[4300,6200]
                        for i=0,nrec-1 do cgtext,lines[0,i],lines[1,i]+0.3,string(lines[0,i],format="(F7.2)"),charsize=0.9,orientation=90.
                    ENDIF ELSE BEGIN
                        
                        IFURED_LOG_UPD, [systime()+":","##### Dispersion curve calculation #####"]
                        WIDGET_CONTROL, slit_bord[1],get_val=slit_br
                        slit_br=fix(slit_br[0])
                        IFURED_DISPERSION_CURVE, w_dir=w_dir, left_shift=slit_br,fileneon='neon_s.fts', n_deg=lin_deg,$
                                      fwhm=fw_lin, yfit=lin_yfit, ifu_dir=ifu_dir, show=show, /logfield
                        ;IF keyword_set(auto_wlout) then IFURED_UPDATE_WLOUT
                        IFURED_LOG_UPD, [" "]
                    ENDELSE
                    
                    
                  END 
                  
                  
                  "linearisation": BEGIN
                     ; (9) - Linearisation
                    step=9
                    WIDGET_CONTROL,status_field,set_val="STATE:  Processing (Step # "+string(step,format="(I0)")+")"
                    
                    
                    
                    WIDGET_CONTROL,wlout_params[0],get_val=v0
                    WIDGET_CONTROL,wlout_params[1],get_val=v1
                    WIDGET_CONTROL,wlout_params[2],get_val=v2
                    wl_out=[float(v0[0]),float(v1[0]),float(v2[0])]
                 
                    filenames=["obj","flat","neon"]
                    if not keyword_set(no_star) then filenames=[filenames,"star"]
                    if not keyword_set(no_sky) then filenames=[filenames,"sky"]
                    IFURED_LOG_UPD, [systime()+":","##### Linearization #####"]
                    IFURED_LINERIS,filenames,w_dir=w_dir,out_param=wl_out;,slit_dx=slit_dx
                    IFURED_LOG_UPD, [" "]
                  END 
                  
                  
                  "flat": BEGIN
                    ; (10) - Flat field normalization
                    step=10
                    WIDGET_CONTROL,status_field,set_val="STATE:  Processing (Step # "+string(step,format="(I0)")+")"
                    IFURED_LOG_UPD, [systime()+":","##### Flat cube creation #####"]
                    IFURED_CUB_EXTRACTION, ["flat"],w_dir=w_dir,lambda_test=lam_test
                    ifured_flatnorm,w_dir=w_dir,show=show
                    file_delete,w_dir+'flat_cub.fts'
                    IFURED_LOG_UPD, [" "]
                  END    
                      
                  
                  "extraction": BEGIN
                     ; (11) - Cub extraction
                    step=11
                    WIDGET_CONTROL,status_field,set_val="STATE:  Processing (Step # "+string(step,format="(I0)")+")"
                    IFURED_LOG_UPD, [systime()+":","##### Create cubes and normailize to flat  #####"]
                    filenames=["obj","neon"]
                    if not keyword_set(no_star) then filenames=[filenames,"star"]
                    if not keyword_set(no_sky) then filenames=[filenames,"sky"]
                    if norm_cub_flat eq 1 then out_suffix='_cub_n.fts' else out_suffix='_cub.fts'
                    IFURED_CUB_EXTRACTION, filenames,w_dir=w_dir,lambda_test=lam_test,out_suffix=out_suffix;'_n.fts'
                    if norm_cub_flat eq 1 then ifured_flatcub_reduct,filenames,suff=out_suffix,w_dir=w_dir
                    
                    IF norm_cub_sky eq 1 then begin
                      if keyword_set(no_sky) then IFURED_LOG_UPD,["There is no SKY data to perform"," additional flat correction"] else begin
                        
                        
                        ;**** Here should be flat correction using SKY data
                        
                        IFURED_LOG_UPD,["Additional flat correction with SKY is not working yet"]
                      
                      endelse
                      
                    ENDIF
                    IFURED_LOG_UPD, [" "]
                  END
                  
                  
                  "sky": BEGIN
                   ; (12) - Sky subtraction
                    step=12
                    
                    ;**** Skiping sky subtraction for now
                    SKIP_SKYSUB=0
                    
                    WIDGET_CONTROL,status_field,set_val="STATE:  Processing (Step # "+string(step,format="(I0)")+")"
                    filenames=["obj"]
                    if not keyword_set(no_star) then filenames=[filenames,"star"]
                    if not keyword_set(no_sky) then filenames=[filenames,"sky"]
                    IFURED_LOG_UPD, [systime()+":","##### Sky subtraction #####"]
                    if not keyword_set(SKIP_SKYSUB) then begin
                        
;                        WIDGET_CONTROL,sky_params,get_val=sky_deg
;                        sky_deg=fix(sky_deg[0])
                        
                        
                        ;***** Note: it is old version of sky subtraction written for 2D spectra, not for cubes. It should not working.
                        IFURED_CUB_SKY_SUBTRACT, filenames, w_dir=w_dir;, ndeg=sky_deg,show=show 
                    endif else begin
                        for current=0,n_elements(filenames)-1 do file_copy,w_dir+filenames[current]+"_cub_n.fts",w_dir+filenames[current]+"_cub-sky.fts",/over
                        IFURED_LOG_UPD,["Sky subtraction was skiped"," output files are copies of those from previous step."]
                    endelse
                    IFURED_LOG_UPD, [" "]
                  END
                  
                  
              
              
                  "dqe": BEGIN
                     ; (13) - DQE determination
                    
                    if keyword_set(no_star) then begin
                      mes=dialog_message("It is not possible to compute DQE without STAR!") 
                    endif else begin
                        step=13
                        WIDGET_CONTROL,status_field,set_val="STATE:  Processing (Step # "+string(step,format="(I0)")+")"
                        IFURED_LOG_UPD, [systime()+":","##### DQE calculation #####"]
                        file="star_cub-sky.fts"
                        ifured_crea_sent,file,star_table,w_dir=w_dir,plot=show, stand_dir=stand_dir
                        IFURED_LOG_UPD, [" "]
                        
                   endelse
                  END
                  
                  
                  "flux_calib": BEGIN
                      ; (14) - Flux calibration
                      step=14
                      WIDGET_CONTROL,status_field,set_val="STATE:  Processing (Step # "+string(step,format="(I0)")+")"
                      IFURED_LOG_UPD, [systime()+":","##### Flux calibration #####"]
                      
                      inf=FILE_INFO(w_dir+"sent.fts")
                      IF NOT (inf.exists) then begin
                        mes=dialog_message("Can't find file 'sent.fts' in working directory ("+w_dir+")")
                        IFURED_LOG_UPD, ["File 'sent.fts' is not found!"]
                      endif else begin
                        
                        ifured_corrsent,"obj_cub-sky.fts", "obj_cub_abs.fts", w_dir=w_dir
                        if not keyword_set(no_star) then ifured_corrsent,"star_cub-sky.fts", "star_cub_abs.fts", w_dir=w_dir
                      
                      endelse
                      IFURED_LOG_UPD, [" "]
                  END
                  
                  
                  
                  "atm_disp": BEGIN
                      ; (15) - Atomspheric dispersion correction
                      step=15
                      WIDGET_CONTROL,status_field,set_val="STATE:  Processing (Step # "+string(step,format="(I0)")+")"
                       IFURED_LOG_UPD, [systime()+":","##### Atmosphere dispersion correction #####"]
                      
                      
                      if not keyword_set(no_star) then begin
                         ;used star
                          IFURED_ATM_DIS,'star',ext='_cub_abs',Ndeg=2,rad=4,w_dir=w_dir,show=show
                          fileref='atmdis_star.txt'
                      endif else begin ; used object
                          IFURED_ATM_DIS,'obj',ext='_cub_abs',Ndeg=2,rad=4,w_dir=w_dir,show=show
                
                          fileref='atmdis_obj.txt'
                      endelse
                         ; corection;
                         IFURED_ATM_COR,'obj',ext='_cub_abs',fileref=fileref,w_dir=w_dir
                
                        ; test of the correction:
                      if not keyword_set(no_star)  then begin
                         IFURED_ATM_DIS,'star',ext='_cub_abs',Ndeg=2,rad=4,w_dir=w_dir,show=show
                         IFURED_ATM_COR,'star',ext='_cub_abs',fileref=fileref,w_dir=w_dir
                      endif
                      
                      IFURED_LOG_UPD, [" "]
                  END
                  
                  
                  "cre_maps": BEGIN
                      ; (16) - Create flux, velocity, dispersion maps in main emission lines 
                      ;        and underlying continuum map in those lines 
                      step=16
                      WIDGET_CONTROL,status_field,set_val="STATE:  Processing (Step # "+string(step,format="(I0)")+")"
                      IFURED_LOG_UPD, [systime()+":","##### Creating the flux, velocity, velocity dispersion and continuum maps #####"]
                       
                       widget_control,sysvel_field,get_val=val
                       sysvel=float(val[0])
                      
                      ifured_skylines_fit, 'obj_cub_n.fts',w_dir=w_dir
                       ifured_cre_maps, 'obj_cub_cor.fts',w_dir=w_dir,sysvel=sysvel, error=error
                      
                      if error eq 1 then begin
                        IFURED_LOG_UPD, [" Error: No pre-defined emission lines were found whithin wavelenght scale of the current cube."]
                      endif
                            
                      IFURED_LOG_UPD, [" "]
                  END
                  
                  ELSE: print, "This option is not working now!" 
              
              ENDCASE
       
      ENDFOR
      
      ;IFURED_UPD_IMLIST
      WIDGET_CONTROL,status_field,set_value="STATE:  Ready!"
    
    END
    
    
    ELSE: BEGIN

      prefix=strmid(ev,0,8)
      IF prefix EQ 'SetStep_' THEN BEGIN
         mode=fix(strmid(ev,8,2))
         markers[mode].set=1-markers[mode].set
      ENDIF
      IF prefix EQ 'SetShow_' THEN BEGIN
         mode=fix(strmid(ev,8,2))
         markers[mode].showres=1-markers[mode].showres
      ENDIF
      
      
      
    END
    
  
  ENDCASE


END

PRO IfuRed_go

  COMMON IFURED_PARAMS
  COMMON IFURED_WIDGET_ELEMENTS
  COMMON IFURED_REDUCTION_PROCESS
  ;COMMON IFURED_DISPLAY
  
  
  IFURED_COMMON_DEF
  IFURED_viewspec_common_def
  
  
;определение используемых структур
    tmp={sizes,x:0L,y:0L}
    tmp={mon,xim:0L,yim:0L,xphys:0L, int:0L, cont:0L, bgr:0L}
    tmp={pos,x0:0E,y0:0E,x1:0E,y1:0E}
    tmp={brt,mn:0E,mx:0E,avg:0E}
  
  
    ;ini_zoom_im_pos=[2,2]
    ;zoom_im=[float(zoom_vals[ini_zoom_im_pos[0]]),float(zoom_vals[ini_zoom_im_pos[1]])]
    
    ;if ndisplays eq 2 then sz[1].y=sz[1].y/2  
    ;sz[0].x=sz[1].x+sz[3].x+4*10+sz[4].x
    ;sz[0].y=max([(sz[1].y*ndisplays+70*ndisplays),sz[3].y,sz[4].y*ndisplays+30*6*ndisplays+70*ndisplays])+4*10
    sz[0].x=sz[3].x+sz[1].x+2*10;+sz[4].x
    sz[0].y=max([sz[3].y,sz[1].y])+4*10
    
  bord_draw={pos,0,0,0,0}
  zoomfactor=[1.,1.]
  pix0_ondisp=[0,0,0,0]
  filepar=''
  im_available=["None"]
  im_available_num=[-1]
  names_correspond_1=[""]
  names_correspond_2=[""]
  start_draw_bord=0
  ifu_mb=widget_base(TITLE='SCORPIO-2 IFU REDUCTION',/row,TLB_Size_Events=1,xsize=sz[0].x,ysize=sz[0].y, uval="mb")
  
  
  ifu_redsteps_base=widget_base(ifu_mb,/column,xsize=sz[3].x,ysize=sz[3].y)
  ifu_engine_base=widget_base(ifu_mb,/column,xsize=sz[1].x,ysize=sz[1].y)
  
  
  
  ;############ Log file block ############
  ifu_log_mbase=widget_base(ifu_engine_base,/column,/frame)
  ifu_log_label=widget_label(ifu_log_mbase,value="Load log-file:",font=titfont)
  ifu_log_base=widget_base(ifu_log_mbase,/row)
  ifu_log_text=widget_text(ifu_log_base,/editable,uvalue="set_log")
  ifu_log_but_base=widget_base(ifu_log_base,/row)
  ifu_log_loadbut=widget_button(ifu_log_but_base,value="Load",uvalue="load_log",xs=sz[2].x,ys=sz[2].y)
  ifu_log_editbut=widget_button(ifu_log_but_base,value="Edit",uvalue="edit_log",xs=sz[2].x,ys=sz[2].y)
  
  ;#####################################
  
  
  
  
  
  
  
  
  ;############ Reduction steps ##############
  reduct_steps=["meanbias","cre_ini","coadding","traectory","geometry_calc","geometry_corr",$
                "cross_talk_rem","disp_curve","linearisation","flat","extraction",$
                "sky","dqe","flux_calib","atm_disp","cre_maps"]
  reduct_name=["Create Meanbias","Create initial", "Co-add images","Traectory","Calc. Geometry","Correct Geometry",$
              "Spectra extraction","Dispersion curve","Linearization","Create flat cube","Create cubes",$
              "Sky subtraction","DQE calculation","Flux calibration", "Atm.disp. correction",'Create maps']
  showind=[0,0,1,1,1,0,1,1,0,1,0,0,1,0,1,1]  ; Set on which steps results may be shown
  names_show=["","", "Show CR","Show traectories", "Show lines", "","Show traectories","Show result", "",$
              "Show residuals", "","Show results","Plot DQE", "", "Plot results", "Show maps"]
  nsteps=n_elements(reduct_steps)
  
  for i=0,nsteps-1 do reduct_name[i]="("+string(i+1,format="(I0)")+") "+reduct_name[i]
  
  but_id=fltarr(nsteps)
  
  tmp={markers_struct,name:'',uval:"SetStep_",set:0B, showres:0B}
  markers=replicate(tmp,nsteps)
  
  ifu_red_base=widget_base(ifu_redsteps_base,/column,/frame)
    ifu_red_label=widget_label(ifu_red_base,value="Select reduction steps:",font=titfont)
    for i=0,nsteps-1 do begin
      if i lt 10 then add='0' else add=''
      if i lt 10 then form="(I1)" else form="(I2)"
      markers[i].name=reduct_name[i]
      markers[i].uval=markers[i].uval+add+string(i,format=form)
      
      
      ifu_option_base=widget_base(ifu_red_base,/row,/frame)
      ifu_marker_base=widget_base(ifu_option_base,/nonex,/col,xs=200);)
      tmpobj=widget_button(ifu_marker_base,value=markers[i].name,uvalue=markers[i].uval)
      but_id[i]=tmpobj
      
      
      ;(2) - Overscan
      if i eq 1 then begin
        ifu_curoption_base=widget_base(ifu_option_base,/row,/frame)
        tmp=widget_label(ifu_curoption_base, val="Cut frames: ")
        ovscan=lonarr(4)
        names=["X0:",'Y0:','X1:','Y1:']
        val=[20,20,4400,0]
        for k=0,n_elements(ovscan)-1 do ovscan[k]=cw_field(ifu_curoption_base,xs=4,val=val[k],tit=names[k])
      ENDIF
      
      
      ;(3) - Treshold for CR cleaning and slit border
      IF i eq 2 then begin
        ifu_curoption_stack=widget_base(ifu_option_base,/column)
        
        ifu_curoption_base=widget_base(ifu_curoption_stack,/row,/frame)
        slit_bord=lonarr(2)
        names=["Left Slit End:","Right Slit Start:"]
        val=[2300,2550]
        FOR k=0,n_elements(slit_bord)-1 do slit_bord[k]=cw_field(ifu_curoption_base,xs=4,val=val[k],tit=names[k])
        
        ifu_curoption_base=widget_base(ifu_curoption_stack,/row,/frame)
        tmp=widget_label(ifu_curoption_base, val="Tr.: ")
        tr_cr_wid=lonarr(6)
        val=[20,60,60,60,30,30]
        names=["Obj","Neon","Flat","Eta","Sky","Star"]
        FOR k=0,n_elements(tr_cr_wid)-1 do tr_cr_wid[k]=cw_field(ifu_curoption_base,xs=2,val=val[k],tit=names[k])
        
      ENDIF
      
      ;(4) - First and last etalon fibers on frame; Traectory step and window; 
      IF i eq 3 then begin
        ifu_curoption_base=widget_base(ifu_option_base,/row,/frame)
        tra_params=lonarr(2)
        names=["Step:", "Win:"]
        val=[50,10]
        FOR k=0,n_elements(tra_params)-1 do tra_params[k]=cw_field(ifu_curoption_base,xs=4,val=val[k],tit=names[k])
        
      ENDIF
      
      ;(5) - Degree for lines fit; Window; 
      IF i eq 4 then begin
        ifu_curoption_base=widget_base(ifu_option_base,/row,/frame)
        tmp=widget_label(ifu_curoption_base, val="Search for neon lines: ")
        names=["Win X:", "Win Y:"]
        val=[25,5]
        win_geometry_x=cw_field(ifu_curoption_base,xs=4,val=val[0],tit=names[0])
        win_geometry_y=cw_field(ifu_curoption_base,xs=4,val=val[1],tit=names[1])
        ifu_markershow_base=widget_base(ifu_curoption_base,/nonex,/col)
        tmpobj=widget_button(ifu_markershow_base,value="Find peaks",uvalue="auto_neon_search",/align_left)
        widget_control,tmpobj,set_button=0
        search_geometry=0
      ENDIF
      
      ;(7) - Option for optimal extraction
      IF i eq 6 then begin
        ifu_curoption_base=widget_base(ifu_option_base,/row,/frame)
        ifu_markershow_base=widget_base(ifu_curoption_base,/nonex,/col)
        tmpobj=widget_button(ifu_markershow_base,value="Optimal extraction",uvalue="optimal_extr",/align_left)
        widget_control,tmpobj,set_button=0
        optimal=0
      ENDIF
      
      ;(8) - Parameters for disp.curve; 
      IF i eq 7 then begin
        
        ifu_curoption_base=widget_base(ifu_option_base,/row,/frame)
        tmp=widget_label(ifu_curoption_base, val="Fit NEON lines params.: ")
        lin_params=lonarr(3)
        names=["Xdeg:", "Ydeg:", "FWHM:"]
        val=[5,2,4.]
        FOR k=0,n_elements(lin_params)-1 do lin_params[k]=cw_field(ifu_curoption_base,xs=4,val=val[k],tit=names[k])
        
      ENDIF
      
      
      ;(9) - Parameters for linearization;
      IF i eq 8 then begin  
        ifu_curoption_base=widget_base(ifu_option_base,/row,/frame)
        tmp=widget_label(ifu_curoption_base, val="Out wavelength params.: ")
        wlout_params=lonarr(3)
        names=["Lam0","Lam1", "Dlam"]
        val=[4200,6100,0.6]
        FOR k=0,n_elements(wlout_params)-1 do wlout_params[k]=cw_field(ifu_curoption_base,xs=5,val=val[k],tit=names[k])
;        ifu_markershow_base=widget_base(ifu_curoption_base,/nonex,/col)
;        tmpobj=widget_button(ifu_markershow_base,value="Auto update",uvalue="wlout_auto_upd",/align_left)
;        widget_control,tmpobj,set_button=1
;        auto_wlout=1
      ENDIF 
      
      ;(11) - Options for cubes creation;
      IF i eq 10 then begin  
        ifu_curoption_base=widget_base(ifu_option_base,/row,/frame)
        ifu_markershow_base=widget_base(ifu_curoption_base,/nonex,/row)
        tmpobj0=widget_button(ifu_markershow_base,value="Norm. to flat",uvalue="cub_flat_norm",/align_left)
        tmpobj1=widget_button(ifu_markershow_base,value="Additional flat corr. with SKY",uvalue="cub_sky_norm",/align_left)
        widget_control,tmpobj0,set_button=1
        widget_control,tmpobj1,set_button=0
        norm_cub_flat=1
        norm_cub_sky=0
      ENDIF 
      
      
       ;(12) - Degree for lines fit; Window; 
;      IF i eq 11 then begin
;        ifu_curoption_base=widget_base(ifu_option_base,/row,/frame)
;        ;tmp=widget_label(ifu_curoption_base, val="Search and fit neon lines curvature: ")
;        names=["Ndeg:"]
;        val=[3]
;        sky_params=cw_field(ifu_curoption_base,xs=4,val=val[0],tit=names[0])
;      ENDIF
;       
      if showind[i] eq 1 then begin
        ifu_markershow_base=widget_base(ifu_option_base,/nonex,/col);,xs=sz[3].x-20)
        tmpobj=widget_button(ifu_markershow_base,value=names_show[i],uvalue="SetShow_"+add+string(i,format=form),/align_right);"Show Results"
        widget_control,tmpobj,set_button=1
        markers[i].showres=1
      endif
      
      
      ; (7) - additional button for showing spectra extraction
      if i eq 6 then begin
        ifu_markershow_base=widget_base(ifu_option_base,/nonex,/col);,xs=sz[3].x-20)
        tmpobj=widget_button(ifu_markershow_base,value="Show extraction",uvalue="show_exract",/align_right)
        widget_control,tmpobj,set_button=0
        spec_extra_show=0
      endif
      
      ; (16) - additional field for setting systemic velocity
      if i eq 15 then begin
        ifu_curoption_base=widget_base(ifu_option_base,/row,/frame)
        ;tmp=widget_label(ifu_curoption_base, val="Sys. velocity: ")
        sysvel_field=cw_field(ifu_curoption_base,xs=8,val=0.,tit='Sys. velocity: ')
      endif
      
    endfor
  
  ;#####################################
  
  
  log_field=WIDGET_text(ifu_engine_base, value=['LOG OF DATA REDUCTION:','**********************'],scr_xs=sz[4].x,scr_ys=sz[4].y,editable=0, /scroll)
  
  
  status_field=WIDGET_LABEL(ifu_engine_base, value="STATE:  Please, load log-file!    ", font=titfont)
  
  ifu_do_base=widget_base(ifu_engine_base,/row,/align_cent)
  ifu_viewbut=widget_button(ifu_do_base,uvalue="view_file",value="View",xs=sz[2].x,ys=sz[2].y)
  tmp=widget_label(ifu_do_base,value=" ",xs=sz[2].x/2.,ys=sz[2].y)
  ifu_runbut=widget_button(ifu_do_base,uvalue="run",value="RUN",xs=sz[2].x,ys=sz[2].y)
  ifu_quitbut=widget_button(ifu_do_base,uvalue="quit",value="QUIT",xs=sz[2].x,ys=sz[2].y)
  
  
  cgCENTERTLB,ifu_mb
  WIDGET_CONTROL, ifu_mb, /realize,group_leader=ifu_mb
  XMANAGER,'IFURED_GO', ifu_mb,no_block=1

END