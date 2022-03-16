; Combining several exposures
PRO IFURED_COMBINE,type,d_dir=d_dir, w_dir=w_dir, night=night, cub=cub, file_in=file_in,ref_frame=ref_frame,show=show,immask=immask,xlim=xlim,$
                  crop=crop,  t=t,zip=zip,slit_break_left=slit_break_left,slit_break_right=slit_break_right, logfield=logfield, no_cr=no_cr,mask_y=mask_y


if not(keyword_set(T)) then T=10



bias_mean=READFITS(w_dir+'meanbias.fts',/SILENT)
  a=size(bias_mean)
  nx=a[1] & ny=a[2]
  
  
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
 ENDIF


  file=file_in
  
  names=ifured_filename(night=night, cub=cub, file=file, ext=".fts")
  dir=slash(d_dir)+slash(night+cub)
  n_image=n_elements(file)
  if keyword_set(zip) and not file_test(dir,/dir) then IFURED_UNZIP_CUB, d_dir, night, cub



if type eq 'neon' or type eq 'obj' then shiftX=1 else shiftX=0

shiftY=1

if n_elements(xlim) ne 4 then xlim=0 
ylim=0;[750,1300];0


 fileout=type+'_i.fts'
 

 IF keyword_set(logfield) then IFURED_LOG_UPD, "  *** Working on "+strupcase(type)+" frames ***"
 
 
 IFURED_addexp,names,fileout,bias='meanbias.fts',d_dir=dir,w_dir=w_dir,t=t,ref_frame=ref_frame,/logfield,mask_y=mask_y,filetest="ch_test_"+type+".fts",immask=immask,$
       cut=cut,nonor=0,shiftx=shiftX,shiftY=shiftY,ylim=ylim,xlim=xlim,win=5,separate=[slit_break_left,slit_break_right],show=show,objtype=type, no_cr=no_cr


 if keyword_set(logfield) then IFURED_LOG_UPD, " "
 
   


end

