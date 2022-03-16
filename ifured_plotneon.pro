pro ifured_plotNeon

  ;Find peaks at central etalon & plot reference neon spectrum
  dirforifu="~/Science/IDLWorkspace/IFURED/"
  w_dir='/Users/mors/Science/IFU/Reduction/Mrk534/';"/Users/mors/Science/IFU/Reduction/Mrk938_0/";"/Volumes/iData/Science/IFU/Reduction/mkn78_nov17_v940@600/"
  lam_beg=4700
  lam_end=8800
  lam_res=0.9
  
  
  G_CORRECTED=1
  
  
  
    IF G_CORRECTED THEN BEGIN
      neon_ini=readfits(w_dir+'neon_warp.fts',hneon)
      flat_ini=readfits(w_dir+'flat_warp.fts',hflat)
    ENDIF ELSE BEGIN
      neon_ini=readfits(w_dir+'neon_i.fts',hneon)
      flat_ini=readfits(w_dir+'flat_i.fts',hflat)
    ENDELSE
    s=sxpar(hneon,'binning')
    disperser=sxpar(hneon,"DISPERSE")

  
  ;**** This part is taken from IFURED_DISPERSION_CURVE and compares observed neon vector with table
   filetab=dirforifu+'lines.tab'

      ;read table of lines comparison spectrum
      table=read_asc(filetab,0)
      lines0=table[0,*]
      intensity0=table[2,*]
      
      ; preliminary dispersion determination
      s=strsplit(disperser,' ',/extr)
      dispname=strupcase(s(0))
      
        dname=['300','400','550','600','940','1200','1720','1800','2310','2300','3000']
        Ngr=0
        for i=0,n_elements(dname)-1 do begin
         if strpos(dispname,dname[i]) gt -1 then Ngr=fix(dname[i])
        endfor
        lev=(4-round(Ngr/600.))>1
      
          
          index=WHERE(lines0 gt lam_beg and lines0 lt lam_end,N_lines)
          lines=lines0[index]
          intensity=intensity0[index]
          
      
      
      
      
      
      file_traectory=w_dir+"traectory.fit"
      traectory=read_asc(file_traectory)
      IF G_CORRECTED THEN BEGIN
        etapos=read_asc(w_dir+"eta_pos_warp.fit")
      ENDIF
      
      win=5
      nx_ini=sxpar(hneon,"NAXIS1")
      ny=sxpar(hneon,"NAXIS2")
      x_ini=indgen(nx_ini)
      
      
      
      cgps_open,w_dir+"test_neonlines.eps",/encaps,xs=10,ys=25
      
      
          
      lam=findgen(round((lam_end-lam_beg)/lam_res))*lam_res+lam_beg
        gau=lam*0
        for i=0,n_lines-1 do gau+=gaussian(lam,[intensity[i],lines[i],2.2])
        cgplot,lam,gau,yr=[-0.1,max(gau)+1],xst=1,xr=minmax(lam),yst=1,layout=[1,3,1]
        for i=0,n_lines-1 do cgtext,lines[i],intensity[i]+0.3,string(lines[i],format="(F7.2)"),charsize=0.9,orientation=90.
       
       
       
       
       FOR slit=0,1 DO BEGIN
          neon=reform(neon_ini[*,*,slit])
          flat=reform(flat_ini[*,*,slit])
          break_slit=max(where(finite(reform(neon[*,ny/2,0]))))
          x_rec=where(x_ini le break_slit,nx)
          x=x_ini[x_rec]
          
          rec_tra=where(traectory[3,*] eq slit,n_fibers)
          tra=traectory[*,rec_tra]
          
;          IF G_CORRECTED THEN BEGIN
;            rec_tra_gc=where(etapos[1,*] eq slit
;          ENDIF
          
          cur_fib=n_fibers/2
          cur_track=poly(x,tra[0:2,cur_fib])
          vector=fltarr(nx)
          vec_flat=fltarr(nx)
          for i=0,nx-1 do begin
              cut=findgen(2*win+1)+round(cur_track[i])-win
              cut_rec=where(cut ge 0 and cut le ny-1,ncut)
              if ncut gt 0 then begin
                cut=cut[cut_rec]
                vector[i]=total(neon[x[i],cut],2)/ncut+8
                vec_flat[i]=total(flat[x[i],cut],2)/ncut
                
              endif
            endfor
            if keyword_set(flat) then vector=vector/(vec_flat/median(vec_flat))
      
          pks= IFURED_FIND_PEAKS(vector,win=4, /find);find_peaks(vector,w=5);
          pks_val=vector[round(pks)]
          pix_pos0=pks
          pix_pos1=pks
          n_pos_out=n_elements(pix_pos0)
          pos_x_out=fltarr(n_pos_out,n_fibers)
          pos_y_out=fltarr(n_pos_out,n_fibers)
          for m=0,n_pos_out-1 do pos_y_out[m,cur_fib]=poly(pix_pos0[m],tra[0:2,cur_fib])
          pos_x_out[*,cur_fib]=pix_pos0
          ref_vector0=vector/max(vector[nx/4:3*nx/4],/nan)
          ref_vector1=ref_vector0
            cgplot,x,vector,yr=[-0.05*max(vector,/nan),max(vector,/nan)*1.05],xr=[0,2100],/xst,/yst,layout=[1,3,slit+2]
            cgoplot,pks,pks_val,ps=4,col="red"
            cgtext,pks,1.05*pks_val,string(pks,format="(F7.2)"),charsize=0.9,orientation=90
          
      ENDFOR
       
       
       
       
      cgps_close 
END