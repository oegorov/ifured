PRO IFURED_GEOMETRY_EXCLUDE_BADLINES,x,y, num_good=num_good
  
  s=size(x)
  good=intarr(s[1])
  for k=0,s[1]-1 do begin
    rec=where(x[k,*] ne 0 and y[k,*] ne 0,nr)
    if nr ge 5 then good[k]=1
  endfor
  rec=where(good eq 1, num_good)
  if num_good gt 0 then begin
    x=x[rec,*]
    y=y[rec,*]
  endif
return
END


PRO IFURED_GEOMETRY_NEON_FIND, neon, x, tra, win=win, flat=flat, neon_tab=neon_tab, pos_tab=pos_tab, wx=wx, bad_fibers=bad_fibers, $
                            pos_x_out=pos_x_out, pos_y_out=pos_y_out, PLOT_NEON=PLOT_NEON,wid=wid, disperse=disperse
  
    if not keyword_set(win) then win=2
    if not keyword_set(wid) then wid=0
    s=size(tra)
    n_fibers=s[2]
    nx=n_elements(x)
    s=size(neon)
    ny=s[2]
    if not keyword_set(wx) then wx=10
    
    if keyword_set(PLOT_NEON) Then cgdisplay,wid=wid,xs=1200
    
    fib0=n_fibers/2
    FOR dj=0,ceil(n_fibers/2) do begin
      FOR iter=0,1 DO BEGIN
        cur_fib=fib0+dj*(-1)^iter
        if n_elements(bad_fibers) gt 0 then begin
          rrr=where(bad_fibers eq cur_fib,this_bad)
          if this_bad ne 0 then continue
        endif
        if cur_fib lt 0 or cur_fib ge n_fibers or (dj eq 0 and iter eq 1) then continue
        vector=fltarr(nx)
        if keyword_set(flat) then vec_flat=fltarr(nx)
        cur_track=poly(x,tra[0:2,cur_fib])
        for i=0,nx-1 do begin
          cut=findgen(2*win+1)+round(cur_track[i])-win
          cut_rec=where(cut ge 0 and cut le ny-1,ncut)
          if ncut gt 0 then begin
            cut=cut[cut_rec]
            vector[i]=total(neon[x[i],cut],2)/ncut
            if keyword_set(flat) then vec_flat[i]=total(flat[x[i],cut],2)/ncut
          endif
        endfor
        if keyword_set(flat) then vector=vector/(vec_flat/median(vec_flat))
        
        
        
        DO_MAIN=1
        DO_SEARCH=0
        SET_TABLE=0
        
        
        if dj eq 0 then begin
          IF keyword_set(neon_tab) and keyword_set(pos_tab) then begin
            SET_TABLE=1
            DO_MAIN=1
          endif else begin
            DO_SEARCH=1
            DO_MAIN=0
          endelse
        endif 
        
        
        
        vector_smo=median([reverse(vector),vector,reverse(vector)],500)
        vector=vector-vector_smo[nx:2*nx-1]
        
        
        IF SET_TABLE then begin
          ref_vector0=neon_tab
          ref_vector1=vector/max(vector[nx/4:3*nx/4],/nan)
          pix_pos0=pos_tab
          max_shift=200
          n_pos_out=n_elements(pos_tab)
          pos_x_out=fltarr(n_pos_out,n_fibers)
          pos_y_out=fltarr(n_pos_out,n_fibers)
        ENDIF else max_shift=2*wx;200
        
        IF DO_SEARCH THEN BEGIN
          pks= IFURED_FIND_PEAKS(vector,win=5, /find,/robo);find_peaks(vector,w=5);
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
          if keyword_set(PLOT_NEON) then begin
            cgplot,x,vector,yr=[-0.05*max(vector,/nan),8],xr=[0,2100],/xst,/yst;1.1*max(vector)],xst=1,yst=1
            cgoplot,pks,pks_val,ps=4,col="red"
            cgtext,pks,1.05*pks_val,string(pks,format="(F7.2)"),charsize=0.9,orientation=90
          endif
        ENDIF
        
        IF DO_MAIN THEN BEGIN
          if iter eq 0 then pix_pos=pix_pos0 else pix_pos=pix_pos1
          if iter eq 0 then ref_vector=ref_vector0 else ref_vector=ref_vector1
          reg=[4*nx/10,6*nx/10]
          dx=def_vecshift(ref_vector[reg[0]:reg[1]],vector[reg[0]:reg[1]],bin=10,M=max_shift)
          pos_tmp=fltarr(N_pos_out)
          for k=0,N_pos_out-1 do begin
              bad_val=0
              p0=round(pix_pos[k]-wx+dx) > 0
              p1=round(pix_pos[k]+wx+dx) < (nx-1)
              if p0 ge nx-1 or p1 le 0 then bad_val=1 else begin 
                 yy=vector[p0:p1]
                 xx=x[p0:p1]
                 rec=where(finite(yy),nr)
                 if nr lt 5 then bad_val=1 else begin 
                   gau=gaussfit(xx[rec],yy[rec],G,Nterms=4)
                   pos=G[1]
                   if abs(pos-pix_pos[k]-dx) gt 10 then bad_val=1
                 endelse 
              endelse
              if bad_val ne 1 then pos_tmp[k]=pos
          endfor
          
          IF SET_TABLE THEN BEGIN ;### FOR CENTRAL FIBER
            pix_pos0=pix_pos+dx
            pix_pos1=pix_pos0
          ENDIF
          
          if iter eq 0 then pix_pos0=pix_pos+dx else pix_pos1=pix_pos+dx
          rec=where(pos_tmp gt 0,nr)
          if nr gt 0 then begin
             if iter eq 0 then  pix_pos0[rec]=pos_tmp[rec] else pix_pos1[rec]=pos_tmp[rec]
             if SET_TABLE THEN pix_pos1[rec]=pos_tmp[rec] ;### FOR CENTRAL FIBER
             pos_x_out[rec,cur_fib]=pos_tmp[rec]
             pos_y_out[rec,cur_fib]=poly(pos_tmp[rec],tra[0:2,cur_fib])
             if iter eq 0 then ref_vector0=vector/max(vector[nx/4:3*nx/4],/nan) else ref_vector1=vector/max(vector[nx/4:3*nx/4],/nan)
          endif
          
        ENDIF  
        
      ENDFOR
    ENDFOR
  return

end







pro IFURED_GEOMETRY, fileneon=fileneon, filestar=filestar,show=show,logfield=logfield,fit_deg=fit_deg, win=win,dirforifu=dirforifu,wx=wx,wy=wy,$
                      lines=lines,tra=tra,fileflat=fileflat, fileeta=fileeta, w_dir=w_dir,fileobj=fileobj,filesky=filesky, night=night, $
                      bad_fibers_lt=bad_fibers_lt, bad_fibers_rt=bad_fibers_rt,set_center=set_center, shi_tab=shi_tab
                    
    if not KEYWORD_SET(WIN) then win=30 ;win for intersection search
    if not KEYWORD_SET(w_dir) then w_dir=''
    if not KEYWORD_SET(wx) then wx=20 ; win along x for lines center search
    if not KEYWORD_SET(wy) then wy=6 ; win along y for lines center search
    if not keyword_set(shi_tab) then shi_tab=0
    wy=wy/2.
    wx=wx/2.
    
    ;add_shift=[400,40]
    
    PLOT_NEON=0
    IF KEYWORD_SET(show) THEN BEGIN
      SHOW_IMAGE=1
      SHOW_CORRECTED=1
    ENDIF ELSE BEGIN
      SHOW_IMAGE=0
      SHOW_CORRECTED=0
    ENDELSE
    ;SET_CENTER=1
    
    
    rec_rt=where(tra[3,*] eq 1,n_fibers_rt)
    tra_rt=tra[*,rec_rt]
    rec_lt=where(tra[3,*] eq 0 ,n_fibers_lt)
    tra_lt=tra[*,rec_lt]
    
    
    eta=readfits(w_dir+fileeta,heta,/sil)
    neon=readfits(w_dir+fileneon,hneon,/sil)
    flat=readfits(w_dir+fileflat,hflat,/sil)
    obj=readfits(w_dir+fileobj,hobj,/sil)
    s=sxpar(hneon,'binning')
    bin=float(strmid(strtrim(s,2),0,1)) ; binning
    if keyword_set(filestar) then star=readfits(w_dir+filestar,hstar,/sil)
    if keyword_set(filesky) then sky=readfits(w_dir+filesky,hsky,/sil)
    
    objname=sxpar(hobj,"OBJECT")
    disperser=sxpar(hneon,"DISPERSE")
    
    s=size(neon)
    nx=s[1]
    ny=s[2]
    x=indgen(nx)
    y=indgen(ny)
    
    break_lt=max(where(finite(reform(neon[*,ny/2,0]))))
    break_rt=max(where(finite(reform(neon[*,ny/2,1]))))
    
    x_lt_rec=where(x le break_lt,nx_lt)
    x_lt=x[x_lt_rec]
    x_rt_rec=where(x le break_rt,nx_rt)
    x_rt=x[x_rt_rec]
    s=size(lines)
    n_lines=s[2]
    
        
    y=indgen(ny)
    
    neon_tab_lt=0
    neon_tab_rt=0
    pos_tab_lt=0
    pos_tab_rt=0
    
    IF KEYWORD_SET(SET_CENTER) THEN begin
      ;#### Get neon table as reference to search for neon lines
      ;**** This part is taken from IFURED_DISPERSION_CURVE and compares observed neon vector with table
      filetab=dirforifu+'lines.tab'

      ;read table of lines comparison spectrum
      table=read_asc(filetab,0)
      lines0=table[0,*]
      intensity0=table[2,*]
      
      ; preliminary dispersion determination
      s=strsplit(disperser,' ',/extr)
      dispname=strupcase(s(0))
      
      filedis=dirforifu+slash("Grisms")+dispname+'.txt'
      IF file_test(filedis) eq 0 then begin
         message,'The dipserser '+dispname+' is unknown, File is not found: '+filedis,/cont
         if keyword_set(logfield) then IFURED_LOG_UPD, ['File with line pos. for dipserser '+dispname+' is not found', 'Use automatic detection algoritm']
      ENDIF ELSE BEGIN
        disp_tab=read_asc(filedis,2)
        dname=['300','400','550','600','940','1200','1720','1800','2310','2300','3000']
        Ngr=0
        for i=0,n_elements(dname)-1 do begin
         if strpos(dispname,dname[i]) gt -1 then Ngr=fix(dname[i])
        endfor
        lev=(4-round(Ngr/600.))>1
      
        FOR slitnum=0,1 DO BEGIN
          if slitnum eq 0 then nx_cur=nx_lt else nx_cur=nx_rt
          rec=where(disp_tab[2,*] eq slitnum)
          wav=reform(disp_tab[1,rec])
          pos=reform(disp_tab[0,rec])
          c_prelim=poly_fit(wav,pos,3) ; preliminary conversion to wavelength
          c_revers=poly_fit(pos,wav,3) ; preliminary conversion to wavelength
          tresh=3.5
          
          ; preliminary lines position
          lambda_beg=poly(-5,c_revers)
          lambda_end=poly(Nx_cur-20,c_revers)
          
          index=WHERE(lines0 gt lambda_beg and lines0 lt lambda_end,N_lines)
          lines=lines0[index]
          intensity=intensity0[index]
          
          ;creation full table spectrum in observed coverage
          pos_previus=poly(lines,c_prelim)
          ;creation tabulated comparison spectra
          x=findgen(Nx_cur)
          neon_tab=fltarr(Nx_cur)
          fwhm=4.
          for k=0,N_lines-1 do neon_tab=neon_tab+gaussian(x,[intensity[k],pos_previus[k],fwhm/2.35])
          neon_tab=neon_tab/max(neon_tab)
          if slitnum eq 1 then begin
            neon_tab_rt=neon_tab 
            pos_tab_rt=pos-(shi_tab)
          endif else begin
            neon_tab_lt=neon_tab
            pos_tab_lt=pos
          endelse
        ENDFOR
      ENDELSE
    ENDIF
    
    
    ;*** Search for xy positions of the lines and traectories crossing ***
    IFURED_GEOMETRY_NEON_FIND,reform(neon[*,*,0]), x_lt, tra_lt,flat=flat[*,*,0], neon_tab=neon_tab_lt,pos_tab=pos_tab_lt, $
                                 pos_x_out=lines_x_lt, pos_y_out=lines_y_lt,PLOT_NEON=plot_neon,wid=1,win=wy,wx=wx, bad_fibers=bad_fibers_lt
    IFURED_GEOMETRY_NEON_FIND,reform(neon[*,*,1]), x_rt, tra_rt,flat=flat[*,*,1], neon_tab=neon_tab_rt,pos_tab=pos_tab_rt,$
                           pos_x_out=lines_x_rt, pos_y_out=lines_y_rt,PLOT_NEON=plot_neon,wid=2, wx=wx,win=wy, bad_fibers=bad_fibers_rt
    
    ;### Select only lines with at least 5 detected peaks along the slit
    IFURED_GEOMETRY_EXCLUDE_BADLINES,lines_x_lt,lines_y_lt, num_good=n_lines_fit_lt
    IFURED_GEOMETRY_EXCLUDE_BADLINES,lines_x_rt,lines_y_rt, num_good=n_lines_fit_rt
    if n_lines_fit_rt lt 5 or n_lines_fit_lt lt 5 then begin
      res=dialog_message("Can't find enough number of good lines...")
      return
    endif
    
    lines_fit_deg=fit_deg
    lines_fit_lt=fltarr(lines_fit_deg+1,n_lines_fit_lt)
    lines_fit_rt=fltarr(lines_fit_deg+1,n_lines_fit_rt)
    
    
    ;### Fit each line with polynome
    for k=0,n_lines_fit_lt-1 do lines_fit_lt[*,k]=robust_poly_fit(lines_y_lt[k,where(lines_y_lt[k,*] ne 0 and lines_x_lt[k,*] ne 0)],$
                                                                  lines_x_lt[k,where(lines_y_lt[k,*] ne 0 and lines_x_lt[k,*] ne 0)],lines_fit_deg)
    
    for k=0,n_lines_fit_rt-1 do lines_fit_rt[*,k]=robust_poly_fit(lines_y_rt[k,where(lines_y_rt[k,*] ne 0 and lines_x_rt[k,*] ne 0)],$
                                                                  lines_x_rt[k,where(lines_y_rt[k,*] ne 0 and lines_x_rt[k,*] ne 0)],lines_fit_deg)
    
    if keyword_set(logfield) then IFURED_LOG_UPD, ["Found "+string(n_lines_fit_lt,format="(I3)")+" lines on left frame",$
                                          "  and "+string(n_lines_fit_rt,format="(I3)")+ " lines on right frame"]
    
    ;### Extend grid
      ;### Add left and right borders
      shift_x=50
      min_shi_lt=max([shift_x,nx_lt-min(lines_x_lt[n_lines_fit_lt-1,where(lines_y_lt[n_lines_fit_lt-1,*] ne 0 and lines_x_lt[n_lines_fit_lt-1,*] ne 0)]),$
                     max(lines_x_lt[0,where(lines_y_lt[0,*] ne 0 and lines_x_lt[0,*] ne 0)])])
      min_shi_rt=max([shift_x,nx_rt-min(lines_x_rt[n_lines_fit_rt-1,where(lines_y_rt[n_lines_fit_rt-1,*] ne 0 and lines_x_rt[n_lines_fit_rt-1,*] ne 0)]),$
                     max(lines_x_rt[0,where(lines_y_rt[0,*] ne 0 and lines_x_rt[0,*] ne 0)])])
      add_shift_lt=[min_shi_lt,30]
      add_shift_rt=[min_shi_rt,30]               
;      if add_shift_lt[0] gt shift_x then n_addlines_lt=2 else n_addlines_lt=1
;      if add_shift_rt[0] gt shift_x then n_addlines_rt=2 else n_addlines_rt=1
      n_addlines_lt=1
      n_addlines_rt=1
      
      lines_ext_lt=fltarr(lines_fit_deg+1,n_lines_fit_lt+n_addlines_lt*2)
      lines_ext_rt=fltarr(lines_fit_deg+1,n_lines_fit_rt+n_addlines_rt*2)
      n_lines_ext_lt=n_lines_fit_lt+n_addlines_lt*2
      n_lines_ext_rt=n_lines_fit_rt+n_addlines_rt*2
      
      xc_lt=fltarr(n_lines_fit_lt)
      xc_rt=fltarr(n_lines_fit_rt)
      for i=0,n_lines_fit_lt-1 do xc_lt[i]=total(poly(y,lines_fit_lt[*,i]))/ny
      for i=0,n_lines_fit_rt-1 do xc_rt[i]=total(poly(y,lines_fit_rt[*,i]))/ny
      
      for i=0,lines_fit_deg do begin
        
        res=robust_poly_fit(xc_lt[*],lines_fit_lt[i,*],1)
        lines_ext_lt[i,0]=poly(xc_lt[0]-add_shift_lt[0],res)
        ;if add_shift_lt[0] gt shift_x then lines_ext_lt[i,1]=poly(xc_lt[0]-add_shift_lt[0],res)
        lines_ext_lt[i,n_lines_fit_lt+n_addlines_lt]=poly(xc_lt[n_lines_fit_lt-1]+add_shift_lt[0],res)
        ;if add_shift_lt[0] gt shift_x then lines_ext_lt[i,n_lines_fit_lt+n_addlines_lt+1]=poly(xc_lt[n_lines_fit_lt-1]+add_shift_lt[0],res)
        lines_ext_lt[i,n_addlines_lt:n_lines_fit_lt-1+n_addlines_lt]=lines_fit_lt[i,0:n_lines_fit_lt-1]
        res=robust_poly_fit(xc_rt[*],lines_fit_rt[i,*],1)
        lines_ext_rt[i,n_lines_fit_rt+n_addlines_lt]=poly(xc_rt[n_lines_fit_rt-1]+add_shift_rt[0],res)
        lines_ext_rt[i,0]=poly(xc_rt[0]-add_shift_rt[0],res)
        ;if add_shift_rt[0] gt shift_x then lines_ext_rt[i,1]=poly(xc_rt[0]-add_shift_rt[0],res)
        lines_ext_rt[i,n_addlines_rt:n_lines_fit_rt-1+n_addlines_rt]=lines_fit_rt[i,0:n_lines_fit_rt-1]
        ;if add_shift_rt[0] gt shift_x then lines_ext_rt[i,n_lines_fit_rt+n_addlines_rt+1]=poly(xc_rt[n_lines_fit_rt-1]+add_shift_rt[0],res)
      endfor    
      
      
      ;### Add top and bottom borders
      tra_ext_lt=fltarr(3,n_fibers_lt+2)
      tra_ext_rt=fltarr(3,n_fibers_rt+2)
      n_fibers_ext_lt=n_fibers_lt+2
      n_fibers_ext_rt=n_fibers_rt+2
      shift_y=add_shift_lt[1]
      yc_lt=fltarr(n_fibers_lt)
      yc_rt=fltarr(n_fibers_rt)
      for i=0,n_fibers_lt-1 do yc_lt[i]=total(poly(x_lt,tra_lt[0:2,i]))/nx_lt
      for i=0,n_fibers_rt-1 do yc_rt[i]=total(poly(x_rt,tra_rt[0:2,i]))/nx_rt
      
      for i=0,2 do begin
        
        res=robust_poly_fit(yc_lt[*],tra_lt[i,*],1)
        tra_ext_lt[i,0]=poly(yc_lt[0]-shift_y,res)
        tra_ext_lt[i,n_fibers_lt+1]=poly(yc_lt[n_fibers_lt-1]+shift_y,res)
        tra_ext_lt[i,1:n_fibers_lt]=tra_lt[i,0:n_fibers_lt-1]
        res=robust_poly_fit(yc_rt[*],tra_rt[i,*],1)
        tra_ext_rt[i,n_fibers_rt+1]=poly(yc_rt[n_fibers_rt-1]+shift_y,res)
        tra_ext_rt[i,0]=poly(yc_rt[0]-shift_y,res)
        tra_ext_rt[i,1:n_fibers_rt]=tra_rt[i,0:n_fibers_rt-1]
      endfor    
      
      compar=[-min(poly(y,lines_ext_lt[*,0])),$
              max(poly(y,lines_ext_lt[*,n_lines_fit_lt+n_addlines_lt]))-ny+1,$
              -min(poly(y,lines_ext_rt[*,0])),$
              max(poly(y,lines_ext_rt[*,n_lines_fit_rt+n_addlines_rt]))-ny+1]
              
      ;if n_addlines eq 2 then compar=[compar,-min(poly(y,lines_ext_lt[*,1])), max(poly(y,lines_ext_rt[*,n_lines_fit_rt+n_addlines+1]))-ny+1]
;      if n_addlines_lt eq 2 then compar=[compar,-min(poly(y,lines_ext_lt[*,1])), max(poly(y,lines_ext_lt[*,n_lines_fit_lt+n_addlines_lt+1]))-ny+1]
;      if n_addlines_rt eq 2 then compar=[compar,-min(poly(y,lines_ext_rt[*,1])), max(poly(y,lines_ext_rt[*,n_lines_fit_rt+n_addlines_rt+1]))-ny+1]
      shift_x=max(compar)
      if shift_x le 0 then shift_x=0 else shift_X=ceil(shift_x)
              shift_y=max([-min(poly(x_lt,tra_ext_lt[*,0])),$
              max(poly(x_lt,tra_ext_lt[*,n_fibers_lt]))-nx_lt+1,$
              -min(poly(x_rt,tra_ext_rt[*,0])),$
              max(poly(x_rt,tra_ext_rt[*,n_fibers_rt]))-nx_rt+1])
      if shift_y le 0 then shift_y=0 else shift_y=ceil(shift_y)
      
      
      if shift_x gt 0 then begin
        xadd_rt=(indgen(shift_x)+1)
        xadd_lt=-xadd_rt
        xadd_lt=xadd_lt[sort(xadd_lt)]
        xlt_extended=[xadd_lt+x_lt[0],x_lt,xadd_rt+x_lt[nx_lt-1]]
        xrt_extended=[xadd_lt+x_rt[0],x_rt,xadd_rt+x_rt[nx_rt-1]]
      endif else begin
        xlt_extended=x_lt
        xrt_extended=x_rt
      endelse
      if shift_y gt 0 then begin
        yadd_up=(indgen(shift_y)+1)
        yadd_dn=-yadd_up
        yadd_dn=yadd_dn[sort(yadd_dn)]
        y_extended=[yadd_dn+y[0],y,yadd_up+y[ny-1]]
      endif else y_extended=y
      
    neon_lt=fltarr(nx_lt+shift_x*2,ny+shift_y*2)
    neon_lt[shift_x:nx_lt-1+shift_x,shift_y:ny-1+shift_y]=reform(neon[x_lt,*,0])
    neon_rt=fltarr(nx_rt+shift_x*2,ny+shift_y*2)
    neon_rt[shift_x:nx_rt-1+shift_x,shift_y:ny-1+shift_y]=reform(neon[x_rt,*,1])
    
    repers_lt=ifured_find_repers(lines_ext_lt,tra_ext_lt,w=win,x=xlt_extended,y=y_extended)
    repers_rt=ifured_find_repers(lines_ext_rt,tra_ext_rt,w=win,x=xrt_extended,y=y_extended)
    
    
    repers_lt=reform(repers_lt,4,n_lines_ext_lt*n_fibers_ext_lt)
    repers_rt=reform(repers_rt,4,n_lines_ext_rt*n_fibers_ext_rt)
    
    repers_lt[0,*]+=shift_x
    repers_lt[1,*]+=shift_y
    repers_rt[0,*]+=shift_x
    repers_rt[1,*]+=shift_y
    repers_lt[2,*]-=min(repers_lt[2,*]);+shift_x)
    repers_rt[2,*]-=min(repers_rt[2,*]);+shift_x)
    mnrt=min(repers_rt[3,*])
    mnlt=min(repers_lt[3,*])
    repers_rt[3,*]-=mnrt;+shift_y)
    repers_lt[3,*]-=mnlt;+shift_y)
    
    new_nx_lt=round(max(repers_lt[2,*]))
    new_nx_rt=round(max(repers_rt[2,*]))
    new_ny=round(max([reform(repers_rt[3,*]),reform(repers_lt[3,*])]))
    
    rec_l=where(repers_lt[0,*] gt -1e6,nrl);where(repers_lt[0,*] gt xlt_extended[0] and repers_lt[1,*] gt y_extended[0],nrl)
    rec_r=where(repers_rt[0,*] gt -1e6,nrr);where(repers_rt[0,*] gt xrt_extended[0] and repers_rt[1,*] gt y_extended[0],nrr) 
    
    ;#### Write warp repers output
    openw,u,w_dir+"warp.fit",/get_lun
    printf,u,shift_x,shift_y,new_nx_lt,new_nx_rt,new_ny, format="(I0, 2X, I0, 2X, I0, 2X, I0, 2X, I0)"
    for i=0,nrl-1 do printf,u,repers_lt[0,rec_l[i]], repers_lt[1,rec_l[i]],repers_lt[2,rec_l[i]],repers_lt[3,rec_l[i]], 0, format="(F10.4, 2X, F10.4, 2X, F10.4, 2X, F10.4, 2X, I1)"
    for i=0,nrr-1 do printf,u,repers_rt[0,rec_r[i]], repers_rt[1,rec_r[i]],repers_rt[2,rec_r[i]],repers_rt[3,rec_r[i]], 1, format="(F10.4, 2X, F10.4, 2X, F10.4, 2X, F10.4, 2X, I1)"
    close,u
    free_lun,u   
    
    
    
    
    
;    ;#### Write eta-fibers positions after warp
    openw,u,w_dir+"eta_pos_warp.fit",/get_lun
    for i=1,n_fibers_ext_lt-2 do printf,u,repers_lt[3,i*n_lines_ext_lt], 0, format="(F9.4, 2X, I1)"
    for i=1,n_fibers_ext_rt-2 do printf,u,repers_rt[3,i*n_lines_ext_rt], 1, format="(F9.4, 2X, I1)"
    close,u
    free_lun,u    
    
    
    
    
      ; #### Graphical output
    FOR plot_iter=0,1 DO BEGIN
      IF (KEYWORD_SET(SHOW_IMAGE) and plot_iter eq 1) or plot_iter eq 0 THEN BEGIN
      
      
          yr=[y_extended[0],y[ny-1]+1+shift_y];[-shift_y,ny+shift_y]
          imlt=neon_lt;congrid(neon_lt,(nx_lt+shift_x*2)/4,(ny+shift_y*2)/4);neon[0:break_lt-1,0:ny-1,0]
          imrt=neon_rt;congrid(neon_rt,(nx_rt+shift_x*2)/4,(ny+shift_y*2)/4);neon[0:break_rt-1,0:ny-1,1],nx_rt/4,(ny)/4)
          slt=size(imlt)
          srt=size(imrt)
          nxim=slt[1]+srt[1]
          nyim=slt[2]
          im_show=fltarr(nxim,nyim)
          im_show[0:slt[1]-1,*]=imlt
          im_show[slt[1]:nxim-1,*]=imrt
          rec=where(~finite(im_show),nr)
          if nr gt 0 then im_show[rec]=0
          s=size(im_show)
          ;im_show=cgresizeimage(im_show,1280,1280.*s[2]/s[1])
          pos0=[0.1,0.1,0.1+float(slt[1])/nxim*0.89,0.95]
          pos1=[0.1+float(slt[1])/nxim*0.89,0.1,0.1+float(slt[1])/nxim*0.89+float(srt[1])/nxim*0.89,0.95]
          pos=[pos0[0],pos0[1],pos1[2],pos1[3]]
          
      
        xs=30.
        IF plot_iter eq 0 then begin
        
        cgPS_open,w_dir+'geometry_tmp.eps',/encaps, xs=xs,ys=xs*nyim/nxim*1.3
          cgimage,sigrange(im_show-median(im_show),frac=0.9),pos=[0,0,1,1],/nega
        cgps_close,png=w_dir+'geometry_tmp.png',dens=250
        endif
        
        png_im=read_png(w_dir+'geometry_tmp.png')
        
      
        IF plot_iter eq 0 then cgPS_open,w_dir+'geometry.eps',charsize=0.8,default_thick=1.5,/encaps,xs=xs/3./(pos[2]-pos[0]),ys=xs/3./(pos[3]-pos[1])*nyim*1.3/nxim ELSE cgdisplay, wid=15, xs=1200/(pos[2]-pos[0]),ys=1200/(pos[3]-pos[1])*nyim/nxim , title="Uncorrected neon"
          print,xs/3./(pos[2]-pos[0]),xs/3./(pos[3]-pos[1])*nyim*1.3/nxim
          cgimage,png_im,pos=pos
;          cgimage,sigrange(im_show-median(im_show),frac=0.9),pos=pos,/nega;cgresizeimage(im_show-median(im_show),s[1]/4.,s[2]/4.)
          cgplot,0,0,/nodata,xst=5,yst=1, ytit="!8y!X, pix",yrange=yr,/noer,pos=pos,charsize=1.6
          cgplot,0,0,/nodata,xst=1,yst=5,xrange=[xlt_extended[0],x_lt[nx_lt-1]+1+shift_x],yrange=yr,/noer,pos=pos0,tit="Left slit", xtit="!8x!X, pix", charsize=1.6
          for i=0,n_fibers_lt-1 do cgoplot,x_lt,poly(x_lt,tra_ext_lt[*,i+1]),color='red',linest=0,thick=2;"BLK6"
          for k=0,n_lines_ext_lt-1 do cgoplot,poly(y,lines_ext_lt[*,k]),y,color='orange',linest=0,thick=2;col="BLK6"

;          cgoplot,lines_x_lt,lines_y_lt,col="green",psym=16
          cgoplot,repers_lt[0,rec_l]-shift_x,repers_lt[1,rec_l]-shift_y,psym=16,symsize=0.6,col='magenta';col="BLK6",
          
          cgtext,-50,2010,'(b)',/data,charsize=1.6
          
          cgplot,0,0,/nodata,xst=1,yst=5,xrange=[xrt_extended[0],x_rt[nx_rt-1]+1+shift_x],yrange=yr,/noer,pos=pos1,tit="Right slit", xtit="!8x!X, pix", charsize=1.6
          cgoplot,[xrt_extended[0],xrt_extended[0]],yr,color="black"
          for i=0,n_fibers_rt-1 do cgoplot,x_rt,poly(x_rt,tra_ext_rt[*,i+1]),color='red',thick=2,linest=0;color="BLK6"
          for k=0,n_lines_ext_rt-1 do cgoplot,poly(y,lines_ext_rt[*,k]),y,thick=2,color='orange',linest=0;col="BLK6"
          
          ;cgoplot,lines_x_rt,lines_y_rt,col="green",psym=16
          cgoplot,repers_rt[0,rec_r]-shift_x,repers_rt[1,rec_r]-shift_y,psym=16,symsize=0.6,col='magenta';col="BLK6",
          
;          cgtext,0.07+(pos[2]-pos[0])/5.,pos[3]+0.03,"Geometry correction. Neon frame of "+objname+", Night "+night+", Disperser "+disperser,/norm
        IF plot_iter eq 0 then cgps_close
        
      ENDIF
      
    ENDFOR
    file_delete,w_dir+'geometry_tmp.png'    
       
        
          
      
      IF KEYWORD_SET(PLOT_NEON) THEN BEGIN
    ;    ### NEON lines
        cgdisplay,wid=3,xs=1400,ys=850
        lam=findgen(6000)+3000
        gau=lam*0
        for i=0,n_lines-1 do gau+=gaussian(lam,[lines[1,i],lines[0,i],2.2])
        cgplot,lam,gau,yr=[-0.1,max(gau)+1],xst=1,xr=[4800,7800],yst=1
        for i=0,n_lines-1 do cgtext,lines[0,i],lines[1,i]+0.3,string(lines[0,i],format="(F7.2)"),charsize=0.9,orientation=90.
      ENDIF

    
END