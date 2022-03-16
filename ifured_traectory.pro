PRO IFURED_TRAECTORY, w_dir=w_dir,first_eta=first_eta, last_eta=last_eta, ifu_dir=ifu_dir, night=night,$
                      show=show, dspec=dspec, win=win,logfield=logfield

  
    ;##### Set input files ####
    file_obj="obj_i.fts"
    file_eta="eta_i.fts"
    file_flat="flat_i.fts"
    
    ;#### Output file ####
    file_traectory=w_dir+"traectory.fit"

    ; ### Borders for processing (relative from 0 and nx (ny) )
      x0_cut=100
      x1_cut=-50
      y0_cut=0
      y1_cut=0
    
    
    wid_tracks=11  ; window number for tracectory showing
    bgrem = 0 ; background removing
    if not keyword_set(first_eta) then first_eta=0 ; number of first etalon fiber
    if not keyword_set(last_eta) then last_eta=11 ; number of last etalon fiber
    npeaks_eta = last_eta-first_eta+1 ; numbers of eta peacks


    start_eta=0
    end_eta=npeaks_eta-1
;    if n_elements(start_eta) eq 0 then begin     ; Number of etalons peak after which fibers starts
;      if first_eta eq 0 then start_eta = 1 else start_eta=0
;    endif

;    if n_elements(end_eta) eq 0 then begin     ; Number of etalons peak after which fibers ends
;      if last_eta eq 13 then end_eta = npeaks_eta-2 else end_eta=npeaks_eta-1
;    endif

    if not keyword_set(dspec) then dspec=70   ; step along dispersion axis
    if not keyword_set(win) then ncolumns=10 else ncolumns=win ; window along dispersion axis for vector extraction

    IF KEYWORD_SET(show) THEN DISPLAY_RESULT=1 ELSE DISPLAY_RESULT=0
    
    
    
    
    ;### let's go!

      
    eta=readfits(w_dir+file_eta,h_eta)
    flat=readfits(w_dir+file_flat,h_flat)

    h_obj=headfits(w_dir+file_obj)
    objname=sxpar(h_obj,"OBJECT")
    disperser=sxpar(h_obj,"DISPERSE")
    IF NOT KEYWORD_SET(night) THEN night=""
    

;      ;#### Read master
    IFURED_READ_MASTER, master_eta, master_fib


;    if n_elements(start_eta) eq 0 then start_eta=1
;    if n_elements(end_eta) eq 0 then end_eta=npeaks_eta-2
    s=size(eta)
    nx=s[1]
    ny=s[2]
    spec0=x0_cut
    nspec=floor((nx-1+x1_cut-x0_cut)/dspec)
    xsc=spec0+findgen(nspec)*dspec
    
    slit_break_left=max(where(finite(reform(eta[*,ny/2,0]))))
    slit_break_right=max(where(finite(reform(eta[*,ny/2,1]))))
    show_borders=[slit_break_left,slit_break_right]
    
    if slit_break_right lt slit_break_left then slit_break_right+=x1_cut
    if slit_break_left lt slit_break_right then slit_break_left+=x1_cut
    x_reper_left=xsc[where(xsc le slit_break_left)]
    x_reper_right=xsc[where(xsc le slit_break_right)]
    
    ;### Background removing
;    if keyword_set(bgrem) then begin
;      for slit=0,1 do begin
;        bgr=estimate_background(reform(eta[*,*,slit]),50)
;        eta[*,*,slit]-=bgr
;      endfor
;     ; bgr=estimate_background(flat,50)
;     ; flat-=bgr
;    endif


    eta_fibs_nums=indgen(npeaks_eta)+first_eta

    npks_eta=intarr(nspec,2)
    peaks_eta=fltarr(nspec,npeaks_eta,2)
    rms_eta=fltarr(2,npeaks_eta)
    peaks_fib=fltarr(nspec,n_elements(master_fib[0,*,0]),2)
    num_fib=fltarr(nspec,n_elements(master_fib[0,*,0]),2)
    peaks_fib[*,*,*]=-1
    num_fib[*,*,*]=-1

    fit_eta_coeff=fltarr(nspec,2)

    for slit=0,1 do begin
    
      
      
      ;ifured_check_master, slit, eta=eta, flat=flat, [master_eta[*,eta_fibs_nums,slit]], [master_fib[*,*,slit]], nfib_eta=npeaks_eta, dspec=dspec
      master_eta_in=[master_eta[0,eta_fibs_nums,slit]]
      master_fib_in=[master_fib[0,*,slit]]
      
      for i=0,nspec-1 do begin
         if slit eq 0 and xsc[i] gt slit_break_left then break
         if slit eq 1 and xsc[i] gt slit_break_right then break
          ;if spec0+i*dspec ge slit_break_left and spec0+i*dspec le slit_break_right then continue
          nosmooth=1
          for j=0,1 do begin
            ; ##### Extraction of the vector along cross-dispersion with window along dispersion = wx ###
            vector=IFURED_CROSS_VECTOR(eta[*,*,slit],spec0+i*dspec,wx=ncolumns,nosmooth=nosmooth)
    
            ; ##### Trying to find NPEAKS peaks on vector ###
            pks_eta=IFURED_FIND_PEAKS(vector,win=6, npeaks=npeaks_eta,/robo,/find)
            npks_eta[i,slit]=n_elements(pks_eta)
            if j eq 0 and npks_eta[i,slit] ne npeaks_eta then nosmooth=0 else break
          endfor

          ; ##### If peaks found then compare with slit_eta ###
          if npks_eta[i,slit] eq npeaks_eta then begin
            ;peaks_fit=IFU_PEAKS_COMPARE(pks_eta,slit_in,max_shift=2)
            ;if n_elements(peaks_fit) eq 1 then npks_eta[i]=0 else begin
              ; ### IF eta peaks finally found
             ; fit_eta_coeff[i,*]=peaks_fit
              peaks_eta[i,*,slit]=pks_eta
    
              ; ### Trying to find peaks on flat
;              IFURED_PEAKS_FIBERS,reform(flat[*,*,slit]), master_eta_in,master_fib_in, pks_eta, $
;                               current_pos=spec0+i*dspec, current_win=5, peaks_out=pks_fib, fiber_num=nm_fib
;              peaks_fib[i,*,slit]=pks_fib
;              num_fib[i,*,slit]=nm_fib
            ;endelse
          endif
       endfor
    endfor


    rec_eta0=where(npks_eta[*,0] eq npeaks_eta,nrec_eta0)
    rec_eta1=where(npks_eta[*,1] eq npeaks_eta,nrec_eta1)
    if nrec_eta0 lt 3 then begin
      r=dialog_message("Searching of necessary number of peaks on ETA0 frames failed")
      return
    endif
    if nrec_eta1 lt 3 then begin
      r=dialog_message("Searching of necessary number of peaks on ETA1 frames failed")
      return
    endif
;    rl=where(xsc[rec_eta0] le slit_break_left,nrl)
;    rr=where(xsc[rec_eta1] le slit_break_right,nrr)


    ; #### Containers for determined fiber positions, fit results and parts of flat image
    nspec_lt=n_elements(x_reper_left)
    nspec_rt=n_elements(x_reper_right)
    
;    nfibers=max(num_fib)+1
    netalons=npeaks_eta
;    fibers_lt=fltarr(nspec_lt,nfibers)
    etalons_lt=fltarr(nspec_lt,netalons)
;    fibers_rt=fltarr(nspec_rt,nfibers)
    etalons_rt=fltarr(nspec_rt,netalons)
;    rms_fib=fltarr(2,nfibers)

;    subflats=ptrarr(end_eta-start_eta,2)
;    subflats_yrange=ptrarr(end_eta-start_eta,2)
;    x_pfib=ptrarr(end_eta-start_eta,2)
;    y_pfib=ptrarr(end_eta-start_eta,2)
;    n_pfib=ptrarr(end_eta-start_eta,2)

    ; ### OPEN file to out traectory
    openw,u,file_traectory,/get_lun    ; ### p0, p1, p2, isEta, isRight
    for j=0,npeaks_eta-1 do begin
      ; ### Interpolate eta peaks
        res=robust_poly_fit(xsc[rec_eta0],peaks_eta[rec_eta0,j,0],2,yfit,sig)
        rms_eta[0,j]=sig
        y_reper_eta_left=poly(x_reper_left,res)
        printf,u,[res,0],format='(3E17.5,I4)'
        etalons_lt[*,j]=y_reper_eta_left[*]
        
        res=robust_poly_fit(xsc[rec_eta1],peaks_eta[rec_eta1,j,1],2,yfit,sig)
        rms_eta[1,j]=sig
        y_reper_eta_right=poly(x_reper_right,res)
        printf,u,[res,1],format='(3E17.5,I4)'
        etalons_rt[*,j]=y_reper_eta_right[*]
    endfor



;#### Now I do not want to calculate fibers position on this step
;    for j=start_eta,end_eta-1 do begin
;      for slit=0,1 do begin
;        if slit eq 0 then begin
;          rec_eta=rec_eta0
;          nrec_eta=nrec_eta0
;        endif else begin
;          rec_eta=rec_eta1
;          nrec_eta=nrec_eta1
;        endelse
;        x_peaks_fib=[-1]
;        y_peaks_fib=[-1]
;        num_peaks_fib=[-1]
;        for i=0,nrec_eta-1 do begin
;          rec=where(peaks_fib[rec_eta[i],*,slit] ge peaks_eta[rec_eta[i],j,slit] and peaks_fib[rec_eta[i],*,slit] le peaks_eta[rec_eta[i],j+1,slit],nrec)
;  
;          rec_fib=where(peaks_fib[rec_eta[i],rec,slit] ne -1,nrec_fib)
;          x_peaks_fib=[x_peaks_fib,replicate(spec0+rec_eta[i]*dspec,nrec_fib)]
;          tmp=fltarr(nrec_fib)
;          tmp[*]=peaks_fib[rec_eta[i],rec[rec_fib[*]],slit]
;          y_peaks_fib=[y_peaks_fib,tmp]
;          tmp[*]=num_fib[rec_eta[i],rec[rec_fib[*]],slit]
;          num_peaks_fib=[num_peaks_fib,tmp]
;        endfor
;        n_p_f=n_elements(y_peaks_fib)-1
;        x_peaks_fib=x_peaks_fib[1:n_p_f]
;        y_peaks_fib=y_peaks_fib[1:n_p_f]
;        num_peaks_fib=num_peaks_fib[1:n_p_f]
;        yr=[min(floor(peaks_eta[rec_eta,j,slit])),max(ceil(peaks_eta[rec_eta,j+1,slit]))< ny]
;        subflats[j-start_eta,slit]=ptr_new(reform(flat[*,yr[0]:yr[1],slit]))
;        subflats_yrange[j-start_eta,slit]=ptr_new(yr)
;        x_pfib[j-start_eta,slit]=ptr_new(x_peaks_fib)
;        y_pfib[j-start_eta,slit]=ptr_new(y_peaks_fib)
;        n_pfib[j-start_eta,slit]=ptr_new(num_peaks_fib)
;        
;        for cur_fib=min(num_peaks_fib), max(num_peaks_fib) do begin
;          rec_cur_fiber_slit=where(num_peaks_fib eq cur_fib and $
;                                  x_peaks_fib ge x0_cut and x_peaks_fib le nx-1+x1_cut, nrec_cur_fiber)
;          
;          if nrec_cur_fiber gt 2 then begin
;             x_peaks_slit=x_peaks_fib[rec_cur_fiber_slit]
;             y_peaks_slit=y_peaks_fib[rec_cur_fiber_slit]
;  
;             res=robust_poly_fit(x_peaks_slit,y_peaks_slit,2,yfit,sig)
;             if slit eq 1 then y_reper_fib_slit=poly(x_reper_right,res) else y_reper_fib_slit=poly(x_reper_left,res)
;             printf,u,[res,0,slit],format='(3E17.5,2I4)'
;             rms_fib[slit,cur_fib]=sig
;             if slit eq 0 then fibers_lt[*,cur_fib]=y_reper_fib_slit[*] else fibers_rt[*,cur_fib]=y_reper_fib_slit[*]
;          endif
;
;        endfor
;      endfor
;
;    endfor

    close,u
    free_lun,u


; ###### Display results ####

    FOR plot_iter=0,1 DO BEGIN
      IF NOT KEYWORD_SET(DISPLAY_RESULT) AND plot_iter eq 1 THEN BREAK
      ;### Make display and show eta and flat images with peaks positions
      IF plot_iter eq 0 then cgPS_open, w_dir+"traectory_eta.eps",charsize=0.8,/encaps,xs=11.236,ys=7.76171 else BEGIN
        cgdisplay,wid=wid_tracks,xs=1200
        last_wid=wid_tracks
      ENDELSE
        loadct,0,/sil
        
        
        imlt=congrid(eta[0:show_borders[0],*,0],(show_borders[0]+1)/2,ny/2,/interp,/cent)
        imrt=congrid(eta[0:show_borders[1],*,1],(show_borders[1]+1)/2,ny/2,/interp,/cent)
        slt=size(imlt)
        srt=size(imrt)
        nxim=slt[1]+srt[1]
        nyim=slt[2]
        im_show=fltarr(nxim,nyim)
        im_show[0:slt[1]-1,*]=imlt
        im_show[slt[1]:nxim-1,*]=imrt
        
        cs_ax=1.5
        cs_rms=1.8
        if plot_iter eq 0 then cs_ax=1.6
        if plot_iter eq 0 then cs_rms=1.6
        
        pos0=[0.1,0.1,0.1+float(slt[1])/nxim*0.89,0.95]
        pos1=[0.1+float(slt[1])/nxim*0.89,0.1,0.1+float(slt[1])/nxim*0.89+float(srt[1])/nxim*0.89,0.95]
        pos=[pos0[0],pos0[1],pos1[2],pos1[3]]
        tmp=sigrange(im_show,frac=0.9,range=range)
        
        col_lines='orange';"BLK6"
        col_text='red';'black'
        
        cgimage,im_show,pos=pos,/nega,maxval=range[1],minval=range[0]
        cgplot,0,0,/nodata,xst=5,yst=1, ytit="!8y!X, pix",yrange=[0,ny],/noer,pos=pos, charsize=cs_ax
        cgplot,0,0,/nodata,xst=1,yst=5,xrange=[0,show_borders[0]],yrange=[0,ny],/noer,pos=pos0,tit="Left slit", xtit="!8x!X, pix", charsize=cs_ax
        
        cgtext,100,1860,'(a)',/data,charsize=cs_rms
        
        for i=0,nrec_eta0-1 do cgoplot,replicate(spec0+rec_eta0[i]*dspec,npeaks_eta),peaks_eta[rec_eta0[i],*,0],psym=4,color=col_lines
        FOR i=0,netalons-1 DO BEGIN
          cgoplot,x_reper_left,etalons_lt[*,i],color=col_lines
          cgtext,mean(x_reper_left),mean(etalons_lt[*,i])+10,"rms="+string(rms_eta[0,i],format="(F4.2)")+" px",col=col_text, charsize=cs_rms
        ENDFOR
        cgplot,0,0,/nodata,xst=1,yst=5,xrange=[0,show_borders[1]],yrange=[0,ny],/noer,pos=pos1,tit="Right slit", xtit="!8x!X, pix", charsize=cs_ax,xtickname=[' ', '500', '1000', '1500', '2000']
        cgoplot,[0,0],[0,ny],color="black"
        
        for i=0,nrec_eta1-1 do cgoplot,replicate(spec0+rec_eta1[i]*dspec,npeaks_eta),peaks_eta[rec_eta1[i],*,1],psym=4,color=col_lines
        FOR i=0,netalons-1 DO BEGIN
          cgoplot,x_reper_right,etalons_rt[*,i],color=col_lines
          cgtext,mean(x_reper_right),mean(etalons_rt[*,i])+10,"rms="+string(rms_eta[1,i],format="(F4.2)")+" px",col=col_text, charsize=cs_rms
        ENDFOR
        ;cgtext,0.1+(pos[2]-pos[0])/5.,pos[3]+0.06,"Etalons traectory of "+objname+"; Night "+night+", Disperser "+disperser,/norm,charsize=cs_ax
      IF plot_iter eq 0 then  cgPS_close



    ENDFOR







    print,"total number of ETA outlier columns: "+string(nspec*2-nrec_eta0-nrec_eta1,format="(I4)")+" of "+string(floor((slit_break_right-x0_cut)/dspec)+floor((slit_break_left-x0_cut)/dspec),format="(I4)")
    
    IF keyword_set(logfield) then IFURED_LOG_UPD, ["Done using "+string(nrec_eta0,format="(I4)")+" of "+string(floor((slit_break_right-x0_cut)/dspec),format="(I4)") + " bins for left slit",$
                      " and"+string(nrec_eta1,format="(I4)")+" of "+string(floor((slit_break_right-x0_cut)/dspec),format="(I4)") + " bins for right slit along dispersion", $
                      "Mean rms for left slit = "+string(mean(rms_eta[0,*]),format="(F0.3)")+" px","Mean rms for right slit = "+string(mean(rms_eta[1,*]),format="(F0.3)")+" px"," "]
    
    
    
    ;ptr_free,subflats,subflats_yrange,x_pfib,y_pfib,n_pfib
END