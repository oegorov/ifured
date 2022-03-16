PRO IFURED_specFLAT_REDUCT, image_type,suff=suff, w_dir=w_dir, fibers_prefix=fibers_prefix


    if not keyword_set(w_dir) then w_dir="" 
    if not keyword_set(fibers_prefix) then fibers="fiber_pos.fit" else $
                                           fibers=fibers_prefix+".fit" 
    if not keyword_set(suff) then suff='_lin.fts'
    
    fileflat=w_dir+'flat'+suff
    fib_pos_all=read_asc(w_dir+fibers)
    
    im_all=float(readfits(fileflat,hflat))
    im_out=im_all*0.
    
    xs=sxpar(hflat,'naxis1')
    ys=sxpar(hflat,'naxis2')
    
    
    for i=0,n_elements(image_type)-1 do begin
        
      if image_type[i] ne 'flat' then begin
          file=image_type[i]+suff
          im_proc_all=readfits(w_dir+file,h)
          im_proc_out=im_proc_all*0
        
         
    
        
          FOR slitnum=0,1 DO BEGIN
         
            rec=where(fib_pos_all[2,*] eq slitnum)
            fib_pos=fib_pos_all[0:1,rec]
            
            sorted=sort(fib_pos[0,*])
            rec=where(fib_pos[1,sorted] eq 0, nrec)
            
            im=reform(im_all[*,*,slitnum])
            im_proc=reform(im_proc_all[*,*,slitnum])
            win=40
            
            
            
            ;if xs gt 1040 then dx=20 else dx=11 ; skip overscan over
            IF slitnum eq 0 then begin
              vec=fltarr(xs)
              for j=0,nrec-1 do vec=vec+reform(im[*,rec[j]])
              
              
              vec=vec/nrec;n_elements(fib_pos)
              
              
              
      ;        window,2+slitnum,tit='Flat smoothing',xs=800
      ;        plot,vec,xst=1,yst=1
              ;vec=vec[dx:*]
              vec=[reverse(vec[0:win-1]),vec,reverse(vec[xs-win:*])]
              vec=median(vec,win)
              vec=smooth(vec,win,/edge)
              norm0=fltarr(xs)+1
              norm0[*]=vec[win:xs+win-1]
              
      ;        oplot,norm0,col=30
            
            ENDIF
            
            for y=0,ys-1 do begin
              res=float(im[*,y])/float(norm0[*])
              im[*,y]=res[*]
            endfor
            ;im[0:dx-1,*]=1.  
            ; normalised flat
            if slitnum eq 0 then norm=median(im[xs*0.3:xs*0.7,ys*0.3:ys*0.7])
            im=im/norm
            im_out[*,*,slitnum]=im
            flat=im
            
            
            if image_type[i] ne 'neon' then im_proc=im_proc/flat
            rec=where(finite(im_proc) eq 0,cc)
            if cc gt 0 then im_proc[rec]=0
            im_proc_out[*,*,slitnum]=im_proc
          ENDFOR
           sxaddhist,'IFURED_FLAT_REDUCT: original file:'+file,h
           writefits,w_dir+image_type[i]+'_n.fts',im_proc_out,h
      endif
    endfor
    
    sxaddhist,'IFURED_FLAT_REDUCT: Flat smoothing along X.  win='+string(win),hflat
    writefits,w_dir+'flat_n.fts',im_out,hflat
        

END