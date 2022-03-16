PRO IFURED_FLAT_REDUCT, image_type,suff=suff, w_dir=w_dir, fibers_prefix=fibers_prefix


    if not keyword_set(w_dir) then w_dir="" 
    if not keyword_set(fibers_prefix) then fibers=["fiber_pos_0.fit","fiber_pos_1.fit"] else $
                                           fibers=fibers_prefix+["_0.fit","_1.fit"] 
    if not keyword_set(suff) then suff='_lin.fts'
    
    
    FOR slitnum=0,1 DO BEGIN
        
        fib_pos=read_asc(w_dir+fibers[slitnum])
        
        sorted=sort(fib_pos[0,*])
        rec=where(fib_pos[1,sorted] eq 1, nrec)
        
        fileflat=w_dir+'flat'+"_"+string(slitnum,format="(I1)")+suff
        
        
        win=40
        
        im=float(readfits(fileflat,h))
        xs=sxpar(h,'naxis1')
        ys=sxpar(h,'naxis2')
        
        if xs gt 1040 then dx=20 else dx=11 ; skip overscan over
        
        vec=fltarr(xs)
        for i=0,nrec-1 do vec=vec+reform(im[*,rec[i]])
        
        
        vec=vec/nrec;n_elements(fib_pos)
        
;        window,2+slitnum,tit='Flat smoothing',xs=800
;        plot,vec,xst=1,yst=1
        vec=vec[dx:*]
        vec=[reverse(vec[0:win-1]),vec,reverse(vec[xs-win:*])]
        vec=median(vec,win)
        vec=smooth(vec,win,/edge)
        norm0=fltarr(xs)+1
        norm0[dx]=vec[win:xs+win-1-dx]
        
;        oplot,norm0,col=30
        
        
        for y=0,ys-1 do begin
          res=float(im[*,y])/float(norm0[*])
          im[*,y]=res[*]
        endfor
        im[0:dx-1,*]=1.  
        ; normalised flat
        norm=median(im[xs*0.3:xs*0.7,ys*0.3:ys*0.7])
        im=im/norm
        sxaddhist,'IFU_FLAT_REDUCT: Flat smoothing along X.  win='+string(win),h
        writefits,w_dir+'flat_'+string(slitnum,format="(I1)")+"_n.fts",im,h
        flat=im
        
        for i=0,n_elements(image_type)-1 do begin
        
         if image_type[i] ne 'flat' then begin
          file=image_type[i]+"_"+string(slitnum,format="(I1)")+suff
          im=readfits(w_dir+file,h)
          if image_type[i] ne 'neon' then im=im/flat
          rec=where(finite(im) eq 0,cc)
          if cc gt 0 then im[rec]=0
          sxaddhist,'IFU_FLAT_REDUCT: original file:'+file,h
          writefits,w_dir+image_type[i]+"_"+string(slitnum,format="(I1)")+'_n.fts',im,h
        
         endif
        endfor
    
    
    ENDFOR


END