PRO IFURED_PEAKS_FIBERS, flat, master_eta, master_fib, peaks_eta,  current_pos=current_pos,$
                      current_win=current_win, peaks_out=peaks, fiber_num=fiber_num;,peaks_obs=peaks_obs
     if not keyword_set(current_win) then current_win=30
     nfib=n_elements(master_fib)
     ;npeaks_eta=n_elements(master_eta)
     win=0
     
     ;vector=reform(IFURED_CROSS_VECTOR(flat,current_pos,wx=current_win,/nosmooth))
     vector=total(flat,1,/nan)
     s=size(vector)
     slit=findgen(s[1])
     
     
         ;##### Establish correspondance between master_fib and observed fibers
          Ndeg=3
          peaks=fltarr(nfib)
          fiber_num=indgen(nfib)
          xx=master_eta[*]
          yy=peaks_eta
          res=goodpoly(xx,yy,Ndeg,3,fit)
          peaks[*]=poly(master_fib[*],res)
          
          
;          shi=median(peaks-peaks_obs)
;          ;res=goodpoly(peaks_obs,peaks,1,1)
;          peaks[*]=peaks_obs+shi;poly(peaks_obs[*],res)
          
;          ;### Search of peaks near found position
;          for j=0,nfib-1 do begin
;              rec=where(slit ge peaks[j]-5. and slit le peaks[j]+5.,nr)
;              if nr eq 0 then begin
;                  peaks[j]=-1
;                  fiber_num[j]=-1
;                  continue
;                endif
;              sl=congrid(slit[rec],nr*100,/interp,/cent)
;              nv=congrid(vector[rec],nr*100,/interp,/cent)
;              
;              peaks_adjust=IFURED_FIND_PEAKS(nv, win=5,npeaks=1)
;              if peaks_adjust[0] gt 0 then peaks[j]=sl[peaks_adjust[0]]
;                
;                
;          endfor    
            
              ;###Correction of found position
              for j=0,nfib-1 do begin
                rec=where(slit ge peaks[j]-3.5 and slit le peaks[j]+3.5,nr)
                
                if nr eq 0 then begin
                  peaks[j]=-1
                  fiber_num[j]=-1
                  continue
                endif
                sl=congrid(slit[rec],nr*100,/interp,/cent)
                nv=congrid(vector[rec],nr*100,/interp,/cent)
                
                rec1=where(sl ge peaks[j]-2.5 and sl le peaks[j]+2.5)
                
                res=poly_fit(sl[rec1],nv[rec1],2)
                
;                  cgdisplay,wid=0,xs=300,ys=200
;                  cgplot,sl,nv
;                  cgoplot,sl,poly(sl,res),col='red'
;                  cgoplot,replicate(-res[1]/2./res[2],2),minmax(nv),col="blue"
;                  wait,0.02
                if abs(-res[1]/2./res[2]-peaks[j])/6.78 lt 0.2 and res[2] lt 0 then   peaks[j]=-res[1]/2./res[2]
              endfor

     
    
    RETURN                
END