PRO IFURED_TEST_WARP, show=show, logfield=logfield
   COMMON IFURED_PARAMS
   
   scrdim = GET_SCREEN_SIZE(RESOLUTION=resolution)  
   xsscr=800
   ysscr=600
   
   IFURED_READ_MASTER,master_eta,master_fib
   master_eta=reform(master_eta[0,*,*])
   master_fib=reform(master_fib[0,*,*])
   
   s1=size(master_eta)
   s2=size(master_fib)
   nfib=(s1[1]+s2[1])*2
   
   eta_fibers=read_asc(w_dir+"eta_pos_warp.fit")
   flat=readfits(w_dir+"flat_warp.fts",h)
   ;fiberpos_obs=readfits(w_dir+"fiber_pos.fts")
   fibers=IFURED_FIND_FIBERS(flat,eta_fibers,master_eta,master_fib);,fiberpos_obs=fiberpos_obs)
   
   
   
   
   winshow=5
   s=size(flat)
   nx=s[1] & ny=s[2]
   
   slitname=["left","right"]
   grad_out=fltarr(2)
   fib_pos_out=fltarr(6,nfib)
   ncurpos=0
   FOR slit=0,1 DO BEGIN
     
     rec_eta=where(fibers[1,*] eq 1 and fibers[2,*] eq slit,neta)
     curflat=reform(flat[*,*,slit])
     x=indgen(nx)
     y=findgen(ny)
     bin=30.
     ybin=findgen(ny*bin)/bin
     win=50
     nbin=nx/win
     xx=findgen(nbin)*win
     yy=fltarr(nbin);*2)
     yy1=fltarr(nbin);*2)
     yy[*]=-100
     yy1[*]=-100
     grad=fltarr(neta-1)
     rec_good=where(finite(total(curflat,2)),nr)
     
     FOR j=0, neta-2 do begin 
        eta_pos0=fibers[0,rec_eta[j]]
        eta_pos1=fibers[0,rec_eta[j+1]]
        
        
        for k=10,nbin-10 do begin
          if xx[k]-win lt rec_good[0] or xx[k] gt rec_good[nr-1] then continue
          recvec=where(y ge eta_pos0-5 and y le eta_pos0+5,nrvec)
          recvec_bin=where(ybin ge eta_pos0-5 and ybin le eta_pos0+5,nrvecbin)
          recvec1=where(y ge eta_pos1-5 and y le eta_pos1+5,nrvec1)
          recvec_bin1=where(ybin ge eta_pos1-5 and ybin le eta_pos1+5,nrvecbin1)
          
          vec=congrid(reform(total(curflat[xx[k]-win:xx[k],recvec],1)),nrvec*bin,/interp)
          rec=where(~finite(vec) or vec le 0,nrbad)
          vec1=congrid(reform(total(curflat[xx[k]-win:xx[k],recvec1],1)),nrvec1*bin,/interp)
          rec1=where(~finite(vec1) or vec1 le 0,nrbad1)
          
          if nrbad eq 0 then begin
            m=max(vec,mpos)
            yy[k]=ybin[recvec_bin[mpos]]-eta_pos0
          endif
          if nrbad1 eq 0 then begin
            m=max(vec1,mpos)
            yy1[k]=ybin[recvec_bin1[mpos]]-eta_pos1
          endif
        endfor
        good=where(yy gt 0,nrgood)
        good1=where(yy1 gt 0,nrgood1)
        
        if nrgood+nrgood1 lt 3 then begin
          grad[j]=-100
          res=[0,0]
        endif else begin
          if nrgood gt 0 and nrgood1 gt 0 then begin
            xarr= [xx[good],xx[good1]]
            yarr= [yy[good],yy1[good1]] 
          endif else begin
            if nrgood gt 0 then begin
              xarr= xx[good]
              yarr= yy[good]
            endif
            if nrgood1 gt 0 then begin
              xarr= xx[good1]
              yarr= yy1[good1]
            endif
          endelse  
          res=poly_fit(xarr,yarr,1)
          grad[j]=res[1]
        endelse
       ; res=[0,0]  
        ;stop
        
        ;### Below I update positions of fibers with calculated gradients to write them into new ascii file
        
;        IF j eq 0 then begin
;          ;write output pos for first etalon fiber
;          fib_pos_out[0,ncurpos]=fibers[0,rec_eta[0]]+res[0]
;          fib_pos_out[1,ncurpos]=1
;          fib_pos_out[2,ncurpos]=slit
;          fib_pos_out[3,ncurpos]=res[1]
;          ncurpos+=1
;        ENDIF
        
        fib_pos_out[0,ncurpos]=fibers[0,rec_eta[j]]+res[0]
        fib_pos_out[1,ncurpos]=1
        fib_pos_out[2,ncurpos]=slit
        fib_pos_out[3,ncurpos]=res[1]
        ncurpos+=1
        
        rec_fib=where(fibers[0,*] gt fibers[0,rec_eta[j]] and fibers[0,*] lt fibers[0,rec_eta[j+1]] and fibers[1,*] eq 0 and fibers[2,*] eq slit, nfib_part)
        
        FOR k=0,nfib_part-1 do begin
          fib_pos_out[0,ncurpos]=fibers[0,rec_fib[k]]+res[0]
          fib_pos_out[1,ncurpos]=0
          fib_pos_out[2,ncurpos]=slit
          fib_pos_out[3,ncurpos]=res[1]
          ncurpos+=1
        endfor
        
        IF j eq neta-2 then begin
          ;write output pos for first etalon fiber
          fib_pos_out[0,ncurpos]=fibers[0,rec_eta[j+1]]+res[0]
          fib_pos_out[1,ncurpos]=1
          fib_pos_out[2,ncurpos]=slit
          fib_pos_out[3,ncurpos]=res[1]
          ncurpos+=1
        ENDIF
      
      ENDFOR
      
      
      
      r=where(fib_pos_out[1,*] eq 1 and fib_pos_out[2,*] eq slit)
      FOR j=0, neta-2 do begin 
        ;  Here I calculate tracks for each fiber and show them
        
        rec_fib=where(fib_pos_out[0,*] ge fib_pos_out[0,r[j]] and fib_pos_out[0,*] le fib_pos_out[0,r[j+1]] and fib_pos_out[2,*] eq slit, nfib_part)
        
        IF j eq 0 or j eq neta-2 then begin ;j eq 1 or 
          if j eq 0 then begin
            fib_pos_out[4,r[0]]=fib_pos_out[0,r[0]]/2.
            fib_pos_out[5,r[0]]=(fib_pos_out[0,r[1]]-fib_pos_out[0,r[0]])/2.
            bord0=0;fib_pos_out[0,r[j-1]]
            rec_tmp=where(fib_pos_out[0,*] lt fib_pos_out[0,r[j+1]] and fib_pos_out[0,*] gt fib_pos_out[0,r[j]] and fib_pos_out[2,*] eq slit, ntmp)
            bord1=fib_pos_out[0,rec_tmp[ntmp-1]]
          endif
          if j eq neta-2 then begin
;            fib_pos_out[5,r[neta-1]]=(ny-fib_pos_out[0,r[neta-1]])/2.
;            fib_pos_out[4,r[neta-1]]=(fib_pos_out[0,r[neta-1]]-fib_pos_out[0,rec_fib[nfib_part-2]])/2.
            rec_tmp=where(fib_pos_out[0,*] lt fib_pos_out[0,r[j]] and fib_pos_out[0,*] gt fib_pos_out[0,r[j-1]] and fib_pos_out[2,*] eq slit, ntmp)
            bord1=ny
            bord0=fib_pos_out[0,rec_tmp[ntmp-1]]
          endif
        endif else begin
          rec_tmp=where(fib_pos_out[0,*] lt fib_pos_out[0,r[j+1]] and fib_pos_out[0,*] gt fib_pos_out[0,r[j]] and fib_pos_out[2,*] eq slit, ntmp)
          bord1=fib_pos_out[0,rec_tmp[ntmp-1]]
          rec_tmp=where(fib_pos_out[0,*] lt fib_pos_out[0,r[j]] and fib_pos_out[0,*] gt fib_pos_out[0,r[j-1]] and fib_pos_out[2,*] eq slit, ntmp)
          bord0=fib_pos_out[0,rec_tmp[ntmp-1]]
        endelse
        
        fib_tst=fltarr(nfib_part+2)
        
        
        
        
        fib_tst=[bord0,reform(fib_pos_out[0,rec_fib]),bord1]
        shi_rt=(shift(fib_tst,-1)-fib_tst)/2.
        shi_lt=(fib_tst-shift(fib_tst,1))/2.
        shi_lt=shi_lt[1:nfib_part]
        shi_rt=shi_rt[1:nfib_part]
        for k=0,nfib_part-1 do begin
          fib_pos_out[4,rec_fib[k]]=shi_lt[k]
          fib_pos_out[5,rec_fib[k]]=shi_rt[k]
        endfor
        
        if keyword_set(show) then begin
          show_pos0=round(fib_pos_out[0,r[j]]-winshow)>0
          show_pos1=round(fib_pos_out[0,r[j+1]]+winshow)<(ny-1)
          if slit eq 1 then wset,j+1 else cgdisplay,xsscr,ysscr,wid=j+1,xpos=j*20,ypos=scrdim[1]-ysscr-j*20,tit="Flat: ETA fibers "+string(j,format="(I0)")+"-"+string(j+1,format="(I0)")
          if slit eq 0 then pos=[0.05,0.05,0.45,0.9] else pos=[0.55,0.05,0.95,0.9]
          
      
          ima=curflat[rec_good[0]:rec_good[nr-1],show_pos0:show_pos1]
          tmp=congrid(ima,50,50)
          rms_ima=stdev(median(tmp[where(finite(tmp))],5),avg_ima)
          map=255-bytscl(ima,avg_ima-rms_ima,avg_ima+rms_ima)      
          
          
          cgimage,map,/axes,yr=[show_pos0,show_pos1],ctind=0,$
                xr=[x[rec_good[0]],x[rec_good[nr-1]]],charsize=0.7,xtit="X, pix",ytit="Y, pix",layout=[2,1,slit+1],title=slitname[slit]+" slit",margin=0.2
          for k=0,nfib_part-1 do  cgoplot,[0,nx],[fib_pos_out[0,rec_fib[k]],fib_pos_out[0,rec_fib[k]]+nx*fib_pos_out[3,rec_fib[k]]],col="orange",thick=2
;            
;          wait,1.
        endif
     ENDFOR
     rec=where(grad ne -100)
     grad_out[slit]=median(grad[rec])
   ENDFOR
   openw,u,w_dir+"fibers_pos.fit",/get_lun
   for i=0,nfib-1 do printf,u,fib_pos_out[*,i],format="(F0.4, X, X, I0, X, X, I0, X, X, G0.3, X, X, F0.4, X, X, F0.4)"
   close,u,/file
   free_lun,u
   
   if keyword_set(logfield) then IFURED_LOG_UPD, ["Resid. slope after warp for left slit = "+string(grad_out[0],format="(F0.5)"),$
                                     "Resid. slope after warp for right slit = "+string(grad_out[1],format="(F0.5)")]
   
END