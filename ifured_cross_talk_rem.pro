FUNCTION IFURED_VOIGT_fit, x, param

;  res=gaussian(x,[param[0],param[1],param[2]/2.35428])

  vel=param[3];/2.
  param2=param[2]/2.35482
  b=(x-param[1])/(sqrt(2.)*param2)
  y=vel/(sqrt(2.)*param[2])
  res=voigt(y,b)/(sqrt(2.*!pi)*param2)
  ;norm=max(res)
  res=param[0]*res+param[4];/norm  ;here param[0] should be a flux of the profile
RETURN, res
END


FUNCTION IFURED_VOIGT, x, param, inst_vel=inst_vel

;  res=gaussian(x,[param[0],param[1],param[2]/2.35428])

  vel=inst_vel;/2.
  param2=param[2]/2.35482
  b=(x-param[1])/(sqrt(2.)*param2)
  y=vel/(sqrt(2.)*param[2])
  res=voigt(y,b)/(sqrt(2.*!pi)*param2)
  ;norm=max(res)
  res=param[0]*res;/norm  ;here param[0] should be a flux of the profile
RETURN, res
END

FUNCTION IFURED_FIT_FIBERS, x,param
  COMMON IFURED_CTALK_MODEL, fibpos, prof,rec_eta, fluxes
   nparams=n_elements(param)
   nfib=n_elements(fibpos)
   fw_fib=param[0]
;   fw_eta=param[1]
   inst_vel=param[1]
   y=prof;-poly(x,[param[2:4]])

   nx=n_elements(x)
   gaumat=fltarr(nx,nfib)
   
   
   
   for j=0, nfib-1 do gaumat[*,j]=IFURED_voigt(x,[1.,fibpos[j]+param[2],fw_fib],inst_vel=inst_vel)

   fluxes=LA_least_squares(transpose(gaumat),y,st=out)
   model=fluxes ## gaumat
   return,model
END


FUNCTION IFURED_CROSS_TALK_REM, images=images, n_fibers=n_fibers, slitnum=slitnum, $
                  show=show,logfield=logfield, tit=tit, type=type, optimal=optimal, w_dir=w_dir
  COMMON IFURED_CTALK_MODEL, fibpos, prof,rec_eta, fluxes
  n_im=n_elements(images)
  IF NOT KEYWORD_SET(tit) THEN tit='this slit'
  
  
  
  y_fib=read_asc(w_dir+"fibers_pos.fit")
  r=where(y_fib[2,*] eq slitnum,nr)
  fibers_pos=y_fib[*,r]
  
  s=size(*images[0])
  nx=s[1]
  ny=s[2]
  y=findgen(ny)
  x=findgen(nx)
  n_fibers=n_elements(fibers_pos[0,*])
  fibers_type=intarr(n_fibers)
  fibers_type=fibers_pos[1,*]
  fibers_wl=reform(fibers_pos[4,*])
  fibers_wr=reform(fibers_pos[5,*])
  fibers_g=reform(fibers_pos[3,*])
  fibers_pos=reform(fibers_pos[0,*])
  s=sort(fibers_pos)
  fibers_type=fibers_type[s]
  fibers_pos=fibers_pos[s]
  fibers_wl=fibers_wl[s]
  fibers_wr=fibers_wr[s]
  fibers_g=fibers_g[s]
  
  rec_eta=where(fibers_type eq 1, n_eta)
    
  
  out=ptrarr(n_im)
  
  im_out=fltarr(nx,n_fibers)
  
  param=dblarr(3);n_fibers+1)
  parinfo=replicate({parinfo_str,fixed:0L,limits:dblarr(2),limited:lonarr(2)},n_elements(param));,N_fibers+1)
  fibpos=fibers_pos
  
  bin=5.
  slit_bin=findgen(ny*bin)/bin
  slit=findgen(ny)  
  
  win=100  
    
  for k=0,n_im-1 do begin
    cur_im=*images[k]
    im_bin=congrid(cur_im,nx,ny*bin,/interp,/cent)
    
    prof=reform(total(cur_im[nx/2-win:nx/2+win,*],1,/nan))
    prof_bin=reform(total(im_bin[nx/2-win:nx/2+win,*],1,/nan))
    norm=max(prof,/nan)
    prof=prof/norm
    
    
    IF not keyword_set(optimal) then begin
       ;*** Simple integration in boxes ***
        pos0=fltarr(n_fibers)
        width=fltarr(n_fibers)
        background=fltarr(nx)
        pos0=round((fibers_pos-fibers_wl)*bin)
        width=round((fibers_wr+fibers_wl)*bin+1)
        
        
        win_ds=win
        if type[k] eq 'flat' then begin
          ref_profile=total(im_bin[nx/2-win_ds:nx/2+win_ds,*],1,/nan)
          add_shift=0
        endif else add_shift=def_vecshift(ref_profile,total(im_bin[nx/2-win_ds:nx/2+win_ds,*],1,/nan),m=2.*bin,plot=0,bin=10)
        
        FOR i=0,nx-1 do begin
;          background[i]=median([reform(im_bin[i,round(pos0[0]+fibers_g[0]*bin*x[i]+add_shift-width[0]/3.):round(pos0[0]+fibers_g[0]*bin*x[i]+add_shift)]),$
;                          reform(im_bin[i,round(pos0[n_fibers-1]+fibers_g[n_fibers-1]*bin*x[i]+add_shift):round(pos0[n_fibers-1]+fibers_g[n_fibers-1]*bin*x[i]+width[n_fibers-1]/3.+add_shift)])])
          for j=0,n_fibers-1 do im_out[i,j]=total(-background[i]+im_bin[i,round(pos0[j]+fibers_g[j]*bin*x[i]+add_shift):round(pos0[j]+fibers_g[j]*bin*x[i]+width[j]+add_shift)],2)/bin
        endfor
    ENDIF ELSE BEGIN
    
        ;**** Optimal extraction ***
        err=sqrt(1000.+abs(prof)) 
        IF type[k] eq 'flat' then begin
          ;*** If current image is flat => compute fibers profiles
            param[0:1]=4.8
            parinfo[2].fixed=1
            parinfo[0:1].fixed=0
            
            
            nfib=n_elements(fibpos)
            rec_fib_first=where(slit gt (fibpos[0]-10) and slit lt (fibpos[0]+10))
            rec_fib_last=where(slit gt (fibpos[nfib-1]-10) and slit lt (fibpos[nfib-1]+10))
            cgplot,slit[rec_fib_first]-fibpos[0],prof[rec_fib_first],col='blue'
            cgoplot,slit[rec_fib_last]-fibpos[nfib-1],prof[rec_fib_last],col='red'
            slit_tmp=(slit[rec_fib_last]-fibpos[nfib-1]+slit[rec_fib_first]-fibpos[0])/2.
            prof_tmp=(prof[rec_fib_first]+prof[rec_fib_last])/2.
            
            cgoplot,slit_tmp,prof_tmp
            param_tmp=fltarr(5)
            parinfo_tmp=replicate({parinfo_str,fixed:0L,limits:dblarr(2),limited:lonarr(2)},n_elements(param_tmp))
            param_tmp[3]=4.8
            param_tmp[2]=4.8
            param_tmp[0]=max(prof_tmp)-min(prof_tmp)
            param_tmp[4]=min(prof_tmp)
            res_tmp=mpfitfun('IFURED_voigt_fit',slit_tmp,prof_tmp,sqrt(1000.+abs(prof_tmp)),param_tmp,yfit=yfit,parinfo=parinfo_tmp,/quiet,status=retstat)
            fit_tmp=IFURED_voigt_fit(slit_tmp,res_tmp)
            cgoplot,slit_tmp,fit_tmp,col='cyan'
            print,res_tmp
;            stop
            
            param[0]=res_tmp[2]
            param[1]=res_tmp[3]
            parinfo[2].fixed=0
            parinfo[0:1].fixed=1
            
            res=mpfitfun('IFURED_FIT_FIBERS',slit,prof,err,param,yfit=yfit,parinfo=parinfo,/quiet,status=retstat)
            res_out=res
        ENDIF else begin
          parinfo[2].fixed=0
          parinfo[0:1].fixed=1
          parinfo[2].limited=[1,1]
          parinfo[2].limits=[-2.,2.]
          res_out=mpfitfun('IFURED_FIT_FIBERS',slit,prof,err,[res[0:1],param[2]],yfit=yfit,parinfo=parinfo,/quiet,status=retstat)
        
        endelse
        
        
;        if type[k] ne 'flat' then  model=IFURED_FIT_FIBERS(slit,res)
           
        
        prof_out=fltarr(ny,n_fibers)
        for j=0,n_fibers-1 do prof_out[*,j] = IFURED_voigt(slit,[fluxes[j]*norm,fibers_pos[j]+res_out[2],res[0]],inst_vel=res[1])
        
        prof_out_bin=fltarr(ny*bin,n_fibers)
        weights=fltarr(ny*bin,n_fibers)
          for j=0,n_fibers-1 do begin
            prof_out_bin[*,j]=IFURED_voigt(slit_bin,[fluxes[j]*norm,fibers_pos[j]+res_out[2],res[0]],inst_vel=res[1]);+fibers_g[j]*bin*x[i]
            rec=where(prof_out_bin[*,j]/fluxes[j]*norm lt 0.003, nr)
            if nr gt 0 then prof_out_bin[rec,j]=0
          endfor
        totw=total(prof_out_bin,2)
        rec_zerow=where(totw eq 0,nr_zerow)  
        for j=0,n_fibers-1 do begin
             weights[*,j]=reform(prof_out_bin[*,j])/totw
             if nr_zerow gt 0 then weights[rec_zerow,j]=0
        endfor
        FOR i=0,nx-1 do begin
              im_out[i,*]=im_bin[i,*]#weights/bin
              ;im_out[i,*]=total(im_bin[i,*]*weights,2)/bin
;           endfor
        ENDFOR
    
    ENDELSE
    out[k]=ptr_new(im_out)
    
    
    if keyword_set(show) then begin
          scrdim = GET_SCREEN_SIZE(RESOLUTION=resolution)  
          xsscr=1200
          ysscr=800
          cgdisplay,xsscr,ysscr,wid=0, tit=type[k]+"("+tit+")", xpos=0,ypos=scrdim[1]-ysscr
          nplots=3
              
          
          IF keyword_set(optimal) then begin
            ;*** Show output plots for optimal extraction
            yfit=total(prof_out[*,*],2)
          
            for j=0,nplots-1 do begin
              cgplot,slit,prof*norm,/xst,xr=[j*ny/float(nplots),(j+1)*ny/float(nplots)],yr=[-0.2,1.]*norm,$
                          layout=[1,nplots,j+1],xtit="X, pix", ytit="Norm. flux"
              cgoplot,slit,yfit,col='red'
              for f=0,n_fibers-1 do cgoplot,slit,prof_out[*,f],col='green'
              cgoplot,slit,prof*norm-yfit,col="blue"
            endfor
          ENDIF ELSE BEGIN
          
          ;*** Show output plots for box extraction
          col="blue"
            for j=0,nplots-1 do begin
            
              rec=where(slit ge j*ny/float(nplots) and slit le (j+1)*ny/float(nplots))
              norm1= max(prof[rec]*norm,/nan)
              cgplot,slit[rec],prof[rec]*norm,/xst,xr=[j*ny/float(nplots),(j+1)*ny/float(nplots)],yr=[-0.2,1.]*norm1,$
                          layout=[1,nplots,j+1],xtit="X, pix", ytit="Flux"
              
              
              rec_fib=where(fibers_pos-fibers_wl+nx/2*fibers_g lt (j+1)*ny/float(nplots) and fibers_pos+fibers_wr+nx/2*fibers_g gt  j*ny/float(nplots) , nrfib)
              
              for f=rec_fib[0],rec_fib[0]+nrfib-1 do begin 
                pos0=fibers_pos[f]-fibers_wl[f]+nx/2*fibers_g[f]+add_shift/bin
                pos1=fibers_pos[f]+fibers_wr[f]+nx/2*fibers_g[f]+add_shift/bin
                rec_cur=where(slit ge pos0 and slit le pos1)
                flx=total(prof[rec_cur],/nan)
                mm=minmax(prof)
                mm[0]=mm[0]>0
                
                cgoplot,[pos0,pos1],[mm[0],mm[0]]*norm,col=col
                cgoplot,[pos0,pos1],[mm[0]+flx/(fibers_wl[f]+fibers_wr[f]),mm[0]+flx/(fibers_wl[f]+fibers_wr[f])]*norm,col=col
                cgoplot,replicate(pos0,2),[mm[0],mm[0]+flx/(fibers_wl[f]+fibers_wr[f])]*norm,col=col
                cgoplot,replicate(pos1,2),[mm[0],mm[0]+flx/(fibers_wl[f]+fibers_wr[f])]*norm,col=col
              endfor
            endfor
          wait,2
          
          
          ENDELSE
        endif
        
        
        
        
        savefig=0
        
          

          
        ;##### This block saves eps figures for the paper
        IF savefig eq 1 and tit eq "right slit" and type[k] ne 'neon' then begin
        
            
            fout="/Users/mors/Science/MyPapers/Current/IFU/figs/"+type[k]+"_extract"
            if keyword_set(optimal) then fout+="_opt_cor.eps" else fout+="_cor.eps"
            pos=[0.08,0.12,0.99,0.99]
            
            xr=[888,1244];[890,1250]
            col="dark green"
            
            if type[k] eq 'flat' then begin
              if k eq 0 then lowess,slit,prof*norm,100,prof_smo,order=10
               pos_sky=[ 900.5801,1068.6613,1234.3610]
;               pos_sky=[892.9220,1060.5691,1225.7734]
               wid=3.5;5   
               rec=where(slit ge xr[0] and slit le xr[1])
              if k eq 0 then prof_smo=prof_smo/max(prof_smo[rec])
            rr=where((slit ge pos_sky[0]-wid and slit le pos_sky[0]+wid) or $
                      (slit ge pos_sky[1]-wid and slit le pos_sky[1]+wid) or $
                      (slit ge pos_sky[2]-wid and slit le pos_sky[2]+wid))
;                      prof_smo=prof_smo*0+1.

            prof_smo[rr]=0.9 
           lowess,slit,prof_smo,20,prof_smo1,order=3
           prof_smo=prof_smo1/max(prof_smo1[rec])
           endif

            
            cgps_open,fout,xs=10,ys=5,/encaps
              rec=where(slit ge xr[0] and slit le xr[1])
              prof_plot=prof/prof_smo
              mn_sub=min(prof_plot[rec],/nan)
              mn_sub=0
              prof_plot=prof_plot-mn_sub
                  norm_plt= max(prof_plot[rec],/nan)
              cgplot,slit,prof_plot/norm_plt,/xst,xr=xr,yr=[-0.05,1.05],xtit="!8y!X, pix", ytit="Normalized flux",pos=pos,charsize=1.4
;              cgoplot,slit,prof_smo,col='red'
              if not keyword_set(optimal) then begin
              letters=['(a)','-','(b)','(c)']
              ;letters=['(a)','-','(b)','(c)']
              ;### Box extraction
              cgtext,900,0.95,letters[k],charsize=1.6
              rec_fib=where(fibers_pos-fibers_wl+nx/2*fibers_g lt xr[1] and fibers_pos+fibers_wr+nx/2*fibers_g gt  xr[0] , nrfib)
              for f=rec_fib[0],rec_fib[0]+nrfib-1 do begin 
                  pos0=fibers_pos[f]-fibers_wl[f]+nx/2*fibers_g[f]
                  pos1=fibers_pos[f]+fibers_wr[f]+nx/2*fibers_g[f]
                  rec_cur=where(slit ge pos0 and slit le pos1)
                  flx=total(prof_plot[rec_cur],/nan)
                  mm=minmax(prof_plot)
                  mm[0]=0;mm[0]>0
                  cgoplot,[pos0,pos1],[mm[0],mm[0]]/norm_plt,col=col
                  cgoplot,[pos0,pos1],[mm[0]+flx/(fibers_wl[f]+fibers_wr[f]),mm[0]+flx/(fibers_wl[f]+fibers_wr[f])]/norm_plt,col=col
                  cgoplot,replicate(pos0,2),[mm[0],mm[0]+flx/(fibers_wl[f]+fibers_wr[f])]/norm_plt,col=col
                  cgoplot,replicate(pos1,2),[mm[0],mm[0]+flx/(fibers_wl[f]+fibers_wr[f])]/norm_plt,col=col
              endfor
              ENDIF ELSE BEGIN
                ;### Optimal extraction
                letters=['(c)','-','(d)','(f)'];['(d)','-','(e)','(f)']
                cgtext,xr[0]+10,0.95,letters[k],charsize=1.6
                yfit=(total(prof_out[*,*],2)/norm/prof_smo-mn_sub)/norm_plt
                
                cgoplot,slit,yfit,col='red'
                for f=0,n_fibers-1 do cgoplot,slit,(prof_out[*,f]/norm/prof_smo)/norm_plt,col='dark green'
                cgoplot,slit,prof_plot/norm_plt-yfit,col="blue"
              ENDELSE
            cgps_close
        
       ENDIF
        
        
        
        
        
       if keyword_set(optimal) then begin 
          yfit=total(prof_out,2)/norm
          message,"Std.dev. of residuals for "+type[k]+"("+tit+") = "+string(stddev(prof-yfit),format="(F0.4)"),/cont
          if keyword_set(logfield) then $
              IFURED_LOG_UPD, ["Std.dev. of residuals for "+type[k]+"("+tit+") = "+string(stddev(prof-yfit),format="(F0.4)")]
        endif
   ENDFOR
  
  
   
  
  return,out

END