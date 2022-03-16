pro IFURED_LINERIS,filein,w_dir=w_dir,out_param=out_param


num=n_elements(filein)
if keyword_set(w_dir) then w_dir=slash(w_dir) else w_dir=''

; read dispersion parameters
if file_test(w_dir+'disper.fts') eq 0 then return
disper_par_all=READFITS(w_dir+'disper.fts')
FOR i=0,num-1 do begin
if file_test(w_dir+filein(i)+'_s.fts') eq 0 then begin
    print,'=== IFURED_LINERIS:  no file '+w_dir+filein(i)+'_s.fts'
   endif ELSE BEGIN
   
   ; read frame
      image_all=readfits(w_dir+filein(i)+'_s.fts',head_i)
    
      Nx_all=sxpar(head_i,'naxis1')
      Ny=sxpar(head_i,'naxis2')
    
      
    FOR slitnum=0,1 DO BEGIN
      disper_par=reform(disper_par_all[*,*,slitnum])
      rec=where(disper_par[*,ny/2] ne -32000 and finite(disper_par[*,ny/2]))
      disper_par=disper_par[rec,*]
      
      
      
      a=size(disper_par)
      N_deg=a(1)-2 & N_y=a(2)
      
      ;determination parameter of linerisation
      
      if N_y ne Ny then message,'number spectra and dispersion curve not equal',/cont
      
      
      image=reform(image_all[*,*,slitnum])
      rec=where(finite(reform(image[*,ny/2])),nx); and reform(image[*,ny/2]) ne 0
      delta_x=rec[0]
      nx=max(rec)-min(rec)+1
      
      image=image[delta_x:delta_x+nx-1,*]
      rec=where(~finite(image),nr)
      if nr gt 0 then image[rec]=0
   
      
      if not(keyword_set(out_param)) then begin
      out_param=float([0,0,0])
      for k=0,N_deg do begin
       out_param[0]=out_param[0]+disper_par[k,Ny/2]*(double(Nx/2.)^k)
      endfor
       out_param[2]=disper_par(1,Ny/2)
       out_param[0]=out_param[0]-Nx*out_param[1]/2
       out_param[1]=out_param[0]+Nx*out_param[1]/2
      endif
      
      
      lambda_0=double(out_param[0])
      d_lambda=float(out_param[2])
      N_lin=floor(double(out_param[1]-out_param[0])/d_lambda)
      
      
      ;message,string(lambda_0)+string(d_lambda)+string(N_lin),/cont
      lambda=findgen(N_lin)*d_lambda+lambda_0
      
      x=dindgen(Nx)+delta_x
      
      if slitnum eq 0 then image_lin=fltarr(N_lin,Ny, 2)
      
      for y=0,ny-1 do begin
       lambda_tmp=fltarr(Nx)
        for j=0,N_deg do begin
         lambda_tmp=lambda_tmp+disper_par(j,y)*x^j
        endfor
        vec=reform(image[*,y])
        
        vec=[median(vec(0:40)),vec]
        image_lin[*,y,slitnum] = INTERPOL(vec,[0,lambda_tmp],lambda)
        rec=where(lambda lt lambda_tmp[0] or lambda gt lambda_tmp[nx-1],nr)
        if nr gt 0 then image_lin(rec,y,slitnum)=!Values.F_NAN
      endfor

      dev_rms=stdev(disper_par(N_deg+1,*),mean_rms)
      
    endfor  
      
      
      ;saving data
      sxaddpar,head_i,'CRVAL1' ,out_param[0],' WAVELENGTH 1-st ELEMENT, ANGSTROM'
      sxaddpar,head_i,'CDELT1' ,out_param[2],after='CRVAL1',' DISPERSION, ANGSTROM/PX'
      sxaddpar,head_i,'CRPIX1' ,1,after='CDELT1'
      sxaddpar,head_i,'CTYPE1' ,"WAV-AWAV",after='CRPIX1'
      writefits,w_dir+filein(i)+'_lin.fts',image_lin,head_i
      
  ENDELSE
  
ENDFOR
END
