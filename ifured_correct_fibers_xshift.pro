PRO IFURED_CORRECT_FIBERS_XSHIFT
  COMMON IFURED_PARAMS
  
  fileneon="neon_s.fts"
  fileflat="flat_s.fts"
  fileobj="obj_s.fts"
  filesky="sky_s.fts"
  filestar="star_s.fts"
  
  star_set=file_test(w_dir+filestar)
  sky_set=file_test(w_dir+filesky)
  
  neon=readfits(w_dir+fileneon,hneon)
  flat=readfits(w_dir+fileflat,hflat)
  obj=readfits(w_dir+fileobj,hobj)
  if star_set then star=readfits(w_dir+filestar,hstar)
  if sky_set then sky=readfits(w_dir+filesky,hsky)
  
  
  

    
  
  fibers=read_asc(w_dir+"fibers_pos.fit")
  IFURED_READ_MASTER, master_eta, master_fib,/get_xshift
  master_eta_y=reform(master_eta[0,*,*])
  master_fib_y=reform(master_fib[0,*,*])
  master_eta_x=reform(master_eta[2,*,*])
  master_fib_x=reform(master_fib[2,*,*])
  for slit=0,1 do begin
    rec_slit=where(fibers[2,*] eq slit,nslit)
    rec_eta=where(fibers[1,rec_slit] eq 1, neta)
    rec_fib=where(fibers[1,rec_slit] eq 0, nfib)
    xx=[master_eta_y[*,slit],master_fib_y[*,slit]]
    yy=[reform(fibers[0,rec_slit[rec_eta]]),reform(fibers[0,rec_slit[rec_fib]])]
    
    res=goodpoly(xx,yy,1,1,yfit)
    mas=res[1]
    
    FOR i=0,neta-1 do begin
      shi=master_eta_x[i,slit]*mas
      if abs(shi) le 0.005 then continue
      neon[*,rec_eta[i],slit]=vecshift(neon[*,rec_eta[i],slit],dx=shi)
      flat[*,rec_eta[i],slit]=vecshift(flat[*,rec_eta[i],slit],dx=shi)
      obj[*,rec_eta[i],slit]=vecshift(obj[*,rec_eta[i],slit],dx=shi)
      if star_set then star[*,rec_eta[i],slit]=vecshift(star[*,rec_eta[i],slit],dx=shi)
      if sky_set then sky[*,rec_eta[i],slit]=vecshift(sky[*,rec_eta[i],slit],dx=shi) 
    endfor
    FOR i=0,nfib-1 do begin
      shi=master_fib_x[i,slit]*mas
      if abs(shi) le 0.005 then continue
      neon[*,rec_fib[i],slit]=vecshift(neon[*,rec_fib[i],slit],dx=shi)
      flat[*,rec_fib[i],slit]=vecshift(flat[*,rec_fib[i],slit],dx=shi)
      obj[*,rec_fib[i],slit]=vecshift(obj[*,rec_fib[i],slit],dx=shi)
      if star_set then star[*,rec_fib[i],slit]=vecshift(star[*,rec_fib[i],slit],dx=shi)
      if sky_set then sky[*,rec_fib[i],slit]=vecshift(sky[*,rec_fib[i],slit],dx=shi) 
    endfor
    
    
  ENDFOR
  
  
  
  writefits,w_dir+fileneon,neon,hneon
  writefits,w_dir+fileflat,flat,hflat
  writefits,w_dir+fileobj,obj,hobj
  if star_set then writefits,w_dir+filestar,star,hstar
  if sky_set then writefits,w_dir+filesky,sky,hsky
END