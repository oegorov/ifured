FUNCTION IFURED_PACK_FIBERS, in_left=in_left, in_right=in_right, Nside_x=Nside_x, nside_y=nside_y, nz=nz, start=start,file_fiberpos=file_fiberpos

;Scheme for pack fibers in IFU SCORPIO-2
;  NS   k         k+12   NS
;  0    1:11     12:22   23           
; |-|<----------|---------->|-| 0    ( 0:10)=>(1:11,0)     ( 0:10)=>(22:12,0)
; |-|---------->|<----------|-| 1    (11:21)=>(1:11,1)     (11:21)=>(22:12,1)
; |-|<----------|---------->|-| 2  (22:32)=>(1:11,2)     (22:32)=>(22:12,2)
; |-|---------->|<----------|-| 3  (33:43)=>(1:11,3)     (33:43)=>(22:12,3)
;.............................
; |-|<----------|---------->|-| 20  (220:230)=>(1:11,20)  (220:230)=>( 1:11,20)
; |-|---------->|<----------|-| 21  (231:242)=>(1:11,20)  (231:241)=>(22:12,20)
;
; After that => rotate 90deg counterclockwise  
;

  IF not keyword_set(Nside_x) then Nside_x=22
  IF not keyword_set(Nside_y) then Nside_y=Nside_x+2
  IF n_elements(start) ne 2 then start=[0,0]
  sl=size(in_left)
  IF not keyword_set(Nz) then Nz=sl[1]-start[0]
  Neta=12
  
    
    out=fltarr(nside_x,nside_y,nz)
    map=fltarr(nside_x,nside_y)  
    map_tmp=fltarr(nside_x,nside_x)
    fibers=read_asc(file_fiberpos)
    
  
    r0=where(fibers[2,*] eq 0)
    r1=where(fibers[2,*] eq 1)
    
    fibers0=fibers[0:1,r0]
    fibers1=fibers[0:1,r1]
    
    sorted0=sort(fibers0[0,*])
    sorted1=sort(fibers1[0,*])
  
    fib_type0=reform(fibers0[1,sorted0])
    fib_type1=reform(fibers1[1,sorted1])
    
    
    rec0=where(fib_type0 eq 0)
    rec1=where(fib_type1 eq 0)
    rec_eta0=where(fib_type0 eq 1)
    rec_eta1=where(fib_type1 eq 1)
    
    FOR z=0,nz-1 do begin
      ; #### Add Obj fibers
;      FOR row=0,nside_x-1 DO BEGIN
;         if row mod 2 eq 0 then begin
;           map_tmp[0:nside_x/2-1,nside_x-1-row]=(reform(in_left[z+start[0],rec0[row*nside_x/2:(row+1)*nside_x/2-1]]))
;           map_tmp[nside_x/2:nside_x-1,nside_x-1-row]=reverse(reform(in_right[z+start[1],rec1[row*nside_x/2:(row+1)*nside_x/2-1]]))
;         endif else begin
;          map_tmp[0:nside_x/2-1,nside_x-1-row]=reverse(reform(in_left[z+start[0],rec0[row*nside_x/2:(row+1)*nside_x/2-1]]))
;          map_tmp[nside_x/2:nside_x-1,nside_x-1-row]=(reform(in_right[z+start[1],rec1[row*nside_x/2:(row+1)*nside_x/2-1]]))
;         endelse 
;      endfor
;      map[0:nside_x-1,1:nside_y-2]=rotate(map_tmp,1)
;      
;      ; #### Add Sky fibers
;      N_eta=Neta
;      sky_left=reform(in_left[z+start[0],rec_eta0[0:N_eta-1]])
;      sky_right=reform(in_right[z+start[1],rec_eta1[0:N_eta-1]])
;      N_eta=N_elements(sky_left)
;      
;      
;      sky_left1=(sky_left+shift(sky_left,-1))/2.
;      sky_left=[sky_left,sky_left1[1:N_eta-2]]
;      sky_right1=(sky_right+shift(sky_right,-1))/2.
;      sky_right=[sky_right,sky_right1[1:N_eta-2]]
      
      
      
      
      FOR row=0,nside_x-1 DO BEGIN
         if row mod 2 eq 0 then begin
           map_tmp[0:nside_x/2-1,nside_x-1-row]=(reform(in_right[z+start[1],rec1[row*nside_x/2:(row+1)*nside_x/2-1]]))
           map_tmp[nside_x/2:nside_x-1,nside_x-1-row]=reverse(reform(in_left[z+start[0],rec0[row*nside_x/2:(row+1)*nside_x/2-1]]))
         endif else begin
          map_tmp[0:nside_x/2-1,nside_x-1-row]=reverse(reform(in_right[z+start[1],rec1[row*nside_x/2:(row+1)*nside_x/2-1]]))
          map_tmp[nside_x/2:nside_x-1,nside_x-1-row]=(reform(in_left[z+start[0],rec0[row*nside_x/2:(row+1)*nside_x/2-1]]))
         endelse 
      endfor
      map[0:nside_x-1,1:nside_y-2]=rotate(map_tmp,1)
      
      ; #### Add Sky fibers
      N_eta=Neta
      sky_left0=reform(in_left[z+start[0],rec_eta0[0:N_eta-1]])
      sky_right0=reform(in_right[z+start[1],rec_eta1[0:N_eta-1]])
      N_eta=N_elements(sky_left0)
      sky_left=fltarr(nside_x)
      sky_right=fltarr(nside_x)
      
      sky_left1=(sky_left0+shift(sky_left0,-1))/2.
      sky_right1=(sky_right0+shift(sky_right0,-1))/2.
      
      for i=0,n_eta-2 do begin
        sky_left[i*2]=sky_left1[i]
        sky_left[i*2+1]=sky_left0[i+1]
        sky_right[i*2]=sky_right1[i]
        sky_right[i*2+1]=sky_right0[i+1]
      endfor
      
      map[*,0]=sky_right;[0:Nsky-1]
      map[*,nside_y-1]=sky_left;[0:Nsky-1]
      
      out[*,*,z]=map
    ENDFOR
    
    return, out
END