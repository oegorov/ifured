;**********************************
;* atmospher dispersion correction
;*program uses  file atm_dis.txt (from atm_cor.pro)
;***********************************

PRO IFURED_ATM_COR,name,ext=ext,fileref=fileref,w_dir=w_dir

if not(keyword_set(fileref)) then fileref='atmdis_'+name+'.txt' ;reference file with barycenter position
if not(keyword_set(ext)) then ext='_abs'



filein=name+ext+'.fts'   ; input file
fileout=name+'_cub_cor.fts' ;output file


; read file

if file_test(w_dir+filein) eq 0 then begin
res=dialog_message('File not found: '+w_dir+filein)
return
endif

obj=readfits(w_dir+filein,header) ; read OBJ
nx=sxpar(header,'NAXIS1')
ny=sxpar(header,'NAXIS2')
nz=sxpar(header,'NAXIS3')
w=2 ; border extention

a=''
refpos=fltarr(3,nz)
openr,u,w_dir+fileref,/get_lun
for i=1,3 do readf,u,a ; skip header
readf,u,refpos
close,u
free_lun,u




; array creation
x1=w
x2=x1+nx-1
y1=w
y2=y1+ny-1


map=findgen(nx+2*w,ny+2*w)
x0=refpos[1,nz/2] ;\refernce barycenter position
y0=refpos[2,nz/2] ;/
FOR i=0,nz-1 do BEGIN

   dx=x0-refpos[1,i]
   dy=y0-refpos[2,i]
   ; border extention:
   map[x1:x2,y1:y2]=reform(obj[*,*,i])
   map[0:x1-1,y1:y2]=reverse(map[x1:x1+w-1,y1:y2],1)
   map[x2+1:*,y1:y2]=reverse(map[x2-w+1:x2,y1:y2],1)
   map[*,0:y1-1]=reverse(map[*,y1:y1+w-1],2)
   map[*,y2+1:*]=reverse(map[*,y2-w+1:y2],2)
   mul=5l

   map=rebin(map,mul*(nx+2*w),mul*(ny+2*w))
   old=map
   map=shift_image(map,dx*mul,dy*mul)
   new=map
   map=rebin(map,(nx+2*w),(ny+2*w),/sample)

   obj[*,*,i]=map[x1:x2,y1:y2]
 
     if i mod 200 eq 0 then print,'Channel ',i
ENDFOR




sxaddhist,'IFURED_ATM_COR.PRO: differencial  dispersion correction from file: '+fileref,header
writefits,w_dir+fileout,obj,header


END
