;**************************************
;* IFU data reduction
;* search all bias-frames and mean bias creating
;* ************************************


PRO IFURED_CRE_BIAS,dir=dir, binning=binning,w_dir=w_dir,nobin=nobin,fileout=fileout, logfield=logfield,zip=zip


if not(keyword_set(binning)) then binning='1x1'
if not(keyword_set(fileout)) then fileout='meanbias.fts'
if keyword_set(nobin) then binning='no'


;w_dir=slash(w_dir)
d_dir=slash(dir)


IF keyword_set(zip) then begin
  ; search zip-files
  pattern="*.zip"
  list=file_search(d_dir+pattern,/fold_case) ;list of all zip-files
  if not keyword_set(list[0]) then begin
    res=DIALOG_MESSAGE("No .ZIP files found in D_DIR!")
    IFURED_LOG_UPD, ["ERROR: No bias frame found!",'']
    return 
  endif
  for i=0,n_elements(list)-1 do begin
    fdecomp,list[i],disk,file_dir,file_name,qual
    if not file_test(slash(disk+file_dir)+file_name,/dir) then IFURED_UNZIP_CUB, slash(disk+file_dir), '', file_name
  endfor
ENDIF


; search bias-frames:
pattern="*.f*ts"
list=file_search(d_dir,pattern,/fold_case) ;list of all fits-files

num=n_elements(list)
blist=['']
print,n_elements(list), ' FITS-files in directory ',d_dir

if n_elements(list) eq 1 then begin
 res=dialog_message(["No FITS-frames in the directory "+w_dir, $
          'Please, check D_DIR'])
 IF keyword_set(logfield) then IFURED_LOG_UPD, ["ERROR: No bias frame found!",'']
return
endif


for i=0, num-1 do begin
  h=headfits(list[i])
  typ=strcompress(strtrim(sxpar(h,'IMAGETYP'),2),/remove_all)
  binc=strcompress(strtrim(sxpar(h,'BINNING'),2),/remove_all)
  if keyword_set(nobin) then binc='no'
  if (STRLOWCASE(typ) eq 'bias') and (binc eq strcompress(binning,/remove_all)) then blist=[blist,list[i]]
endfor

nbias=n_elements(blist)-1
if nbias eq 0 then begin
  res=dialog_message(["No bias-frames with necessary binning!"])
  IF keyword_set(logfield) then IFURED_LOG_UPD, ["ERROR: No bias frame with necessary binning found!",'']
  return
endif
 blist=blist[1:*]
 num=n_elements(blist)
 message,string(num,format='(I0)')+' bias frames was found!',/con
 IF keyword_set(logfield) then IFURED_LOG_UPD, string(num,format='(I0)')+' bias frames was found'

if num eq 0 then begin
 res=dialog_message("No bias-frames in the directory "+w_dir)
 IF keyword_set(logfield) then IFURED_LOG_UPD, ["ERROR: No bias frame found!",'']
 return
endif

 h=headfits(blist[0])
 xs=sxpar(h,'NAXIS1')
 ys=sxpar(h,'NAXIS2')

bias=intarr(xs,ys,num)

; read images

for i=0, num-1 do begin
 print,blist[i]
 tmp=readfits(blist[i],htmp,/sil)

 xstmp=sxpar(htmp,'NAXIS1')
 ystmp=sxpar(htmp,'NAXIS2')

 if xs ne xstmp or ys ne ystmp then begin
  res=dialog_message(['Size of the image '+blist[i], 'is not equal to the size of the first bias frame!','Please remove it!'] )
  return
 endif

 bias[*,*,i]=tmp

endfor

;Make meanbias
 IFURED_MEAN_BI,bias,meanbias

 ; write result
 MKHDR,header,meanbias
 sxaddpar,header,'OBJECT','meanbias'
 sxaddhist,'IFURED_CRE_BIAS: used files:'+blist,header

IF keyword_set(logfield) then IFURED_LOG_UPD, ["Used files: ",blist]

 ;directory creaion:
 s=file_search(w_dir+'*')
 if s(0) eq '' then SPAWN,'mkdir '+w_dir

 writefits,w_dir+fileout,meanbias/num,header
 IF keyword_set(logfield) then IFURED_LOG_UPD, ["Meanbias created: "+fileout,""]
END