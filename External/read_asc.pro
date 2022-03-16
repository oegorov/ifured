; function for ASCII file reading
function READ_ASC,file,head
s=''


if n_params() eq 1 then head=0
if n_params() eq 0 then begin
 print,'result=READ_ASC(file,head)'
 return,[-1]
endif

; determination of the column number

tst=file_test(file)
if tst eq 0 then begin
  res=dialog_message('File '+file+' isn''t found!')
 return,0
endif

openr,u,file,/get_lun
for i=1,head do readf,u,s
readf,u,s
 ns=strtrim(strcompress(s),2)
 nc=n_elements(str_sep(ns,' '))
close,u
free_lun,u
;read file
d=float(str_sep(ns,' '))
openr,u,file,/get_lun
for i=0,head do readf,u,s
s=' '
while not(eof(u)) and (s ne '') do begin
readf,u,s

ns=strtrim(strcompress(s),2)
if ns ne '' then d=[d,str_sep(ns,' ')]
endwhile
close,u
free_lun,u
;print,d
d=reform(d,nc,n_elements(d)/nc)
return,d
END