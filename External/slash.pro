; add slash in directory name
Function SLASH,name
 l=strlen(name)
 if  !VERSION.OS_family eq 'Windows' then sl='\' else sl='/'

 if strmid(name,l-1,1) ne sl then name=name+sl
 return,name

END