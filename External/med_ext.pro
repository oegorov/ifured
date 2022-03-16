function MED_EXT,vector,mwin
; median filter for vector with extention border
  ll=n_elements(vector)
  vec=[reverse(vector(0:mwin-1)),vector,reverse(vector(ll-mwin:*))]
  vec=median(vec,mwin)
return,vec(mwin:ll+mwin-1)
END