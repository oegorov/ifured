function CALC_EXT,lambda,Z
;calculation extintion
a=0.008 & c=0.115
extin=(a*1./((lambda/10000.)^4.)+c)/2.3
extin=10.^(-extin/cos(z*!DTOR))
return,extin
end