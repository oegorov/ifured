function CROSS_NORM,ffir,fsec,nspr
;+ 
;NAME: 
;      CROSS_NORM
; 
;CLASS: 
;
;    cross-correlation 
; 
;CATEGORY: 
; 
;PURPOSE: 
; 
;   To compute a cross correlation for two spectral segments 
;   which are sampled on the same linear or log(lambda) scale,normalised at 
;   dispersion
; 
;CALLING SEQUENCE: 
; 
;   result=CROSS_NORM(FFIR,FSEC,NSPR) 
; 
;PARAMETERS: 
; 
;   FFIR    (REQ) (I)  (1) (F) 
;           Required input vector giving the flux data for the first  
;           spectrum. 
; 
;   FSEC    (REQ) (I)  (1) (F)  
;           Required input vector giving the flux data for the second   
;           spectrum.  
;  
;   NSPR    (REQ) (I)  (0) (F)  
;           Required input parameter specifying the spectral range to  
;           be considered in the cross-correlation function.  
;  
;  
;EXAMPLES:  
;  
;    To compute the cross-correlation function for two spectra, FIRST  
;    and SECOND, using the recommended initial spectral range from CRSCOR,  
;  
;    result=CROSS_NORM(FIRST,SECOND)
;  
;SYSTEM VARIABLES USED:  
;  
;INTERACTIVE INPUT:  
;  
;SUBROUTINES CALLED:  
;  
;    PARCHECK  
;  
;FILES USED:  
;  
;SIDE EFFECTS:  
;  
;RESTRICTIONS:  
;  
;NOTES:  
;       Assumes same number of elements in both spectra. (Both fluxes are  
;       divided by the number of elements in the first spectrum.)  
;  
;PROCEDURE:  
;  
;     CROSS is determined for (2*nspr + 1) tags or shifts going from -15  
;     to +15 shifts from the starting locations.   
;     After subtracting the average flux from each spectrum, the cross  
;     correlation function is computed as follows for each point in   
;     the spectra,   
;      TEMP = (second spectrum) * SHIFT(first spectrum,ns)  
;      CROSS(L) = TOTAL(TEMP(ls:us)/nele)   
;  
;  
;MODIFICATION HISTORY:  
;  
;	25 Jun 1991  PJL cleaned up; added PARCHECK and parameter eq 0  
;			 print; tested on SUN and VAX; updated prolog  
;  
;-  

 npar = n_params(0)  
 if npar eq 0 then begin  
    print,' CRSPROD,FFIR,FSEC,NSPR,CROSS,CRMIN,CRMAX'  
    retall  
 endif  ; npar  

 mi   = n_elements(ffir)   
 ff= ffir/sqrt(total(ffir^2)/mi)	;normalised spectra
 fs= fsec/sqrt(total(fsec^2)/mi)
 ntot = nspr+nspr+1  
 cross= fltarr(ntot)  
 temp = fs  
 for l=0,ntot-1 do begin  
    ns = nspr - l  
    temp = fs*shift(ff,ns)  
    ls = ns > 0  
    us = mi  - 1 + (ns < 0)  
    nele = us - ls + 1  
    cross(l) = total(temp(ls:us)) / nele  
 endfor  ; l  
 return,cross
 end  
