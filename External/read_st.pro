;
;Function read_star
;
;   10.10.01  VLA
;
function read_st,table_name,wavelength,PRINT=print
;
N=N_elements(wavelength)
lambda1=8000   &   lambda2=15000
if keyword_set(print) then message,'READ STANDART STAR SPECTRUM FROM FILE: '+table_name,/cont
;
N_tab=numlines(table_name)
table=strarr(N_tab)
openr,UNIT,table_name,/GET_LUN
readf,UNIT,table
close,UNIT
FREE_LUN,UNIT
lambda_tab=float(strmid(strcompress(table),0,10))
flux_tab=float(strmid(strcompress(table),11,22))
;
if wavelength(N-1) gt lambda_tab(N_tab-1) then begin
;linear extrapolation spectra star
R=where(lambda_tab ge lambda1)
x=lambda_tab(R)
y=alog10(flux_tab(R))
f=goodpoly(x,y,1,3,Yfit)
N_new=fix((lambda2-lambda_tab(N_tab-1))/10.)
x_new=findgen(N_new)*10+lambda_tab(N_tab-1)+10
y_new=f(0)+f(1)*x_new
lambda_tab=[lambda_tab,x_new]
flux_tab=[flux_tab,10.^y_new]
endif
;
;creation spectra standard star in new wavelength scale
index=where(lambda_tab gt wavelength(0) and $
		lambda_tab lt wavelength(N-1))
lambda_tab=lambda_tab(index)
flux_tab=flux_tab(index)
flux_obs=INTERPOL(flux_tab,lambda_tab,wavelength)*1.0E-016
return,flux_obs
end
