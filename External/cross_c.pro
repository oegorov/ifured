function CROSS_C,A,B
 ; Cross-correlation between vectors A and B
 N=n_elements(A)
 shiftval=n/2 +(n mod 2)
 ; mean subtraction
 Am=A-total(A)/N
 Bm=B-total(B)/N
 ; dispersion:
 covA=total(Am^2/n)
 covB=total(Bm^2/n)
 Am=Am/sqrt(covA)
 Bm=Bm/sqrt(covB)
 ;FFT

 cross=shift(float(fft(fft(Am)*conj(fft(Bm)),1)),shiftval)
 ;cross=cross/(sqrt(covA*covB))
 return,cross

END;CROSS_C
