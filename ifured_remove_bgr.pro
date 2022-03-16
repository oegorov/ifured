FUNCTION IFURED_REMOVE_BGR, image

  
  bgr=estimate_background(image,150)
  bgr+=estimate_background(image-bgr,50)
  bgr+=estimate_background(image-bgr,20)
  ;writefits,"/Volumes/iData/Science/IFU/Reduction/mkn78_nov17_v940@600/masked_ini/bgr.fits",bgr
  image_corr=image-bgr

RETURN, image_corr
END