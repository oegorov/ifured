pro IFURED_viewspec_common_def

  COMMON IFURED_viewspec_Wbase, base_main,base_ccd,base_platform,base_scorpio,base_view,base_plot
  COMMON IFURED_viewspec_LABELblock, pixel_label,value_label,mean_label,rms_label,POS_label,FWHM_label,FLUX_label,SEE_label,wave_label
  COMMON IFURED_viewspec_SLIDEdlock, viewrange,viewbackground
  COMMON IFURED_viewspec_Wbutton, button3,button4,button5,pln_group
  COMMON IFURED_viewspec_IMAblock, file,h,frame,ima,zero_pix,wdir,Nx,Ny,Nz,sc,bin,plane,d_lambda,lambda_0,over,x_size,y_size
  COMMON IFURED_viewspec_RANGEblock, range,level,avg_ima,rms_ima
  COMMON IFURED_viewspec_WINblock, win_num,swin,planeW,draw,micro
  COMMON IFURED_viewspec_WIN, num_win,s_win,xs,ys,xgau,sw_gau,x_pre,y_pre
  COMMON IFURED_viewspec_BOXblock, map,x1,x2,y1,y2,x_2,y_2,sw
  COMMON IFURED_viewspec_REG, x_min,y_min,x_max,y_max,region,x,y,a,values
end