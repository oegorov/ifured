PRO IFURED_COMMON_DEF

  COMMON IFURED_PARAMS, filepar, ifu_dir, stand_dir, w_dir, d_dir, log_dir, night, obj_cub, neon_cub, flat_cub, eta_cub, star_cub,$
                        test_cub, obj_file, neon_file, flat_file, eta_file, star_file, test_file, star_table, $
                        zip, slit_break_left, slit_break_right, crop, no_star,no_sky,tr_cr,rawdata_dir, reddata_dir,$
                        wl_out, sky_deg, lin_deg, fw_lin, lin_yfit, tra_win,tra_dspec, first_eta, last_eta, lam_test,bin
  COMMON IFURED_WIDGET_ELEMENTS, ifu_log_text, ifu_mb, but_id, status_field,titfont, sz, log_field, tr_cr_wid, ovscan,slit_bord,norm_cub_sky,norm_cub_flat,$
                                  tra_params,win_geometry_x,win_geometry_y,lin_params,wlout_params,sky_params,spec_extra_show, optimal,search_geometry,sysvel_field
  COMMON IFURED_REDUCTION_PROCESS, reduct_steps, markers
  COMMON IFURED_EXTERNAL, unzipper, clean_dir
  
  ;####### Определение необходимых начальных настроек
  
  
  
  tmp={sizes,x:0L,y:0L}
  clean_dir=['']
  
  ifu_dir="~/Science/IDLWorkspace/IFURED/"  ; Root directory with installed IFURED
  stand_dir="~/Science/standards/data/"     ; Direcrory with spectra of spectrophotometric standards
  log_dir="~/Science/"                      ; Direcrory with log files
  rawdata_dir="/Volumes/iData/Science/IFU/" ; Direcrory with raw data
  reddata_dir="/Volumes/iData/Science/IFU/Reduction/" ; Direcrory with reduced data
  
  if !VERSION.OS_FAMILY  eq 'Windows' then unzipper=ifu_dir+'7z.exe x' else unzipper='unzip'  ; Indicate your external package for unzip cubes (if necessary)
  
  ;##### Размеры блоков в пикселях
  sz=[{sizes,0,0},$  ;0 - Все окно
      {sizes,360,840},$   ;1 - База для логов и кнопок
      {sizes,100,25},$   ;2 - Размер кнопок
      {sizes,790,840},$   ;3 - База для селекторов 
      {sizes,400,650}]   ;4 - Размер поля для лога
      
   ; ndisplays=1   ; Колличество дисплеев
   ;im_disp_pos=[0.0,0.0,1.,1.]   ; Положение изображения на дисплее
   ; zoom_vals=['2','4','6','8','10']  ; Возможные варианты увеличения для "микроскопа" (значение по умолчанию - 3 по порядку)
    
    ; Шрифт, используемый для заголовков
    if (!VERSION.OS_FAMILY eq "Windows") then titfont="Sans Serif" else titfont="-misc-fixed-bold-r-normal--13-120-75-75-c-70-iso8859-1" 


END