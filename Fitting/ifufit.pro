PRO IFT_DEF
;#### Here all common blocks and parameters for IFUFIT are defined
  COMMON IFT_WIDGET_ELEMENTS, ift_buttons, ift_mb, ift_titfont,ift_sz
  COMMON IFT_DATA, curfile
  
  if (!VERSION.OS_FAMILY eq "Windows") then ift_titfont="Sans Serif" else $
                        ift_titfont="-misc-fixed-bold-r-normal--13-120-75-75-c-70-iso8859-1"
                        
  tmp={sizes,x:0L,y:0L}
  tmp={obj_par,name:'',uval:'',obj:0L,val:0E,sens:1L}
  
  ift_sz=[{sizes,0,0},$  ;0 - Все окно
        {sizes,512,512},$   ;1 - Дисплей
        {sizes,125,30},$   ;2 - Размер кнопок
        {sizes,400,400},$   ;3 - Дисплей для спектра
        {sizes,25,70},$   ;4 - Размер каждого поля мониторов (два варианта)
        {sizes,23,23}]    ; 5 - резерв для скролл-бара
    
  
END



;#### Procedures for widgets design #####
PRO IFT_Buttons_Cre,base,name,uval,but_obj,xs=xs,ys=ys,break_arr=break_arr,frame=frame,$
           sens=sens,nonexclusive=nonexclusive,EXCLUSIVE=EXCLUSIVE,No_release=No_release,$
            cre_stat=cre_stat ,status=status, titfont=titfont
       ; #### Creates buttons
  nbut=n_elements(name)
  tmp=base
  if not (keyword_set(xs)) then xs=0
  if not (keyword_set(ys)) then ys=0
  if not (keyword_set(frame)) then frame=0
  if not (keyword_set(No_release)) then No_release=0
  
  if keyword_set(cre_stat) then status=lonarr(nbut)
  
  
  nbr=N_elements(break_arr)
  j=0
  dobrk=-10
  IF (nbr gt 0) then begin
    break_arr=break_arr[sort(break_arr)]
    dobrk=break_arr[j]
  ENDIF
  
  but_obj=lonarr(nbut)
  For i=0,nbut-1 do begin
    if (nbr gt 0 and dobrk eq i-1) then begin
      j++
      if (nbr gt j) then dobrk=break_arr[j] else dobrk=-10
      i--
      tmp_base=WIDGET_BASE(base,ys=10,xs=30,xpad=0,ypad=0,xoffset=0,yoffset=0,frame=frame)

    endif else begin 

      thisbutbase=WIDGET_BASE(base,/row,xpad=0,ypad=0,xoffset=0,yoffset=0) 

      if (xs+ys gt 0) then tmp=WIDGET_BASE(thisbutbase,xs=xs,ys=ys,nonexclusive=nonexclusive,exclusive=exclusive,xpad=0,ypad=0,xoffset=0,yoffset=0) else $
          tmp=WIDGET_BASE(thisbutbase,nonexclusive=nonexclusive,exclusive=exclusive,xpad=0,ypad=0,xoffset=0,yoffset=0)
  
      but_obj[i]=WIDGET_BUTTON(tmp,value=name[i],uvalue=uval[i],xsize=xs,ysize=ys,sensitive=sens[i],frame=frame,NO_RELEASE=No_release)
      if keyword_set(cre_stat) then status[i]=WIDGET_LABEL(thisbutbase,val="     ",font=titfont)
    endelse
  ENDFOR
END
                








PRO IFUFIT_GO_EVENT,event
  COMMON IFT_WIDGET_ELEMENTS
  COMMON IFT_DATA
  WIDGET_CONTROL,event.ID,get_uvalue=ev
  
  CASE ev OF
    'quit': BEGIN
;        is_set=WIDGET_INFO(ks_anal_b,/VALID_ID)
;        if (is_set eq 1) then WIDGET_CONTROL, ks_anal_b, /DESTROY
;             is_set=WIDGET_INFO(ks_resman_b,/VALID_ID)
;             if (is_set eq 1) then WIDGET_CONTROL, ks_resman_b, /DESTROY
;             is_set=WIDGET_INFO(ks_profman_b,/VALID_ID)
;             if (is_set eq 1) then WIDGET_CONTROL, ks_profman_b, /DESTROY
;             KS_FREE_ALL_POINTERS
       WIDGET_CONTROL,/DESTROY,event.top
    END
    
    else:
  ENDCASE
END

PRO IFUFIT_GO,openfile=openfile,ifured_mb=ifured_mb
  COMMON IFT_WIDGET_ELEMENTS
  COMMON IFT_DATA
  if keyword_set(openfile) then curfile=openfile
  if keyword_set(ifured_mb) then ift_mb=WIDGET_BASE(TITLE="IFU data FITting",/row,group_leader=ifured_mb) $
  else ift_mb=WIDGET_BASE(TITLE="IFU data FITting",/row)
  
  ;####### Base for buttons
  ift_butbase=WIDGET_BASE(ift_mb,/col)
  ;==== Buttons & monitors
      
    ift_buttons=[{obj_par,'Load Cube','load_cube',0,0,1},$ ;#0
               {obj_par,'Cube Header','show_header_c',0,0,0},$ ;#1
               {obj_par,'Analysis','analysis',0,0,0},$ ;#2
               {obj_par,'Results Manager','res_man',0,0,0},$ ;#3
               {obj_par,'Quit','quit',0,0,1},$ ;#4
               ;Buttons for zoom
               {obj_par,'Reset to 1','zoom_reset_cub',0,0,0},$ ;#5
               {obj_par,'Set Color','coltab_cub',0,0,1},$  ;#6
               {obj_par,'Reset to 1','zoom_reset_res',0,0,0},$ ;#7
               {obj_par,'Set Color','coltab_res',0,0,0},$ ;#8
               {obj_par,'Save PS','saveps_cub',0,0,0},$ ;#9
               {obj_par,'Save PS','saveps_res',0,0,0}] ;#10
               
  
    ift_butbase_cols = WIDGET_BASE(ift_butbase,/row, xpad=0,xoffset=0)
    ift_butbase_l = WIDGET_BASE(ift_butbase_cols,/col, xpad=0,xoffset=0)
    IFT_Buttons_Cre,ift_butbase_l,ift_buttons[0:1].name,ift_buttons[0:1].uval,output,sens=ift_buttons[0:1].sens,xs=ift_sz[2].x,ys=ift_sz[2].y
    ift_buttons[0:1].obj=output
    
    ift_break=WIDGET_BASE(ift_butbase,/column, ys=5)
    
    ind=[2,3,4]
    IFT_Buttons_Cre,ift_butbase,ift_buttons[ind].name,ift_buttons[ind].uval,output,sens=ift_buttons[ind].sens,xs=ift_sz[2].x,ys=ift_sz[2].y,break_arr=[1]
    ift_buttons[ind].obj=output
      
    ift_break=WIDGET_BASE(ift_butbase,/column, ys=5)
    
    
    
    cgCENTERTLB,ift_mb
    if keyword_set(ifured_mb) then leader=ifured_mb else leader=ift_mb
    WIDGET_CONTROL, ift_mb, /realize,group_leader=leader

    XMANAGER,'IFUFIT_GO', ift_mb,no_block=1
    
END

PRO IFUFIT,file=file, ifured_mb=ifured_mb
   
   IFT_DEF
   ;RESOLVE_ROUTINE,"_fitting"
   ;RESOLVE_ROUTINE,"ks_res_manager"
   ;RESOLVE_ROUTINE,"ks_profile_manager"
   ;RESOLVE_ROUTINE,"ks_analysis_tuner"
   ;RESOLVE_ROUTINE,"ks_curprof_fit_manager"
   IFUFIT_GO, openfile=file, ifured_mb=ifured_mb
   
END
