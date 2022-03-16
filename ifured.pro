;******************************************
;* IFURED: IFU REDUCTION                  *
;*       Tested on IDL 7.0                *
;*  This routine uses astrolib, coyote,   *
;*          mpfit, buie libraries         *
;*                   (c) Egorov Oleg      *
;*                      2017              *
;******************************************
.rnew   external/read_asc.pro
.rnew   external/slash.pro
.rnew   external/lowess.pro
.rnew   external/moment4.pro
.rnew   external/robomean.pro
.rnew   external/trimrank.pro
.rnew   external/goodpoly.pro
.rnew   external/robust_poly_fit.pro
.rnew   external/cosin_apod.pro
.rnew   external/med_ext.pro
.rnew   external/cross_c.pro
.rnew   external/cross_norm.pro
.rnew   external/multigaus.pro
.rnew   external/vecshift.pro
.rnew   external/def_vecshift.pro
.rnew   external/shift_image.pro
.rnew   external/acre.pro
.rnew   external/read_st.pro
.rnew   external/calc_ext.pro
.rnew   viewers/ifured_viewspec_common_def.pro
.rnew   viewers/ifured_viewspec_wplot.pro
.rnew   viewers/ifured_viewspec.pro
.rnew   viewers/ifured_cviz_common_def.pro
.rnew   viewers/ifured_cviz.pro

.rnew   ifured_common_def.pro
.rnew   ifured_auxiliary.pro
.rnew   ifured_go.pro


.rnew   ifured_mean_bi.pro
.rnew   ifured_cre_bias.pro
.rnew   ifured_ch_both2.pro
.rnew   ifured_addexp.pro
.rnew   ifured_combine.pro
.rnew   ifured_traectory.pro
.rnew   ifured_peaks_fibers.pro
.rnew   IFURED_PACK_FIBERS.pro
.rnew   ifured_geometry.pro
.rnew   ifured_test_warp.pro
.rnew   IFURED_CORRECT_FIBERS_XSHIFT.pro
.rnew   ifured_cross_talk_rem.pro
.rnew   ifured_cub_extraction.pro
.rnew   ifured_dispersion_curve.pro
.rnew   ifured_lineris.pro
.rnew   ifured_flatcub_reduct.pro
.rnew   ifured_cub_sky_subtract.pro
.rnew   ifured_dqe_sent.pro
.rnew   ifured_crea_sent.pro
.rnew   ifured_corrsent.pro
.rnew   ifured_atm_dis.pro
.rnew   ifured_atm_cor.pro
.rnew   ifured_cre_maps.pro
ifured_go
