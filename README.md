This repository accompanies the manuscript titled "Large-scale spatial variability in urban tolerance of birds" by Callaghan et al.

The main script to complete the analyses is titled "EDA.R". But the first step was to randomly sample polygons, which was done in the "randomly_sample_polygons.R" script. The mixed model described in the text was performed in the "quick_mixed_model_to_look_at_variance_DB.R" script.

The scripts starting with "assess" were used to look at components of supporting information and test for robustness of our results. The other scripts are small helper scripts and evident how they were used to help with figures etc.

The Data folder contains all the raw data from eBird stratified by month, with the format: ebird_data_raw_MONTH.RDS. The Clements checklist is also included, as well as some spatial layers used for analyses that are called in specific scripts. The VIIRS scores for each eBird checklist from Google Earth Engine are stored in the folder "checklists_viirs_scores".

The code last worked on November 23rd 2022, under the following session information:

R version 4.1.2 (2021-11-01)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows Server x64 (build 17763)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] rptR_0.9.22         patchwork_1.1.1     forcats_0.5.1       ggridges_0.5.3      tibble_3.1.6        urbnmapr_0.0.0.9002 rnaturalearth_0.1.0 readr_2.1.2         tidyr_1.1.4        
[10] dplyr_1.0.7         ggplot2_3.3.5       sf_1.0-5           

loaded via a namespace (and not attached):
 [1] tidyselect_1.1.1   janitor_2.1.0      purrr_0.3.4        splines_4.1.2      lattice_0.20-45    snakecase_0.11.0   colorspace_2.0-2   vctrs_0.3.8        generics_0.1.2    
[10] mgcv_1.8-38        utf8_1.2.2         rlang_1.0.0        nloptr_2.0.0       e1071_1.7-9        pillar_1.7.0       glue_1.6.1         withr_2.4.3        DBI_1.1.2         
[19] sp_1.4-6           plyr_1.8.6         lifecycle_1.0.1    stringr_1.4.0      munsell_0.5.0      gtable_0.3.0       coda_0.19-4        tzdb_0.2.0         parallel_4.1.2    
[28] class_7.3-20       fansi_1.0.2        broom_0.7.12       Rcpp_1.0.8         KernSmooth_2.23-20 scales_1.1.1       backports_1.4.1    arm_1.12-2         classInt_0.4-3    
[37] abind_1.4-5        lme4_1.1-27.1      hms_1.1.1          stringi_1.7.6      ggrepel_0.9.1      grid_4.1.2         cli_3.1.1          tools_4.1.2        magrittr_2.0.2    
[46] proxy_0.4-26       wesanderson_0.3.6  crayon_1.4.2       ape_5.6-1          pkgconfig_2.0.3    ellipsis_0.3.2     MASS_7.3-55        Matrix_1.4-0       broom.mixed_0.2.7 
[55] lubridate_1.8.0    assertthat_0.2.1   minqa_1.2.4        R6_2.5.1           boot_1.3-28        units_0.7-2        nlme_3.1-155       compiler_4.1.2   

If you have any questions, please email me at callaghan.corey.t@gmail.com.

