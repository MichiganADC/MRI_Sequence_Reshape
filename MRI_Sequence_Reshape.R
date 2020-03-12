# MRI_Sequence_Reshape.R


# Packages ----

library(dplyr)
library(tidyr)
library(readr)
library(stringr)


# Config + Helpers ----

source("~/Box/Documents/R_helpers/config.R")
source("~/Box/Documents/R_helpers/helpers.R")


# Extract ----

fields_ug_raw <-
  c(
    "subject_id",
    "exam_date"
  )

forms_ug_raw <-
  c(
    "mri_imaging_form"
  )

fields_ug <- fields_ug_raw %>% paste(collapse = ",")
forms_ug <- forms_ug_raw %>% paste(collapse = ",")

json_ug <- export_redcap_records(uri = REDCAP_API_URI,
                                 token = REDCAP_API_TOKEN_UMMAP_GEN,
                                 fields = fields_ug,
                                 forms = forms_ug,
                                 vp = FALSE)

df_ug <- json_ug %>% 
  jsonlite::fromJSON() %>% 
  na_if("")

df_ug_cln <- df_ug %>% 
  mutate(exam_date = lubridate::as_date(exam_date)) %>% 
  filter(!is.na(exam_date)) %>% 
  filter(str_detect(subject_id, "^UM\\d{8}$")) %>% 
  filter(exam_date >= lubridate::as_date("2017-03-01")) %>% 
  type_convert(col_types = cols(.default = col_integer(),
                                subject_id = col_character(),
                                redcap_event_name = col_character(),
                                exam_date = col_date(),
                                mri_date = col_date(),
                                mri_sched_date = col_date(),
                                fmri_tech = col_character(),
                                ra_name = col_character(),
                                seq_num = col_character(),
                                mri_sub_id = col_character(),
                                start_time = col_character(),
                                end_time = col_character(),
                                scan_01_3plane_motion = col_number(),
                                scan_02_calibration_motion = col_number(),
                                scan_03_t1_sagittal_motion = col_number(),
                                scan_04_field_map_motion = col_number(),
                                scan_05_func_rest_motion = col_number(),
                                scan_06_vasc_3dasl_motion = col_number(),
                                scan_07_vasc_2dfq_motion = col_number(),
                                scan_08_fm_objloc_motion = col_number(),
                                scan_09_func_objloc_motion = col_number(),
                                scan_10_t2_flair_sagittal_motion = col_number(),
                                scan_11_t2_sagittal_motion = col_number(),
                                scan_12_diffusion_field_map_motion = col_number(),
                                scan_13_dti_motion = col_number(),
                                scan_14_b1_mapping_motion = col_number(),
                                scan_15_mwf_motion = col_number(),
                                scan_16_ihmt_motion = col_number(),
                                scan_17_pcasl_motion = col_number(),
                                scan_18_mrf_motion = col_number(),
                                scan_funcrest_perc_good = col_number(),
                                scan_3plane_notes = col_character(),
                                scan_calibration_notes = col_character(),
                                scan_t1sagittal_notes = col_character(),
                                scan_fieldmap_notes = col_character(),
                                scan_funcrest_notes = col_character(),
                                scan_vasc2dfq_notes = col_character(),
                                scan_vasc3dasl_notes = col_character(),
                                scan_fmobjloc_notes = col_character(),
                                scan_funcobjloc_notes = col_character(),
                                scan_t2flairsagittal_notes = col_character(),
                                scan_t2sagittal_notes = col_character(),
                                scan_diffusionfieldmap_notes = col_character(),
                                scan_dti_notes = col_character(),
                                scan_b1mapping_notes = col_character(),
                                scan_mwf_notes = col_character(),
                                scan_ihmt_notes = col_character(),
                                scan_pcasl_notes = col_character(),
                                scan_mrf_notes = col_character(),
                                mri_co2_before = col_number(),
                                mri_co2_after = col_number(),
                                reason_not = col_character(),
                                mri_form = col_character(),
                                pet_staff_initials = col_character(),
                                # other stuff here... probably all empty
                                mri_consensus_notes = col_character()))
glimpse(df_ug_cln)

df_ug_cln_slc <- df_ug_cln %>% 
  select(subject_id, redcap_event_name, matches("^scan_\\d{2}_"))



df_ug_cln_slc_rsh1 <- df_ug_cln_slc %>% 
  gather(key = "scan", value = "completed", -subject_id, -redcap_event_name) %>% 
  filter(!is.na(completed))
df_ug_cln_slc_rsh1 %>% pull(scan) %>% sort() %>% unique()


df_ug_cln_slc_rsh2 <- df_ug_cln_slc_rsh1 %>% 
  mutate(scan = case_when(
    scan == "scan_03_t1_sagittal" ~ "scan_03_t1sagittal",
    scan == "scan_03_t1_sagittal_motion" ~ "scan_03_t1sagittal_motion",
    scan == "scan_04_field_map" ~ "scan_04_fieldmap",
    scan == "scan_04_field_map_motion" ~ "scan_04_fieldmap_motion",
    scan == "scan_05_func_rest" ~ "scan_05_funcrest",
    scan == "scan_05_func_rest_motion" ~ "scan_05_funcrest_motion",
    scan == "scan_06_vasc_3dasl" ~ "scan_06_vasc3dasl",
    scan == "scan_06_vasc_3dasl_motion" ~ "scan_06_vasc3dasl_motion",
    scan == "scan_07_vasc_2dfq" ~ "scan_07_vasc2dfq",
    scan == "scan_07_vasc_2dfq_motion" ~ "scan_07_vasc2dfq_motion",
    scan == "scan_08_fm_objloc" ~ "scan_08_fmobjloc",
    scan == "scan_08_fm_objloc_motion" ~ "scan_08_fmobjloc_motion",
    scan == "scan_08_t2_flair_sagittal" ~ "scan_08_t2flairsagittal",
    scan == "scan_09_func_objloc" ~ "scan_09_funcobjloc",
    scan == "scan_09_func_objloc_motion" ~ "scan_09_funcobjloc_motion",
    scan == "scan_09_t2_sagittal" ~ "scan_09_t2sagittal",
    scan == "scan_10_diffusion_field_map" ~ "scan_10_diffusionfieldmap",
    scan == "scan_10_t2_flair_sagittal" ~ "scan_10_t2flairsagittal",
    scan == "scan_10_t2_flair_sagittal_motion" ~ "scan_10_t2flairsagittal_motion",
    scan == "scan_11_t2_sagittal" ~ "scan_11_t2sagittal",
    scan == "scan_11_t2_sagittal_motion" ~ "scan_11_t2sagittal_motion",
    scan == "scan_12_b1_mapping" ~ "scan_12_b1mapping",
    scan == "scan_12_diffusion_field_map" ~ "scan_12_diffusionfieldmap",
    scan == "scan_12_diffusion_field_map_motion" ~ "scan_12_diffusionfieldmap_motion",
    scan == "scan_14_b1_mapping" ~ "scan_14_b1mapping",
    scan == "scan_14_b1_mapping_motion" ~ "scan_14_b1mapping_motion",
    TRUE ~ scan
  ))
df_ug_cln_slc_rsh2 %>% pull(scan) %>% sort() %>% unique()

df_ug_cln_slc_rsh3 <- df_ug_cln_slc_rsh2 %>% 
  mutate(order = as.integer(str_extract(scan, "\\d{2}"))) %>% 
  mutate(scan = str_replace(scan, "\\d+_", ""))

df_ug_cln_slc_rsh4 <- df_ug_cln_slc_rsh3 %>% 
  mutate(scan = str_replace(scan, "^(.*)_motion$", "\\1_goodval"))

df_ug_cln_slc_rsh5_rest <- df_ug_cln_slc_rsh4 %>% 
  filter(str_detect(scan, "^.*_goodval$", negate = TRUE))
df_ug_cln_slc_rsh5_gval <- df_ug_cln_slc_rsh4 %>% 
  filter(str_detect(scan, "^.*_goodval$"))

df_ug_cln_slc_rsh6_rest <- df_ug_cln_slc_rsh5_rest %>% 
  gather(key = "key", value = "value", -subject_id, -redcap_event_name, -scan)
df_ug_cln_slc_rsh6_gval <- df_ug_cln_slc_rsh5_gval %>% 
  select(subject_id, redcap_event_name, key = scan, value = completed)  %>% 
  spread(key, value)

df_ug_cln_slc_rsh7_rest <- df_ug_cln_slc_rsh6_rest %>% 
  unite("scan_key", scan, key, sep = "_") %>% 
  filter(!(subject_id == "UM00000833" & redcap_event_name == "visit_08_arm_1")) %>% 
  filter(!(subject_id == "UM00001693" & redcap_event_name == "baseline_arm_1")) %>% 
  filter(!(subject_id == "UM00002019" & redcap_event_name == "baseline_arm_1"))

df_ug_cln_slc_rsh8_rest <- df_ug_cln_slc_rsh7_rest %>% 
  spread(key = scan_key, value = value)

df_ug_cln_rsh9 <- df_ug_cln_slc_rsh8_rest %>% 
  full_join(df_ug_cln_slc_rsh6_gval, 
            by = c("subject_id" = "subject_id", "redcap_event_name" = "redcap_event_name"))
df_ug_cln_slc %>% 
  filter(!is.na(scan_01_3plane)) %>% 
  pull(subject_id) %>% sort() %>% unique() %>% length()

df_ug_cln_rsh9 %>% write_csv(paste0("df_to_import_", Sys.Date(), ".csv"), na = "")
