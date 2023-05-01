# Cleaning week 4 data for Bellwether 2023 class 
# 2023-05-01
# Last updated by Krista Kaput 

# load --------
library(tidyverse)
library(readxl)


# Using the Tennessee Data 

sch_adm_raw <-  read_excel("week_4_data/raw_data/LEAProjections_TISA_2.19.22.xlsx", 
                           sheet = "22-23SCHADMPrjt")


sch_el_raw <- read_excel("week_4_data/raw_data/LEAProjections_TISA_2.19.22.xlsx", 
                         sheet = "EL22-23", skip = 1, n_max = 1900)


sch_ed_raw <- read_excel("week_4_data/raw_data/LEAProjections_TISA_2.19.22.xlsx", 
                         sheet = "ED22-23", skip = 1, n_max = 1885)


sch_sped_raw <- read_excel("week_4_data/raw_data/LEAProjections_TISA_2.19.22.xlsx", 
                           sheet = "SPED22-23")

charter_status_raw <- read_excel("week_4_data/raw_data/Test1_1.9.22 - FY24_LEAProjections.xlsx", 
                                 sheet = "SCHADM22-23")


# create sch_id cleaning function --------

# must have columns named dist_no and sch_no
id_cleanr <- function(df){

  df %>%
    # create tidy sch_id var
    mutate(dist_no = str_pad(dist_no, width = 3, side = "left", pad = "0"),
           sch_no = str_pad(sch_no, width = 4, side = "left", pad = "0"),
           sch_id = paste0(dist_no, "-", sch_no)) %>%
    select(sch_id, everything())

}


# clean adm data ----------

sch_adm_clean <- sch_adm_raw |>
  # tidy column names
  rename_with("tolower") |>
  # rename_with( ~ str_replace_all(., "no", "id")) %>%
  rename_with( ~ str_replace_all(., "district", "dist")) |>
  rename_with( ~ str_replace_all(., "school", "sch"))  |> 
  rename_with( ~ str_replace_all(., " ", "_")) |>
  rename(voc_adm = "vocadm",
         gr_n_adm = "n",
         gr_k_adm = "k",
         gr_1_adm = "1",
         gr_2_adm = "2",
         gr_3_adm = "3",
         gr_4_adm = "4",
         gr_5_adm = "5",
         gr_6_adm = "6",
         gr_7_adm = "7",
         gr_8_adm = "8",
         gr_9_adm = "9",
         gr_10_adm = "10",
         gr_11_adm = "11",
         gr_12_adm = "12") |>
  # create tidy sch_id var
  mutate(dist_no = str_pad(dist_no, width = 3, side = "left", pad = "0"),
         sch_no = str_pad(sch_no, width = 4, side = "left", pad = "0"), 
         sch_id = paste0(dist_no, "-", sch_no)) |>
  rename(tutoring_adm = "tcap_2019_-gr_3") |>
  filter(total_adm > 0) |>
  select(sch_id, dist_no,
         dist_name, sch_name,charter, rural, 
         gr_n_adm, gr_k_adm, gr_1_adm, gr_2_adm, gr_3_adm, gr_4_adm,
         gr_5_adm, gr_6_adm, gr_7_adm, gr_8_adm, gr_9_adm, gr_10_adm,
         gr_11_adm, gr_12_adm, total_adm, voc_adm, dyslexia, early_literacy,
         tutoring_adm) %>%
  arrange(sch_id)
  
  


# clean adm data ------------
sch_adm <- sch_adm_raw %>%
  # tidy column names
  rename_with("tolower") %>%
  # rename_with( ~ str_replace_all(., "no", "id")) %>%
  rename_with( ~ str_replace_all(., "district", "dist")) %>%
  rename_with( ~ str_replace_all(., "school", "sch")) %>%
  rename_with( ~ str_replace_all(., " ", "_")) %>%
  rename(voc_adm = "vocadm") %>%
  # create tidy sch_id var
  mutate(dist_no = str_pad(dist_no, width = 3, side = "left", pad = "0"),
         sch_no = str_pad(sch_no, width = 4, side = "left", pad = "0"), 
         sch_id = paste0(dist_no, "-", sch_no)) %>%
  rename(tutoring_adm = "tcap_2019_-gr_3") %>%
  filter(total_adm > 0) %>%
  select(sch_id, dist_no,
         dist_name, sch_name,charter, rural, 
         gr_n_adm, gr_k_adm, gr_1_adm, gr_2_adm, gr_3_adm, gr_4_adm,
         gr_5_adm, gr_6_adm, gr_7_adm, gr_8_adm, gr_9_adm, gr_10_adm,
         gr_11_adm, gr_12_adm, total_adm, voc_adm, dyslexia, early_literacy,
         tutoring_adm) %>%
  arrange(sch_id)


# clean ed data -----------

sch_ed <- sch_ed_raw %>%
  rename(sch_id = leaid_schid,
         ed_enroll = ed,
         title_i = "Title I") %>%
  select(sch_id, ed_enroll, title_i, schoolname)

# clean el data ------

sch_el <- sch_el_raw %>%
  rename_with(tolower) %>%
  rename(dist_no = leaid,
         sch_no = schid,
         schoolname = school, 
         el_l_enroll = l,
         el_w_enroll = w,
         el_t1_enroll = `t1`,
         el_t2_enroll = `t2`,
         el_total_enroll = lw12) %>%
  id_cleanr() %>%
  select(sch_id, everything(), -dist_no, -lea, -sch_no)


# clean special ed data ---------

sch_sped <- sch_sped_raw %>%
  rename_with("tolower") %>%
  rename(dist_no = district_no,
         sch_no = school_no,
         schoolname = school_name,
         enroll = "2022-23 i&s") %>%
  id_cleanr() %>%
  mutate(option_number = paste0("option", option_number, "_enroll")) %>%
  select(-dist_no, -sch_no)  %>%
  pivot_wider(names_from = option_number,
              values_from = enroll) %>%
  select(sch_id, schoolname, 
         option1_enroll, option2_enroll, option3_enroll, option4_enroll, option5_enroll,
         option6_enroll, option7_enroll, option8_enroll, option9_enroll, option10_enroll) 

# clean charter data 
charter_status <- charter_status_raw %>%
  rename(sch_id = Code, 
         charter = Charter) %>%
  select(sch_id, charter)


# join data ---------

sch_clean <- sch_adm %>%
  left_join(sch_ed %>% select(-schoolname)) %>%
  left_join(sch_el %>% select(-schoolname)) %>%
  left_join(sch_sped %>% select(-schoolname)) %>%
  # checking the k3 literacy and dyslexia to see if it aligns with the SCHADM spreadsheet. 
  # It does!!! 
  mutate(dyslexia_adm = total_adm * 0.05,
         k3_literacy_adm = gr_k_adm + gr_1_adm + gr_2_adm + gr_3_adm,
         dyslexia_diff = dyslexia - dyslexia_adm, 
         k3_lit_diff = early_literacy - k3_literacy_adm) %>%
  # remove special state-run schools funded outside of formula
  filter(!dist_no %in% c("960", "961","963","964")) %>%
  select(-dyslexia, -early_literacy, -k3_lit_diff, -dyslexia_diff, -charter) %>%
  left_join(charter_status, by = "sch_id")

# create a district summary -----

dist_summary <- sch_clean %>% 
  group_by(dist_no) %>% 
  summarise(dist_name = first(dist_name),
            total_adm = sum(total_adm, na.rm = T))


# tidy work space ------

rm(sch_adm_raw, sch_el_raw, sch_sped_raw, sch_ed_raw, sch_adm, sch_sped,
   sch_el, sch_ed, charter_status, charter_status_raw)
