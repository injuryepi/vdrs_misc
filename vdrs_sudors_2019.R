
# R packages to load ------------------------------------------------------
# Store(objects())
# SOAR::Attach()
library(SOAR)
suppressMessages(library(tidyverse))
suppressMessages(library(Hmisc))
suppressMessages(library(lubridate))
library(injuryepi)
library(overdoser)
library(rvdrs)

# Reading the data --------------------------------------------------------

load(".R_Cache/prelim_dth@.RData")

fs::file_info(
	file.path(prelim_dth, "death2019.csv")) %>% 
	pull(modification_time)

d2019pre <- read_csv(file.path(prelim_dth, "death2019.csv"))

load(".R_Cache/clean_var_names@.RData")
load(".R_Cache/sams_vars_sel@.RData")
load(".R_Cache/import_width@.RData")

d2019pre <- d2019pre %>% 
	set_names(clean_var_names)

d2019pre <- d2019pre %>% 
	mutate(certno = state_file_number)


# Find VDRS cases -------------------------------------------------------

nvdrs_regex_ <- "X[6-9]|Y[012]|Y3[0-4]|Y35[0-467]|Y87[012]|Y89[09]|W3[2-4]|U0[123]"

u_col <- grep("underly", names(d2019pre), value = F)

d2019pre <- d2019pre %>% 
	mutate(nvdrs_case = create_diag(., expr = nvdrs_regex_, colvec = u_col)) 

# Find Y86 ----------------------------------------------------------------

d2019pre <- d2019pre %>% 
	mutate(y86_case = create_diag(., expr = "Y86", colvec = u_col))

# Add intent and mechanism

d2019pre <- d2019pre %>% 
	add_ice_intent_mech(uid = certno,  underlying_cause = underlying_cod_code)

# Add overdose ------------------------------------------------------------

## functions to capture unintentional
od_ui_fatal_opioid <- function(data, underly_col, mult_col) {
  data %>%
    mutate(ui_drug = od_create_diag(.,
      expr = "X4[0-4]",
      colvec = underly_col
    )) %>%
    mutate(ui_opioid = od_create_cond_diag(.,
      expr = "T40[0-46]",
      colvec = mult_col,
      cond.var = ui_drug
    )) %>%
    select(-ui_drug)
}

od_und_fatal_opioid <- function(data, underly_col, mult_col) {
  data %>%
    mutate(und_drug = od_create_diag(.,
      expr = "Y1[0-4]",
      colvec = underly_col
    )) %>%
    mutate(und_intent_opioid = od_create_cond_diag(.,
      expr = "T40[0-46]",
      colvec = mult_col,
      cond.var = und_drug
    )) %>%
    select(-und_drug)
}

####

col_mult <- grep("underlying_cod_code$|record_axis_code", names(d2019pre), value = F)

col_under <- grep("underlying_cod_code$", names(d2019pre), value = F)

d2019pre <- d2019pre %>% 
	od_fatal_drug_list(underly_col = col_under, mult_col = col_mult)

d2019pre <- d2019pre %>% 
	od_ui_fatal_opioid(underly_col = col_under, mult_col = col_mult)

# pregnancy "0" doesn't exist

d2019pre <- d2019pre %>% 
	mutate(pregnancy = ifelse(pregnancy == 0, 9, pregnancy))

# Cases to consider SUDORS or NVDRS -------------------------------------

# include all counties
d2019pre <- d2019pre %>% 
	mutate(esoos_case = ifelse(ui_opioid == 1, 1, 0))

# Cases to import

d2019pre_vdod <- d2019pre %>% 
	filter(nvdrs_case == 1 | esoos_case == 1)

# Adding new variables ----------------------------------------------------

d2019pre_vdod <- d2019pre_vdod %>% mutate(
  dob = lubridate::mdy(date_of_birth),
  ForceNewRecord = rep("Y", nrow(.)),
  OverwriteConflicts = rep(" ", nrow(.)),
  IncidentYear = rep(" ", nrow(.)),
  IncidentNumber = rep(" ", nrow(.)),
  VictimNumber = rep(" ", nrow(.)),
  DCNumberLastFour = stringr::str_sub(string = certno, -4),
  CMENumberLastFour = stringr::str_sub(
    gsub("\\W|_", "", me_coroner_case_num, perl = T),
    -4
  ),
  LastNameFirstInitial = stringr::str_sub(decedent_last_name, , 1),
  BirthDayofMonth = lubridate::day(dob),
  RACE_MVR = rep(" ", nrow(.)),
  OCCUPC = vector(mode = "numeric", length = nrow(.)),
  INDUSTC = vector(mode = "numeric", length = nrow(.)),
  TOI_HR = str_pad(
    gsub("\\W", "", time_of_injury), # to remove ":" between hr and min
    width = 6, side = "left", pad = 0
  ),
  INACT = substr(underlying_cod_code, 5, 5),
  INJPL = rep(" ", nrow(.)),
  INJPL2 = rep(" ", nrow(.)),
  OLDEDUC = rep(" ", nrow(.)),
  PRNCDDT_MO = rep(99, nrow(.)),
  PRNCDDT_DY = rep(99, nrow(.)),
  PRNCDDT_YR = rep(9999, nrow(.)),
  DC_CnsBlk = rep(" ", nrow(.)),
  DC_CnsTrt = rep(" ", nrow(.)),
  Dc_surviv = rep(999, nrow(.)),
  dc_Sunit = rep(9, nrow(.)),
  Empty_Indust = rep(" ", nrow(.)),
  DeathMannerAbstractor = rep(" ", nrow(.))
)

d2019pre_vdod <- d2019pre_vdod %>% set_names(tolower)

d2019pre_vdod <- d2019pre_vdod %>% mutate( 
	import_id = paste0(dcnumberlastfour, 
										 lastnamefirstinitial, stringr::str_pad(birthdayofmonth, w = 2, side = "left", pad = "0")))



d2019pre_vdod <- d2019pre_vdod %>%
  mutate(
    death_state_code = death_state_nchs_code,
    birthplace_country_code = birthplace_country_fips_code,
    residence_state_code = residence_state_nchs_code,
    residence_country_code = residence_country_fips_code,
    marital = marital_status,
    injury_date_month = date_of_injury_month,
    injury_date_day = date_of_injury_day,
    injury_date_year = date_of_injury_year,
    residence_zip = residence_zip_code
  )



# To review for the nvdrs cases

d2019pre_vdod <- d2019pre_vdod %>% 
	mutate(birthplace_state_nchs_code = 
				 	str_pad(birthplace_state_nchs_code,
				 					width = 2, side = "left", pad = 0) )

d2019pre_vdod <- d2019pre_vdod %>%
	left_join(select(state_codes, wa_codes, target) %>%
							mutate(wa_codes = as.character(wa_codes)) %>% 
							rename(birthplace_state_nchs_code = wa_codes)) %>%
	mutate(birthplace_state_nchs_code = target) %>%
	select(-target) 

# residence_county_wa_code	ResidenceCounty	COUNTYC


d2019pre_vdod <- d2019pre_vdod %>%
	left_join(wa_counties_fips %>% 
							rename(target = county_fips) %>% 
							select(county, target) %>%
							mutate(county = toupper(county)) %>% 
							rename(residence_county = county)) %>%
	mutate(residence_county = target) %>%
	select(-target) 

d2019pre_vdod <- d2019pre_vdod %>% 
	mutate(residence_county = 
				 	str_pad(residence_county,
				 					width = 3, side = "left", pad = 0) )

# injury_county_wa_code	County	INJCOUNTY

d2019pre_vdod <- d2019pre_vdod %>%
	left_join(wa_counties_fips %>% 
							rename(target = county_fips) %>% 
							mutate(county = toupper(county)) %>% 
							rename(injury_county = county)) %>%
	mutate(injury_county = target) %>%
	select(-target) 

d2019pre_vdod <- d2019pre_vdod %>% 
	mutate(injury_county = 
				 	str_pad(injury_county,
				 					width = 3, side = "left", pad = 0) )

# describe(d2019pre_vdod$injury_county)


# residence_state_code	ResidenceState	STATEC  

# describe(d2019pre_vdod$residence_state_code)

d2019pre_vdod <- d2019pre_vdod %>% 
	mutate(residence_state_code = 
				 	str_pad(residence_state_code,
				 					width = 2, side = "left", pad = 0) )

d2019pre_vdod <- d2019pre_vdod %>%
  left_join(select(state_codes, wa_codes, target) %>%
    mutate(wa_codes = as.character(wa_codes)) %>%
    rename(residence_state_code = wa_codes)) %>%
  mutate(residence_state_code = target) %>%
  select(-target)

table(d2019pre_vdod$residence_state_code)

# residence_city_fips_code	ResidenceCity	CITYC	City of residence

d2019pre_vdod <- d2019pre_vdod %>% 
	mutate(residence_city_fips_code = 
				 	str_pad(residence_city_fips_code,
				 					width = 5, side = "left", pad = 0) )


#death_state_nchs_code	DeathState	DSTATE

d2019pre_vdod <- d2019pre_vdod %>% 
	mutate(death_state_code = 
				 	str_pad(death_state_code,
				 					width = 2, side = "left", pad = 0) )


d2019pre_vdod <- d2019pre_vdod %>%
	left_join(select(state_codes, wa_codes, target) %>%
							mutate(wa_codes = as.character(wa_codes)) %>% 
							rename(death_state_code = wa_codes)) %>%
	mutate(death_state_code = target) %>%
	select(-target) 

# marital	MaritalStatus	MARITAL	Marital status

d2019pre_vdod <- d2019pre_vdod %>% 
	mutate(marital = ifelse(marital == "P", "M", marital))

# place_of_death_type	DeathPlace	DPLACE	Place of Death

d2019pre_vdod <- d2019pre_vdod %>% 
	mutate(place_of_death_type = as.character(place_of_death_type)) %>% 
	left_join(dplace_conv %>% 
							rename(place_of_death_type = dplace)) %>% 
	mutate(place_of_death_type = tgt) %>% 
	select(-tgt)

#residence_zip	ResidenceZip	RESZIP	Zip code of residence

d2019pre_vdod <- d2019pre_vdod %>% 
	mutate(residence_zip = substr(residence_zip, 1, 5))

#injury_zip_code	InjuryZip	NA	ZIP Code where injury occurred

d2019pre_vdod <- d2019pre_vdod %>% 
	mutate(injury_zip_code = substr(injury_zip_code, 1, 5))

# injury_stateInjuryState	INJSTATE	State or territory where injury occurred

d2019pre_vdod <- d2019pre_vdod %>% 
	mutate(injury_state = 
				 	str_pad(injury_state,
				 					width = 2, side = "left", pad = 0) )

d2019pre_vdod <- d2019pre_vdod %>%
	left_join(select(state_codes, state, target) %>%
							rename(injury_state = state)) %>%
	mutate(injury_state = target) %>%
	select(-target) 

# armed_forces	Military	DC_Vetran	Current or former military personnel


d2019pre_vdod <- d2019pre_vdod %>% 
	left_join(vet_conv) %>% 
	mutate(armed_forces = tgt) %>% 
	select(-tgt)

#injury_city	InjuryCity	DC_InjPlace	City where injury occurred

injcity <- wa_cities_codes %>% select(city, placefp) %>% 
	mutate(city = str_trim(toupper(city))) %>% 
	rename(injury_city = city,
				 tgt = placefp)

d2019pre_vdod <- d2019pre_vdod %>% 
	mutate(injury_city = str_trim(toupper(injury_city))) %>%
	left_join(injcity) 

d2019pre_vdod <- d2019pre_vdod %>% 
	mutate(injury_city = tgt) %>% 
	select(-tgt)


#

d2019pre_vdod <- d2019pre_vdod %>% 
	select(-county_wa_code)

d2019pre_vdod <- d2019pre_vdod[vars_to_import_x]

# identical(names(d2019pre_vdod), vars_to_import_x)

vars_to_import_x2 <- c("certno", "import_id", sams_vars_sel$nchs_vars)

names(d2019pre_vdod) <- vars_to_import_x2




# Formatting ICD-10 codes -------------------------------------------------

format_icd10 <- compose(
  function(x) paste0("  ", stringi::stri_pad_right(x, w = 5), " "),
  function(x) gsub("\\.$", "", x),
  function(x) gsub("(?<=^(.{3}))", "\\.", x, perl = TRUE),
  function(x) ifelse(is.na(x), " ", x)
)

var_causes <- grep("^ACME|^EAC", 
									 names(d2019pre_vdod), value = T, ignore.case = T)

d2019pre_vdod[var_causes] <- sapply(d2019pre_vdod[var_causes], format_icd10) 


# Race and Ethnicity ------------------------------------------------------


# race
var_race <- grep("RACE", 
								 names(d2019pre_vdod), value = T, ignore.case = T)

d2019pre_vdod[var_race] <- sapply(d2019pre_vdod[var_race], function(x) gsub("U", "", x ))

# ethnicity 
var_eth <- grep("^dethnic", names(d2019pre_vdod), value = T, ignore.case = T)

f_eth <- function(x) gsub("U", "N", x, perl = T)

d2019pre_vdod[var_eth] <- d2019pre_vdod[var_eth] %>% 
	map_df(f_eth) 


# Other Formatting --------------------------------------------------------

d2019pre_vdod <- d2019pre_vdod %>% 
	mutate(toi_hr = gsub("\\D+|NA", "", toi_hr),
				 toi_hr = str_pad(toi_hr,width = 6, side = "left",  pad = 0),
				 toi_hr = if_else(nchar(toi_hr) < 6, "      ", toi_hr),
				 toi_hr = str_sub(toi_hr, ,4),
				 dc_censst = str_trim(dc_censst),
				 dc_censst = stringr::str_sub(dc_censst, 1, 7),
				 dc_censst = as.character(ifelse(is.na(dc_censst), "9999.99", dc_censst)),
				 dc_censst = str_pad(dc_censst,width = 7, side = "left",  pad = 0),
				 dc_cnstrt = rep("9999.99", nrow(.)),
				 dc_cnsblk = rep(" ", nrow(.)),
				 dc_pdthtx = substr(dc_pdthtx , 1, 30),
				 dc_pdthtx = ifelse(dplace != 7, " ", dc_pdthtx),
				 # country of birth
				 dc_bthtxt = ifelse(bplace_cnt != "88", " ", dc_bthtxt),
				 # country of residence
				 dc_countr = ifelse(countryc != "88", " ", dc_countr))


# replace missing with space

d2019pre_vdod[] <- sapply(d2019pre_vdod[], function(x) ifelse(is.na(x), " ", x)) 

# replace country ZZ with "  "

d2019pre_vdod[] <- sapply(d2019pre_vdod[], 
													function(x) gsub("^ZZ$", "  ", x, perl = T)) 




# Selected Corrections to minimize import errors --------------------------

ctc <- c("88","99","US", "RQ","VQ","GQ","CA","CU","MX","AQ","CQ")

d2019pre_vdod <- d2019pre_vdod %>% 
	mutate(bplace_cnt = ifelse(bplace_cnt %in% ctc, bplace_cnt, ""))

# replace "U" in toi_u
d2019pre_vdod <- d2019pre_vdod %>% 
	mutate(toi_u = ifelse(toi_u == "U", "", toi_u))

# not city fips 

notc <- c('00000', '09425', '58845', '09425', '99033', '99041', '17250') 

d2019pre_vdod <- d2019pre_vdod %>% 
	mutate(cityc = ifelse(cityc %in% notc, "99999", cityc),
				 cityc = ifelse(statec == " ", "     ", cityc),
				 countyc = ifelse(statec == "99", "   ", countyc),
				 dc_censst = rep("9999.99", nrow(.)),
				 dc_censbl = rep(" ", nrow(.)),
				 reszip = gsub("\\D+", "", reszip),
				 reszip = ifelse(nchar(reszip) < 5, "     ", reszip),
				 injuryzip = ifelse(nchar(injuryzip) < 5, "     ", injuryzip),
				 doi_yr = ifelse(doi_yr == " ", "9999", doi_yr)
	)



# Check victims already in SAMS to avoid duplications ---------------------

victim <- readr::read_csv("from_sams/temp/Victim.zip")

victim <- victim %>% 
	filter(IncidentYear == 2019)

victim <- victim %>% mutate( 
	import_id = paste0(DCNumberLastFour, LastNameFirstInitial, stringr::str_pad(BirthDayOfMonth, w = 2, side = "left", pad = "0")))


# NVDRS Cases -------------------------------------------------------------

d2019pre_nvdrs <- d2019pre %>% 
	filter(nvdrs_case == 1)

d2019pre_nvdrs_sams <- d2019pre_vdod %>% 
	filter(injstate == "53",
				 certno %in% d2019pre_nvdrs$certno, 
				 !(import_id %in% victim$import_id ))
	


# SUDORS cases ----------------------------------------------------------

d2019pre_sudors <- d2019pre %>% 
	filter(esoos_case == 1)

sudors_13counties_ <- "Kitsap|Thurston|Spokane|Skagit|Grays Harbor|King|Snohomish|Yakima|Pierce|Island|Clark|Clallam|Whatcom"

counties13 <- wa_counties_fips %>% 
	filter(grepl(sudors_13counties_, county)) %>% 
															pull(county_fips) %>% unlist

d2019pre_sudors_sams <- d2019pre_vdod %>%
  filter(
    injstate == "53",
    certno %in% d2019pre_sudors$certno,
    !(import_id %in% victim$import_id),
    str_pad(injcounty, 3, "left", 0) %in% counties13
  )


# Save NVDRS file to load into SAMS ---------------------------------------

gdata::write.fwf(d2019pre_nvdrs_sams %>%
								 	select(-certno, -import_id) %>%
								 	as.data.frame(),
								 glue::glue("to_sams/d2019pre_nvdrs_{today()}.txt"), width = import_width, colnames = F, sep="", na = " ")


# Save SUDORS file to load into SAMS --------------------------------------

gdata::write.fwf(d2019pre_sudors_sams %>%
								 	select(-certno, -import_id) %>%
								 	as.data.frame(),
								 glue::glue("to_sams/d2019pre_sudors_{today()}.txt"), width = import_width, colnames = F, sep="", na = " ")


# Copies to Data folder ---------------------------------------------------

write_rds(d2019pre_nvdrs_sams, glue::glue("data/pre_sams/d2019pre_nvdrs_{today()}.rds"))

write_rds(d2019pre_nvdrs, "Data/pre_sams/d2019pre_nvdrs.rds", compress = "xz")

write_rds(d2019pre_sudors_sams, glue::glue("data/pre_sams/d2019pre_sudors_{today()}.rds"))

write_rds(d2019pre_sudors, "Data/pre_sams/d2019pre_sudors.rds", compress = "xz")
