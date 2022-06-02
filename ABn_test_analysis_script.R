# IMPORTANT NOTE: Search for ***FUNCTION CALLING*** to see the lines where you call functions
# Notes:
# deparse(substitute(x)) when you want to change a variable to a character
# assign("x", y) is equivalent to x <- y
# eval(parse(text = "x")) when you want to convert a character to an object/variable

# Step 1: Load libraries
pacman::p_load(dplyr, ggplot2, bigrquery, reshape2, formattable, stringr, FedData, tibble, ggpubr)

# Step 1.2: Define the input variables that are going to be used throughout the code
target_group_names_tg1_non_tg <- c("TG1", "Non_TG") # Names of the target groups that should be filtered for in the SQL query. Always follow a chronological order
target_group_names_tg2_non_tg <- c("TG2", "Non_TG") # Names of the target groups that should be filtered for in the SQL query. Always follow a chronological order

# Define the column order for the business KPIs --> Structure (Grp_Control, Grp_Var1, Grp_Delta_V1_C, Grp_Var2, Grp_Delta_V2_C, Grp_Var3, Grp_Delta_V3_C, etc.). USED IN df_overall_reshaped
col_order <- c("entity_id", "experiment_id", "variable", # Grouping variables
               # Group = Non-TG
               "Non_TG_Control", "Non_TG_Var1", "Delta_Pct_V1_C_Non_TG", "Delta_Abs_V1_C_Non_TG", "P_Val_V1_C_Non_TG", 
               "Non_TG_Var2", "Delta_Pct_V2_C_Non_TG", "Delta_Abs_V2_C_Non_TG", "P_Val_V2_C_Non_TG",
               "Non_TG_Var3", "Delta_Pct_V3_C_Non_TG", "Delta_Abs_V3_C_Non_TG", "P_Val_V3_C_Non_TG",
               "Non_TG_Var4", "Delta_Pct_V4_C_Non_TG", "Delta_Abs_V4_C_Non_TG", "P_Val_V4_C_Non_TG",
               
               # Group = TG1
               "TG1_Control", "TG1_Var1", "Delta_Pct_V1_C_TG1", "Delta_Abs_V1_C_TG1", "P_Val_V1_C_TG1",
               "TG1_Var2", "Delta_Pct_V2_C_TG1", "Delta_Abs_V2_C_TG1", "P_Val_V2_C_TG1",
               "TG1_Var3", "Delta_Pct_V3_C_TG1", "Delta_Abs_V3_C_TG1", "P_Val_V3_C_TG1",
               "TG1_Var4", "Delta_Pct_V4_C_TG1", "Delta_Abs_V4_C_TG1", "P_Val_V4_C_TG1",
               
               # Group = TG2
               "TG2_Control", "TG2_Var1", "Delta_Pct_V1_C_TG2", "Delta_Abs_V1_C_TG2", "P_Val_V1_C_TG2", 
               "TG2_Var2", "Delta_Pct_V2_C_TG2", "Delta_Abs_V2_C_TG2", "P_Val_V2_C_TG2",
               "TG2_Var3", "Delta_Pct_V3_C_TG2", "Delta_Abs_V3_C_TG2", "P_Val_V3_C_TG2",
               "TG2_Var4", "Delta_Pct_V4_C_TG2", "Delta_Abs_V4_C_TG2", "P_Val_V4_C_TG2",
               
               # Group = TG1 + Non-TG
               "TG1_Non_TG_Control", "TG1_Non_TG_Var1", "Delta_Pct_V1_C_TG1_Non_TG", "Delta_Abs_V1_C_TG1_Non_TG", "P_Val_V1_C_TG1_Non_TG",
               "TG1_Non_TG_Var2", "Delta_Pct_V2_C_TG1_Non_TG", "Delta_Abs_V2_C_TG1_Non_TG", "P_Val_V2_C_TG1_Non_TG",
               "TG1_Non_TG_Var3", "Delta_Pct_V3_C_TG1_Non_TG", "Delta_Abs_V3_C_TG1_Non_TG", "P_Val_V3_C_TG1_Non_TG",
               "TG1_Non_TG_Var4", "Delta_Pct_V4_C_TG1_Non_TG", "Delta_Abs_V4_C_TG1_Non_TG", "P_Val_V4_C_TG1_Non_TG",
               
               # Group = TG2 + Non-TG
               "TG2_Non_TG_Control", "TG2_Non_TG_Var1", "Delta_Pct_V1_C_TG2_Non_TG", "Delta_Abs_V1_C_TG2_Non_TG", "P_Val_V1_C_TG2_Non_TG", 
               "TG2_Non_TG_Var2", "Delta_Pct_V2_C_TG2_Non_TG", "Delta_Abs_V2_C_TG2_Non_TG", "P_Val_V2_C_TG2_Non_TG",
               "TG2_Non_TG_Var3", "Delta_Pct_V3_C_TG2_Non_TG", "Delta_Abs_V3_C_TG2_Non_TG", "P_Val_V3_C_TG2_Non_TG",
               "TG2_Non_TG_Var4", "Delta_Pct_V4_C_TG2_Non_TG", "Delta_Abs_V4_C_TG2_Non_TG", "P_Val_V4_C_TG2_Non_TG")

# Combined target groups
combined_tg1_non_tg <- "TG1_Non_TG" # If target groups need to be combined, specify the new target group designation here
combined_tg2_non_tg <- "TG2_Non_TG" # If target groups need to be combined, specify the new target group designation here

# Target group order
tg_order <- c("Non_TG", "TG1", "TG2", "TG1_Non_TG", "TG2_Non_TG")

# Number of cvr_metrics calculated in df_cvr_dwnld
num_cvr_metric = 3 # 3 for CVR2, CVR3, and mCVR4

# BQ table name
bq_tbl_name <- "loved_brands_final_results_business_kpis_th_pkk"

# Experiment IDs
exp_id_var = 57 # 34: Misamis Oriental, 57: PKK

# Entity ID (Because experiments in different entities can have the same ID)
ent_id <- "FP_TH"

# Irrelevant variants in the non-LB target group
non_lb_tg <- "TG2"
irrelevants_vars <- c("Variation1", "Variation2", "Variation4")

##-----------------------------------------------------END OF STEP 1-----------------------------------------------------##

# Step 2: Downloading the test's datasets
options(gargle_oauth_email = "omar.elmaria@deliveryhero.com")

# Define the project ID and dataset's name
project_id_data <- "dh-logistics-product-ops"
project_id_billing <- "logistics-data-staging-flat"
data_set <- "pricing"

bq_conn <- dbConnect(bigquery(),
                     project = project_id_data,
                     dataset = data_set,
                     billing = project_id_billing,
                     use_legacy_sql = FALSE)

# Orders data
df_ord <- tbl(bq_conn, "ab_test_individual_orders_cleaned_loved_brands_th_ph") # Contains business and logistical KPIs of the LB experiment(s)

# CVR data (NEEDS MODIFICATION)
df_cvr_overall <- tbl(bq_conn, "ab_test_cvr_data_cleaned_loved_brands_th_ph_overall") # Contains conversion and user count data (Overall)
df_cvr_per_day <- tbl(bq_conn, "ab_test_cvr_data_cleaned_loved_brands_th_ph_per_day") # Contains conversion and user count data (Per Day)

##-----------------------------------------------------END OF STEP 2 AND INPUT SECTION-----------------------------------------------------##

# Step 3: Calculation of the correction factors based on the user counts

# Create a function to extract the user count data from df_cvr_overall_central_pre_post
corr_factor_func <- function(per_day) {
  # Select the right CVR table for the correction factor
  if (per_day == "No") {
    cvr_df <<- df_cvr_overall # Global assignment
  } else if (per_day == "Yes") {
    cvr_df <<- df_cvr_per_day # Global assignment
  }
  
  df_corr_factor <- cvr_df %>%
    group_by(experiment_id) %>% 
    {if (per_day == "Yes") select(., entity_id, experiment_id, created_date, target_group, variant, users) else select(., entity_id, experiment_id, target_group, variant, users)} %>% 
    {if (per_day == "Yes") group_by(., entity_id, experiment_id, created_date, target_group, variant) else group_by(., entity_id, experiment_id, target_group, variant)} %>% 
    collect() %>% 
    as.data.frame()
  
  # Isolate the user count of the control group
  users_control_grp <- df_corr_factor %>%
    filter(variant == "Control")
  
  # Join the user count of the control group to df_corr_factor as a new column and calculate the correction factor
  df_corr_factor <- df_corr_factor %>% 
    {if (per_day == "Yes") 
      left_join(., users_control_grp %>% select(-variant), by = c("entity_id", "experiment_id", "target_group", "created_date"), suffix = c("_variant", "_control")) else 
      left_join(., users_control_grp %>% select(-variant), by = c("entity_id", "experiment_id", "target_group"), suffix = c("_variant", "_control"))
    } %>% 
    mutate(corr_factor = users_control / users_variant - 1) %>% 
    {if (per_day == "Yes") arrange(., entity_id, experiment_id, created_date, target_group, variant) else arrange(., entity_id, experiment_id, target_group, variant)}
  
  return(df_corr_factor)
}

# ***FUNCTION CALLING***
df_corr_factor_overall <- corr_factor_func("No")
df_corr_factor_per_day <- corr_factor_func("Yes")

##-----------------------------------------------------END OF STEP 3-----------------------------------------------------##

# Step 4: Create a function that does the data aggregation for the different target groups
results_func <- function(raw_ord_df, tg_incl_filter_var, tg, ts_or_no_ts, per_day, corr_factor_overall, corr_factor_per_day, exp_id, entity) {
  if (ts_or_no_ts == 'no_ts') {
    df_temp <- raw_ord_df %>%
      filter(experiment_id == exp_id & entity_id == entity & target_group %in% tg_incl_filter_var) %>%
      mutate(target_group = tg)
  } else {
    df_temp <- raw_ord_df %>%
      filter(experiment_id == exp_id & entity_id == entity)
  }

  df_temp <- df_temp %>% 
    {if (per_day == "Yes") group_by(., entity_id, experiment_id, created_date, target_group, variant) else group_by(., entity_id, experiment_id, target_group, variant)} %>% 
    summarise(tot_orders = n_distinct(order_id),
              
              tot_df_local = sum(actual_df_paid_by_customer, na.rm = TRUE),
              avg_df_local = sum(actual_df_paid_by_customer, na.rm = TRUE) / n_distinct(order_id),
              
              tot_commission_local = sum(commission_local, na.rm = TRUE),
              avg_commission_local = sum(commission_local, na.rm = TRUE) / n_distinct(order_id),
              
              tot_joker_vendor_fee_local = sum(joker_vendor_fee_local, na.rm = TRUE),
              avg_joker_vendor_fee_local = sum(joker_vendor_fee_local, na.rm = TRUE) / n_distinct(order_id),
              
              tot_gmv_local = sum(gmv_local, na.rm = TRUE),
              avg_gmv_local = sum(gmv_local, na.rm = TRUE) / n_distinct(order_id),
              
              tot_gfv_local = sum(gfv_local, na.rm = TRUE),
              avg_gfv_local = sum(gfv_local, na.rm = TRUE) / n_distinct(order_id),
              
              avg_mov_local = mean(mov_local, na.rm = TRUE),
              
              tot_sof_local = sum(sof_local, na.rm = TRUE),
              avg_sof_local = sum(sof_local, na.rm = TRUE) / n_distinct(order_id),
              
              tot_surge_fee_local = sum(dps_surge_fee_local, na.rm = TRUE),
              avg_surge_fee_local = sum(dps_surge_fee_local, na.rm = TRUE) / n_distinct(order_id),
              
              tot_service_fee_local = sum(service_fee_local, na.rm = TRUE),
              avg_service_fee_local = sum(service_fee_local, na.rm = TRUE) / n_distinct(order_id),
              
              tot_rev_local = sum(revenue_local, na.rm = TRUE),
              avg_rev_local = sum(revenue_local, na.rm = TRUE) / n_distinct(order_id),
              
              tot_del_costs_local = sum(delivery_costs_local, na.rm = TRUE),
              avg_del_costs_local = sum(delivery_costs_local, na.rm = TRUE) / n_distinct(order_id),
              
              tot_profit_local = sum(gross_profit_local, na.rm = TRUE),
              avg_profit_local = sum(gross_profit_local, na.rm = TRUE) / n_distinct(order_id),
              
              avg_travel_time = mean(dps_travel_time, na.rm = TRUE),
              avg_delivery_dist = mean(delivery_distance_m / 1000, na.rm = TRUE),
              avg_fleet_delay = mean(dps_mean_delay, na.rm = TRUE)) %>%
    {if (per_day == "Yes") arrange(., entity_id, experiment_id, created_date, target_group, variant) else arrange(., entity_id, experiment_id, target_group, variant)} %>% 
    collect()
  
  # Incorporate the correction factor
  df_temp <- df_temp %>%
    {if (per_day == "Yes") 
      left_join(., corr_factor_per_day %>% select(entity_id, experiment_id, created_date, target_group, variant, corr_factor), by = c("entity_id", "experiment_id", "created_date", "target_group", "variant")) else
      left_join(., corr_factor_overall %>% select(entity_id, experiment_id, target_group, variant, corr_factor), by = c("entity_id", "experiment_id", "target_group", "variant"))
    } %>% 
    {if (per_day == "Yes") group_by(., entity_id, experiment_id, created_date, target_group, variant) else group_by(., entity_id, experiment_id, target_group, variant)} %>%
    mutate(tot_orders_adj = tot_orders * (1 + corr_factor),
           tot_df_local_adj = avg_df_local * tot_orders_adj,
           tot_commission_local_adj = avg_commission_local * tot_orders_adj,
           tot_joker_vendor_fee_local_adj = avg_joker_vendor_fee_local * tot_orders_adj,
           tot_gmv_local_adj = avg_gmv_local * tot_orders_adj,
           tot_gfv_local_adj = avg_gfv_local * tot_orders_adj,
           tot_sof_local_adj = avg_sof_local * tot_orders_adj,
           tot_surge_fee_local_adj = avg_surge_fee_local * tot_orders_adj ,
           tot_service_fee_local_adj = avg_service_fee_local * tot_orders_adj,
           tot_rev_local_adj = avg_rev_local * tot_orders_adj,
           tot_del_costs_local_adj = avg_del_costs_local * tot_orders_adj,
           tot_profit_local_adj = avg_profit_local * tot_orders_adj)
  
  # Re-organize the columns to be in the correct order
  kpis <- c(# Total KPIs
            "tot_orders", "tot_orders_adj", "corr_factor",
            "tot_df_local", "tot_df_local_adj",
            "tot_commission_local", "tot_commission_local_adj",
            "tot_joker_vendor_fee_local", "tot_joker_vendor_fee_local_adj",
            "tot_gmv_local", "tot_gmv_local_adj",
            "tot_gfv_local", "tot_gfv_local_adj",
            "tot_sof_local", "tot_sof_local_adj",
            "tot_surge_fee_local", "tot_surge_fee_local_adj",
            "tot_service_fee_local", "tot_service_fee_local_adj",
            "tot_rev_local", "tot_rev_local_adj",
            "tot_del_costs_local", "tot_del_costs_local_adj",
            "tot_profit_local", "tot_profit_local_adj",
            
            # Per-order KPIs
            "avg_df_local", "avg_commission_local", "avg_joker_vendor_fee_local", "avg_gmv_local", "avg_gfv_local",
            "avg_mov_local", "avg_sof_local", "avg_surge_fee_local", "avg_service_fee_local",
            "avg_rev_local", "avg_del_costs_local", "avg_profit_local",
            
            # Logistical KPIs
            "avg_travel_time", "avg_delivery_dist", "avg_fleet_delay")
  
  df_temp <- df_temp %>% 
    {if (per_day == "Yes") select(., entity_id, experiment_id, created_date, target_group, variant, all_of(kpis)) else select(., entity_id, experiment_id, target_group, variant, all_of(kpis))} 
  
  # Adjust the formatting of the columns
  df_temp <- df_temp %>%
    mutate_at(c("tot_orders", "tot_orders_adj"), as.integer) %>% 
    mutate_if(is.numeric, round, 4)
}

##-----------------------------------------------------END OF STEP 4-----------------------------------------------------##

# Step 5: Create a dataset of aggregated KPIs for the different target groups

# Overall (i.e. no grouping by created_date) with target groups
# ***FUNCTION CALLING***
df_overall_with_tg <- results_func(df_ord, NULL, NULL, "ts", "No", df_corr_factor_overall, df_corr_factor_per_day, exp_id_var, ent_id)

# df_overall without target groups
# First: TG1 + Non-TG
df_overall_tg1_non_tg <- results_func(df_ord, target_group_names_tg1_non_tg, combined_tg1_non_tg, "no_ts", "No", df_corr_factor_overall, df_corr_factor_per_day, exp_id_var, ent_id)

# Second: TG2 + Non-TG
df_overall_tg2_non_tg <- results_func(df_ord, target_group_names_tg2_non_tg, combined_tg2_non_tg, "no_ts", "No", df_corr_factor_overall, df_corr_factor_per_day, exp_id_var, ent_id)

# Combine df_overall_with_tg, df_overall_tg1_non_tg, and df_overall_tg2_non_tg
overall_env_vars <- sort(ls(.GlobalEnv)[grep("df_overall_", ls(.GlobalEnv))], decreasing = TRUE)
df_overall <- do.call(rbind, lapply(list(overall_env_vars)[[1]], function(x) {eval(parse(text = x))}))

# Change the structure of the df_overall table such that it shows the KPIs as rows and the variant + target groups as columns
df_overall_reshaped <-  recast(df_overall, entity_id + experiment_id + variable ~ target_group + variant, id.var = c("entity_id", "experiment_id", "target_group", "variant"))

# Change the formatting of the cells so that the numbers have only four decimal places
df_overall_reshaped <- df_overall_reshaped %>%
  mutate_if(is.numeric, round, 4)

##-----------------------------------------------------END OF STEP 5-----------------------------------------------------##

# Step 6: Calculate the deltas for df_overall_reshaped and shortening "Variation" to "Var"

# Get all columns that have the word "Variation" in their names
delta_calc_func <- function(df) {
  variation_col_names <- colnames(df)[grep("Variation|Var", colnames(df))]

  for (i in variation_col_names) {
    # Get the target group associated with the column
    # Breakdown of the regex below --> # \w matches any word character (equivalent to [a-zA-Z0-9_]). + matches the previous token between one and unlimited times, as many times as possible, giving back as needed (greedy). (?=_Variation[0-9]) --> Positive Look-ahead
    tg_associated_with_col <- str_extract_all(i, "\\w+(?=_Variation[0-9])")[[1]]
    
    # Create new columns for the delta between variation (x) and control
    df <- df %>%
      dplyr::mutate(Delta_Pct = round(!!as.name(i) / !!as.name(paste0(tg_associated_with_col, "_Control")) - 1, 4),
                    Delta_Abs = round(!!as.name(i) - !!as.name(paste0(tg_associated_with_col, "_Control")), 4)) # Matches the "TG" + "_Control" (e.g., Non_TG_Control, TG1_Control, etc.)
    
    # Get the variation number
    last_string_character <- substr_right(tail(strsplit(i, "_")[[1]], 1), 1) # If the string = "TG1_TG2_Variation1", this will return "1"
    
    # Add the correct suffixes to the columns
    colnames(df)[length(colnames(df))] <- paste0("Delta_Abs", "_V", last_string_character, "_C_", tg_associated_with_col)
    colnames(df)[length(colnames(df)) - 1] <- paste0("Delta_Pct", "_V", last_string_character, "_C_", tg_associated_with_col)
  }
  
  return(df)
}

df_overall_reshaped <- delta_calc_func(df_overall_reshaped)

# Shortening "Variation" to "Var"
colnames(df_overall_reshaped) <- str_replace_all(colnames(df_overall_reshaped), "Variation", "Var")

##-----------------------------------------------------END OF STEP 6-----------------------------------------------------##

# Step 7: Calculate significance
# Step 7.1: Create a data frame with the data grouped by (entity_id, experiment_id, created_date, target_group, variant)

# Per Day with target groups
# ***FUNCTION CALLING***
df_per_day_with_tg <- results_func(df_ord, NULL, NULL, "ts", "Yes", df_corr_factor_overall, df_corr_factor_per_day, exp_id_var, ent_id)

# df_per_day without target groups
# First: TG1 + Non-TG
df_per_day_tg1_non_tg <- results_func(df_ord, target_group_names_tg1_non_tg, combined_tg1_non_tg, "no_ts", "Yes", df_corr_factor_overall, df_corr_factor_per_day, exp_id_var, ent_id)

# Second: TG2 + Non-TG
df_per_day_tg2_non_tg <- results_func(df_ord, target_group_names_tg2_non_tg, combined_tg2_non_tg, "no_ts", "Yes", df_corr_factor_overall, df_corr_factor_per_day, exp_id_var, ent_id)

# Combine df_per_day_with_tg, df_per_day_tg1_non_tg, and df_per_day_tg2_non_tg
per_day_env_vars <- sort(ls(.GlobalEnv)[grep("df_per_day_", ls(.GlobalEnv))], decreasing = TRUE)
df_per_day <- do.call(rbind, lapply(list(per_day_env_vars)[[1]], function(x) {eval(parse(text = x))}))

# Replace "Variation" with "Var" to be consistent with the nomenclature above
df_per_day$variant <- gsub("Variation", "Var", df_per_day$variant) # Must access the column using the "$" operator

# Arrange the newly formed data frame and change the target_group and variant columns into ordered factors
df_per_day <- df_per_day %>% 
  mutate_at(.vars = vars(target_group), ~ factor(., levels = tg_order, ordered = TRUE)) %>% 
  mutate_at(.vars = vars(variant), ~ factor(., levels = sort(unique(df_per_day$variant)), ordered = TRUE))

df_per_day <- df_per_day %>% 
  arrange(entity_id, experiment_id, created_date, target_group, variant)

##-----------------------------------------------------END OF STEP 7.1-----------------------------------------------------##

# Step 7.2: Create intermediary data frames to calculate significance

colnames_tot <- colnames(df_per_day)[grep("tot", colnames(df_per_day))] # Column names starting with "tot"
colnames_avg <- colnames(df_per_day)[grep("avg", colnames(df_per_day))] # Column names starting with "avg"

# Create a data frame that will contain the p-values for all KPIs, variants, target groups for the V(x)_C pair
pval_tbl_biz_kpi_func <- function(df_per_day, df_overall_reshaped){
  # Create an empty data frame
  df_pval_tbl <- data.frame(matrix(ncol = nlevels(df_per_day$target_group) + 3, nrow = nrow(df_overall_reshaped))) # of columns is the # of target groups + 3 for entity_id, exp_id, and var_name. # of rows is the # of KPIs
  
  # Get the col numbers of the relevant columns
  index_kpis_col <- grep("entity_id|experiment_id|variable", colnames(df_overall_reshaped)) # The column number that contains the KPIs
  
  # Name the columns
  colnames(df_pval_tbl) <- c(colnames(df_overall_reshaped)[index_kpis_col], levels(df_per_day$target_group)) # Levels gives us the number of factors
  
  # Populate the columns
  df_pval_tbl$entity_id <- df_overall_reshaped$entity_id
  df_pval_tbl$experiment_id <- df_overall_reshaped$experiment_id
  df_pval_tbl$variable <- df_overall_reshaped$variable
  
  return(df_pval_tbl)
}

# ***FUNCTION CALLING***
num_variants <- length(unique(df_per_day$variant)[unique(df_per_day$variant) != "Control"]) # We want to know how many df_pval data frames to create. We get that from the number of variants in "df_per_day" or "df_overall"

for (i in 1:num_variants) {
  assign(paste0("df_pval_v", i, "_c"), pval_tbl_biz_kpi_func(df_per_day, df_overall_reshaped))
}

##-----------------------------------------------------END OF STEP 7.2-----------------------------------------------------##

# Step 7.3: Calculate significance for all KPIs and target groups

target_groups <- levels(df_per_day$target_group) # Used in the for loop to filter for different target groups

# Create a function that calculates the p-values of all KPIs for any variation pair
pval_calc_biz_kpi_func <- function(df_per_day, variation, df_pval, var_pair_num) {
  j <- 4 # Helper counter for the Wilcoxon test command in the "else" section
  
  for (x in 1:length(levels(df_per_day$target_group))) { # Loops over the different df_pval columns
    for (i in 1:ncol(df_per_day)) { # Loops over the different columns in df_per_day
      if (colnames(df_per_day[, i]) %in% c("entity_id", "experiment_id", "created_date", "variant", "target_group", "corr_factor") | any(is.na(df_per_day[, i]))){ # HERE: Conditions could be changed
        next
      } else {
        pval_stg <- df_per_day %>% 
          filter(variant %in% c(variation, "Control"))
        
        df_pval[i-5, j] <- round(wilcox.test( # "i-5" because the first 3 columns in df_per_day will be skipped
          na.omit(df_per_day[df_per_day$variant == variation & df_per_day[, "target_group"] == target_groups[x], i])[[1]], # You must have [[1]]
          na.omit(df_per_day[df_per_day$variant == "Control" & df_per_day[, "target_group"] == target_groups[x], i])[[1]],  # You must have [[1]]
          paired = TRUE, alternative = "two.sided", conf.int = FALSE, exact = FALSE, correct = FALSE)$p.value, 4)
      }
    }
    j <- j + 1 # Goes from one column to the next in df_pval
  }
  
  df_pval <- df_pval %>% 
    mutate(Var_Pair = paste0("V", var_pair_num, "_Control"))
  
  return(df_pval)
}

# x refers to the target group
# i refers to the KPI
# j refers to the columns in df_pval_v1_c (i.e. which target group will be used as a filter in the df_per_day data frame)
# Note: We decrease "i" by 3 because the first 3 elements of df_per_day are skipped (i.e. i becomes 4) and we need to start with i = 1 to populate df_pval_v1_c

# ***FUNCTION CALLING***
# Populate the df_pval_v(x)_c data frames
## Pull all environment variables that correspond to "df_pval_v[1-9]_c" and remove the NAs
biz_env_vars <- str_extract(ls(.GlobalEnv), "df_pval_v[1-9]_c")
df_pval_var_names <- biz_env_vars[!is.na(biz_env_vars)]

for (i in df_pval_var_names) {
  var_of_concern <- paste0("Var", str_extract_all(i, "[1-9]")[[1]])
  assign(i, pval_calc_biz_kpi_func(df_per_day, var_of_concern, eval(sym(i)), substr_right(var_of_concern, 1)))
}

##-----------------------------------------------------END OF STEP 7.3-----------------------------------------------------##

# Step 8: Combine the p-values of V1_C, V2_C, etc. and display the final result (p-values of the business KPIs)
df_pval_biz_kpis_all <- do.call(rbind, lapply(list(df_pval_var_names)[[1]], function(x) {eval(parse(text = x))})) # You don't need to list the df_pvals individually if you do it that way

# Re-order the columns
one_random_df_pval_biz <- eval(parse(text = df_pval_var_names[1])) # Instead of specifying a data frame, we do it programatically
df_pval_biz_kpis_all <- df_pval_biz_kpis_all[, c("Var_Pair", colnames(one_random_df_pval)[1:(length(one_random_df_pval_biz)-1)])]

# Add the prefix "P_Val" to the column names
col_names_with_tg <- colnames(df_pval_biz_kpis_all)[grep("TG", colnames(df_pval_biz_kpis_all))]
colnames(df_pval_biz_kpis_all)[grep("TG", colnames(df_pval_biz_kpis_all))] <- sapply(col_names_with_tg, function(x) {paste0("P_Val_", x)})

# Add the p-values to df_overall_reshaped We need to add each "Var_Pair" as a separate partition because there are no direct join keys
p_val_join_func <- function(df_pval, df_kpis) {
  for (i in sort(unique(df_pval$Var_Pair))) {
    y <- df_pval # A temporary placeholder for the "df_pval" data frame
    df_pval_cvr_all_col_name_sep <- sapply(colnames(y)[grep("P_Val", colnames(y))], function(x) {strsplit(x, "P_Val")[[1]][2]}) # This function separates "P_Val_TG1_Non_TG" to "nothing" and "_TG1_Non_TG". It applies this manipulation to all columns that have "P_Val" in their name
    df_pval_cvr_all_col_name_new <- sapply(df_pval_cvr_all_col_name_sep, function(y, x) {paste0("P_Val_", str_sub(x, 1, 4), y)}, x = i) # First argument of the function f(x) always refers to the first argument of the apply function
    
    colnames(y)[grep("P_Val", colnames(y))] <- df_pval_cvr_all_col_name_new # Rename the columns that contain "P_Val" in their name to "df_pval_cvr_all_col_name_new"
    
    df_kpis <- df_kpis %>% # Global Assignment 
      left_join(y %>% filter(Var_Pair == i) %>% select(c(-Var_Pair)), by = c("entity_id", "experiment_id", "variable")) # Join the P_Val columns of all target groups for a "specific" variant to "df_cvr_dwnld_reshaped"
  }
  
  return(df_kpis)
}

# ***FUNCTION CALLING***
df_overall_reshaped <- p_val_join_func(df_pval_biz_kpis_all, df_overall_reshaped)

# Re-order the columns and choose the relevant ones
df_overall_reshaped <- df_overall_reshaped[, col_order]

##-----------------------------------------------------END OF STEP 8-----------------------------------------------------##

# Step 9: Display the CVR data frame in the same format as the business KPIs data frame

# Retrieve the CVR data frame
contingency_tbl_func <- function(per_day, exp_id, entity) {
  # Select the right CVR table
  if (per_day == "No") {
    cvr_df <<- df_cvr_overall # Global assignment
  } else if (per_day == "Yes") {
    cvr_df <<- df_cvr_per_day # Global assignment
  }
  
  # Filter the CVR table for the right experiment ID and target group. Add a condition to filter for the right surge_event_flag if it exists
  df_cvr_dwnld <- cvr_df %>% 
    filter(experiment_id == exp_id, entity_id == entity) %>%
    collect()
  
  df_cvr_conting <- df_cvr_dwnld %>% # Contingency table for significance calculation
    select(entity_id, experiment_id, target_group, variant, transactions, shop_list_sessions, shop_menu_sessions, checkout_sessions, checkout_transaction) # These columns can be used to calculate CVR2, CVR3, and mCVR4
  
  return(list(df_cvr_dwnld, df_cvr_conting))
}

# Call the function
# ***FUNCTION CALLING***
df_cvr_dwnld <- contingency_tbl_func("No", exp_id_var, ent_id)[[1]] # We need the "overall" here, not "per_day" cuz we are not calculating significance, so use "No"
df_cvr_conting <- contingency_tbl_func("No", exp_id_var, ent_id)[[2]] # We need the "overall" here, not "per_day" cuz we are not calculating significance, so use "No"

# Change the structure of the df_overall table such that it shows the KPIs as rows and the variant + target groups as columns
df_cvr_dwnld_reshaped <- recast(df_cvr_dwnld, entity_id + experiment_id + variable ~ target_group + variant, id.var = c("entity_id", "experiment_id", "target_group", "variant"))

# Convert all columns after the grouping columns to numeric
df_cvr_dwnld_reshaped <- df_cvr_dwnld_reshaped %>% 
  mutate_at(colnames(.)[grep("Variation|Var|Control", colnames(.))], as.numeric)

# Calculate the deltas for df_cvr_dwnld
df_cvr_dwnld_reshaped <- delta_calc_func(df_cvr_dwnld_reshaped)

# Shorten all column names with the word "Variation" to "Var"
colnames(df_cvr_dwnld_reshaped)[grep("Variation", colnames(df_cvr_dwnld_reshaped))] <- gsub("Variation", "Var", colnames(df_cvr_dwnld_reshaped)[grep("Variation", colnames(df_cvr_dwnld_reshaped))])

##-----------------------------------------------------END OF STEP 9-----------------------------------------------------##

# Step 10: Calculate the significance of CVR2, CVR3, and mCVR4. Create a function that calculates the p-values of CVR2 and CVR3 (several steps involved)

# First, change the target_group column to a factor column
df_cvr_conting <- df_cvr_conting %>% 
  mutate_at(.vars = vars(target_group), ~ factor(., levels = tg_order, ordered = TRUE))

# Second, create a function that calculates the p-values based on the entity_id, experiment_id, target group, variant, and CVR metric of interest
pval_calc_cvr_func <- function(tg, variation, cvr_metric, cvr_df, exp_id, entity) {
  cvr_df <- cvr_df %>% 
    filter(target_group == tg, variant %in% c("Control", variation), experiment_id == exp_id, entity_id == entity) %>% 
    {if (cvr_metric == "CVR2") 
      select(., entity_id, experiment_id, target_group, variant, transactions, shop_list_sessions) 
      else if (cvr_metric == "CVR3") 
        select(., entity_id, experiment_id, target_group, variant, transactions, shop_menu_sessions)
      else
        select(., entity_id, experiment_id, target_group, variant, checkout_transaction, checkout_sessions)
    }
  
  if (cvr_metric == "CVR2") {
    return(round(prop.test(x = cvr_df$transactions, n = cvr_df$shop_list_sessions, alternative = "two.sided", correct = FALSE)$p.value, 4)[[1]])
  } else if (cvr_metric == "CVR3") {
    return(round(prop.test(x = cvr_df$transactions, n = cvr_df$shop_menu_sessions, alternative = "two.sided", correct = FALSE)$p.value, 4)[[1]])
  } else if (cvr_metric == "mCVR4") {
    return(round(prop.test(x = cvr_df$checkout_transaction, n = cvr_df$checkout_sessions, alternative = "two.sided", correct = FALSE)$p.value, 4)[[1]])
  }
}

# Third, write a function that creates empty data frames that will later be populated with p-values for a specific variant
pval_tbl_cvr_func <- function(cvr_conting, cvr_dwnld, num_of_cvr_metrics, variation, var_pair_num, exp_id, entity){
  # Create an empty dataframe
  df_pval_tbl <- data.frame(matrix(ncol = nlevels(cvr_conting$target_group) + 3, nrow = num_of_cvr_metrics)) # of columns is the # of target groups + 3 for entity_id, experiment_id, and var_name. # of rows is the # of KPIs

  # Get the col numbers of the relevant grouping columns
  index_kpis_col <- grep("entity_id|experiment_id", colnames(cvr_conting))
  
  # Name the columns
  colnames(df_pval_tbl) <- c(colnames(df_overall_reshaped)[index_kpis_col], "variable", levels(cvr_conting$target_group)) # Levels gives us the number of factors
  
  # Populate the columns
  df_pval_tbl$entity_id <- unique(cvr_conting$entity_id)
  df_pval_tbl$experiment_id <- unique(cvr_conting$experiment_id)
  df_pval_tbl$variable <- colnames(cvr_dwnld)[grep("CVR", colnames(cvr_dwnld))]
  
  # Call the row names CVR(x) (i.e., make them similar to the row values of the first column)
  row.names(df_pval_tbl) <- df_pval_tbl[, "variable"]
  
  # Populate the empty table with p-values calculated through the Z-test for proportions (http://www.sthda.com/english/wiki/two-proportions-z-test-in-r)
  for (j in levels(cvr_conting$target_group)) {
    for (i in row.names(df_pval_tbl)) {
      df_pval_tbl[i,j] <- pval_calc_cvr_func(j, variation, i, df_cvr_conting, exp_id, entity) # df_cvr_conting is the actual dataframe
    }
  }
  
  df_pval_tbl <- df_pval_tbl %>% 
    mutate(Var_Pair = paste0("V", var_pair_num, "_Control"))
  
  return(df_pval_tbl)
}

# ***FUNCTION CALLING***
for (i in 1:num_variants) {
  assign(paste0("df_pval_cvr_v", i, "_c"), pval_tbl_cvr_func(df_cvr_conting, df_cvr_dwnld, num_cvr_metric, paste0("Variation", i), i, exp_id_var, ent_id)) # 2 for CVR2 and CVR3. If you have more CVRs or proportions, increase the number
}

##-----------------------------------------------------END OF STEP 10-----------------------------------------------------##

# Step 11: Combine the p-values of V1_C, V2_C, etc. and display the final result (p-values of the business KPIs)
## Pull all environment variables that correspond to "df_pval_v[1-9]_c" and remove the NAs
cvr_env_vars <- str_extract(ls(.GlobalEnv), "df_pval_cvr_v[1-9]_c")
df_pval_cvr_var_names <- cvr_env_vars[!is.na(cvr_env_vars)]

df_pval_cvr_all <- do.call(rbind, lapply(list(df_pval_cvr_var_names)[[1]], function(x) {eval(parse(text = x))}))

# Re-order the columns
one_random_df_pval_cvr <- eval(parse(text = df_pval_cvr_var_names[1]))
df_pval_cvr_all <- df_pval_cvr_all[, c("Var_Pair", colnames(one_random_df_pval_cvr)[1:(length(one_random_df_pval_cvr)-1)])]

# Add the prefix "P_Val" to the column names
colnames(df_pval_cvr_all)[grep("TG", colnames(df_pval_cvr_all))] <- sapply(col_names_with_tg, function(x) {paste0("P_Val_", x)}) # col_names_with_tg comes from the df_pval_biz_kpis section

# Delete the row names as we no longer need them
row.names(df_pval_cvr_all) <- NULL

# Add the p-values to df_cvr_dwnld_reshaped. We need to add each "Var_Pair" as a separate partition because there are no direct join keys
df_cvr_dwnld_reshaped <- p_val_join_func(df_pval_cvr_all, df_cvr_dwnld_reshaped)

# Re-order the columns and choose the relevant ones
df_cvr_dwnld_reshaped <- df_cvr_dwnld_reshaped[, col_order]

##-----------------------------------------------------END OF STEP 11-----------------------------------------------------##

# Step 12: Display the final results (business KPIs + CVRs + deltas)

# Pick the columns that you want to display in the final output
# Step 1: Remove the unnecessary rows from df_overall_reshaped
row_selection_biz_kpis <- df_overall_reshaped$variable[grep("adj|corr_factor|avg", df_overall_reshaped$variable)]

full_results <- df_overall_reshaped %>% 
  filter(variable %in% row_selection_biz_kpis) %>% # Choose the desired rows
  rbind(df_cvr_dwnld_reshaped)

# Add a column counter that you can order by when you upload the data frame to BQ
full_results$counter <- seq(1, nrow(full_results), 1)

##-----------------------------------------------------END OF STEP 12-----------------------------------------------------##

# Step 12.1: Upload the data frames containing the results to GBQ

# Full results of the business KPIs
bq_perform_upload(paste0(project_id_data, ".", data_set, ".", bq_tbl_name), values = full_results, fields = full_results, 
                  write_disposition = "WRITE_TRUNCATE", create_disposition = "CREATE_IF_NEEDED")

##-----------------------------------------------------END OF STEP 12.1-----------------------------------------------------##

# NOTE: DID NOT VET THIS PART FOR FULL AUTOMATION
# Step 13: Prepare some data sets for the analysis of success metrics instead of doing that in Excel
df_overall_control_grp <- df_overall %>% 
  filter(target_group %in% c("TG1", "TG2", "TG1_Non_TG") & variant == "Control") %>% 
  group_by(entity_id, experiment_id, target_group) %>% 
  select(entity_id, experiment_id, target_group, tot_orders_adj, tot_profit_local_adj, avg_df_local)

## Sensitivity data set (comparing variants across target groups)
df_sensitivity <- df_overall %>% 
  select(entity_id, experiment_id, target_group, variant, tot_orders_adj, avg_df_local) %>% 
  filter(target_group %in% c("TG1", "TG2")) %>% # Filter for the target groups under the treatment scope
  left_join(df_overall_control_grp %>% select(-tot_profit_local_adj), 
            by = c("entity_id", "experiment_id", "target_group"),
            suffix = c("_var", "_control")) %>% 
  mutate(avg_df_inc_wrt_control = round(avg_df_local_var / avg_df_local_control - 1, 4),
         elasticity = round((tot_orders_adj_var / tot_orders_adj_control - 1) / (avg_df_local_var / avg_df_local_control - 1), 4)) %>% 
  filter(variant != "Control") %>%  # Remove the control group from the data set
  select(-c(avg_df_local_var, avg_df_local_control, tot_orders_adj_var, tot_orders_adj_control)) # Remove unnecessary columns

### Remove the values for variations 1, 2, and 4
df_sensitivity[df_sensitivity$variant %in% irrelevants_vars & df_sensitivity$target_group == non_lb_tg, c(ncol(df_sensitivity), ncol(df_sensitivity) - 1)] <- NA

### Change "Variation" to "Var" under the variant column
df_sensitivity$variant <- gsub("Variation", "Var", df_sensitivity$variant)

### Add a counter so that we can sort the rows when the df gets uploaded to GBQ
df_sensitivity$counter = seq(1, nrow(df_sensitivity), 1)

## Profit increase to order drop tradeoff
df_tradeoff <- df_overall %>% 
  select(entity_id, experiment_id, target_group, variant, tot_orders_adj, tot_profit_local_adj, avg_df_local) %>%
  filter(target_group %in% c("TG1", "TG1_Non_TG")) %>% # Filter for the target group under the treatment scope + overall
  left_join(df_overall_control_grp, 
            by = c("entity_id", "experiment_id", "target_group"),
            suffix = c("_var", "_control")) %>% 
  mutate(order_drop_wrt_control = round(tot_orders_adj_var / tot_orders_adj_control - 1, 4),
         tot_gp_inc_wrt_control = round(tot_profit_local_adj_var / tot_profit_local_adj_control - 1, 4),
         gp_inc_per_order_lost_ratio = round(tot_gp_inc_wrt_control / abs(order_drop_wrt_control), 2),
         avg_df_inc_wrt_control = round(avg_df_local_var / avg_df_local_control - 1, 4),
         data_timestamp = Sys.time()) %>% 
  filter(variant != "Control") %>% # Remove the control group from the data set
  select(-c(avg_df_local_var, avg_df_local_control, tot_orders_adj_var, tot_orders_adj_control, tot_profit_local_adj_var, tot_profit_local_adj_control)) # Remove unnecessary columns

### Change "Variation" to "Var" under the variant column
df_tradeoff$variant <- gsub("Variation", "Var", df_tradeoff$variant)

### Add the "TG_Var" column for the tradeoff chart
df_tradeoff <- add_column(df_tradeoff, tg_var = paste0(df_tradeoff$target_group, " | ", df_tradeoff$variant), .before = df_tradeoff$order_drop_wrt_control)

# Add a counter so that we can sort the rows when the df gets uploaded to GBQ
df_tradeoff$counter = seq(1, nrow(df_tradeoff), 1)

##-----------------------------------------------------END OF STEP 13-----------------------------------------------------##

# Step 14: Upload the special data sets to GBQ

## df_sensitivity
bq_perform_upload(paste0(project_id_data, ".", data_set, ".", str_replace(bq_tbl_name, "_business_kpis_", "_df_sensitivity_")),
                  values = df_sensitivity, fields = df_sensitivity, write_disposition = "WRITE_TRUNCATE", create_disposition = "CREATE_IF_NEEDED")

## df_tradeoff
bq_perform_upload(paste0(project_id_data, ".", data_set, ".", str_replace(bq_tbl_name, "_business_kpis_", "_df_tradeoff_")),
                  values = df_tradeoff, fields = df_tradeoff, write_disposition = "WRITE_TRUNCATE", create_disposition = "CREATE_IF_NEEDED")
