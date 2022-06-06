DECLARE entity_var, city_name_var, d_type STRING;
DECLARE vertical_var ARRAY <STRING>;
DECLARE start_date, end_date DATE;
DECLARE start_date_tt_adj, end_date_tt_adj DATE;
DECLARE sessions_ntile_thr, orders_ntile_thr, cvr3_ntile_thr FLOAT64;
DECLARE highest_main_df FLOAT64;
DECLARE threshold_is_scheme_or_main_df STRING;
DECLARE qcommerce_bool BOOL;
SET (entity_var, city_name_var, d_type) = ('FP_TH', 'Prachuap khiri khan', 'OWN_DELIVERY');
SET vertical_var = ['restaurants', 'street_food', 'caterers'];
SET (start_date, end_date) = (DATE_SUB(DATE('2022-04-04'), INTERVAL 62 DAY), DATE('2022-04-04')); -- You can replace DATE('2022-04-04') with CURRENT_DATE()
SET (start_date_tt_adj, end_date_tt_adj) = (DATE_SUB(DATE('2022-04-04'), INTERVAL 32 DAY), DATE('2022-04-04')); -- You can replace DATE('2022-04-04') with CURRENT_DATE()
SET (sessions_ntile_thr, orders_ntile_thr, cvr3_ntile_thr) = (0.75, 0.75, 0.75);
SET highest_main_df = 36; -- If there is a case like PH where there is a huge drop in orders after a specific DF tier, you can include that value here so that data of DF tiers beyond that value are not considered
SET threshold_is_scheme_or_main_df = 'scheme'; -- Enter 'main' or 'scheme' to determine how the DF threshold should be defined (max DF of the scheme or max DF of the main tiers like in PH)
SET qcommerce_bool = FALSE; -- TRUE if you select Q-commerce verticals. FALSE if not
-- We are not using the hard coded "pricing_scheme", "sessions_threshold", or "cvr3_threshold" anymore. Instead, we are using "sessions_ntile_thr", "orders_ntile_thr", "cvr3_ntile_thr" which are percentile rank thresholds. 
-- Note: THIS SCRIPT ONLY WORKS FOR ONE ENTITY AND ONE CITY

##------------------------------------------------------------------------END OF THE INPUT SECTION------------------------------------------------------------------------##

-- Query 1: Get data on the different pricing schemes used in a particular city and pick the dominant one
CREATE OR REPLACE TABLE `dh-logistics-product-ops.pricing.loved_brands_apac_dominant_price_scheme_pkk` AS
SELECT 
    *,
    ROUND(SUM(Vendor_Share) OVER (ORDER BY Vendor_Count DESC), 4) AS Vendor_Share_Cumulative,
    ROUND(SUM(Order_Share) OVER (ORDER BY Order_Count DESC), 4) AS Order_Share_Cumulative,
FROM (
    SELECT
        a.entity_id,
        a.city_name,
        a.city_id,
        a.scheme_id,
        sch.scheme_name,
        COUNT(DISTINCT vendor_id) AS Vendor_Count,  
        ROUND(COUNT(DISTINCT vendor_id) / SUM(COUNT(DISTINCT vendor_id)) OVER (), 4) AS Vendor_Share,
        SUM(COUNT(DISTINCT vendor_id)) OVER () AS Total_Vendors,
        COUNT(DISTINCT order_id) AS Order_Count,
        ROUND(COUNT(DISTINCT order_id) / SUM(COUNT(DISTINCT order_id)) OVER (), 4) AS Order_Share,
        SUM(COUNT(DISTINCT order_id)) OVER () AS Total_Orders
    FROM `fulfillment-dwh-production.cl.dps_sessions_mapped_to_orders_v2` a
    LEFT JOIN (
        SELECT DISTINCT 
            entity_id, 
            dps.scheme_name, 
            dps.scheme_id 
        FROM `fulfillment-dwh-production.cl.vendors_v2` 
        LEFT JOIN UNNEST(dps) dps
    ) sch ON a.entity_id = sch.entity_id AND a.scheme_id = sch.scheme_id
    WHERE TRUE 
        AND created_date BETWEEN start_date AND end_date -- 30 days of data
        AND a.entity_id = entity_var -- Change the entity according to your use case
        AND city_name = city_name_var -- Change the city according to your use case
        AND is_own_delivery -- OD or MP
        AND delivery_status = 'completed'
        AND vertical_type IN UNNEST(vertical_var)
    GROUP BY 1,2,3,4,5
)
ORDER BY Order_Share DESC;

##------------------------------------------------------------------------END OF THE DOMINANT SCHEME PART------------------------------------------------------------------------##

CREATE OR REPLACE TABLE `dh-logistics-product-ops.pricing.loved_brands_apac_city_data_pkk` AS
--  A query to select the location details of zones within a city. This table can be joined to "dps_sessions_mapped_to_ga_sessions" and the "zone_shape" geo field can be used to filter for sessions in the zones that are of interest to us
SELECT 
    p.entity_id,
    country_code,
    ci.name AS city_name,
    ci.id AS city_id,
    zo.shape AS zone_shape, 
    zo.name AS zone_name,
    zo.id AS zone_id
FROM fulfillment-dwh-production.cl.countries co
LEFT JOIN UNNEST(co.platforms) p
LEFT JOIN UNNEST(co.cities) ci
LEFT JOIN UNNEST(ci.zones) zo
WHERE TRUE 
    AND entity_id = entity_var -- Entity ID
    AND ci.name = city_name_var -- City Name
    AND zo.is_active -- Active city
    AND ci.is_active; -- Active zone

##------------------------------------------------------------------------END OF THE CITY DATA PART------------------------------------------------------------------------##

-- Query 2: Get the orders, CVR3 and sessions per vendor for all vendors in a particular city under the scheme chosen above
CREATE OR REPLACE TABLE `dh-logistics-product-ops.pricing.loved_brands_apac_all_vendors_pkk` AS
WITH vendors AS ( -- A query to select all vendors within a city that are under a specific price scheme
    SELECT DISTINCT
        v.entity_id,
        city_name_var AS city_name,
        v.vendor_code
    FROM `fulfillment-dwh-production.cl.vendors_v2` v
    LEFT JOIN UNNEST(v.vendor) ven
    LEFT JOIN UNNEST(v.dps) dps
    LEFT JOIN UNNEST(v.hurrier) hur
    LEFT JOIN UNNEST(v.zones) z
    CROSS JOIN UNNEST(delivery_provider) AS delivery_type -- "delivery_provider" is an array that sometimes contains multiple elements, so we need to unnest it and break it down to its individual components
    INNER JOIN `dh-logistics-product-ops.pricing.loved_brands_apac_city_data_pkk` cd ON v.entity_id = cd.entity_id AND ST_CONTAINS(cd.zone_shape, v.location) -- This is an alternative to using dps.name/dps.id in the WHERE clause. Here, we filter for sessions in the chosen city
    WHERE TRUE 
        AND v.entity_id = entity_var -- Entity ID
        AND v.is_active -- Active vendors
        AND v.vertical_type IN UNNEST(vertical_var) -- Restaurants vertical only
        AND delivery_type = d_type -- Filter for OD vendors
        AND dps.scheme_id = (SELECT scheme_id FROM `dh-logistics-product-ops.pricing.loved_brands_apac_dominant_price_scheme_pkk` WHERE Order_Share = (SELECT MAX(Order_Share) FROM `dh-logistics-product-ops.pricing.loved_brands_apac_dominant_price_scheme_pkk` WHERE scheme_id IS NOT NULL)) -- Scheme ID with the highest order share in the city
),

sessions AS ( -- Get session data for **all** sessions in the chosen city over the specified timeframe. We don't need to specify the expedition type (OD vs. pickup) or the vertical (restaurants, darkstores, pharmacies, etc.) because we already have a defined list of vendors that we will JOIN to below
    SELECT DISTINCT
        x.created_date, -- Date of the ga session
        x.entity_id, -- Entity ID
        x.platform, -- Operating system (iOS, Android, Web, etc.)
        x.brand, -- Talabat, foodpanda, Foodora, etc.
        x.events_ga_session_id, -- GA session ID
        x.fullvisitor_id, -- The visit_id defined by Google Analytics
        x.visit_id, -- 	The visit_id defined by Google Analytics
        x.has_transaction, -- A field that indicates whether or not a session ended in a transaction
        x.total_transactions, -- The total number of transactions in the GA session
        x.ga_dps_session_id, -- DPS session ID
        
        x.sessions.dps_session_timestamp, -- The timestamp of the DPS log.
        x.sessions.endpoint, -- The endpoint from where the DPS request is coming, including MultipleFee, which could come from Listing Page or others. and SingleFee, which could come from Menu page or others
        x.sessions.perseus_client_id, -- A unique customer identifier based on the device
        x.sessions.variant, -- AB variant (e.g. Control, Variation1, Variation2, etc.)
        x.sessions.customer_status, -- The customer.tag in DPS log, indicating whether the customer is new or not
        x.sessions.location, -- The customer.location in DPS log
        x.sessions.variant_concat, -- The concatenation of all the existing variants in the DPS log for the dps session id. There might be multiple variants due to location changes or session timeout
        x.sessions.location_concat, -- The concatenation of all the existing locations in the DPS log for the dps session id
        x.sessions.customer_status_concat, -- 	The concatenation of all the existing customer.tag in the DPS log for the dps session id

        e.event_action, -- Can have five values --> home_screen.loaded, shop_list.loaded, shop_details.loaded, checkout.loaded, transaction
        e.vendor_code, -- Vendor ID
        e.event_time, -- The timestamp of the event's creation.
        e.transaction_id, -- The transaction id for the GA session if the session has a transaction (i.e. order code)
        e.expedition_type, -- The delivery type of the session, pickup or delivery

        dps.id, -- Zone ID based on the DPS session
        dps.name, -- Zone name based on the DPS session
        dps.timezone, -- Time zone of the city based on the DPS session

        ST_ASTEXT(x.ga_location) AS ga_location -- GA location expressed as a STRING
    FROM `fulfillment-dwh-production.cl.dps_sessions_mapped_to_ga_sessions` x
    LEFT JOIN UNNEST(events) e
    LEFT JOIN UNNEST(dps_zone) dps
    INNER JOIN `dh-logistics-product-ops.pricing.loved_brands_apac_city_data_pkk` cd ON x.entity_id = cd.entity_id AND ST_CONTAINS(cd.zone_shape, x.ga_location) -- This is an alternative to using dps.name/dps.id in the WHERE clause. Here, we filter for sessions in the chosen city
    WHERE TRUE
        AND x.entity_id = entity_var -- Filter for the entity of interest
        AND x.created_date BETWEEN start_date AND end_date -- Sessions' start and end date
        AND e.event_action IN ('shop_details.loaded', 'transaction') -- transaction / shop_details.loaded = CVR3
),

orders AS ( -- Get the number of orders of **all** vendors in the chosen city over the specified timeframe. We don't need to specify the expedition type (OD vs. pickup) or the vertical (restaurants, darkstores, pharmacies, etc.) because we already have a defined list of vendors that we will JOIN to below
    SELECT
        global_entity_id,
        vendor_id,
        COUNT(DISTINCT order_id) AS Num_orders
    FROM `fulfillment-dwh-production.curated_data_shared_central_dwh.orders` o
    INNER JOIN `dh-logistics-product-ops.pricing.loved_brands_apac_city_data_pkk` cd ON o.global_entity_id = cd.entity_id AND ST_CONTAINS(cd.zone_shape, ST_GEOGPOINT(delivery_location.longitude, delivery_location.latitude)) -- Filter for delivery locations in the city of choice
    WHERE TRUE
        AND global_entity_id = entity_var
        AND DATE(placed_at) BETWEEN start_date AND end_date
        AND is_sent -- Successful order
    GROUP BY 1,2
),

all_metrics AS ( 
    SELECT 
        v.entity_id,
        v.city_name,
        v.vendor_code,
        COALESCE(o.Num_orders, 0) AS Num_orders,
        COALESCE(COUNT(DISTINCT CASE WHEN event_action = 'shop_details.loaded' THEN events_ga_session_id ELSE NULL END), 0) AS Num_Unique_Vendor_Visits, -- If a vendor was visited more than once in the same session, this is considered one visit
        COALESCE(COUNT(DISTINCT CASE WHEN event_action = 'shop_details.loaded' THEN event_time ELSE NULL END), 0) AS Num_Total_Vendor_Impressions, -- If a vendor was visited more than once in the same session, all impressions are counted
        COALESCE(COUNT(DISTINCT CASE WHEN event_action = 'transaction' THEN event_time ELSE NULL END), 0) AS Num_Transactions,
        COALESCE(ROUND(COUNT(DISTINCT CASE WHEN event_action = 'transaction' THEN event_time ELSE NULL END) / NULLIF(COUNT(DISTINCT CASE WHEN event_action = 'shop_details.loaded' THEN events_ga_session_id ELSE NULL END), 0), 3), 0) AS CVR3,
    FROM vendors v
    LEFT JOIN sessions s ON v.entity_id = s.entity_id  AND v.vendor_code = s.vendor_code -- LEFT JOIN because we assume that vendors_v2 contains ALL vendors with and without sessions, so any vendors without sessions will get a "zero"
    LEFT JOIN orders o ON v.entity_id = o.global_entity_id AND v.vendor_code = o.vendor_id -- LEFT JOIN for the same reason in the statement above
    GROUP BY 1,2,3,4
),

pct_ranks AS (
    SELECT
        *,
        ROUND(PERCENT_RANK() OVER (PARTITION BY entity_id, city_name ORDER BY Num_orders), 4) AS Orders_pct_rank,
        ROUND(PERCENT_RANK() OVER (PARTITION BY entity_id, city_name ORDER BY Num_Unique_Vendor_Visits), 4) AS Unique_visits_pct_rank,
        ROUND(PERCENT_RANK() OVER (PARTITION BY entity_id, city_name ORDER BY CVR3), 4) AS CVR3_pct_rank
    FROM all_metrics
)

SELECT * FROM pct_ranks;

-- Filtering for vendors based on percentile ranks
CREATE OR REPLACE TABLE `dh-logistics-product-ops.pricing.loved_brands_apac_all_vendors_after_session_order_cvr_filters_pkk` AS
SELECT * FROM `dh-logistics-product-ops.pricing.loved_brands_apac_all_vendors_pkk`
WHERE Unique_visits_pct_rank >= sessions_ntile_thr AND Orders_pct_rank >= orders_ntile_thr AND CVR3_pct_rank >= cvr3_ntile_thr
ORDER BY 7 DESC;

#############################################################################################################################################################################################################
#####------------------------------------------------------------------------END OF THE ORDERS, SESSIONS, CVR3 FILTERING PROCESS------------------------------------------------------------------------#####
#############################################################################################################################################################################################################

# Get data about the DF tiers under the dominant price scheme in the city
CREATE OR REPLACE TABLE `dh-logistics-product-ops.pricing.loved_brands_apac_df_tiers_pkk` AS
SELECT 
    *,
    RANK() OVER (PARTITION BY scheme_id, component_id ORDER BY threshold) AS tier
FROM (
    SELECT DISTINCT 
        v.entity_id,
        d.scheme_id,
        d.scheme_name,
        pc.travel_time_config.id AS component_id,
        pc.travel_time_config.name AS component_name,
        CASE WHEN pc.travel_time_config.threshold IS NULL THEN 9999999 ELSE pc.travel_time_config.threshold END AS threshold,
        CASE 
            WHEN pc.travel_time_config.threshold IS NULL THEN 9999999 
            ELSE ROUND(FLOOR(pc.travel_time_config.threshold) + (pc.travel_time_config.threshold - FLOOR(pc.travel_time_config.threshold)) * 60/100, 2) 
        END AS threshold_in_min_and_sec,
        pc.travel_time_config.fee
    FROM `fulfillment-dwh-production.cl.vendors_v2` v
    LEFT JOIN UNNEST(zones) z
    LEFT JOIN UNNEST(dps) d
    LEFT JOIN UNNEST(d.vendor_config) vc
    LEFT JOIN UNNEST(vc.pricing_config) pc
    INNER JOIN `dh-logistics-product-ops.pricing.loved_brands_apac_city_data_pkk` cd ON v.entity_id = cd.entity_id AND ST_CONTAINS(cd.zone_shape, v.location)
    WHERE TRUE 
        AND v.entity_id = entity_var
        AND cd.city_name = city_name_var
        AND d.is_active
        AND d.scheme_id = (SELECT scheme_id FROM `dh-logistics-product-ops.pricing.loved_brands_apac_dominant_price_scheme_pkk` WHERE Order_Share = (SELECT MAX(Order_Share) FROM `dh-logistics-product-ops.pricing.loved_brands_apac_dominant_price_scheme_pkk` WHERE scheme_id IS NOT NULL)) -- Scheme ID with the highest order share in the city
)
ORDER BY 1,2,4;

##------------------------------------------------------------------------END OF THE DF TIER DATA EXTRACTION PART------------------------------------------------------------------------##

-- Get data about the DF from the DPS logs
CREATE OR REPLACE TABLE `dh-logistics-product-ops.pricing.loved_brands_apac_dps_logs_pkk` AS
WITH dps_logs_stg_1 AS ( -- Will be used to get the CVR per DF
    SELECT DISTINCT
        logs.entity_id,
        logs.created_date,
        endpoint,
        customer.user_id AS perseus_id,
        customer.session.id AS dps_session_id,
        v.id AS vendor_code,
        v.delivery_fee.total AS DF_total,
        customer.session.timestamp AS session_timestamp,
        logs.created_at
FROM `fulfillment-dwh-production.cl.dynamic_pricing_user_sessions` logs
LEFT JOIN UNNEST(vendors) v
INNER JOIN `dh-logistics-product-ops.pricing.loved_brands_apac_city_data_pkk` cd ON logs.entity_id = cd.entity_id AND ST_CONTAINS(cd.zone_shape, logs.customer.location) -- Filter for sessions in the city specified above
WHERE TRUE 
    AND logs.entity_id = entity_var
    AND logs.created_date BETWEEN start_date AND end_date
    AND logs.customer.session.id IS NOT NULL -- We must have the dps session ID to be able to obtain the session's DF in the next query
    AND endpoint = 'singleFee' -- Caters for the following events --> "shop_details.loaded", "checkout.loaded" and "transaction"
    AND v.id IN (SELECT vendor_code FROM `dh-logistics-product-ops.pricing.loved_brands_apac_all_vendors_after_session_order_cvr_filters_pkk`) -- Filter for relevant DPS sessions ONLY (i.e., those that belong to vendor IDs that were selected in the first code section)
),

dps_logs_stg_2 AS (
    SELECT 
        *,
        ROW_NUMBER() OVER (PARTITION BY dps_session_id, vendor_code ORDER BY created_at DESC) AS row_num_dps_logs -- Create a row counter to take the last delivery fee seen in the session. We assume that this is the one that the customer took their decision to purchase/not purchase on
    FROM dps_logs_stg_1
),

dps_logs AS(
    SELECT *
    FROM dps_logs_stg_2 
    WHERE row_num_dps_logs = 1 -- Take the last DF seen by the customer during the session
)

SELECT * FROM dps_logs
ORDER BY dps_session_id, vendor_code, created_at;

##-------------------------------------------------------------------------END OF THE DPS LOGS PART-------------------------------------------------------------------------##

CREATE OR REPLACE TABLE `dh-logistics-product-ops.pricing.loved_brands_apac_ga_dps_sessions_pkk` AS
WITH ga_dps_sessions AS (
    SELECT DISTINCT
        x.created_date, -- Date of the ga session
        x.entity_id, -- Entity ID
        x.platform, -- Operating system (iOS, Android, Web, etc.)
        x.brand, -- Talabat, foodpanda, Foodora, etc.
        x.events_ga_session_id, -- GA session ID
        x.fullvisitor_id, -- The visit_id defined by Google Analytics
        x.visit_id, -- 	The visit_id defined by Google Analytics
        x.has_transaction, -- A field that indicates whether or not a session ended in a transaction
        x.total_transactions, -- The total number of transactions in the GA session
        x.ga_dps_session_id, -- DPS session ID
        
        x.sessions.dps_session_timestamp, -- The timestamp of the DPS log.
        x.sessions.endpoint, -- The endpoint from where the DPS request is coming, including MultipleFee, which could come from Listing Page or others. and SingleFee, which could come from Menu page or others
        x.sessions.perseus_client_id, -- A unique customer identifier based on the device
        x.sessions.variant, -- AB variant (e.g. Control, Variation1, Variation2, etc.)
        x.sessions.customer_status, -- The customer.tag in DPS log, indicating whether the customer is new or not
        x.sessions.location, -- The customer.location in DPS log
        x.sessions.variant_concat, -- The concatenation of all the existing variants in the DPS log for the dps session id. There might be multiple variants due to location changes or session timeout
        x.sessions.location_concat, -- The concatenation of all the existing locations in the DPS log for the dps session id
        x.sessions.customer_status_concat, -- 	The concatenation of all the existing customer.tag in the DPS log for the dps session id

        e.event_action, -- Can have five values --> home_screen.loaded, shop_list.loaded, shop_details.loaded, checkout.loaded, transaction
        e.vendor_code, -- Vendor ID
        e.event_time, -- The timestamp of the event's creation.
        e.transaction_id, -- The transaction id for the GA session if the session has a transaction (i.e. order code)
        e.expedition_type, -- The delivery type of the session, pickup or delivery

        dps.id, -- Zone ID based on the DPS session
        dps.name, -- Zone name based on the DPS session
        dps.timezone, -- Time zone of the city based on the DPS session

        ST_ASTEXT(x.ga_location) AS ga_location, -- GA location expressed as a STRING
        
        logs.DF_total
    FROM `fulfillment-dwh-production.cl.dps_sessions_mapped_to_ga_sessions` x
    LEFT JOIN UNNEST(events) e
    LEFT JOIN UNNEST(dps_zone) dps
    INNER JOIN `dh-logistics-product-ops.pricing.loved_brands_apac_city_data_pkk` cd ON x.entity_id = cd.entity_id AND ST_CONTAINS(cd.zone_shape, x.ga_location) -- This is an alternative to using dps.name/dps.id in the WHERE clause. Here, we filter for sessions in the chosen city
    LEFT JOIN `dh-logistics-product-ops.pricing.loved_brands_apac_dps_logs_pkk` logs -- You can use an INNER JOIN here if it's important for your to have a DF value associated with every session
        ON TRUE
        AND x.entity_id = logs.entity_id 
        AND x.ga_dps_session_id = logs.dps_session_id 
        AND x.created_date = logs.created_date 
        AND e.vendor_code = logs.vendor_code -- **IMPORTANT**: Sometimes, the dps logs give us multiple delivery fees per session. One reason for this could be a change in location. We eliminated sessions with multiple DFs in the previous step to keep the dataset clean
    WHERE TRUE
        AND x.entity_id = entity_var
        AND x.created_date BETWEEN start_date AND end_date -- Sessions' start and end date
        AND e.event_action IN ('shop_details.loaded', 'transaction') -- transaction / shop_details.loaded = CVR3
        AND e.vendor_code IN (SELECT vendor_code FROM `dh-logistics-product-ops.pricing.loved_brands_apac_all_vendors_after_session_order_cvr_filters_pkk`) -- Filter for relevant DPS sessions ONLY (i.e., those that belong to vendor IDs that were selected in the first code section)
)

SELECT * FROM ga_dps_sessions 
ORDER BY events_ga_session_id, event_time;

-------------------------------------------------------------------------END OF THE GA SESSIONS + DPS LOGS PART-------------------------------------------------------------------------

-- Now, calculate the conversion rate per DF bucket for each vendor
CREATE OR REPLACE TABLE `dh-logistics-product-ops.pricing.loved_brands_apac_cvr_per_df_bucket_pkk` AS
SELECT
    ven.entity_id,
    ven.city_name,
    ven.vendor_code,
    ses.DF_total,
    COALESCE(COUNT(DISTINCT CASE WHEN ses.event_action = 'shop_details.loaded' THEN ses.events_ga_session_id ELSE NULL END), 0) AS Num_Unique_Vendor_Visits, -- If a vendor was visited more than once in the same session, this is considered one visit
    COALESCE(COUNT(DISTINCT CASE WHEN ses.event_action = 'shop_details.loaded' THEN ses.event_time ELSE NULL END), 0) AS Num_Total_Vendor_Impressions, -- If a vendor was visited more than once in the same session, all impressions are counted
    COALESCE(COUNT(DISTINCT CASE WHEN ses.event_action = 'transaction' THEN ses.event_time ELSE NULL END), 0) AS Num_Transactions,
    COALESCE(ROUND(COUNT(DISTINCT CASE WHEN ses.event_action = 'transaction' THEN ses.event_time ELSE NULL END) / NULLIF(COUNT(DISTINCT CASE WHEN ses.event_action = 'shop_details.loaded' THEN ses.events_ga_session_id ELSE NULL END), 0), 3), 0) AS CVR3
FROM `dh-logistics-product-ops.pricing.loved_brands_apac_all_vendors_after_session_order_cvr_filters_pkk` ven
LEFT JOIN `dh-logistics-product-ops.pricing.loved_brands_apac_ga_dps_sessions_pkk` ses ON ven.entity_id = ses.entity_id AND ven.vendor_code = ses.vendor_code -- No need to add a city_name condition because we are working with one single city at a time
WHERE ses.DF_total IS NOT NULL -- Remove DPS sessions that do not return a DF value because any such record would be meaningless
GROUP BY 1,2,3,4;

-- Add the minimum and maximum DF tiers. These are used to enrich the declared variables that you used in the non-optimized script (df_lowest_tier AND highest_main_df)
CREATE OR REPLACE TABLE `dh-logistics-product-ops.pricing.loved_brands_apac_cvr_per_df_bucket_pkk` AS
SELECT 
    a.*,
    b.Max_DF_Scheme,
    b.Min_DF_Scheme
FROM `dh-logistics-product-ops.pricing.loved_brands_apac_cvr_per_df_bucket_pkk` a
LEFT JOIN (
    SELECT
        entity_id,
        city_name,
        MAX(DF_total) AS Max_DF_Scheme, -- One of two choices to determine the DF threshold beyond which data should not be considered for calculating whether or not a particular vendor passes the LB test 
        MIN(DF_total) AS Min_DF_Scheme -- The replacement of df_lowest_tier in the non-optimized script
    FROM `dh-logistics-product-ops.pricing.loved_brands_apac_cvr_per_df_bucket_pkk`
    GROUP BY 1,2
) b USING (entity_id, city_name)
ORDER BY 1,2,3,4;

##-------------------------------------------------------------------------END OF THE CVR3 PER DF BUCKET FOR EACH VENDOR PART-------------------------------------------------------------------------##

-- LB TEST

-- Instead of specifying CVR3 drop thresholds that cannot be exceeded in a manual manner, we will calculate the overall CVR3 per DF bucket and use the percentage changes from the lowest DF tier to each subsequent one as our thresholds. We normally go with the "overall" because the median can sometimes be zero
-- Firstly, calculate the overall and median CVR3 per DF tier/bucket
CREATE OR REPLACE TABLE `dh-logistics-product-ops.pricing.loved_brands_apac_cvr_per_df_bucket_plus_cvr_thresholds_pkk` AS
WITH cvr_per_df_bucket AS (
    SELECT
        entity_id,
        city_name,
        DF_total,
        SUM(Num_Unique_Vendor_Visits) AS Num_Unique_Vendor_Visits,
        SUM(Num_Transactions) AS Num_Transactions,
        ROUND(SUM(Num_Transactions) / SUM(Num_Unique_Vendor_Visits), 3) AS Overall_cvr3_per_df,
    FROM `dh-logistics-product-ops.pricing.loved_brands_apac_cvr_per_df_bucket_pkk`
    WHERE DF_total IN (SELECT fee FROM `dh-logistics-product-ops.pricing.loved_brands_apac_df_tiers_pkk`) -- Filter for the main DF thresholds ONLY
    GROUP BY 1,2,3
),

-- Secondly, calculate the overall and median CVR3 at the smallest DF so that we can calculate the percentage drop in CVR3 from that base 
cvr3_at_min_df_tbl AS (
    SELECT
        a.entity_id,
        a.city_name,
        a.Overall_cvr3_per_df AS Overall_cvr3_at_min_df, 
    FROM cvr_per_df_bucket a
    INNER JOIN (
        SELECT 
            entity_id,
            city_name,
            MIN(DF_total) AS Min_DF_total 
        FROM cvr_per_df_bucket
        GROUP BY 1,2
    ) b ON a.entity_id = b.entity_id AND a.city_name = b.city_name AND a.DF_total = b.Min_DF_total
),

-- Thirdly, calculate the percentage drop in CVR3 from the base calculated in the previous step
cvr3_chngs AS (
    SELECT 
        *, 
        ROUND(Overall_cvr3_per_df / Overall_cvr3_at_min_df - 1, 3) AS Pct_chng_of_overall_cvr3_from_base
    FROM cvr_per_df_bucket a 
    LEFT JOIN cvr3_at_min_df_tbl b USING(entity_id, city_name)
    WHERE Overall_cvr3_per_df != 0 -- Filter our DF tiers that have a zero overall CVR3 per DF tier
),

cvr_thresholds AS (
    SELECT
        a.*,
        b.CVR3 AS CVR3_df_lowest_tier, -- CVR3 at the lowest DF tier of the price scheme, not the lowest DF observed in the vendor's sessions
        ROUND(a.CVR3 / NULLIF(b.CVR3, 0) - 1, 3) AS Pct_chng_of_actual_cvr3_from_base, -- The base here being the lowest DF in the price scheme not the lowest DF observed in the vendor's sessions
        
        c.Overall_cvr3_per_df,
        c.Pct_chng_of_overall_cvr3_from_base,
        CASE 
            WHEN a.DF_total = a.Min_DF_Scheme THEN NULL
            ELSE ROUND(b.CVR3 * (1 + c.Pct_chng_of_overall_cvr3_from_base), 3) -- "+" as c.Pct_chng_of_overall_cvr3_from_base is negative 
        END AS Overall_cvr3_threshold
    FROM `dh-logistics-product-ops.pricing.loved_brands_apac_cvr_per_df_bucket_pkk` a
    LEFT JOIN `dh-logistics-product-ops.pricing.loved_brands_apac_cvr_per_df_bucket_pkk` b ON a.entity_id = b.entity_id AND a.city_name = b.city_name AND a.vendor_code = b.vendor_code AND b.DF_total = a.Min_DF_Scheme -- To get the CVR3 at the lowest DF tier of the price scheme
    LEFT JOIN cvr3_chngs c ON a.entity_id = c.entity_id AND a.city_name = c.city_name AND a.DF_total = c.DF_total
    WHERE a.DF_total IN (SELECT fee FROM `dh-logistics-product-ops.pricing.loved_brands_apac_df_tiers_pkk`) -- Filter for the main DF thresholds ONLY
),

cvr_thresholds_with_flags AS (
    SELECT 
        *,
        IF (
            threshold_is_scheme_or_main_df = 'main', -- The user has two choices of how to define the DF threshold (check the comment in the input variable declaration section)
            CASE WHEN CVR3 >= Overall_cvr3_threshold AND Overall_cvr3_threshold IS NOT NULL AND DF_total <= highest_main_df THEN 'Y' ELSE 'N' END,
            CASE WHEN CVR3 >= Overall_cvr3_threshold AND Overall_cvr3_threshold IS NOT NULL AND DF_total <= Max_DF_Scheme THEN 'Y' ELSE 'N' END 
        )  AS Is_test_passed_overall_cvr3, -- Don't consider the lowest DF tier OR DFs higher than "highest_main_df" OR "Max_DF_Scheme" depending on the condition specified at the start of the script
        
        IF (
            threshold_is_scheme_or_main_df = 'main', -- The user has two choices of how to define the DF threshold (check the comment in the input variable declaration section)
            SUM(CASE WHEN CVR3 >= Overall_cvr3_threshold AND Overall_cvr3_threshold IS NOT NULL AND DF_total <= highest_main_df THEN 1 ELSE 0 END) OVER (PARTITION BY entity_id, vendor_code),
            SUM(CASE WHEN CVR3 >= Overall_cvr3_threshold AND Overall_cvr3_threshold IS NOT NULL AND DF_total <= Max_DF_Scheme THEN 1 ELSE 0 END) OVER (PARTITION BY entity_id, vendor_code)
        ) AS Flag_overall_cvr3 -- Don't consider the lowest DF tier OR DFs higher than "highest_main_df"
    FROM cvr_thresholds
)

-- Display the results first
SELECT *
FROM cvr_thresholds_with_flags
ORDER BY 1,2,3,4;

-------------------------------------------------------------------------END OF THE LB TEST PART-------------------------------------------------------------------------

-- Pull all the data associated with the filtered vendors in addition to the share of total orders they constitute in the city
CREATE OR REPLACE TABLE `dh-logistics-product-ops.pricing.loved_brands_apac_final_vendor_list_all_data_pkk` AS
WITH orders_city AS ( -- Orders of ALL OD restaurant vendors in the city
    SELECT
        o.global_entity_id,
        city_name_var AS city_name,
        COUNT(DISTINCT order_id) AS Orders_city
    FROM `fulfillment-dwh-production.curated_data_shared_central_dwh.orders` o
    INNER JOIN `dh-logistics-product-ops.pricing.loved_brands_apac_city_data_pkk` cd ON o.global_entity_id = cd.entity_id AND ST_CONTAINS(cd.zone_shape, ST_GEOGPOINT(o.delivery_location.longitude, o.delivery_location.latitude))
    WHERE TRUE
        AND o.global_entity_id = entity_var
        AND DATE(o.placed_at) BETWEEN start_date AND end_date
        AND o.is_sent -- Successful order
        AND o.is_own_delivery -- Own delivery vendors (no need to filter for restaurant vendors as we already filter for those in the vendors sub-table)
        AND o.is_qcommerce = qcommerce_bool -- restaurant orders  
    GROUP BY 1,2
),

filtered_vendors_cvr_by_df AS (
    SELECT
        entity_id,
        city_name,
        vendor_code,
        Flag_overall_cvr3,
        ARRAY_TO_STRING(ARRAY_AGG(CAST(DF_total AS STRING) ORDER BY DF_total), ', ') AS DFs_seen_in_sessions,
        ARRAY_TO_STRING(ARRAY_AGG(CAST(CVR3 AS STRING) ORDER BY DF_total), ', ') AS Actual_vendor_cvr3_by_df,
        ARRAY_TO_STRING(ARRAY_AGG(CAST(Pct_chng_of_actual_cvr3_from_base AS STRING) ORDER BY DF_total), ', ') AS Pct_chng_of_actual_vendor_cvr3_from_base,
        ARRAY_TO_STRING(ARRAY_AGG(CAST(Overall_cvr3_per_df AS STRING) ORDER BY DF_total), ', ') AS Overall_cvr3_per_df,
        ARRAY_TO_STRING(ARRAY_AGG(CAST(Pct_chng_of_overall_cvr3_from_base AS STRING) ORDER BY DF_total), ', ') AS Pct_chng_of_overall_cvr3_from_base,
        ARRAY_TO_STRING(ARRAY_AGG(CAST(Is_test_passed_overall_cvr3 AS STRING) ORDER BY DF_total), ', ') AS Is_test_passed_overall_cvr3
    FROM `dh-logistics-product-ops.pricing.loved_brands_apac_cvr_per_df_bucket_plus_cvr_thresholds_pkk`
    WHERE Flag_overall_cvr3 > 0 -- Include vendors which passed at least one of the DF_threshold tests
    GROUP BY 1,2,3,4
),

filtered_vendors_all_metrics AS (
    SELECT 
        a.*,
        b.DFs_seen_in_sessions,
        b.Actual_vendor_cvr3_by_df,
        b.Pct_chng_of_actual_vendor_cvr3_from_base,
        b.Overall_cvr3_per_df,
        b.Pct_chng_of_overall_cvr3_from_base,
        b.Is_test_passed_overall_cvr3,
        b.Flag_overall_cvr3,
        c.Orders_city,
        SUM(a.Num_orders) OVER (PARTITION BY a.entity_id, a.city_name) AS Orders_filtered_vendors
    FROM `dh-logistics-product-ops.pricing.loved_brands_apac_all_vendors_after_session_order_cvr_filters_pkk` a
    INNER JOIN filtered_vendors_cvr_by_df b USING(entity_id, city_name, vendor_code) -- Must have an inner join here because "a" contains more vendors than "b"
    INNER JOIN orders_city c ON a.entity_id = c.global_entity_id AND a.city_name = c.city_name
)

SELECT * FROM filtered_vendors_all_metrics 
ORDER BY 1,2;

#############################################################################################################################################################################################################
#####------------------------------------------------------------------------END OF THE CVR3 PER DF BUCKET PROCESS--------------------------------------------------------------------------------------#####
#############################################################################################################################################################################################################

-- Travel time data section
CREATE OR REPLACE TABLE `dh-logistics-product-ops.pricing.loved_brands_apac_entities_pkk` AS
SELECT
    ent.region,
    p.entity_id,
    ent.country_iso,
    ent.country_name,
FROM `fulfillment-dwh-production.cl.entities` ent
LEFT JOIN UNNEST(platforms) p
INNER JOIN (SELECT DISTINCT entity_id FROM `fulfillment-dwh-production.cl.dps_sessions_mapped_to_orders_v2`) dps ON p.entity_id = dps.entity_id 
WHERE TRUE
    AND p.entity_id NOT LIKE 'ODR%' -- Eliminate entities starting with DN_ as they are not part of DPS
    AND p.entity_id NOT LIKE 'DN_%' -- Eliminate entities starting with ODR (on-demand riders)
    AND p.entity_id NOT IN ('FP_DE', 'FP_JP') -- Eliminate JP and DE because they are not DH markets any more
    AND p.entity_id != 'TB_SA' -- Eliminate this incorrect entity_id for Saudi
    AND p.entity_id != 'HS_BH'; -- Eliminate this incorrect entity_id for Bahrain

CREATE OR REPLACE TABLE `dh-logistics-product-ops.pricing.loved_brands_apac_delivery_costs_pkk` AS
--This code block extracts accurate delivery costs in an entire city over a specified time frame
SELECT
    p.entity_id,
    p.city_name,
    p.order_id,
    l.platform_order_code,
    SUM(delivery_costs) delivery_costs_local
FROM `fulfillment-dwh-production.cl.utr_timings` p
LEFT JOIN `fulfillment-dwh-production.cl.orders` l ON p.entity_id = l.entity.id AND p.order_id = l.order_id -- Use the platform_order_code in this table as a bridge to join the order_id from utr_timings to order_id from central_dwh.orders 
WHERE TRUE 
    AND entity_id = entity_var
    AND city_name = city_name_var
    AND p.created_date BETWEEN start_date_tt_adj AND end_date_tt_adj
GROUP BY 1,2,3,4;

CREATE OR REPLACE TABLE `dh-logistics-product-ops.pricing.loved_brands_apac_orders_data_for_threshold_adj_pkk` AS
--This code block aggregates metrics (DF, travel times) on an order level for the "loved brands" ONLY
WITH agg1 AS (
    SELECT 
        od.entity_id,
        od.created_date,
        od.order_placed_at,
        od.order_id,
        od.platform_order_code,
        od.vendor_id,
        od.chain_id,
        od.chain_name, 
        od.vertical_type,
        od.zone_name AS zone_name,
        od.zone_id AS zone_id,
        od.scheme_id,
        od.vendor_price_scheme_type,
        -- If an order had a basket value below MOV (i.e. small order fee was charged), add the small order fee calculated as MOV - GFV to the profit 
        COALESCE(
            pd.delivery_fee_local, 
            IF(od.is_delivery_fee_covered_by_discount = TRUE OR od.is_delivery_fee_covered_by_voucher = TRUE, 0, od.dps_delivery_fee_local)
        ) + od.commission_local + od.joker_vendor_fee_local + COALESCE(od.service_fee, 0) + COALESCE(o.value.mov_customer_fee_local, IF(od.gfv_local < od.dps_minimum_order_value_local, (od.dps_minimum_order_value_local - od.gfv_local), 0)) AS revenue_local,

        COALESCE(
            pd.delivery_fee_local, 
            IF(od.is_delivery_fee_covered_by_discount = TRUE OR od.is_delivery_fee_covered_by_voucher = TRUE, 0, od.dps_delivery_fee_local)
        ) + od.commission_local + od.joker_vendor_fee_local + COALESCE(od.service_fee, 0) + COALESCE(o.value.mov_customer_fee_local, IF(od.gfv_local < od.dps_minimum_order_value_local, (od.dps_minimum_order_value_local - od.gfv_local), 0)) - c.delivery_costs_local AS gross_profit_local,
        
        c.delivery_costs_local,
        od.delivery_fee_local AS delivery_fee_local,
        od.dps_travel_time_fee_local,
        od.dps_surge_fee_local,
        od.dps_delivery_fee_local,
        od.travel_time AS travel_time,
        ROUND(od.travel_time_distance_km, 4) AS travel_time_distance_km,

        -- Special fields
        CASE
            WHEN ent.region IN ('Europe', 'Asia') THEN COALESCE( -- Get the delivery fee data of Pandora countries from Pandata tables
                pd.delivery_fee_local, 
                IF(od.is_delivery_fee_covered_by_discount = TRUE OR od.is_delivery_fee_covered_by_voucher = TRUE, 0, od.dps_delivery_fee_local)
            )
            WHEN ent.region NOT IN ('Europe', 'Asia') THEN (CASE WHEN is_delivery_fee_covered_by_voucher = FALSE AND is_delivery_fee_covered_by_discount = FALSE THEN od.delivery_fee_local ELSE 0 END) -- If the order comes from a non-Pandora country, use delivery_fee_local
        END AS actual_df_paid_by_customer
    FROM `fulfillment-dwh-production.cl.dps_sessions_mapped_to_orders_v2` od
    LEFT JOIN `fulfillment-dwh-production.curated_data_shared_central_dwh.orders` o ON od.entity_id = o.global_entity_id AND od.platform_order_code = o.order_id
    LEFT JOIN `fulfillment-dwh-production.pandata_curated.pd_orders` pd ON od.entity_id = pd.global_entity_id AND od.platform_order_code = pd.code AND od.created_date = pd.created_date_utc -- Contains info on the orders in Pandora countries
    LEFT JOIN `dh-logistics-product-ops.pricing.loved_brands_apac_delivery_costs_pkk` c on od.entity_id = c.entity_id AND od.platform_order_code = c.platform_order_code
    INNER JOIN `dh-logistics-product-ops.pricing.loved_brands_apac_final_vendor_list_all_data_pkk` lov ON od.entity_id = lov.entity_id AND od.vendor_id = lov.vendor_code -- Filter for orders of loved brands ONLY
    INNER JOIN `dh-logistics-product-ops.pricing.loved_brands_apac_entities_pkk` ent ON od.entity_id = ent.entity_id -- INNER JOIN to only include active DH entities
    WHERE TRUE
        AND od.entity_id = entity_var
        AND od.city_name = city_name_var
        AND od.created_date BETWEEN start_date_tt_adj AND end_date_tt_adj
        AND od.is_own_delivery
        AND od.vertical_type IN UNNEST(vertical_var)
        AND o.is_sent -- delivery_status = 'completed' in dps_sessions_mapped_to_orders_v2 won't yield the exact same results
        AND od.vendor_price_scheme_type <> 'Campaigns' -- Exclude campaign orders
        AND od.scheme_id = (SELECT scheme_id FROM `dh-logistics-product-ops.pricing.loved_brands_apac_dominant_price_scheme_pkk` WHERE Order_Share = (SELECT MAX(Order_Share) FROM `dh-logistics-product-ops.pricing.loved_brands_apac_dominant_price_scheme_pkk` WHERE scheme_id IS NOT NULL)) -- Scheme ID with the highest order share in the city. You need this filter to filter only for orders that were priced using the chosen price scheme
)

--This final section joins orders to their respective tiers. Not done in the previous code block due to aggregation & multiple travel times.
SELECT 
    a.*,
    SUM(CASE WHEN travel_time > t.threshold THEN 1 ELSE 0 END) + 1 AS tier
FROM agg1 a
CROSS JOIN `dh-logistics-product-ops.pricing.loved_brands_apac_df_tiers_pkk` t
GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23
ORDER BY 1,2, chain_name, vendor_id, scheme_id, delivery_fee_local, order_placed_at;

#############################################################################################################################################################################################################
#####------------------------------------------------------------------------END OF THE TRAVEL TIME DATA SECTION----------------------------------------------------------------------------------------#####
#############################################################################################################################################################################################################

-- Non-LBs with the same order share as LBs
CREATE OR REPLACE TABLE `dh-logistics-product-ops.pricing.loved_brands_apac_non_lbs_with_same_order_share_as_lbs_pkk` AS 
-- Order the "Loved Brands" from most loved to least loved
WITH lb_rank AS (
    SELECT 
        a.*,
        ROUND(SUM(a.Unique_visits_pct_rank) OVER (PARTITION BY a.entity_id, a.city_name, a.vendor_code) + SUM(a.Orders_pct_rank) OVER (PARTITION BY a.entity_id, a.city_name, a.vendor_code) + SUM(a.CVR3_pct_rank) OVER (PARTITION BY a.entity_id, a.city_name, a.vendor_code), 4) AS Filter_Sum,
        'Yes' AS LB_Flag
    FROM `dh-logistics-product-ops.pricing.loved_brands_apac_all_vendors_pkk` a
    INNER JOIN `dh-logistics-product-ops.pricing.loved_brands_apac_final_vendor_list_all_data_pkk` b ON a.entity_id = b.entity_id AND a.city_name = b.city_name AND a.vendor_code = b.vendor_code -- LBs
),

non_lb_rank AS (
    SELECT 
        *,
        ROUND(SUM(Unique_visits_pct_rank) OVER (PARTITION BY entity_id, city_name, vendor_code) + SUM(Orders_pct_rank) OVER (PARTITION BY entity_id, city_name, vendor_code) + SUM(CVR3_pct_rank) OVER (PARTITION BY entity_id, city_name, vendor_code), 4) AS Filter_Sum,
        'No' AS LB_Flag
    FROM `dh-logistics-product-ops.pricing.loved_brands_apac_all_vendors_pkk`
    WHERE vendor_code NOT IN (SELECT vendor_code FROM `dh-logistics-product-ops.pricing.loved_brands_apac_final_vendor_list_all_data_pkk`) -- Non-LBs
),

union_all_tbl AS (
    SELECT *
    FROM lb_rank a

    UNION ALL

    SELECT *
    FROM non_lb_rank b
)

SELECT 
    *,
    ROUND(SUM(Num_orders) OVER (PARTITION BY LB_Flag ORDER BY Filter_Sum DESC) / b.Orders_city, 4) AS CumSum_Order_Share
FROM union_all_tbl a
LEFT JOIN (SELECT DISTINCT Orders_city FROM `dh-logistics-product-ops.pricing.loved_brands_apac_final_vendor_list_all_data_pkk`) b ON TRUE
ORDER BY LB_Flag DESC, Filter_Sum DESC;

#############################################################################################################################################################################################################
#####------------------------------------------------------------------------END OF THE NON-LOVED BRANDS SECTION----------------------------------------------------------------------------------------#####
#############################################################################################################################################################################################################

-- Sample size calculation based on CVR
CREATE OR REPLACE TABLE `dh-logistics-product-ops.pricing.sample_size_calculation_with_sessions_pkk` AS
WITH cvr_events AS (
    SELECT 
        e.country_code,
        city_name_var AS city_name,
        e.created_date
        , COUNT(DISTINCT e.ga_session_id) AS total_sessions
        , COUNT(DISTINCT e.shop_list_no) AS shop_list_sessions
        , COUNT(DISTINCT e.shop_menu_no) AS shop_menu_sessions
        , COUNT(DISTINCT e.checkout_no) AS checkout_sessions
        , COUNT(DISTINCT e.checkout_transaction) AS checkout_transaction_sessions
        , COUNT(DISTINCT e.transaction_no) AS transactions
        , COUNT(DISTINCT e.perseus_client_id) AS users
    FROM `fulfillment-dwh-production.cl.dps_cvr_events` e
    CROSS JOIN UNNEST(e.vendor_code) ven
    WHERE TRUE 
        AND e.entity_id = entity_var -- HERE
        AND e.created_date BETWEEN start_date_tt_adj AND end_date_tt_adj -- HERE
        AND (e.dps_city_name LIKE CONCAT('%', city_name_var, '%')) -- HERE
        AND ven IN (SELECT DISTINCT vendor_code FROM `dh-logistics-product-ops.pricing.loved_brands_apac_final_vendor_list_all_data_pkk`) -- HERE: Treatment scope vendors (replace with your own table)
    GROUP BY 1,2,3
)

SELECT 
    city_name,
    COUNT(DISTINCT created_date) AS Count_days,
    SUM (users) AS users,
    SUM (total_sessions) AS total_sessions,
    SUM (shop_list_sessions) AS shop_list_sessions,
    SUM (shop_menu_sessions) AS shop_menu_sessions,
    SUM (checkout_sessions) AS checkout_sessions,
    SUM (checkout_transaction_sessions) AS checkout_transaction_sessions,
    SUM (transactions) AS transactions,
    ROUND(SUM (transactions) / SUM (total_sessions), 4) AS CVR1,
    ROUND(SUM (transactions) / SUM (shop_menu_sessions), 4) AS CVR3,
    ROUND(SUM (checkout_transaction_sessions) / SUM (checkout_sessions), 4) AS mCVR4_prime
FROM cvr_events e
GROUP BY 1; 

-- There are two ways of getting the CVR data on the level of your treatment scope 
-- Method 1: Get the sessions on the city level as a whole (i.e. across all verticals and delivery types, then we multiply by the **order share** of our treatment scope assuming that CVR is a constant multiplication factor)
-- Method 2 (the method we follow here because it is more rigorous): Filter for the sessions coming from the vendors under your target group as shown in the cvr_events query

-- Sample size calculation based on GPO
CREATE OR REPLACE TABLE `dh-logistics-product-ops.pricing.sample_size_calculation_with_orders_pkk` AS
WITH orders_raw_data AS (
    SELECT
        od.entity_id,
        city_name_var AS city_name,
        od.created_date,
        od.order_id,
        COALESCE(
            pd.delivery_fee_local, 
            IF(od.is_delivery_fee_covered_by_discount = TRUE OR od.is_delivery_fee_covered_by_voucher = TRUE, 0, od.dps_delivery_fee_local)
        ) + od.commission_local + od.joker_vendor_fee_local + COALESCE(od.service_fee, 0) + COALESCE(o.value.mov_customer_fee_local, IF(od.gfv_local < od.dps_minimum_order_value_local, (od.dps_minimum_order_value_local - od.gfv_local), 0)) - cst.delivery_costs_local AS gross_profit_local,
    FROM `fulfillment-dwh-production.cl.dps_sessions_mapped_to_orders_v2` od
    LEFT JOIN `fulfillment-dwh-production.curated_data_shared_central_dwh.orders` o ON od.entity_id = o.global_entity_id AND od.platform_order_code = o.order_id -- We use this table so that we can join on city_data
    LEFT JOIN `fulfillment-dwh-production.pandata_curated.pd_orders` pd ON od.entity_id = pd.global_entity_id AND od.platform_order_code = pd.code AND od.created_date = pd.created_date_utc -- Contains info on the orders in Pandora countries
    INNER JOIN `dh-logistics-product-ops.pricing.loved_brands_apac_city_data_pkk` cd ON o.global_entity_id = cd.entity_id AND ST_CONTAINS(cd.zone_shape, ST_GEOGPOINT(delivery_location.longitude,delivery_location.latitude)) -- Filter for delivery locations in the city of choice
    INNER JOIN `dh-logistics-product-ops.pricing.loved_brands_apac_final_vendor_list_all_data_pkk` lb ON od.entity_id = lb.entity_id AND od.vendor_id = lb.vendor_code -- HERE: Treatment scope vendors (replace with your own table)
    LEFT JOIN `dh-logistics-product-ops.pricing.loved_brands_apac_delivery_costs_pkk` cst ON od.entity_id = cst.entity_id AND od.order_id = cst.order_id -- The table that stores the cost per order
    WHERE TRUE
        AND od.entity_id = entity_var -- HERE
        AND od.created_date BETWEEN start_date_tt_adj AND end_date_tt_adj -- HERE
        AND is_sent -- Successful order
)

SELECT
    entity_id,
    city_name,
    COUNT(DISTINCT created_date) AS Count_days,
    COUNT(DISTINCT order_id) AS Total_orders,
    ROUND(SUM(gross_profit_local), 2) AS Total_GP,
    ROUND(SUM(gross_profit_local) / COUNT(DISTINCT order_id), 2) AS GPO,
    ROUND(b.GPO_std_dev, 3) AS GPO_std_dev,
    ROUND(COUNT(DISTINCT order_id) / COUNT(DISTINCT created_date), 0) AS Avg_daily_orders,
FROM orders_raw_data a
LEFT JOIN (SELECT STDDEV_SAMP(gross_profit_local) AS GPO_std_dev FROM orders_raw_data) b ON TRUE -- To calculate std dev of GPO (used as input to the inference for means calculator)
GROUP BY 1,2,b.GPO_std_dev
ORDER BY 1,2;

#############################################################################################################################################################################################################
#####------------------------------------------------------------------------END OF SAMPLE SIZE CALCULATION SECTION-------------------------------------------------------------------------------------#####
#############################################################################################################################################################################################################

-- Values of the infographic
CREATE OR REPLACE TABLE `dh-logistics-product-ops.pricing.loved_brands_apac_infographic_pkk` AS
SELECT 
    SUM(Num_orders) AS Total_orders,
    COUNT(DISTINCT vendor_code) AS Total_vendors,
    ROUND(SUM(Num_orders) / AVG(b.Orders_city), 4) AS Total_order_share,

    -- After filtering for vendors with the top 25% of visits
    SUM(CASE WHEN Unique_visits_pct_rank >= 0.75 THEN Num_orders ELSE NULL END) AS Orders_top25_visits,
    COUNT(DISTINCT CASE WHEN Unique_visits_pct_rank >= 0.75 THEN vendor_code ELSE NULL END) AS Vendors_top25_visits,
    ROUND(SUM(CASE WHEN Unique_visits_pct_rank >= 0.75 THEN Num_orders ELSE NULL END) / AVG(b.Orders_city), 4) AS Order_share_top25_visits,

    -- After filtering for vendors with the top 25% of vists AND orders
    SUM(CASE WHEN Unique_visits_pct_rank >= 0.75 AND Orders_pct_rank >= 0.75 THEN Num_orders ELSE NULL END) AS Orders_top25_visits_and_orders,
    COUNT(DISTINCT CASE WHEN Unique_visits_pct_rank >= 0.75 AND Orders_pct_rank >= 0.75	THEN vendor_code ELSE NULL END) AS Vendors_top25_visits_and_orders,
    ROUND(SUM(CASE WHEN Unique_visits_pct_rank >= 0.75 AND Orders_pct_rank >= 0.75 THEN Num_orders ELSE NULL END) / AVG(b.Orders_city), 4) AS Order_share_visits_and_orders,

    -- After filtering for vendors with the top 25% of vists AND orders AND CVR3
    SUM(CASE WHEN Unique_visits_pct_rank >= 0.75 AND Orders_pct_rank >= 0.75 AND CVR3_pct_rank >= 0.75 THEN Num_orders ELSE NULL END) AS Orders_top25_visits_orders_and_cvr3,
    COUNT(DISTINCT CASE WHEN Unique_visits_pct_rank >= 0.75 AND Orders_pct_rank >= 0.75 AND CVR3_pct_rank >= 0.75 THEN vendor_code ELSE NULL END) AS Vendors_top25_visits_orders_and_cvr3,
    ROUND(SUM(CASE WHEN Unique_visits_pct_rank >= 0.75 AND Orders_pct_rank >= 0.75 AND CVR3_pct_rank >= 0.75 THEN Num_orders ELSE NULL END) / AVG(b.Orders_city), 4) AS Order_share_visits_orders_and_cvr3,

    -- LBs
    AVG(c.Orders_LBs) AS Orders_LBs,
    AVG(c.Vendors_LBs) AS Vendors_LBs,
    ROUND(AVG(c.Order_share_LBs), 4) AS Order_share_LBs
FROM `dh-logistics-product-ops.pricing.loved_brands_apac_all_vendors_pkk` a
LEFT JOIN (SELECT DISTINCT Orders_city FROM `dh-logistics-product-ops.pricing.loved_brands_apac_final_vendor_list_all_data_pkk`) b ON TRUE
LEFT JOIN (
    SELECT 
        SUM(Num_orders) AS Orders_LBs,
        COUNT(DISTINCT vendor_code) AS Vendors_LBs,
        SUM(Num_orders) / AVG(Orders_city) AS Order_share_LBs,
FROM `dh-logistics-product-ops.pricing.loved_brands_apac_final_vendor_list_all_data_pkk`
) c ON TRUE
