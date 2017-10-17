setwd("/Users/apple/Documents/expedia/data")
source("/Users/apple/Documents/expedia/utility.R")
library(ggplot2)
library(gridExtra)
library(data.table)
library(cvTools)
library(gbm)
library(sqldf)

load("expedia.train.RData")

############################ convert variable to proper type for search info variables #########################
expedia.train$srch_id = as.integer(as.character(expedia.train$srch_id))
expedia.train$site_id = as.integer(as.character(expedia.train$site_id))
expedia.train$date_time = as.character(expedia.train$date_time)
expedia.train$srch_destination_id = as.integer(as.character(expedia.train$srch_destination_id))
expedia.train$srch_length_of_stay = as.integer(as.character(expedia.train$srch_length_of_stay))
expedia.train$srch_booking_window = as.integer(as.character(expedia.train$srch_booking_window))
expedia.train$srch_adults_count = as.integer(as.character(expedia.train$srch_adults_count))
expedia.train$srch_children_count = as.integer(as.character(expedia.train$srch_children_count))
expedia.train$srch_room_count = as.integer(as.character(expedia.train$srch_room_count))
expedia.train$srch_saturday_night_bool = as.integer(expedia.train$srch_saturday_night_bool)

expedia.train$prop_id = as.integer(expedia.train$prop_id)
expedia.train$prop_country_id = as.integer(as.character(expedia.train$prop_country_id))
expedia.train$prop_starrating = as.integer(as.character(expedia.train$prop_starrating))
expedia.train$prop_review_score = as.numeric(as.character(expedia.train$prop_review_score))
expedia.train$prop_brand_bool = as.integer(expedia.train$prop_brand_bool)
expedia.train$prop_location_score1 = as.numeric(as.character(expedia.train$prop_location_score1))
expedia.train$prop_log_historical_price = as.numeric(as.character(expedia.train$prop_log_historical_price))

expedia.train$position = as.integer(as.character(expedia.train$position))
expedia.train$price_usd = as.numeric(as.character(expedia.train$price_usd))
expedia.train$promotion_flag = as.integer(expedia.train$promotion_flag)
expedia.train$click_bool = as.integer(expedia.train$click_bool)
expedia.train$booking_bool = as.integer(expedia.train$booking_bool)
expedia.train$random_bool = as.integer(expedia.train$random_bool)
expedia.train$visitor_location_country_id = as.integer(as.character(expedia.train$visitor_location_country_id))

######################################### data preprocessing  ####################################
# remove gross_booking_usd
expedia.train = expedia.train[, -which(colnames(expedia.train) %in% c("gross_bookings_usd"))]

# data imputation for prop_location_score2, prop_review_score, orig_destination_distance, srch_query_affinity_score
expedia.train$prop_location_score2 = as.numeric(as.character(expedia.train$prop_location_score2))
missing_index = which(is.na(expedia.train$prop_location_score2))
expedia.train$prop_location_score2[missing_index] = quantile(expedia.train[-missing_index, "prop_location_score2"], 0.25)

expedia.train$orig_destination_distance = as.numeric(as.character(expedia.train$orig_destination_distance))
missing_index = which(is.na(expedia.train$orig_destination_distance))
expedia.train$orig_destination_distance[missing_index] = quantile(expedia.train[-missing_index, "orig_destination_distance"], 0.25)

expedia.train$prop_review_score = as.numeric(as.character(expedia.train$prop_review_score))
missing_index = which(is.na(expedia.train$prop_review_score))
expedia.train$prop_review_score[missing_index] = quantile(expedia.train[-missing_index, "prop_review_score"], 0.25)

expedia.train$srch_query_affinity_score = as.numeric(as.character(expedia.train$srch_query_affinity_score))
missing_index = which(is.na(expedia.train$srch_query_affinity_score))
expedia.train$srch_query_affinity_score[missing_index] = quantile(expedia.train[-missing_index, "srch_query_affinity_score"], 0.25)

# data imputation for visitor_hist_starrating, visitor_hist_adr_usd
expedia.train$visitor_hist_starrating = as.numeric(as.character(expedia.train$visitor_hist_starrating))
missing_index = which(is.na(expedia.train$visitor_hist_starrating))
expedia.train$visitor_hist_starrating[missing_index] = round(expedia.train$prop_starrating[missing_index] + runif(1, min=-1, max=1)*1.5)
expedia.train$visitor_hist_starrating[which(expedia.train$visitor_hist_starrating<0)] = 0
expedia.train$visitor_hist_starrating[which(expedia.train$visitor_hist_starrating>5)] = 5

expedia.train$visitor_hist_adr_usd = as.numeric(as.character(expedia.train$visitor_hist_adr_usd))
missing_index = which(is.na(expedia.train$visitor_hist_adr_usd))
expedia.train$visitor_hist_adr_usd[missing_index] = expedia.train$price_usd[missing_index] + runif(1, min=-1, max=1)

# data imputation for comp_rate, comp_inv, comp_rate_percent_diff
expedia.train$comp1_inv = as.integer(as.character(expedia.train$comp1_inv))
missing_index = which(is.na(expedia.train$comp1_inv))
expedia.train$comp1_inv[missing_index] = 0
expedia.train$comp1_rate = as.integer(as.character(expedia.train$comp1_rate))
missing_index = which(is.na(expedia.train$comp1_rate))
expedia.train$comp1_rate[missing_index] = 0
expedia.train$comp1_rate_percent_diff = as.numeric(as.character(expedia.train$comp1_rate_percent_diff))
missing_index = which(is.na(expedia.train$comp1_rate_percent_diff))
expedia.train$comp1_rate_percent_diff[missing_index] = 0

expedia.train$comp2_inv = as.integer(as.character(expedia.train$comp2_inv))
missing_index = which(is.na(expedia.train$comp2_inv))
expedia.train$comp2_inv[missing_index] = 0
expedia.train$comp2_rate = as.integer(as.character(expedia.train$comp2_rate))
missing_index = which(is.na(expedia.train$comp2_rate))
expedia.train$comp2_rate[missing_index] = 0
expedia.train$comp2_rate_percent_diff = as.numeric(as.character(expedia.train$comp2_rate_percent_diff))
missing_index = which(is.na(expedia.train$comp2_rate_percent_diff))
expedia.train$comp2_rate_percent_diff[missing_index] = 0

expedia.train$comp3_inv = as.integer(as.character(expedia.train$comp3_inv))
missing_index = which(is.na(expedia.train$comp3_inv))
expedia.train$comp3_inv[missing_index] = 0
expedia.train$comp3_rate = as.integer(as.character(expedia.train$comp3_rate))
missing_index = which(is.na(expedia.train$comp3_rate))
expedia.train$comp3_rate[missing_index] = 0
expedia.train$comp3_rate_percent_diff = as.numeric(as.character(expedia.train$comp3_rate_percent_diff))
missing_index = which(is.na(expedia.train$comp3_rate_percent_diff))
expedia.train$comp3_rate_percent_diff[missing_index] = 0

expedia.train$comp4_inv = as.integer(as.character(expedia.train$comp4_inv))
missing_index = which(is.na(expedia.train$comp4_inv))
expedia.train$comp4_inv[missing_index] = 0
expedia.train$comp4_rate = as.integer(as.character(expedia.train$comp4_rate))
missing_index = which(is.na(expedia.train$comp4_rate))
expedia.train$comp4_rate[missing_index] = 0
expedia.train$comp4_rate_percent_diff = as.numeric(as.character(expedia.train$comp4_rate_percent_diff))
missing_index = which(is.na(expedia.train$comp4_rate_percent_diff))
expedia.train$comp4_rate_percent_diff[missing_index] = 0

expedia.train$comp5_inv = as.integer(as.character(expedia.train$comp5_inv))
missing_index = which(is.na(expedia.train$comp5_inv))
expedia.train$comp5_inv[missing_index] = 0
expedia.train$comp5_rate = as.integer(as.character(expedia.train$comp5_rate))
missing_index = which(is.na(expedia.train$comp5_rate))
expedia.train$comp5_rate[missing_index] = 0
expedia.train$comp5_rate_percent_diff = as.numeric(as.character(expedia.train$comp5_rate_percent_diff))
missing_index = which(is.na(expedia.train$comp5_rate_percent_diff))
expedia.train$comp5_rate_percent_diff[missing_index] = 0

expedia.train$comp6_inv = as.integer(as.character(expedia.train$comp6_inv))
missing_index = which(is.na(expedia.train$comp6_inv))
expedia.train$comp6_inv[missing_index] = 0
expedia.train$comp6_rate = as.integer(as.character(expedia.train$comp6_rate))
missing_index = which(is.na(expedia.train$comp6_rate))
expedia.train$comp6_rate[missing_index] = 0
expedia.train$comp6_rate_percent_diff = as.numeric(as.character(expedia.train$comp6_rate_percent_diff))
missing_index = which(is.na(expedia.train$comp6_rate_percent_diff))
expedia.train$comp6_rate_percent_diff[missing_index] = 0

expedia.train$comp7_inv = as.integer(as.character(expedia.train$comp7_inv))
missing_index = which(is.na(expedia.train$comp7_inv))
expedia.train$comp7_inv[missing_index] = 0
expedia.train$comp7_rate = as.integer(as.character(expedia.train$comp7_rate))
missing_index = which(is.na(expedia.train$comp7_rate))
expedia.train$comp7_rate[missing_index] = 0
expedia.train$comp7_rate_percent_diff = as.numeric(as.character(expedia.train$comp7_rate_percent_diff))
missing_index = which(is.na(expedia.train$comp7_rate_percent_diff))
expedia.train$comp7_rate_percent_diff[missing_index] = 0

expedia.train$comp8_inv = as.integer(as.character(expedia.train$comp8_inv))
missing_index = which(is.na(expedia.train$comp8_inv))
expedia.train$comp8_inv[missing_index] = 0
expedia.train$comp8_rate = as.integer(as.character(expedia.train$comp8_rate))
missing_index = which(is.na(expedia.train$comp8_rate))
expedia.train$comp8_rate[missing_index] = 0
expedia.train$comp8_rate_percent_diff = as.numeric(as.character(expedia.train$comp8_rate_percent_diff))
missing_index = which(is.na(expedia.train$comp8_rate_percent_diff))
expedia.train$comp8_rate_percent_diff[missing_index] = 0

# bounding numeric variable (to mitigate outlier influence)
bound.var.list = c("prop_starrating", "prop_review_score", "prop_location_score1", "prop_location_score2", 
                   "prop_log_historical_price", "price_usd", "orig_destination_distance")

for(i in 1:length(bound.var.list)) {
  var.upper = sort(expedia.train[, bound.var.list[i]])[as.integer(0.95*dim(expedia.train)[1])]
  bound.index = which(expedia.train[, bound.var.list[i]] > var.upper)
  expedia.train[, bound.var.list[i]][bound.index] = var.upper
}

# variable data missing rate calcuation 
l = length(colnames(expedia.train))
col.type = data.frame(name=rep("A", l), type=rep("A",  l))
col.type$name = as.character(col.type$name)
col.type$type = as.character(col.type$type)

for(i in 1:l) {
  col.type[i, 1] = colnames(expedia.train)[i]
  col.type[i, 2] = class(expedia.train[, i])[1]
}

#################### sample 10% of data based on search id #################
unique.srchid.list = unique(expedia.train$srch_id)
cv.index = cvFolds(length(unique.srchid.list), K=20, type="random")
selected.srchid = unique.srchid.list[cv.index$subsets[which(cv.index$which==20)]]
sample_train = expedia.train[which(expedia.train$srch_id %in% selected.srchid), ]

############## normalize on price_usd, prop_starrating, prop_location_score1, prop_location_score2 #############
sample_train$price_usd = log(expedia.train$price_usd, 10)      # take log of hotel price          
normalized_mat = sample_train[, c("srch_id", "prop_id", "srch_destination_id", "price_usd", "prop_starrating",
                                  "prop_location_score1", "prop_location_score2", "click_bool", "booking_bool")]
normalized_mat$nor_srch_price = 0
normalized_mat$nor_prop_price = 0
normalized_mat$nor_dest_price = 0
normalized_mat$nor_srch_starrating = 0
normalized_mat$nor_prop_starrating = 0
normalized_mat$nor_dest_starrating = 0
normalized_mat$nor_srch_loc_score1 = 0
normalized_mat$nor_prop_loc_score1 = 0
normalized_mat$nor_dest_loc_score1 = 0
normalized_mat$nor_srch_loc_score2 = 0
normalized_mat$nor_prop_loc_score2 = 0
normalized_mat$nor_dest_loc_score2 = 0

normalized_mat = as.matrix(normalized_mat)

all_srch_list = as.vector(sample_train$srch_id)
all_prop_list = as.vector(sample_train$prop_id)
all_dest_list = as.vector(sample_train$srch_destination_id)
srch_list = as.vector(unique(sample_train$srch_id))
prop_list = as.vector(unique(sample_train$prop_id))
dest_list = as.vector(unique(sample_train$srch_destination_id))

for(i in 1:length(srch_list)) {
  srch_index = which(all_srch_list == srch_list[i])
  normalized_mat[srch_index, "nor_srch_price"] = scale(normalized_mat[srch_index, "price_usd"])
  normalized_mat[srch_index, "nor_srch_starrating"] = scale(normalized_mat[srch_index, "prop_starrating"])
  normalized_mat[srch_index, "nor_srch_loc_score1"] = scale(normalized_mat[srch_index, "prop_location_score1"])
  normalzied_mat[srch_index, "nor_srch_loc_score2"] = scale(normalized_mat[srch_index, "prop_location_score2"])
  if(i%%200==0) cat("1 pct")
}

for(i in 1:length(prop_list)) {
  prop_index = which(all_prop_list == prop_list[i])
  normalized_mat[prop_index, "nor_prop_price"] = scale(normalized_mat[prop_index, "price_usd"])
  normalized_mat[prop_index, "nor_prop_starrating"] = scale(normalized_mat[prop_index, "prop_starrating"])
  normalized_mat[prop_index, "nor_prop_loc_score1"] = scale(normalized_mat[prop_index, "prop_location_score1"])
  normalized_mat[prop_index, "nor_prop_loc_score2"] = scale(normalized_mat[prop_index, "prop_location_score2"])
  if(i%%200==0) cat("1 pct")
}

for(i in 1:length(dest_list)) {
  dest_index = which(all_dest_list == dest_list[i])
  normalized_mat[dest_index, "nor_dest_price"] = scale(normalized_mat[dest_index, "price_usd"])
  normalized_mat[dest_index, "nor_dest_starrating"] = scale(normalized_mat[dest_index, "prop_starrating"])
  normalized_mat[dest_index, "nor_dest_loc_score1"] = scale(normalized_mat[dest_index, "prop_location_score1"])
  normalized_mat[dest_index, "nor_dest_loc_score2"] = scale(normalized_mat[dest_index, "prop_location_score2"])
  if(i%%200==0) cat("1 pct")
}

sample_train$nor_srch_price = normalized_mat[, "nor_srch_price"]
sample_train$nor_prop_price = normalized_mat[, "nor_prop_price"]
sample_train$nor_dest_price = normalzied_mat[, "nor_dest_price"]
sample_train$nor_srch_starrating = normalized_mat[, "nor_srch_starrating"]
sample_train$nor_prop_starrating = normalized_mat[, "nor_prop_starrating"]
sample_train$nor_dest_starrating = normalzied_mat[, "nor_dest_starrating"]
sample_train$nor_srch_loc_score1 = normalized_mat[, "nor_srch_loc_score1"]
sample_train$nor_prop_loc_score1 = normalized_mat[, "nor_prop_loc_score1"]
sample_train$nor_dest_loc_score1 = normalized_mat[, "nor_dest_loc_score1"]
sample_train$nor_srch_loc_score2 = normalized_mat[, "nor_srch_loc_score2"]
sample_train$nor_prop_loc_score2 = normalized_mat[, "nor_prop_loc_score2"]
sample_train$nor_dest_loc_score2 = normalized_mat[, "nor_dest_loc_score2"]

######################### average over srch_id / prop_id / destination_id for numeric variable ########################
numeric_var_list = c("prop_starrating", "prop_review_score", "prop_location_score1", "prop_location_score2",
                 "prop_log_historical_price", "price_usd", "srch_length_of_stay", "srch_booking_window",
                 "srch_adults_count", "srch_children_count", "srch_room_count", "orig_destination_distance")

sample_numeric_part_mat = as.matrix(sample_train[, c("srch_id", "prop_id", "srch_destination_id", numeric_var_list)])
srchid_avg_mat = matrix(0, nrow = dim(sample_train)[1], ncol = length(numeric_var_list))
propid_avg_mat = matrix(0, nrow = dim(sample_train)[1], ncol = length(numeric_var_list))
propid_std_mat = matrix(0, nrow = dim(sample_train)[1], ncol = length(numeric_var_list))
destid_avg_mat = matrix(0, nrow = dim(sample_train)[1], ncol = length(numeric_var_list))

for(i in 1:length(numeric_var_list)) {
  selected_col = as.vector(sample_numeric_part_mat[, numeric_var_list[i]])
  
  sample_srchid_list = as.vector(sample_numeric_part_mat[, "srch_id"])
  unique_sample_srch_list = unique(sample_srchid_list)
  
  sample_propid_list = as.vector(sample_numeric_part_mat[, "prop_id"])
  unique_sample_prop_list = unique(sample_propid_list)
  
  sample_destid_list = as.vector(sample_numeric_part_mat[, "srch_destination_id"])
  unique_sample_dest_list = unique(sample_destid_list)
  
  for(j in 1:length(unique_sample_srch_list)) {
    single_srch_index = which(sample_srchid_list == unique_sample_srch_list[j])
    srchid_avg_mat[single_srch_index, i] = mean(selected_col[single_srch_index]) 
  }
  
  for(k in 1:length(unique_sample_prop_list)) {
    single_prop_index = which(sample_propid_list == unique_sample_prop_list[k])
    propid_avg_mat[single_prop_index, i] = mean(selected_col[single_prop_index])
    propid_std_mat[single_prop_index, i] = sd(selected_col[single_prop_index])
  }
  
  for(l in 1:length(unique_sample_dest_list)) {
    single_dest_index = which(sample_destid_list == unique_sample_dest_list[l])
    destid_avg_mat[single_dest_index, i] = mean(selected_col[single_dest_index])
  }

  cat(numeric_var_list[i], " ")
}

for(i in 1:length(numeric_var_list)) {
  sample_train = cbind(sample_train, srchid_avg_mat[, i])
  setnames(sample_train, dim(sample_train)[2], paste("srch_avg", numeric_var_list[i], sep = "_"))
}

for(i in 1:length(numeric_var_list)) {
  sample_train = cbind(sample_train, propid_avg_mat[, i])
  setnames(sample_train, dim(sample_train)[2], paste("prop_avg", numeric_var_list[i], sep = "_"))
}

for(i in 1:length(numeric_var_list)) {
  sample_train = cbind(sample_train, propid_std_mat[, i])
  setnames(sample_train, dim(sample_train)[2], paste("prop_std", numeric_var_list[i], sep = "_"))
}

for(i in 1:length(numeric_var_list)) {
  sample_train = cbind(sample_train, destid_avg_mat[, i])
  setnames(sample_train, dim(sample_train)[2], paste("dest_avg", numeric_var_list[i], sep = "_"))
}

#save(avg.srch.matrix, file = "avg_srch_matrix.RData")
#save(avg.prop.matrix, file = "avg_prop_matrix.RData")

########################### composite feature #########################
sample_train$price_diff_btw_current_and_historical = sample_train$price_usd - sample_train$prop_log_historical_price

sample_train$starrating_prop_diff = 0
sample_train$starrating_srch_diff = 0
sample_train$starrating_dest_diff = 0
sample_train$review_score_prop_diff = 0
sample_train$review_score_srch_diff = 0
sample_train$review_score_dest_diff = 0
sample_train$loc_score1_prop_diff = 0
sample_train$loc_score1_srch_diff = 0
sample_train$loc_score1_dest_diff = 0
sample_train$loc_score2_prop_diff = 0
sample_train$loc_score2_srch_diff = 0
sample_train$loc_score2_dest_diff = 0
sample_train$starrating_prop_diff = sample_train$prop_avg_prop_starrating - sample_train$prop_starrating
sample_train$starrating_srch_diff = sample_train$srch_avg_prop_starrating - sample_train$prop_starrating
sample_train$starrating_dest_diff = sample_train$dest_avg_prop_starrating - sample_train$prop_starrating
sample_train$review_score_prop_diff = sample_train$prop_avg_prop_review_score - sample_train$prop_review_score
sample_train$review_score_srch_diff = sample_train$srch_avg_prop_review_score - sample_train$prop_review_score
sample_train$review_score_dest_diff = sample_train$dest_avg_prop_review_score - sample_train$prop_review_score
sample_train$loc_score1_prop_diff = sample_train$prop_avg_prop_location_score1 - sample_train$prop_location_score1
sample_train$loc_score1_srch_diff = sample_train$prop_avg_srch_location_score1 - sample_train$srch_location_score1
sample_train$loc_score1_dest_diff = sample_train$prop_avg_dest_location_score1 - sample_train$dest_location_score1
sample_train$loc_score2_prop_diff = sample_train$prop_avg_prop_location_score2 - sample_train$prop_location_score2
sample_train$loc_score2_srch_diff = sample_train$prop_avg_srch_location_score2 - sample_train$srch_location_score2
sample_train$loc_score2_dest_diff = sample_train$prop_avg_dest_location_score2 - sample_train$dest_location_score2

###################################### listwise rank feature ###################################
# hotel attributes rank within each search 
sample_train$price_order_within_srch = rep(0, dim(sample_train)[1])
sample_train$hist_price_order_within_srch = rep(0, dim(sample_train)[1])
sample_train$starrating_order_within_srch = 0
sample_train$review_score_order_within_srch = 0
sample_train$loc_score1_order = 0
sample_train$loc_score2_order = 0

search_list = unique(sample_train$srch_id)

for(i in 1:length(search_list)) {
  single_search_index = which(sample_train$srch_id == search_list[i])
  sample_train$price_order_within_srch[single_search_index] = 
                        rank(sample_train$price_usd[single_search_index], ties.method = "random")
  sample_train$hist_price_order_within_srch[single_search_index] = 
                        rank(sample_train$prop_log_historical_price[single_search_index], ties.method = "random")
  sample_train$starrating_order_wthin_srch[single_search_index] = 
                        rank(sample_train$prop_starrating[single_search_index], ties.method = "random")
  sample_train$review_score_order_within_srch[single_search_index] =
                        rank(sample_train$prop_review_score[single_search_index], ties.method = "random")
  sample_train$loc_score1_order[single_search_index] = 
                        rank(sample_train$prop_location_score1[single_search_index], ties.method = "random")
  sample_train$loc_score2_order[single_search_index] = 
                        rank(sample_train$prop_location_score2[single_search_index], ties.method = "random")
}

################### prop search frequency based on dest_id / country_id / visitor_location_id ###############
prop_destid_comb = sqldf("select prop_id, srch_destination_id, count(*) as prop_destid_cnt from sample_train 
                         group by prop_id, srch_destination_id order by prop_id")
prop_countryid_comb = sqldf("select prop_id, prop_country_id, count(*) as prop_countryid_cnt from sample_train 
                         group by prop_id, prop_country_id order by prop_id")
prop_visitor_loc_comb = sqldf("select prop_id, visitor_location_country_id, count(*) as prop_visitor_loc_cnt from sample_train 
                         group by prop_id, visitor_location_country_id order by prop_id")
sample_train = merge(sample_train, prop_destid_comb, by.x = c("prop_id", "srch_destination_id"), 
                     by.y = c("prop_id", "srch_destination_id"))
sample_train = merge(sample_train, prop_countryid_comb, by.x = c("prop_id", "prop_country_id"), 
                     by.y = c("prop_id", "prop_country_id"))
sample_train = merge(sample_train, prop_visitor_loc_comb, by.x = c("prop_id", "visitor_location_country_id"), 
                     by.y = c("prop_id", "visitor_location_country_id"))

#################### generate target position ####################
sample_train$rel_score = 0
sample_train$rank_in_srch = 0
booking_index = which(sample_train$booking_bool==1)
click_index = which(sample_train$booking_bool==0 & sample_train$click_bool==1)
non_click_index = which(sample_train$click_bool == 0)

sample_train$rel_score[booking_index] = 3
sample_train$rel_score[click_index] = 2
sample_train$rel_score[non_click_index] = 1

srch_list = unique(sample_train$srch_id)
complete_srch_list = as.vesample_train$srch_id
for(i in 1:length(srch_list)) {
  srch_index = which(complete_srch_list == srch_list[i])
  sample_train$rank_in_srch[srch_index] = rank(sample_train$rel_score[srch_index], ties.method = "random")
}

# average position over srch_id without self position 
save(sample_train, file = "sample_train.RData")

################################## training #################################
setwd("/Users/apple/Documents/kaggle/expedia/data")
library("gbm")
load(file = "sample_train.RData")

train = sample_train
train$prop_id = NULL
train$srch_destination_id = NULL
train$position_over_propid = NULL
train$position_over_srchid = NULL
train$random_bool = NULL      # there is no variation in random_bool
train$prop_std_prop_starrating = NULL
train$prop_std_prop_review_score = NULL
train$prop_std_prop_location_score1 = NULL
train$prop_std_prop_location_score2 = NULL

#train$booking_bool = as.factor(train$booking_bool)
#train$click_bool = as.factor(train$click_bool)
train$prop_brand_bool = as.factor(train$prop_brand_bool)
train$promotion_flag = as.factor(train$promotion_flag)
train$srch_saturday_night_bool = as.factor(train$srch_saturday_night_bool)

booking_prediction_data = train[, -which(colnames(train) %in% c("click_bool", "position", "srch_id"))]
click_prediction_data = train[, -which(colnames(train) %in% c("booking_bool", "position","srch_id"))]
position_prediction_data = train[, -which(colnames(train) %in% c("click_bool", "booking_bool"))]

TREE_NUM = 2000
SHRINKAGE = 0.01
INTERACTION_NUM = 5
TERMINAL_NODE_NUM = 15

position_prediction_model = gbm(rank_in_srch ~ ., 
                                data = position_prediction_data,
                                distribution = list(name="pairwise", group="srch_id", metric="ndcg"),
                                keep.data = FALSE,
                                verbose = TRUE,
                                n.trees = TREE_NUM,
                                shrinkage = SHRINKAGE,
                                interaction.depth = INTERACTION_NUM,
                                n.minobsinnode = TERMINAL_NODE_NUM)
save(position_prediction_model, file = "position_model.RData")
position_model = summary(position_prediction_model)
position_model$var = factor(position_model$var, levels = as.character(rev(position_model$var)))
ggplot(data=position_model[1:40,], aes(var, weight=rel.inf, fill=rel.inf)) + 
  geom_bar() + coord_flip() + labs(x="", y="Relevant Importance")

booking_prediction_model = gbm(booking_bool ~ ., 
                               data = booking_prediction_data,
                               distribution = "bernoulli",
                               keep.data = FALSE,
                               verbose = TRUE,
                               n.trees = TREE_NUM,
                               shrinkage = SHRINKAGE,
                               interaction.depth = INTERACTION_NUM,
                               n.minobsinnode = TERMINAL_NODE_NUM)
save(booking_prediction_model, file = "booking_model.RData")
booking_model = summary(booking_prediction_model) 
booking_model$var = factor(booking_model$var, levels = as.character(rev(booking_model$var)))
library(ggplot2)
ggplot(data=booking_model[1:40,], aes(var, weight=rel.inf, fill=rel.inf)) + 
  geom_bar() + coord_flip() + labs(x="", y="Relevant Importance")
#537 443

click_prediction_model = gbm(click_bool ~ .,
                             data = click_prediction_data,
                             distribution = "bernoulli",
                             keep.data = FALSE,
                             verbose = TRUE,
                             n.trees = TREE_NUM,
                             shrinkage = SHRINKAGE,
                             interaction.depth = INTERACTION_NUM,
                             n.minobsinnode = TERMINAL_NODE_NUM)
save(click_prediction_model, file = "click_model.RData")
click_model = summary(click_prediction_model)
booking_model$var = factor(click_model$var, levels = as.character(rev(click_model$var)))
library(ggplot2)
ggplot(data=booking_model[1:40,], aes(var, weight=rel.inf, fill=rel.inf)) + 
  geom_bar() + coord_flip() + labs(x="", y="Relevant Importance")

########################### 5 fold cross validation ########################### 
CV_FOLD_NUM = 5

position_prediction_model = list()
booking_prediction_model = list()
click_prediction_model = list()

unique_srchid_list = sort(unique(train$srch_id))
cv_info = cvFolds(length(unique_srchid_list), K=CV_FOLD_NUM, type="random")

for(fold_count in 1:CV_FOLD_NUM) {
  selected_srchid = unique_srchid_list[cv.info$subsets[which(cv.info$which==fold_count)]]
  selected_test_index = which(train$srch_id %in% selected_srchid)
  
  position_prediction_model[[fold_count]] = gbm(position ~ ., 
                                             data = position_prediction_data[-selected_test_index, ],
                                             distribution = list(name="pairwise", group="srch_id", metric="ndcg"),
                                             keep.data = FALSE,
                                             verbose = TRUE,
                                             n.trees = TREE_NUM,
                                             shrinkage = SHRINKAGE,
                                             interaction.depth = INTERACTION_NUM,
                                             n.minobsinnode = TERMINAL_NODE_NUM)
  
  booking_prediction_model[[fold_count]] = gbm(booking_bool ~ ., 
                                            data = booking_prediction_data[-selected_test_index, ],
                                            distribution = "bernoulli",
                                            keep.data = FALSE,
                                            verbose = TRUE,
                                            n.trees = TREE_NUM,
                                            shrinkage = SHRINKAGE,
                                            interaction.depth = INTERACTION_NUM,
                                            n.minobsinnode = TERMINAL_NODE_NUM)
  
  click_prediction_model[[fold_count]] = gbm(click_bool ~ .,
                                          data = click_prediction_data[-selected_test_index, ],
                                          distribution = "bernoulli",
                                          keep.data = FALSE,
                                          verbose = TRUE,
                                          n.trees = TREE_NUM,
                                          shrinkage = SHRINKAGE,
                                          interaction.depth = INTERACTION_NUM,
                                          n.minobsinnode = TERMINAL_NODE_NUM)
  
  if(fold_count == CV_FOLD_NUM) {
    cat("model training complete..")
  }
}

############################## compute test error #############################
click_test_error_list = rep(0, CV_FOLD_NUM)
booking_test_error_list = rep(0, CV_FOLD_NUM) 
ndcg_list = rep(0, CV_FOLD_NUM)
THRESHOLD = 0.9

# compute click prediction error
for(fold_count in 1:CV_FOLD_NUM) {
  selected_srchid = unique_srchid_list[cv.info$subsets[which(cv.info$which==fold_count)]]
  selected_test_index = which(train$srch_id %in% selected_srchid)
  predicted_click = predict(click_prediction_model[[fold_count]], 
                            click_prediction_data[selected_test_index, ],
                            n.trees = 100,
                            type = "response")
  #cat(quantile(predicted_click, 0.96), " ")
  predicted_click[which(predicted_click*10 > THRESHOLD)] = 1
  predicted_click[which(predicted_click*10 <= THRESHOLD)] = 0
  click_test_error_list[fold_count] = 
    length(which(predicted_click != click_prediction_data$click_bool[selected_test_index])) / length(selected_test_index)
}
cat(click_test_error_list, mean(click_test_error_list))

# compute booking error
for(fold_count in 1:CV_FOLD_NUM) {
  selected_srchid = unique_srchid_list[cv.info$subsets[which(cv.info$which==fold_count)]]
  selected_test_index = which(train$srch_id %in% selected_srchid)
  predicted_booking = predict(booking_prediction_model[[fold_count]], 
                              booking_predict_data[selected_test_index, ],
                              n.trees = 100,
                              type = "response")
  #cat(quantile(predicted_booking*10, 0.96), " ")
  predicted_booking[which(predicted_booking*10 > THRESHOLD)] = 1
  predicted_booking[which(predicted_booking*10 <= THRESHOLD)] = 0
  booking_test_error_list[fold_count] = 
  length(which(predicted_booking != booking_prediction_data$booking_bool[selected_test_index])) / length(selected_test_index)
}
cat(booking_test_error_list, "\nbooking test error: ", mean(booking_test_error_list))
 
# compute ndcg - normalized discounted cumulative gain
for(fold_count in 1:CV_FOLD_NUM) {
fold_count=1
selected_srchid = unique_srchid_list[cv.info$subsets[which(cv.info$which==fold_count)]]
selected_test_index = which(train$srch_id %in% selected_srchid)
predicted_position = predict(position_prediction_model[[fold_count]], 
                               position_prediction_data[selected_test_index, ],
                               n.trees = 100,
                               type = "response")
  
  position_compare = data.frame(srch_id = position_prediction_data$srch_id[selected_test_index], 
                                original_pos = position_prediction_data$position[selected_test_index], 
                                predicted_pos = predicted_position*10,
                                booking = train$booking_bool[selected_test_index],
                                click = train$click_bool[selected_test_index],
                                rel = rep(0, length(selected_test_index)))
  position_compare = position_compare[order(position_compare$srch_id), ]
  
  search_list = unique(position_compare$srch_id)
  for(i in 1:length(search_list)) {
    single_search_index = which(position_compare$srch_id == search_list[i])
    position_compare[single_search_index, "predicted_pos"] = rank(position_compare$predicted_pos[single_search_index], 
                                                                  ties.method = "first")
  }
  
  dcg = 0
  booking_index = which(position_compare$booking == 1)
  click_index = which(position_compare$click == 1 & position_compare$booking == 0)
  non_index = which(position_compare$booking == 0 & position_compare$click == 0)
  position_compare[booking_index, "rel"] = 5
  position_compare[click_index, "rel"] = 1
  position_compare[non_index, "rel"] = 0
  rel_list = position_compare$rel
  rank_list = position_compare$predicted_pos
  booking_list = position_compare$booking
  click_list = position_compare$click
  
  for(i in 1:length(search_list)) {
    single_search_index = which(position_compare$srch_id == search_list[i])
    for(j in 1:length(single_search_index)) {
      dcg = dcg + (2^rel_list[j] - 1) / log2(rank_list[j] + 1)
    }
  }
  
  ideal = 0
  for(i in 1:length(search_list)) {
    single_search_index = which(position_compare$srch_id == search_list[i])
    single_search_table = position_compare[single_search_index, ]
    
    rel_lst = sort(single_search_table$rel, decreasing = T)
    #rank_lst = sort(single_search_table$predicted_pos)
    #cat(rank_lst, "\n")
    
    for(j in 1:length(single_search_index)) {
      ideal = ideal + (2^rel_lst[j] - 1)/log2(j + 1)
    }
    
  }
  
  ndcg_list[fold_count] = dcg / ideal
}
