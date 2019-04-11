library(dplyr)
library(ggplot2)
library(onehot)
raw_data <- read.csv("compas-analysis-master/compas-scores-two-years.csv")
nrow(raw_data)


df <- raw_data %>% 
  filter(days_b_screening_arrest <= 30) %>%
  filter(days_b_screening_arrest >= -30) %>%
  filter(is_recid != -1) %>%
  filter(c_charge_degree != "O") %>%
  filter(score_text != 'N/A') %>%
  filter(race != 'Other')
nrow(df)

include_features = suppressWarnings({read.csv("compas_features_included.csv")})
features = colnames(include_features)[include_features == 1]
predictor_df = df[features]

# process predictor to make more interpretable
predictor_df = predictor_df %>% mutate(months_in_jail_so_far = round(as.numeric(as.Date(c_jail_out)-as.Date(c_jail_in))/30)) %>% select(-c_jail_in, -c_jail_out)

# separate out the columns that dependent on the recidivism outcome
outcomes_df = predictor_df %>% select(is_violent_recid, is_recid, two_year_recid)
predictor_df = predictor_df %>% select(-is_violent_recid, -is_recid, -two_year_recid)

# create numeric predictor df to use in sampling scheme and model training 
onehotencoder <- function(df_orig) {
  df<-cbind(df_orig)
  df_clmtyp<-data.frame(clmtyp=sapply(df,class))
  df_col_typ<-data.frame(clmnm=colnames(df),clmtyp=df_clmtyp$clmtyp)
  for (rownm in 1:nrow(df_col_typ)) {
    if (df_col_typ[rownm,"clmtyp"]=="factor") {
      clmn_obj<-df[toString(df_col_typ[rownm,"clmnm"])] 
      dummy_matx<-data.frame(model.matrix( ~.-1, data = clmn_obj))
      dummy_matx<-dummy_matx[,c(1,3:ncol(dummy_matx))]
      df[toString(df_col_typ[rownm,"clmnm"])]<-NULL
      df<-cbind(df,dummy_matx)
      df[toString(df_col_typ[rownm,"clmnm"])]<-NULL
    }  }
  return(df)
}

encoder = onehot(predictor_df)
numeric_df = predict(encoder, predictor_df)

# z-score columlapply(numeric_df, scale, )
scaled_df = scale(numeric_df)
m = dim(scaled_df)[1]

# form matrix of L1 distances 
l1_distance = function(r1, r2){
  return(sum(abs(r1-r2), na.rm = TRUE))
}
distances = outer(1:m, 1:m, Vectorize(function(i,j) l1_distance(scaled_df[i,], scaled_df[j,])))

save(distances, file='distances.RData')
save(predictor_df, file='predictor_df.RData')
save(numeric_df, file='numeric_df.RData')






