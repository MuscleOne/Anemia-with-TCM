# import the data and then have a look
raw_data = read.csv("data/clinical_data.csv")

# the structure of dataframe
raw_data %>% dim()

# split the data into different categories
## modify the name at first
col_names = raw_data %>% names()

## check the missing percentage
num_missing = 
  apply(is.na(raw_data), 2, sum) %>% data.frame() %>% t

dim(num_missing)
## remove the missing > 32
length(num_missing[, num_missing < 32])
# 38

## check the gays who are anemia
raw_data[raw_data['是否贫血']=='有', ] %>% length()
num_missing_anemia = 
  apply(
    is.na(raw_data[raw_data['是否贫血']=='有', ]), 2, sum
  ) %>% 
  data.frame() %>% t

length(num_missing_anemia[, num_missing_anemia < 25])
# 38





