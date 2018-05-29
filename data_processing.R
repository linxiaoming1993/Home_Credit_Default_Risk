# 数据预处理
library('data.table')
library('magrittr')
library('xgboost')

Discretization <- function(x, groups = 5, breaks = NULL, na_rm = TRUE, eps = 1e-5){
  # description:
  #   将连续型变量分组
  # args:
  #   x, vector
  #   groups，分组数目
  #   breaks, 分组区间
  #   na_rm, 是否忽视NA
  # output:
  #   x的每个元素对应的组别
  if(is.null(breaks)){
    breaks <- quantile(x, seq(0, 1, length = groups + 1))
    if(any(duplicated(breaks))){
      breaks <- breaks + (1:length(breaks)) * eps
    }
    breaks <- c(-Inf, breaks[-1])
  }
  as.numeric(as.factor(cut(x, breaks)))
}
## application_train/application_test----
application_train <- fread('./data/application_train.csv')
application_test <- fread('./data/application_test.csv')

# 添加45-91维的特征的缺失判断
na_judge_features_train <- application_train[, APARTMENTS_AVG:EMERGENCYSTATE_MODE] %>% as.matrix()
na_judge_features_train <- ifelse(is.na(na_judge_features_train) | nchar(na_judge_features_train) == 0, 1, 0) %>% as.data.frame()
setDT(na_judge_features_train)
application_train <- cbind(application_train, na_judge_features_train)

na_judge_features_test <- application_test[, APARTMENTS_AVG:EMERGENCYSTATE_MODE] %>% as.matrix()
na_judge_features_test <- ifelse(is.na(na_judge_features_test) | nchar(na_judge_features_test) == 0, 1, 0) %>% as.data.frame()
setDT(na_judge_features_test)
application_test <- cbind(application_test, na_judge_features_test)

# 添加一些特征
application_train[, `:=`(flag_1 = scale(AMT_INCOME_TOTAL / AMT_ANNUITY), # 年收入与年金比值的标准化
                         flag_2 = scale(AMT_ANNUITY / AMT_CREDIT), # 年金与贷款额度比值的标准化
                         flag_3 = scale(AMT_GOODS_PRICE / AMT_CREDIT), # 商品价格和贷款额度比值的标准化
                         flag_4 = is.na(EXT_SOURCE_1), 
                         flag_5 = is.na(EXT_SOURCE_3))]
application_test[, `:=`(flag_1 = scale(AMT_INCOME_TOTAL / AMT_ANNUITY), # 年收入与年金比值的标准化
                        flag_2 = scale(AMT_ANNUITY / AMT_CREDIT), # 年金与贷款额度比值的标准化
                        flag_3 = scale(AMT_GOODS_PRICE / AMT_CREDIT), # 商品价格和贷款额度比值的标准化
                        flag_4 = is.na(EXT_SOURCE_1), 
                        flag_5 = is.na(EXT_SOURCE_3))]

application_train_target <- application_train$TARGET
all_data <- rbind(application_train[, -2], application_test)
# 对非类别特征标准化/处理成类别特征
scale_features <- c('AMT_INCOME_TOTAL', 'AMT_CREDIT', 'AMT_ANNUITY', 'AMT_GOODS_PRICE', 'REGION_POPULATION_RELATIVE', 
                    'DAYS_BIRTH', 'DAYS_EMPLOYED', 'DAYS_REGISTRATION', 'DAYS_ID_PUBLISH', 
                    'OWN_CAR_AGE', 'EXT_SOURCE_1', 'EXT_SOURCE_2', 'EXT_SOURCE_3', 
                    names(application_train[, APARTMENTS_AVG:NONLIVINGAREA_MEDI]), 
                    'TOTALAREA_MODE', 'OBS_30_CNT_SOCIAL_CIRCLE', 'OBS_60_CNT_SOCIAL_CIRCLE', 
                    'DAYS_LAST_PHONE_CHANGE')
eval(parse(text = paste0('all_data[, `:=`(', 
                         paste0(scale_features, ' = scale(', scale_features, ')', collapse = ', '), 
                         ')]')))
all_data[, `:=`(NAME_CONTRACT_TYPE = NULL, 
                CODE_GENDER = ifelse(CODE_GENDER == 'XNA', sample(c('F','M'), 1, prob = c(0.67, 0.33)), CODE_GENDER), 
                CNT_CHILDREN = ifelse(CNT_CHILDREN > 3, 4, CNT_CHILDREN), # 孩子数量最大设为4
                FLAG_MOBIL = NULL,  
                FLAG_CONT_MOBILE = NULL,  
                CNT_FAM_MEMBERS = ifelse(CNT_FAM_MEMBERS > 5, 6, CNT_FAM_MEMBERS),  # 家庭成员数量最大设为6
                ELEVATORS_AVG = ifelse(ELEVATORS_AVG > 0.05, ELEVATORS_AVG, 0), 
                NONLIVINGAPARTMENTS_AVG = NULL, 
                NONLIVINGAREA_AVG = NULL, 
                LANDAREA_MODE = NULL, 
                NONLIVINGAPARTMENTS_MODE = NULL, 
                NONLIVINGAREA_MODE = NULL, 
                DEF_30_CNT_SOCIAL_CIRCLE = ifelse(DEF_30_CNT_SOCIAL_CIRCLE > 3, 3, DEF_30_CNT_SOCIAL_CIRCLE), 
                DEF_60_CNT_SOCIAL_CIRCLE = ifelse(DEF_60_CNT_SOCIAL_CIRCLE > 3, 3, DEF_60_CNT_SOCIAL_CIRCLE), 
                FLAG_DOCUMENT_2 = NULL, 
                FLAG_DOCUMENT_4 = NULL, 
                FLAG_DOCUMENT_5 = NULL,    # 单独看没区分度
                FLAG_DOCUMENT_7 = NULL, 
                FLAG_DOCUMENT_10 = NULL, 
                FLAG_DOCUMENT_11 = NULL, 
                FLAG_DOCUMENT_12 = NULL, 
                FLAG_DOCUMENT_13 = NULL, 
                FLAG_DOCUMENT_14 = NULL, 
                FLAG_DOCUMENT_15 = NULL, 
                FLAG_DOCUMENT_16 = NULL, 
                FLAG_DOCUMENT_17 = NULL, 
                FLAG_DOCUMENT_18 = NULL, 
                FLAG_DOCUMENT_19 = NULL, 
                FLAG_DOCUMENT_20 = NULL, 
                FLAG_DOCUMENT_21 = NULL, 
                AMT_REQ_CREDIT_BUREAU_HOUR = NULL, 
                AMT_REQ_CREDIT_BUREAU_DAY = NULL, 
                AMT_REQ_CREDIT_BUREAU_WEEK = NULL, 
                AMT_REQ_CREDIT_BUREAU_MON = NULL, 
                AMT_REQ_CREDIT_BUREAU_QRT = ifelse(AMT_REQ_CREDIT_BUREAU_QRT > 2, 2, 
                                                   AMT_REQ_CREDIT_BUREAU_QRT), 
                AMT_REQ_CREDIT_BUREAU_YEAR = ifelse(AMT_REQ_CREDIT_BUREAU_YEAR > 6, 7, 
                                                    AMT_REQ_CREDIT_BUREAU_YEAR))]


# xgboost 
dtrain <- xgb.DMatrix(data = data.matrix(all_data[1:nrow(application_train), CODE_GENDER:flag_5]), 
                      label = application_train[, TARGET])
dtest <- xgb.DMatrix(data = data.matrix(all_data[-(1:nrow(application_train)), CODE_GENDER:flag_5]))

params <- list(objective = 'binary:logistic', 
               max_depth = 4, 
               min_child_weight = 10, 
               eta = 0.01, 
               eval_metric = 'auc')
bst_cv <- xgb.cv(params = params, dtrain, nrounds = 3000, nfold = 10, missing = NA,
                 verbose = TRUE, early_stopping_rounds = 20, maximize = TRUE)


bst <- xgb.train(params = params, data = dtrain, nrounds = 2200, missing = NA, 
                 watchlist = list(train = dtrain), verbose = TRUE)
pred_test <- predict(bst, dtest)
pred_test <- data.table(SK_ID_CURR = application_test$SK_ID_CURR, 
                        TARGET = pred_test)
fwrite(pred_test, './predict/submission1.csv')