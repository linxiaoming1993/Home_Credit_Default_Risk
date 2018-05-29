# 数据洞察
# 在数据处理前先简单的了解数据，单独考虑每一个特征
library('data.table')
library('magrittr')


## application_train/application_test----
CusTable <- function(train_feature, test_feature, train_target, is_factor = TRUE, 
                     groups = 2, na_rm = FALSE, eps = 1e-5){
  output <- list()
  if(na_rm){
    output$na_ratio <- data.frame(train = sum(is.na(train_feature)), 
                                  test = sum(is.na(test_feature)))
  }
  if(!is_factor){
    train_view <- quantile(train_feature, seq(0, 1, length = groups + 1), na.rm = na_rm)
    if(any(duplicated(train_view))){
      train_view <- train_view + (1:length(train_view)) * eps
    }
    test_view <- quantile(test_feature, seq(0, 1, length = groups + 1), na.rm = na_rm)
    train_feature <- cut(train_feature, c(-Inf, as.numeric(train_view)[-1])) %>% 
      as.factor() %>% as.numeric()
  }else{
    train_view <- table(train_feature)
    test_view <- table(test_feature)
  }
  train_target_view <- table(train_feature, train_target)
  train_target_view <- apply(train_target_view, 1, function(x) c(x, x[2] / x[1])) %>% t()
  output$train_view <- train_view
  output$test_view <- test_view
  output$train_target_view <- train_target_view
  return(output)
}
FeatureView <- function(feature_name, ...){
  CusTable(application_train[[feature_name]], 
           application_test[[feature_name]], 
           application_train$TARGET, ...)
}
application_train <- fread('./data/application_train.csv')
application_test <- fread('./data/application_test.csv')

# 1、SK_ID_CUR唯一检查
nrow(application_train) == application_train$SK_ID_CURR %>% unique() %>% length()
nrow(application_test) == application_test$SK_ID_CURR %>% unique() %>% length()
# 结果：ID唯一

# 2、TARGET
table(application_train$TARGET)
# 0      1 
# 282686  24825
# 结果：标签分布不平衡，大部分为0

# 3、NAME_CONTRACT_TYPE
FeatureView('NAME_CONTRACT_TYPE')
# 结论：现金/转账分布不平衡，更重要的是此数据在训练集和测试集差异太大，该特征可考虑删除。

# 4、CODE_GENDER
FeatureView('CODE_GENDER')
# 结论：性别可考虑是一个影响因素

# 5、FLAG_OWN_CAR
FeatureView('FLAG_OWN_CAR')
# 结论：是否有车可考虑是一个影响因素

# 6、FLAG_OWN_REALTY
FeatureView('FLAG_OWN_REALTY')
# 结论：是否有房

# 7、CNT_CHILDREN
FeatureView('CNT_CHILDREN')
# 结论：孩子数据量，孩子数量大于3的可统一看成4

# 8、AMT_INCOME_TOTAL
FeatureView('AMT_INCOME_TOTAL', is_factor = FALSE, groups = 10)
# 结论：收入，近似单调，

# 9、AMT_CREDIT
FeatureView('AMT_CREDIT', is_factor = FALSE, groups = 10)
# 结论：AMT_CREDIT，非单调，

# 10、AMT_ANNUITY
FeatureView('AMT_ANNUITY', is_factor = FALSE, na_rm = TRUE, groups = 10)
# 结论：AMT_ANNUITY缺失数据很少，非单调，

# 11、AMT_GOODS_PRICE
FeatureView('AMT_GOODS_PRICE', is_factor = FALSE, groups = 5, na_rm = TRUE)
# 结论：AMT_GOODS_PRICE缺失数据较少，非单调，

# 12、NAME_TYPE_SUITE
FeatureView('NAME_TYPE_SUITE')
# 结论： 陪行人员情况，

# 13、NAME_INCOME_TYPE
FeatureView('NAME_INCOME_TYPE')
# 结论：收入类型，区分度大，

# 14、NAME_EDUCATION_TYPE
FeatureView('NAME_EDUCATION_TYPE')
# 结论：受教育程度，区分度大，

# 15、NAME_FAMILY_STATUS
FeatureView('NAME_FAMILY_STATUS')
# 结论：家庭状况，区分度大, 。

# 16、NAME_HOUSING_TYPE
FeatureView('NAME_HOUSING_TYPE')
# 结论：居住情况，区分度大，。

# 17、REGION_POPULATION_RELATIVE
FeatureView('REGION_POPULATION_RELATIVE', is_factor = FALSE, groups = 10)
# 结论：居住区标准人口，非单调，

# 18、DAYS_BIRTH
FeatureView('DAYS_BIRTH', is_factor = FALSE, groups = 10)
# 结论：年龄区分度大，单调，

# 19、DAYS_EMPLOYED
FeatureView('DAYS_EMPLOYED', is_factor = FALSE, groups = 10)
# 结论：工作时长区分度大，非单调，

# 20、DAYS_REGISTRATION
FeatureView('DAYS_REGISTRATION', is_factor = FALSE, groups = 10)
# 结论：注册时长区分度大，单调。

# 21、DAYS_ID_PUBLISH
FeatureView('DAYS_ID_PUBLISH', is_factor = FALSE, groups = '10')
# 结论：实名制时间区分度大，单调

# 22、OWN_CAR_AGE
FeatureView('DAYS_ID_PUBLISH', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：汽车年限区分度大，单调

# 23、FLAG_MOBIL
FeatureView('FLAG_MOBIL')
# 结论：没手机号的很少，特征删除

# 24、FLAG_EMP_PHONE
FeatureView('FLAG_EMP_PHONE')
# 结论：FLAG_EMP_PHONE，区分度大

# 25、FLAG_WORK_PHONE
FeatureView('FLAG_WORK_PHONE')
# 结论：FLAG_WORK_PHONE，有区分度

# 26、FLAG_CONT_MOBILE 
FeatureView('FLAG_CONT_MOBILE')
# 结论：FLAG_CONT_MOBILE，标签为0的数据很少，并且区分度小，删除

# 27、FLAG_PHONE 
FeatureView('FLAG_PHONE')
# 结论：FLAG_PHONE，有区分度

# 28、FLAG_EMAIL 
FeatureView('FLAG_EMAIL')
# 结论：FLAG_EMAIL，数据分布不均，区分度不大，可以考虑删除

# 29、OCCUPATION_TYPE 
FeatureView('OCCUPATION_TYPE')
# 结论：职业类型，有区分度，职业有空值(非NA，不过猜测是数据缺失)

# 30、CNT_FAM_MEMBERS 
FeatureView('CNT_FAM_MEMBERS')
# 结论：家庭成员数量，有区分度，非单调。成员数量大于5人的可设为6人。

# 31、REGION_RATING_CLIENT 
FeatureView('REGION_RATING_CLIENT')
# 结论：居住情况评级，有区分度

# 32、REGION_RATING_CLIENT_W_CITY 
FeatureView('REGION_RATING_CLIENT_W_CITY')
# 结论：居住城市情况评级，有区分度。测试集有一位用户评级缺失。

# 33、WEEKDAY_APPR_PROCESS_START 
FeatureView('WEEKDAY_APPR_PROCESS_START')
# 结论：周几贷款，单独看区分度小。可考虑删除。

# 34、HOUR_APPR_PROCESS_START 
FeatureView('HOUR_APPR_PROCESS_START')
# 结论：贷款的时间点（小时），有区分度

# 35、REG_REGION_NOT_LIVE_REGION 
FeatureView('REG_REGION_NOT_LIVE_REGION')
# 结论：REG_REGION_NOT_LIVE_REGION，有区分度

# 36、REG_REGION_NOT_WORK_REGION 
FeatureView('REG_REGION_NOT_WORK_REGION')
# 结论：REG_REGION_NOT_WORK_REGION，有区分度

# 37、LIVE_REGION_NOT_WORK_REGION 
FeatureView('LIVE_REGION_NOT_WORK_REGION')
# 结论：LIVE_REGION_NOT_WORK_REGION，有区分度

# 38、REG_CITY_NOT_LIVE_CITY 
FeatureView('REG_CITY_NOT_LIVE_CITY')
# 结论：REG_CITY_NOT_LIVE_CITY，有区分度

# 39、REG_CITY_NOT_WORK_CITY 
FeatureView('REG_CITY_NOT_WORK_CITY')
# 结论：REG_CITY_NOT_WORK_CITY，有区分度

# 40、LIVE_CITY_NOT_WORK_CITY 
FeatureView('LIVE_CITY_NOT_WORK_CITY')
# 结论：LIVE_CITY_NOT_WORK_CITY，有区分度

# 41、ORGANIZATION_TYPE 
FeatureView('ORGANIZATION_TYPE')
# 结论：ORGANIZATION_TYPE，有区分度，缺失值用“XNA”表示。

# 42、EXT_SOURCE_1 
FeatureView('EXT_SOURCE_1', is_factor = FALSE, groups = 5, na_rm = TRUE)
# 结论：EXT_SOURCE_1，区分度很大，不过数据缺失严重，一般多的用户数据缺失。

# 43、EXT_SOURCE_2 
FeatureView('EXT_SOURCE_2', is_factor = FALSE, groups = 5, na_rm = TRUE)
# 结论：EXT_SOURCE_2，区分度很大，数据缺失很少。

# 44、EXT_SOURCE_3 
FeatureView('EXT_SOURCE_3', is_factor = FALSE, groups = 5, na_rm = TRUE)
# 结论：EXT_SOURCE_3，区分度很大，数据缺失五分之一。

# 45、APARTMENTS_AVG 
FeatureView('APARTMENTS_AVG', is_factor = FALSE, groups = 5, na_rm = TRUE)
# 结论：APARTMENTS_AVG，有区分度，一半左右数据缺失。

# 46、BASEMENTAREA_AVG 
FeatureView('BASEMENTAREA_AVG', is_factor = FALSE, groups = 5, na_rm = TRUE)
# 结论：BASEMENTAREA_AVG，有区分度，一半左右数据缺失。

# 47、YEARS_BEGINEXPLUATATION_AVG 
FeatureView('YEARS_BEGINEXPLUATATION_AVG', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：YEARS_BEGINEXPLUATATION_AVG，有区分度，一半左右数据缺失。

# 48、YEARS_BUILD_AVG 
FeatureView('YEARS_BUILD_AVG', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：YEARS_BUILD_AVG，有区分度，一半左右数据缺失。

# 49、COMMONAREA_AVG 
FeatureView('COMMONAREA_AVG', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：COMMONAREA_AVG，有区分度，一半左右数据缺失。

# 50、ELEVATORS_AVG 
FeatureView('ELEVATORS_AVG', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：ELEVATORS_AVG，有区分度，一半左右数据缺失。测试集很多数据很小，对应的测试集很多数据为0，
#       考虑小于0.05的数据都置零，如果还无效的话，考虑删除。

# 51、ENTRANCES_AVG 
FeatureView('ENTRANCES_AVG', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：ENTRANCES_AVG，有一些区分度，一半左右数据缺失。近似单调。

# 52、FLOORSMAX_AVG 
FeatureView('FLOORSMAX_AVG', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：FLOORSMAX_AVG，有强区分度，一半左右数据缺失。单调

# 53、FLOORSMIN_AVG 
FeatureView('FLOORSMIN_AVG', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：FLOORSMIN_AVG，有一些区分度，一半左右数据缺失。

# 54、LANDAREA_AVG 
FeatureView('LANDAREA_AVG', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：LANDAREA_AVG，一半左右数据缺失。区分度不大，可考虑删除。

# 55、LIVINGAPARTMENTS_AVG 
FeatureView('LIVINGAPARTMENTS_AVG', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：LIVINGAPARTMENTS_AVG，有区分度，一半左右数据缺失。单调。

# 56、LIVINGAREA_AVG 
FeatureView('LIVINGAREA_AVG', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：LIVINGAREA_AVG，有区分度，一半左右数据缺失。单调。

# 57、NONLIVINGAPARTMENTS_AVG 
FeatureView('NONLIVINGAPARTMENTS_AVG', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：NONLIVINGAPARTMENTS_AVG，区分度小，一半左右数据缺失。考虑删除。

# 58、NONLIVINGAREA_AVG 
FeatureView('NONLIVINGAREA_AVG', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：NONLIVINGAREA_AVG，区分度小，一半左右数据缺失。考虑删除。

# 59、APARTMENTS_MODE 
FeatureView('APARTMENTS_MODE', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：APARTMENTS_MODE，有区分度，一半左右数据缺失。近似单调

# 60、BASEMENTAREA_MODE 
FeatureView('BASEMENTAREA_MODE', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：BASEMENTAREA_MODE，有区分度，一半左右数据缺失。近似单调。

# 61、YEARS_BEGINEXPLUATATION_MODE 
FeatureView('YEARS_BEGINEXPLUATATION_MODE', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：YEARS_BEGINEXPLUATATION_MODE，有区分度，一半左右数据缺失。近似单调。

# 62、YEARS_BUILD_MODE 
FeatureView('YEARS_BUILD_MODE', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：YEARS_BUILD_MODE，有区分度，一半左右数据缺失。近似单调。

# 63、COMMONAREA_MODE 
FeatureView('COMMONAREA_MODE', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：COMMONAREA_MODE，有区分度，一半左右数据缺失。

# 64、ELEVATORS_MODE 
FeatureView('ELEVATORS_MODE', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：ELEVATORS_MODE，有区分度，一半左右数据缺失。单调。

# 65、ENTRANCES_MODE 
FeatureView('ENTRANCES_MODE', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：ENTRANCES_MODE，有区分度，一半左右数据缺失。单调。

# 66、FLOORSMAX_MODE 
FeatureView('FLOORSMAX_MODE', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：FLOORSMAX_MODE，有区分度，一半左右数据缺失。单调。

# 67、FLOORSMIN_MODE 
FeatureView('FLOORSMIN_MODE', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：FLOORSMIN_MODE，有区分度，一半左右数据缺失。不单调。

# 68、LANDAREA_MODE 
FeatureView('LANDAREA_MODE', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：LANDAREA_MODE，有区分度，一半左右数据缺失。考虑删除。

# 69、LIVINGAPARTMENTS_MODE 
FeatureView('LIVINGAPARTMENTS_MODE', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：LIVINGAPARTMENTS_MODE，有区分度，一半左右数据缺失。单调。

# 70、LIVINGAREA_MODE 
FeatureView('LIVINGAREA_MODE', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：LIVINGAREA_MODE，有区分度，一半左右数据缺失。单调

# 71、NONLIVINGAPARTMENTS_MODE 
FeatureView('NONLIVINGAPARTMENTS_MODE', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：NONLIVINGAPARTMENTS_MODE，区分度小，一半左右数据缺失。考虑删除

# 72、NONLIVINGAREA_MODE 
FeatureView('NONLIVINGAREA_MODE', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：NONLIVINGAREA_MODE，区分度小，一半左右数据缺失。考虑删除。

# 73、APARTMENTS_MEDI 
FeatureView('APARTMENTS_MEDI', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：APARTMENTS_MEDI，有区分度，一半左右数据缺失。单调。

# 74、BASEMENTAREA_MEDI 
FeatureView('BASEMENTAREA_MEDI', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：BASEMENTAREA_MEDI，有区分度，一半左右数据缺失。单调。

# 75、YEARS_BEGINEXPLUATATION_MEDI 
FeatureView('YEARS_BEGINEXPLUATATION_MEDI', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：YEARS_BEGINEXPLUATATION_MEDI，有区分度，一半左右数据缺失。单调。

# 76、YEARS_BUILD_MEDI 
FeatureView('YEARS_BUILD_MEDI', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：YEARS_BUILD_MEDI，有区分度，一半左右数据缺失。单调。

# 77、COMMONAREA_MEDI 
FeatureView('COMMONAREA_MEDI', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：COMMONAREA_MEDI，有区分度，一半左右数据缺失。近似单调。

# 78、ELEVATORS_MEDI 
FeatureView('ELEVATORS_MEDI', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：ELEVATORS_MEDI，有区分度，一半左右数据缺失。近似单调。

# 79、ENTRANCES_MEDI 
FeatureView('ENTRANCES_MEDI', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：ENTRANCES_MEDI，区分度小，一半左右数据缺失。近似单调。

# 80、FLOORSMAX_MEDI 
FeatureView('FLOORSMAX_MEDI', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：FLOORSMAX_MEDI，有区分度，一半左右数据缺失。单调。

# 81、FLOORSMIN_MEDI 
FeatureView('FLOORSMIN_MEDI', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：FLOORSMIN_MEDI，有区分度，一半左右数据缺失。近似单调。

# 82、LANDAREA_MEDI 
FeatureView('LANDAREA_MEDI', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：LANDAREA_MEDI，有区分度，一半左右数据缺失。

# 83、LIVINGAPARTMENTS_MEDI 
FeatureView('LIVINGAPARTMENTS_MEDI', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：LIVINGAPARTMENTS_MEDI，有区分度，一半左右数据缺失。近似单调。

# 84、LIVINGAREA_MEDI 
FeatureView('LIVINGAREA_MEDI', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：LIVINGAREA_MEDI，有区分度，一半左右数据缺失。单调。

# 85、NONLIVINGAPARTMENTS_MEDI 
FeatureView('NONLIVINGAPARTMENTS_MEDI', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：NONLIVINGAPARTMENTS_MEDI，有区分度，一半左右数据缺失。

# 86、NONLIVINGAREA_MEDI 
FeatureView('NONLIVINGAREA_MEDI', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：NONLIVINGAREA_MEDI，区分度小，一半左右数据缺失。考虑删除

# 87、FONDKAPREMONT_MODE 
FeatureView('FONDKAPREMONT_MODE')
# 结论：FONDKAPREMONT_MODE，有区分度，缺失值为“”。

# 88、HOUSETYPE_MODE 
FeatureView('HOUSETYPE_MODE')
# 结论：HOUSETYPE_MODE，有区分度，缺失值为“”。

# 89、TOTALAREA_MODE 
FeatureView('TOTALAREA_MODE', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：TOTALAREA_MODE，有区分度，一半左右数据缺失。单调

# 90、WALLSMATERIAL_MODE 
FeatureView('WALLSMATERIAL_MODE')
# 结论：WALLSMATERIAL_MODE，有区分度，缺失值为“”。

# 91、EMERGENCYSTATE_MODE 
FeatureView('EMERGENCYSTATE_MODE')
# 结论：EMERGENCYSTATE_MODE，有区分度，缺失值为“”。

# 结论：45-91的特征，本身很多数据缺失，不过“是否缺失”本身是个有区分度的特征。

# 92、OBS_30_CNT_SOCIAL_CIRCLE 
FeatureView('OBS_30_CNT_SOCIAL_CIRCLE', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：OBS_30_CNT_SOCIAL_CIRCLE，缺失值少，区分度不大

# 93、DEF_30_CNT_SOCIAL_CIRCLE 
FeatureView('DEF_30_CNT_SOCIAL_CIRCLE')
# 结论：DEF_30_CNT_SOCIAL_CIRCLE，大于3的可当作3来看。

# 94、OBS_60_CNT_SOCIAL_CIRCLE 
FeatureView('OBS_60_CNT_SOCIAL_CIRCLE', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：OBS_60_CNT_SOCIAL_CIRCLE，缺失值少，区分度不大。

# 95、DEF_60_CNT_SOCIAL_CIRCLE 
FeatureView('DEF_60_CNT_SOCIAL_CIRCLE')
# 结论：DEF_60_CNT_SOCIAL_CIRCLE，大于3的可当作3来看。

# 96、DAYS_LAST_PHONE_CHANGE 
FeatureView('DAYS_LAST_PHONE_CHANGE', is_factor = FALSE, groups = 10, na_rm = TRUE)
# 结论：DAYS_LAST_PHONE_CHANGE，缺失值少，近似单增

# 97、FLAG_DOCUMENT_2 
FeatureView('FLAG_DOCUMENT_2')
# 结论：FLAG_DOCUMENT_2，训练集标签为1的少，测试集标签都为0，删除。

# 98、FLAG_DOCUMENT_3 
FeatureView('FLAG_DOCUMENT_3')
# 结论：FLAG_DOCUMENT_3，有区分度

# 99、FLAG_DOCUMENT_4 
FeatureView('FLAG_DOCUMENT_4')
# 结论：FLAG_DOCUMENT_4，训练集标签为1的少，测试集标签为1的少，删除。

# 100、FLAG_DOCUMENT_5 
FeatureView('FLAG_DOCUMENT_5')
# 结论：FLAG_DOCUMENT_5，没有区分度，考虑删除。

# 101、FLAG_DOCUMENT_6 
FeatureView('FLAG_DOCUMENT_6')
# 结论：FLAG_DOCUMENT_6，有区分度

# 102、FLAG_DOCUMENT_7 
FeatureView('FLAG_DOCUMENT_7')
# 结论：FLAG_DOCUMENT_7，1的标签太少，删除

# 103、FLAG_DOCUMENT_8 
FeatureView('FLAG_DOCUMENT_8')
# 结论：FLAG_DOCUMENT_8，有一些区分度

# 104、FLAG_DOCUMENT_9 
FeatureView('FLAG_DOCUMENT_9')
# 结论：FLAG_DOCUMENT_9，有区分度

# 105、FLAG_DOCUMENT_10 
FeatureView('FLAG_DOCUMENT_10')
# 结论：FLAG_DOCUMENT_10，标签为1的太少，删除

# 106、FLAG_DOCUMENT_11 
FeatureView('FLAG_DOCUMENT_11')
# 结论：FLAG_DOCUMENT_11，标签为1的太少，删除

# 107、FLAG_DOCUMENT_12 
FeatureView('FLAG_DOCUMENT_12')
# 结论：FLAG_DOCUMENT_12，标签为1的太少，删除

# 108、FLAG_DOCUMENT_13 
FeatureView('FLAG_DOCUMENT_13')
# 结论：FLAG_DOCUMENT_13，测试集标签全为1，删除。

# 109、FLAG_DOCUMENT_14 
FeatureView('FLAG_DOCUMENT_14')
# 结论：FLAG_DOCUMENT_14，测试集标签全为1，删除。

# 110、FLAG_DOCUMENT_15 
FeatureView('FLAG_DOCUMENT_15')
# 结论：FLAG_DOCUMENT_15，测试集标签全为1，删除。

# 111、FLAG_DOCUMENT_16 
FeatureView('FLAG_DOCUMENT_16')
# 结论：FLAG_DOCUMENT_16，测试集标签全为1，删除。

# 112、FLAG_DOCUMENT_17 
FeatureView('FLAG_DOCUMENT_17')
# 结论：FLAG_DOCUMENT_17，测试集标签全为1，删除。

# 113、FLAG_DOCUMENT_18 
FeatureView('FLAG_DOCUMENT_18')
# 结论：FLAG_DOCUMENT_18，有区分度，不过标签1的标签很少， 考虑删除。

# 114、FLAG_DOCUMENT_19 
FeatureView('FLAG_DOCUMENT_19')
# 结论：FLAG_DOCUMENT_19，测试集标签全为1，删除。

# 115、FLAG_DOCUMENT_20 
FeatureView('FLAG_DOCUMENT_20')
# 结论：FLAG_DOCUMENT_20，测试集标签全为1，删除。

# 116、FLAG_DOCUMENT_21 
FeatureView('FLAG_DOCUMENT_21')
# 结论：FLAG_DOCUMENT_21，测试集标签全为1，删除。

# 117、AMT_REQ_CREDIT_BUREAU_HOUR 
FeatureView('AMT_REQ_CREDIT_BUREAU_HOUR')
# 结论：AMT_REQ_CREDIT_BUREAU_HOUR，区分度太小，删除。

# 118、AMT_REQ_CREDIT_BUREAU_DAY 
FeatureView('AMT_REQ_CREDIT_BUREAU_DAY')
# 结论：AMT_REQ_CREDIT_BUREAU_DAY，测试集标签大于1的数据太少，删除。

# 119、AMT_REQ_CREDIT_BUREAU_WEEK 
FeatureView('AMT_REQ_CREDIT_BUREAU_WEEK')
# 结论：AMT_REQ_CREDIT_BUREAU_WEEK，测试集标签大于1的数据太少，删除。

# 120、AMT_REQ_CREDIT_BUREAU_MON 
FeatureView('AMT_REQ_CREDIT_BUREAU_MON')
# 结论：AMT_REQ_CREDIT_BUREAU_MON，测试集标签大于1的数据太少，删除。

# 121、AMT_REQ_CREDIT_BUREAU_QRT 
FeatureView('AMT_REQ_CREDIT_BUREAU_QRT')
# 结论：AMT_REQ_CREDIT_BUREAU_QRT，大于2的赋值为2

# 122、AMT_REQ_CREDIT_BUREAU_YEAR 
FeatureView('AMT_REQ_CREDIT_BUREAU_YEAR')
# 结论：AMT_REQ_CREDIT_BUREAU_YEAR，有区分度。大于6赋值为7

## bureau----
bureau <- fread('./data/bureau.csv')

# 1、SK_ID_CURR
nrow(bureau) == bureau$SK_ID_CURR %>% unique() %>% length()
match(application_train$SK_ID_CURR, bureau$SK_ID_CURR %>% unique()) %>% is.na() %>% table()
match(application_test$SK_ID_CURR, bureau$SK_ID_CURR %>% unique()) %>% is.na() %>% table()
# 结论：SK_ID_CURR不唯一, 对应的训练集和测书记数据都有14%左右数据缺失

# 2、SK_ID_BUREAU
nrow(bureau) == bureau$SK_ID_BUREAU %>% unique() %>% length()
# 结论：SK_ID_BUREAU唯一

# 3、CREDIT_ACTIVE
table(bureau$CREDIT_ACTIVE)
# 结论：

# 4、CREDIT_CURRENCY
table(bureau$CREDIT_CURRENCY)

# 5、DAYS_CREDIT
quantile(bureau$DAYS_CREDIT)

# 6、CREDIT_DAY_OVERDUE
nrow(bureau[CREDIT_DAY_OVERDUE > 0])
quantile(bureau[CREDIT_DAY_OVERDUE > 0]$CREDIT_DAY_OVERDUE)
application_train[SK_ID_CURR %in% bureau[CREDIT_DAY_OVERDUE > 0, unique(SK_ID_CURR)], table(TARGET)]
# 结论：是否存在CREDIT_DAY_OVERDUE > 0的SK_ID_CURR是一个有区分度的特征

# 7、DAYS_CREDIT_ENDDATE
nrow(bureau[is.na(DAYS_CREDIT_ENDDATE)])
application_train[SK_ID_CURR %in% bureau[DAYS_CREDIT_ENDDATE < 0, unique(SK_ID_CURR)], table(TARGET)]
application_train[! (SK_ID_CURR %in% bureau[DAYS_CREDIT_ENDDATE < 0, unique(SK_ID_CURR)]), table(TARGET)]
# 结论：DAYS_CREDIT_ENDDATE的正负是一个有区分度的特征

# 8、DAYS_ENDDATE_FACT
nrow(bureau[is.na(DAYS_ENDDATE_FACT)])

# 9、AMT_CREDIT_MAX_OVERDUE
application_train[SK_ID_CURR %in% bureau[AMT_CREDIT_MAX_OVERDUE > 0, unique(SK_ID_CURR)], table(TARGET)]
# 结论：是否有过AMT_CREDIT_MAX_OVERDUE > 0的记录是一个有区分度的特征

# 10、CNT_CREDIT_PROLONG
nrow(bureau[CNT_CREDIT_PROLONG > 0])
application_train[SK_ID_CURR %in% bureau[CNT_CREDIT_PROLONG > 0, unique(SK_ID_CURR)], table(TARGET)]
# 结论：是否有过信用贷延期是一个有区分度的特征

# 11、AMT_CREDIT_SUM
application_train[SK_ID_CURR %in% bureau[AMT_CREDIT_SUM == 0, unique(SK_ID_CURR)], table(TARGET)]
application_train[SK_ID_CURR %in% bureau[AMT_CREDIT_SUM > 0, unique(SK_ID_CURR)], table(TARGET)]
application_train[! (SK_ID_CURR %in% bureau[AMT_CREDIT_SUM > 0, unique(SK_ID_CURR)]), table(TARGET)]
# 结论：是否存在AMT_CREDIT_SUM > 0的SK_ID_BUREAU是一个有区分度的特征。

# 12、AMT_CREDIT_SUM_DEBT
application_train[! (SK_ID_CURR %in% bureau[AMT_CREDIT_SUM_DEBT > 0, unique(SK_ID_CURR)]), table(TARGET)]
# 结论：是否存在AMT_CREDIT_SUM_DEBT > 0的SK_ID_BUREAU是一个有区分度的特征。

# 13、AMT_CREDIT_SUM_LIMIT
application_train[! (SK_ID_CURR %in% bureau[AMT_CREDIT_SUM_LIMIT > 0, unique(SK_ID_CURR)]), table(TARGET)]

# 14、AMT_CREDIT_SUM_OVERDUE
application_train[! (SK_ID_CURR %in% bureau[AMT_CREDIT_SUM_OVERDUE > 0, unique(SK_ID_CURR)]), table(TARGET)]

# 15、CREDIT_TYPE
table(bureau[, CREDIT_TYPE])

# 16、DAYS_CREDIT_UPDATE
application_train[! (SK_ID_CURR %in% bureau[AMT_CREDIT_SUM_OVERDUE < -400, unique(SK_ID_CURR)]), table(TARGET)]
# 结论：是否存在AMT_CREDIT_SUM_DEBT < -400的SK_ID_BUREAU是一个有区分度的特征

# 17、AMT_ANNUITY
bureau[, table(is.na(AMT_ANNUITY))]
bureau[AMT_ANNUITY > 0, quantile(AMT_ANNUITY)]
application_train[! (SK_ID_CURR %in% bureau[AMT_ANNUITY > 0, unique(SK_ID_CURR)]), table(TARGET)]
# 结论：是否存在AMT_ANNUITY > 0的SK_ID_BUREAU是一个有区分度的特征


## bureau_balance----
bureau_balance <- fread('./data/bureau_balance.csv')
bureau_balance$SK_ID_BUREAU %>% unique %>% length
bureau_balance[, table(STATUS)]
# 在bureau中有些SK_ID_BUREAU在bureau_balance中没有，即对应的bureau_balance的特征缺失。

## POS_CASH_balance----
POS_CASH_balance <- fread('./data/POS_CASH_balance.csv')

# 1、SK_ID_PREV
POS_CASH_balance[, any(duplicated(SK_ID_PREV))]
