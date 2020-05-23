install_load <- function(pkg){
  # Load packages & Install them if needed.
  # CODE SOURCE: https://gist.github.com/stevenworthington/3178163
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
}

# required packages
packages <- c("tidyverse","knitr",  "mice", "VIM", "RCurl", "knitcitations", 
              "janitor", "missForest", "DMwR", "splitstackshape", "car")

install_load(packages)

# Read data
url_train <- paste0("https://raw.githubusercontent.com/kaiserxc/DATA621FinalProject/",
                    "master/house-prices-advanced-regression-techniques/train.csv")
url_test <-  paste0("https://raw.githubusercontent.com/kaiserxc/DATA621FinalProject/",
                    "master/house-prices-advanced-regression-techniques/test.csv")

stand_read <- function(url){
  return(read.csv(text = getURL(url)))
}

o_train <- 
  stand_read(url_train) %>% 
  mutate(d_name = 'train')
o_test <- stand_read(url_test) %>% 
  mutate(SalePrice = NA, d_name = 'test')

full_set <- rbind(o_train, o_test)

na_review <- function(df){
  # returns df of vars w/ NA qty desc.
  na_qty <- colSums(is.na(df)) %>% as.data.frame(stringsAsFactors=F)
  colnames(na_qty) <- c("NA_qty")
  na_qty <- cbind('Variable' = rownames(na_qty), na_qty) %>% 
    select(Variable, NA_qty)
  rownames(na_qty) <- NULL
  
  na_qty <- na_qty %>% 
    arrange(desc(NA_qty)) %>% filter(NA_qty > 0) %>% 
    mutate(Variable = as.character(Variable)) %>% 
    mutate(Pct_of_Tot =  round(NA_qty/nrow(df), 4) * 100)
  
  return(na_qty)
}

first_pass <- 
  full_set %>% 
  # first_pass is train.csv and test.csv combined for NA reviews 
  # and imputation planning and calculated columns
  mutate(House_Age_Yrs = YrSold - YearBuilt, 
         RemodAdd_Age_Yrs = YrSold - YearRemodAdd, 
         Garage_Age_Yrs = YrSold - GarageYrBlt) 

naVars <- na_review(first_pass %>% select(-SalePrice))
naVars


set_aside <- c(2600, 2504, 2421, 2127, 2041, 2186, 2525, 1488, 949, 2349, 2218, 2219, 333)
set_asideA <- '2600|2504|2421|2127|2041|2186|2525|1488|949|2349|2218|2219|333' # 13
set_asideB <- '|2550|524|2296|2593' # negative values in '_Age' columns

x <- first_pass %>% 
  # exclude set_aside observations to fill in known NA's
  filter(!grepl(paste0(set_asideA, set_asideB), Id))

naVarsx <- na_review(x %>% select(-SalePrice))
naVarsx

obtain_data <- function(df){
  # like first_pass but with imputation that addresses 
  # observations that have known NA's
  df %>%
    mutate(PoolQC = fct_explicit_na(PoolQC, na_level='NoP'),
           MiscFeature = fct_explicit_na(MiscFeature, na_level='NoM'),
           Alley = fct_explicit_na(Alley, na_level='NoA'),
           Fence = fct_explicit_na(Fence, na_level = 'NoF'),
           FireplaceQu = fct_explicit_na(FireplaceQu, na_level = 'NoFp'), 
           LotFrontage = ifelse(is.na(LotFrontage), 0, LotFrontage),
           
           # Note GarageYrBlt set to 9999 may be a problem
           GarageYrBlt = ifelse(is.na(GarageYrBlt), 9999, GarageYrBlt), 
           GarageFinish = fct_explicit_na(GarageFinish, na_level = 'NoG'), 
           GarageQual = fct_explicit_na(GarageQual, na_level = 'NoG'), 
           GarageCond = fct_explicit_na(GarageCond, na_level = 'NoG'), 
           # NOTE: Garage_Age_Yrs: 0 doesn't seem appropriate... 
           Garage_Age_Yrs = ifelse(is.na(Garage_Age_Yrs), 0, Garage_Age_Yrs),
           GarageType = fct_explicit_na(GarageType, na_level = 'NoG'), 
           
           BsmtQual = fct_explicit_na(BsmtQual, na_level = 'NoB'),
           BsmtCond = fct_explicit_na(BsmtCond, na_level = 'NoB'),
           BsmtExposure = fct_explicit_na(BsmtExposure, na_level = 'NoB'),
           BsmtFinType1 = fct_explicit_na(BsmtFinType1, na_level = 'NoB'),
           BsmtFinType2 = fct_explicit_na(BsmtFinType2, na_level = 'NoB')
    )
}

probl_obs <- full_set %>% 
  mutate(House_Age_Yrs = YrSold - YearBuilt, 
         RemodAdd_Age_Yrs = YrSold - YearRemodAdd, 
         Garage_Age_Yrs = YrSold - GarageYrBlt) %>% 
  filter(grepl(paste0(set_asideA, set_asideB), Id))

known_obs <- full_set %>% 
  filter(!grepl(paste0(set_asideA, set_asideB), Id)) %>% 
  mutate(House_Age_Yrs = YrSold - YearBuilt, 
         RemodAdd_Age_Yrs = YrSold - YearRemodAdd, 
         Garage_Age_Yrs = YrSold - GarageYrBlt)


full_set_clean <- rbind(obtain_data(known_obs), probl_obs) %>% arrange(Id)
str(full_set_clean)

#View(full_set_clean)
#summary(full_set_clean)
naVarsy <- na_review(full_set_clean %>% select(-SalePrice))
sum(naVarsy$NA_qty) # 176

# ord_vars per the Data Dictionary.  
ord_vars <- c("LotShape","Utilities", "LandSlope", "ExterQual", 
              "ExterCond", "BsmtQual", "BsmtCond", "BsmtExposure",
              "BsmtFinType1", "BsmtFinType2", "HeatingQC", "Electrical",
              "KitchenQual", "Functional", "FireplaceQu", "GarageFinish",
              "GarageQual", "GarageCond", "PavedDrive", "PoolQC", "Fence")

# Order of levels for ordinal variables 
# all are ordered most favorible to least favorible, below
LotShape_ <- c("Reg", "IR1", "IR2", "IR3")        # needs repair
Utilities_ <- c("AllPub", "NoSeWa")               # ok - No "NoSewr", "ELO"
LandSlope_ <- c("Gtl","Mod", "Sev")               # ok
ExterQual_ <- c("Ex", "Gd", "TA", "Fa")           # needs repair - No "Po"

ExterCond_ <- c("Ex", "Gd", "TA", "Fa", "Po")     # needs repair
BsmtQual_ <- c("Ex", "Gd", "TA", "Fa", "NoB")     # needs repair
BsmtCond_ <- c("Gd", "TA", "Fa", "NoB")           # needs repair
BsmtExposure_ <- c("Gd", "Av", "Mn", "No", "NoB") # needs repair

BsmtFinType1_ <- c("GLQ", "ALQ", "BLQ", 
                   "Rec", "LwQ", "Unf", "NoB")    # needs repair
BsmtFinType2_ <- c("GLQ", "ALQ", "BLQ", 
                   "Rec", "LwQ", "Unf", "NoB")    # needs repair
HeatingQC_ <- c("Ex", "Gd", "TA", "Fa", "Po")     # needs repair 
Electrical_ <- c("SBrkr", "FuseA", "FuseF",
                 "FuseP", "Mix")                  # needs repair

KitchenQual_ <- c("Ex", "Gd", "TA", "Fa")         # needs repair - no "Po"
Functional_ <- c("Typ", "Min1", "Min2", "Mod",
                 "Maj1", "Maj2", "Sev")           # needs repair - no "Sal"
FireplaceQu_ <- c("Ex", "Gd", "TA", "Fa", 
                  "Po", "NoFp")                   # needs repair
GarageFinish_ <- c("Fin", "RFn", "Unf", "NoG")    # ok

GarageQual_ <- c("Ex", "Gd", "TA", "Fa", "Po", 
                 "NoG")                           # needs repair
GarageCond_ <- c("Ex", "Gd", "TA", "Fa", "Po", 
                 "NoG")                           # needs repair
PavedDrive_ <- c("Y", "P", "N")                   # needs repair
PoolQC_ <- c("Ex", "Gd", "Fa", "NoP")             # needs repair - no "TA"
Fence_ <- c("GdPrv", "MnPrv", "GdWo", "MnWw",
            "NoF")                                # needs repair

# list of lists of the correct factor levels
n_levels <- list(LotShape_, Utilities_,  LandSlope_,  ExterQual_,  
                 ExterCond_,  BsmtQual_,  BsmtCond_,  BsmtExposure_, 
                 BsmtFinType1_,  BsmtFinType2_,  HeatingQC_,  Electrical_, 
                 KitchenQual_,  Functional_,  FireplaceQu_,  GarageFinish_, 
                 GarageQual_,  GarageCond_,  PavedDrive_,  PoolQC_,  Fence_)
names(n_levels) <- ord_vars                       # name vars so I can index

relevel_data <- function(df, ord_list, new_lvls){
  # updates factor cols df[ord_list] with new_lvls (list of lists)
  i = sapply(colnames(full_set_clean), 
             function (x) x %in% ord_list)        # obtain order list cols
  df[i] = lapply(df[i], as.character)             # convert factors to char
  
  for(s_var in ord_list){                         # correct levels 
    df[[s_var]] = factor(df[[s_var]], rev(new_lvls[[s_var]]))
  }
  return(df)
}

full_set_clean <- relevel_data(full_set_clean, ord_vars, n_levels)

var_types <- function(df){
  # returns df of Variable name and Type from df
  var_df <- sapply(df, class) %>% as.data.frame()
  colnames(var_df) <- c("Var_Type")
  var_df <- cbind(var_df, 'Variable' = rownames(var_df)) %>% 
    select(Variable, Var_Type) %>% 
    mutate(Variable = as.character(Variable),Var_Type = as.character(Var_Type))
  return(var_df)
}

var_review <- 
  var_types(full_set_clean %>% 
              select(-c(Id,SalePrice,d_name)))

fac_vars <- var_review %>% 
  filter(Var_Type == 'factor') %>% 
  select(Variable) %>% 
  t() %>% 
  as.character() 

# 43 total length(fac_vars)
num_vars <- var_review %>% 
  filter(grepl('character|integer|numeric', Var_Type)) %>% 
  select(Variable) %>% t() %>% as.character() # 39 total but see GarageYrBlt 

sum(complete.cases(full_set %>% select(-SalePrice)))       # 0
sum(complete.cases(full_set_clean %>% select(-SalePrice))) # 2,861 ~ 98%
nrow(full_set_clean) - 2861 # 58 NA
stat_info <- psych::describe(full_set_clean %>% select(num_vars, -Id, -d_name))
stat_info[c(2:nrow(stat_info)),c(2:5,8:9,13:ncol(stat_info)-1)]

train_data <- full_set_clean %>% filter(d_name == 'train') %>% select(-d_name)
test_data <- full_set_clean %>% filter(d_name == 'test') %>% select(-d_name)

##View(train_data)
dim(train_data)
dim(test_data)

dplyr::filter(full_set_clean, 
              House_Age_Yrs < 0 | RemodAdd_Age_Yrs < 0 | Garage_Age_Yrs < 0) %>% 
  dplyr::select(YrSold, YearBuilt, YearRemodAdd, House_Age_Yrs, GarageYrBlt, 
                RemodAdd_Age_Yrs, Garage_Age_Yrs) %>% 
  kable(caption = "Table 3.1: Invalid Negative Values")

# Mutute Variables
# bc of the new Age vars, remove the YearBuilt, YearRemodAdd, GarageYrBlt 
# set negative Ages to zero, scaled the YrSold, MoSold as a factor
full_set_clean_kyle <- 
  full_set_clean %>% 
  arrange(desc(d_name)) %>% 
  dplyr::select(-c(Id, YearBuilt, YearRemodAdd, GarageYrBlt, d_name)) %>% 
  mutate(
    House_Age_Yrs = pmax(0, House_Age_Yrs),
    RemodAdd_Age_Yrs = pmax(0, RemodAdd_Age_Yrs),
    Garage_Age_Yrs = pmax(0, Garage_Age_Yrs),
    YrSold = as.ordered(YrSold),
    MoSold = as.ordered(MoSold),
    MSSubClass = as.factor(MSSubClass)
  )

factor_differences <- 
  full_set_clean %>% 
  mutate(d_name = factor(d_name)) %>%   
  select_if(is.factor) %>% 
  #na.omit() %>% 
  reshape2::melt(id.var = "d_name") %>% 
  group_by(d_name, variable) %>% 
  summarise(unique_values = length(na.omit(unique(value)))) %>% 
  spread(key = d_name, value = unique_values) %>% 
  dplyr::filter(test != train) %>% 
  left_join(
    gather(full_set_clean) %>% 
      group_by(key) %>% 
      summarize(NAs = sum(as.integer(is.na(value)))) %>% 
      dplyr::select(variable = key, NAs)
  )

kable(factor_differences, 
      caption = "Table 3.2: Differences in Factor Values between Test & Training Sets")

#combine data sets for imputation
predictors_for_imputation <- 
  full_set_clean_kyle %>% 
  dplyr::select(-SalePrice)

# https://www.rdocumentation.org/packages/VIM/versions/4.7.0/topics/aggr
missing_plot <- VIM::aggr(predictors_for_imputation,
                          #numbers = T,
                          sortVars = T,
                          combine = T,
                          col = c("#d3fbea", "#176171", "orange"),
                          labels=str_sub(names(predictors_for_imputation), 1, 8),
                          ylab="Figure 3.1: Missing Values in Train Set"
)

kable(data.frame(complete_cases_pct = missing_plot$percent[1]),
      caption = "Table 3.3 % of Complete Cases",
      digits = 1)

dtypes <- rapply(predictors_for_imputation, class)
dtypes <- data.frame(
  Variable = names(dtypes),
  dtype = dtypes
)

missing_summary <- 
  missing_plot$missings %>% 
  arrange(-Count) %>% 
  janitor::adorn_totals() %>% 
  mutate(
    pct_missing = Count / nrow(predictors_for_imputation) * 100
  ) %>%
  filter(pct_missing > 0) %>% 
  left_join(dtypes) 

missing_summary[nrow(missing_summary), "pct_missing"] <- NA

kable(missing_summary, digits = 3, row.names = T, 
      caption = "Table 3.4 Missing Values by Variable")  

if (!exists("predictors_imputed")){
  #https://www.rdocumentation.org/packages/mice/versions/2.46.0/topics/mice
  mice_mod <- mice(predictors_for_imputation, m = 1, method = "cart", seed = 5) 
  predictors_imputed <- mice::complete(mice_mod)
}

full_set_imputed <- 
  predictors_imputed %>% 
  mutate(SalePrice = full_set_clean_kyle$SalePrice) %>% 
  droplevels()

train_data_imputed <- 
  full_set_imputed[1:nrow(train_data), ] 

test_data_imputed <- 
  full_set_imputed[nrow(train_data) + 1:nrow(test_data), ] %>% 
  dplyr::select(-SalePrice) 

# Visualize the imputations
# SOURCE: https://stackoverflow.com/questions/12056989/
# density-plots-with-multiple-groups?utm_medium=organic&
# utm_source=google_rich_qa&utm_campaign=google_rich_qa

# Melt into long format
# Add a variable for the plot legend
mice_data <- mice::complete(mice_mod, "long", include = TRUE)
mice_mod_viz <- 
  mice_data %>% 
  select_if(is.numeric) %>% 
  mutate(Imputed = ifelse(mice_data$.imp == "0", "Observed", "Imputed")) %>%   
  reshape2::melt("Imputed") %>% 
  na.omit()

if (!exists("mice_density_plot")){
  mice_density_plot <- 
    ggplot(mice_mod_viz, aes(x=value, colour = factor(Imputed))) + 
    stat_density(geom = "path") +
    facet_wrap(~variable, scales="free") +
    labs(title = "Figure 3.2: Denisity plots of Observed & Imputed Values")
}
mice_density_plot
stripplot(mice_mod, pch = 20, cex = 1.2, 
          main = "Figure 3.3: Strip Plots of Observed & Imputed Values")

# http://web.maths.unsw.edu.au/~dwarton/missingDataLab.html

### Side-by-Side Boxplots of Categorical Variables
# create data
boxplot_data <- 
  train_data_imputed %>% 
  select_if(function(x) !is.numeric(x)) %>% 
  mutate(SalePrice = train_data_imputed$SalePrice) %>% 
  reshape2::melt(id.vars = "SalePrice")

### Boxplots
ggplot(data = boxplot_data, aes(x = value, y = SalePrice)) +
  geom_boxplot() +
  facet_wrap( ~ variable, scales = "free") +
  coord_flip() +
  labs(title = paste0("Figure 3.4: Side-by-Side Box Plots of the Categorical Variables ",
                      "versus the Response"))

# Reference: https://stackoverflow.com/questions/14604439/
# plot-multiple-boxplot-in-one-graph?utm_medium=organic&utm_source=
# google_rich_qa&utm_campaign=google_rich_qa

## CORRELATIONS
# correlation matrix

train_data_numeric <- 
  train_data_imputed %>% 
  select_if(is.numeric)

cm <- cor(train_data_numeric, use = "pairwise.complete.obs")

#plot
corrplot::corrplot(cm, method = "square", type = "upper")

#find the top correlations
correlation_df <- function(cm){
  #Creates a df of pairwise correlations
  correlations <- c(cm[upper.tri(cm)])
  cor_df <- data.frame(
    Var1 = rownames(cm)[row(cm)[upper.tri(cm)]],
    Var2 = colnames(cm)[col(cm)[upper.tri(cm)]],
    Correlation = correlations,
    Rsquared = correlations^2
  ) %>% 
    arrange(-Rsquared)
  return(cor_df)
}

cor_df <- correlation_df(cm)
kable(head(cor_df, 10), digits = 2, row.names = T, 
      caption = "Top Correlated Variable Pairs")
kable(head(dplyr::filter(cor_df, Var1 == "SalePrice" | Var2 == "SalePrice"  ), 10), 
      digits = 2, row.names = T, caption = "Top Correlated Variable Pairs")

# Reference: https://stackoverflow.com/questions/28035001/
# transform-correlation-matrix-into-dataframe-with-records-for-each-row-column-pai

### CORRELATIONS WITH RESPONSE
pred_vars <- dplyr::select(train_data_numeric, -SalePrice)

# categorical_dummy_vars
categorical_vars <-
  train_data_imputed %>%
  select_if(function(x) !is.numeric(x)) %>%
  mutate(SalePrice = train_data_imputed$SalePrice)

categorical_dummy_vars <-
  model.matrix(SalePrice ~ ., data = categorical_vars) %>%
  data.frame() %>%
  dplyr::select(-X.Intercept.)

#squared variables
squared_vars <-
  apply(pred_vars, 2, function(x) x^2) %>%
  as.data.frame()
colnames(squared_vars) <- paste0(names(squared_vars), "_2")

#square root variables
sqrt_vars <-
  apply(pred_vars, 2, function(x) x^2) %>%
  as.data.frame()
colnames(sqrt_vars) <- paste0(names(sqrt_vars), "_sqrt")

#log variables
log_vars <-
  apply(pred_vars, 2, function(x) log(x + .01)) %>%
  as.data.frame()
colnames(log_vars) <- paste0(names(log_vars), "_log")

#combine all transformed variables
individual_vars <- cbind(categorical_dummy_vars, 
                         squared_vars, 
                         sqrt_vars, 
                         log_vars, 
                         pred_vars) 

# create interaction variables
# https://stackoverflow.com/questions/2080774/
# generating-interaction-variables-in-r-dataframes?
# utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
if (!exists("all_interactions")){
  all_interactions <- data.frame(t(apply(individual_vars, 1, combn, 2, prod)))
  colnames(all_interactions) <- combn(names(individual_vars), 2, paste, collapse=":")
}

# combine the individual variables and interactions
all_predictors <- cbind(individual_vars, all_interactions)

# response variable transformations
response_transformed <- 
  train_data_numeric %>% 
  transmute(
    SalePrice = SalePrice,
    SalePrice_2 = SalePrice^2,
    SalePrice_sqrt = sqrt(SalePrice),
    SalePrice_log = log(SalePrice)
  )

# create pairwise correlation df
if (!exists("response_correlations")){
  response_correlations <-  
    cor(response_transformed, all_predictors, use = "pairwise.complete.obs") %>% 
    correlation_df() %>% 
    na.omit()
}

n_rows <- 50
kable(head(dplyr::filter(response_correlations, Var1 == "SalePrice_sqrt"), n_rows), 
      digits = 3, 
      caption = "Table 3.5: Top Correlations with the Original Response Variable")

# 1. Original Variables Imputed
# divide into training & test
train_orig_vars_imputed <- full_set_imputed[1:nrow(train_data), ] 

test_orig_vars_imputed <- 
  full_set_imputed[nrow(train_data) + 1:nrow(test_data), ]

# 2. Several Predictor Transformations, including
# 7 categorical re-classifications & 5 interactions
full_set_predictors_transformed <- 
  full_set_imputed %>% 
  mutate(
    RoofMatl_WdShngl = as.integer(RoofMatl == "WdShngl"),
    FireplaceQu_Ex = as.integer(FireplaceQu == "Ex"),
    HeatingQC_Ex = as.integer(HeatingQC == "Ex"),
    GarageQual_abv_avg = as.integer(GarageQual %in% c("TA", "Gd", "Ex")),
    PoolQC_Ex = as.integer(PoolQC == "Ex"),
    Heating_Gas = as.integer(Heating %in% c("GasA", "GasW")),
    SaleCondition_Partial = as.integer(SaleCondition == "Partial"),
    OverallQual2_x_GarageCars = OverallQual^2 * GarageCars,
    OverallQual2_x_TotRmsAbvGrd_log = OverallQual^2 * log(TotRmsAbvGrd),
    OverallQual2_x_GrLivArea = OverallQual^2 * GrLivArea,
    OverallQual2_x_LotArea_log = OverallQual^2 * log(LotArea),
    OverallQual_2 = OverallQual^2
  ) %>% 
  dplyr::select(-c(RoofMatl, FireplaceQu, HeatingQC, GarageQual, PoolQC, 
                   SaleCondition, Heating))

#divide into training & test
train_predictors_transformed <- full_set_predictors_transformed[1:nrow(train_data), ] 

test_predictors_transformed <- 
  full_set_predictors_transformed[nrow(train_data) + 1:nrow(test_data), ] 


#3. Box-cox response transformation added to the existing predictor transformations
lmod <- lm(SalePrice ~ ., data = train_predictors_transformed)
n <- nrow(train_predictors_transformed)

if (!exists("BIC_lmod")) BIC_lmod <- step(lmod, trace = 0, k = log(n))

PT <- car::powerTransform(as.formula(BIC_lmod$call), data = train_predictors_transformed)

train_BC_transformed <- 
  train_predictors_transformed %>% 
  mutate(SalePrice_BC = SalePrice^PT$lambda) %>% 
  dplyr::select(-SalePrice)

# setwd("C:\\Users\\kyleg\\DATA621FinalProject\\data-imputed-transformed\\")
# write.csv(train_orig_vars_imputed, "train_orig_vars_imputed.csv")
# write.csv(train_predictors_transformed, "train_predictors_transformed.csv")
# write.csv(train_BC_transformed, "train_BC_transformed.csv")
# write.csv(test_orig_vars_imputed, "test_orig_vars_imputed.csv")
# write.csv(test_predictors_transformed, "test_predictors_transformed.csv")


# Read prepared data
bcData = read.csv(paste0('https://raw.githubusercontent.com/kaiserxc/',
                         'DATA621FinalProject/',
                         'master/data-imputed-transformed/train_BC_transformed.csv'))
bcData$X = NULL
imputedData = read.csv(paste0('https://raw.githubusercontent.com/kaiserxc/',
                              'DATA621FinalProject/master/data-imputed-transformed/',
                              'train_orig_vars_imputed.csv'))
imputedData$X = NULL
transformedData = read.csv(paste0('https://raw.githubusercontent.com/kaiserxc/',
                                  'DATA621FinalProject/master/data-imputed-transformed/',
                                  'train_predictors_transformed.csv'))
transformedData$X = NULL

library(psych)
describe(bcData)

m1BC = lm(data=bcData,formula =SalePrice_BC~. )
m1IMP = lm(data = imputedData, formula = SalePrice~.)
anova(m1IMP,m1TD)
m1TD = lm(data=transformedData,formula = SalePrice~.)
m2BCstep =step(m1BC,direction = 'backward', trace=0)
summary(m2BCstep)
m3BC = lm(data = bcData, formula = SalePrice_BC~OverallCond+Condition2+Condition1+
            Neighborhood+MSZoning +X1stFlrSF+X2ndFlrSF+LowQualFinSF+KitchenQual+
            Fireplaces +ScreenPorch+House_Age_Yrs+RoofMatl_WdShngl+
            GarageQual_abv_avg +OverallQual2_x_GrLivArea+
            OverallQual2_x_TotRmsAbvGrd_log+OverallQual2_x_GarageCars) 
m4BC = lm(data = bcData, formula = SalePrice_BC~OverallCond+Condition2+Condition1+
            Neighborhood+MSZoning +X1stFlrSF+X2ndFlrSF+LowQualFinSF+KitchenQual+
            Fireplaces+WoodDeckSF+Functional+FullBath+BsmtFullBath+BsmtFinType1+
            BsmtExposure +BsmtQual +LandSlope +LandContour+LotArea +LotFrontage+ 
            LotConfig + Utilities + HouseStyle + RoofStyle + MasVnrArea +
            ScreenPorch+House_Age_Yrs+RoofMatl_WdShngl+GarageQual_abv_avg +
            OverallQual2_x_GrLivArea+OverallQual2_x_TotRmsAbvGrd_log+
            OverallQual2_x_GarageCars) 
m5imp = lm(data = imputedData, formula = log(SalePrice)~OverallCond+Condition2+
             Condition1+Neighborhood+MSZoning +X1stFlrSF+X2ndFlrSF+LowQualFinSF+
             KitchenQual+(Fireplaces)^2+WoodDeckSF+Functional+FullBath+
             BsmtFullBath+BsmtFinType1 + BsmtExposure +BsmtQual +LandSlope +
             LandContour+log(LotArea) + LotFrontage+ LotConfig + Utilities + 
             HouseStyle + RoofStyle + MasVnrArea +ScreenPorch+House_Age_Yrs) 
m6TD = lm(log(SalePrice)~OverallCond+Condition2+
            Condition1+Neighborhood+MSZoning +X1stFlrSF+X2ndFlrSF+LowQualFinSF+
            KitchenQual+Fireplaces+WoodDeckSF+Functional+FullBath+BsmtFullBath+
            BsmtFinType1 + BsmtExposure +BsmtQual +LandSlope +LandContour+
            log(LotArea) + LotFrontage+ LotConfig + Utilities + HouseStyle + 
            RoofStyle + MasVnrArea +ScreenPorch+House_Age_Yrs+RoofMatl_WdShngl+
            GarageQual_abv_avg +OverallQual2_x_GrLivArea+
            OverallQual2_x_TotRmsAbvGrd_log+OverallQual2_x_GarageCars,
          data = transformedData)  

# Get AIC
AIC (m1BC, m2BCstep, m3BC, m4BC, m5imp, m6TD)

summary(m1BC)
summary(m2BCstep)
summary(m3BC)
summary(m4BC)
summary(m5imp)
summary(m6TD)

# Read test data
transformedTest = read.csv(paste0('https://raw.githubusercontent.com/kaiserxc/',
                                  'DATA621FinalProject/master/data-imputed-transformed/',
                                  'test_predictors_transformed.csv'))
index <- transformedTest$X
transformedTest$X <- NULL

# Tune model and run prediction
library(caret)
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
model_fit <- train(log(SalePrice)~OverallCond+Condition2+
                     Condition1+Neighborhood+MSZoning +X1stFlrSF+X2ndFlrSF+LowQualFinSF+
                     KitchenQual+Fireplaces+WoodDeckSF+Functional+FullBath+BsmtFullBath+
                     BsmtFinType1 + BsmtExposure +BsmtQual +LandSlope +LandContour+
                     log(LotArea) + LotFrontage+ LotConfig + Utilities + HouseStyle + 
                     RoofStyle + MasVnrArea +ScreenPorch+House_Age_Yrs+RoofMatl_WdShngl+
                     GarageQual_abv_avg +OverallQual2_x_GrLivArea+
                     OverallQual2_x_TotRmsAbvGrd_log+OverallQual2_x_GarageCars,  
                   data=transformedData, method="lm", trControl = ctrl, tuneLength = 5)
pred <- predict(model_fit, newdata=transformedTest)
results <- cbind(index, exp(pred))
write.csv(results, "c://temp//results_tune.csv", row.names = FALSE)

summary(model_fit)

library(ggplot2)
library(ggfortify)
autoplot(m6TD2)

library(car)
vif(m6TD2)
alias(m6TD2)

transformedData2 <- transformedData[-c(826,524,1299,89),]
m6TD2 = lm(log(SalePrice)~OverallCond+ # Condition2+
             Condition1+
             MSZoning +X1stFlrSF+X2ndFlrSF+LowQualFinSF+Neighborhood+
             KitchenQual+Fireplaces+WoodDeckSF+Functional+FullBath+BsmtFullBath+
             BsmtFinType1 + BsmtExposure +BsmtQual +
             LandSlope +LandContour+
             log(LotArea) + LotFrontage+ LotConfig + HouseStyle + #Utilities + 
             RoofStyle + MasVnrArea +ScreenPorch+House_Age_Yrs + RoofMatl_WdShngl+
             GarageQual_abv_avg +OverallQual2_x_GrLivArea+
             OverallQual2_x_TotRmsAbvGrd_log+OverallQual2_x_GarageCars,
           data = transformedData2)  
summary(m6TD2)
pred <- predict(m6TD2, newdata=transformedTest)
results <- cbind(index, exp(pred))
write.csv(results, "c://temp//results_m5TD2.csv", row.names = FALSE)

AIC(m6TD2)

table(transformedData2$OverallCond)
table(transformedData2$Condition1)
table(transformedData2$Condition2) # Removed
table(transformedData2$MSZoning)
table(transformedData2$Neighborhood)
table(transformedData2$KitchenQual)
table(transformedData2$Fireplaces)
table(transformedData2$WoodDeckSF)
table(transformedData2$FullBath)
table(transformedData2$BsmtFullBath)
table(transformedData2$BsmtFinType1)
table(transformedData2$BsmtExposure)
table(transformedData2$BsmtQual)
table(transformedData2$LandSlope)
table(transformedData2$LandContour)
table(transformedData2$LotConfig)
table(transformedData2$Utilities) # Removed
table(transformedData2$HouseStyle)
table(transformedData2$RoofStyle)
table(transformedData2$RoofMatl_WdShngl)
table(transformedData2$GarageQual_abv_avg)
table(transformedData2$MasVnrArea)

ce <- as.data.frame(m6TD2$coefficients)
colnames(ce) <- c("Coefficient")
#write.csv(round(ce, 6), "c://temp//embedded_table2_coef.csv", row.names = TRUE)

