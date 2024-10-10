df <- read_csv("dataset/Wine_UCI.csv", col_names = FALSE)
colnames(df) <- c("Origin", "Alcohol", "Malic_acid", "Ash", "Alkalinity_of_ash", 
                  "Magnesium", "Total_phenols", "Flavanoids", "Nonflavonoids_phenols", 
                  "Proanthocyanins", "Color_intensity", "Hue", "OD280_OD315_diluted_wines", 
                  "Proline")

glimpse(df)

##################  The origin is our dependent variable. Let’s make it a factor.

df$Origin <- as.factor(df$Origin)

#Let's check our explained variable distribution of origin
round(prop.table(table(df$Origin)), 2)

###########################  That’s nice, our explained variable is almost equally distributed with the 3 set of origin.

# Let's also check if we have any NA values

summary(df)


############### Here we noticed that the range of values in our variable is quite wide. It means our data will need to be standardize. We also note that we no “NA” values. That’s quite a nice surprise!

#################### Understand the data

########### We first slide our data in a training and testing set.

df2 <- df
param_split_df2 <- createDataPartition(df2$Origin, p = 0.75, list = FALSE)

train_df2 <- df2[param_split_df2, ]
test_df2 <- df2[-param_split_df2, ]

############ The great with caret is we can standardize our data in the the training phase.

trnctrl_df2 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model_knn_df2 <- train(Origin ~., data = train_df2, method = "knn", 
                       trControl = trnctrl_df2, 
                       preProcess = c("center", "scale"),  
                       tuneLength = 10)
plot(model_knn_df2)
################# Let’s use our model to make our prediction

prediction_knn_df2 <- predict(model_knn_df2, newdata = test_df2)

confusionMatrix(prediction_knn_df2, reference = test_df2$Origin)

