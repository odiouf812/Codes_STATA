df <- read.csv(file.choose(), header = TRUE)
df
glimpse(df)
*************************************Change the diagnosis result into a factor, then remove the ID variable as it does not bring anything.
df$diagnosis_result <- factor(df$diagnosis_result, levels = c("B", "M"),labels = c("Benign", "Malignant"))

library(magrittr)

df2 <- df %>% select(-id)

df2

# Checking how balance is the dependend variable 

prop.table(table(df2$diagnosis_result))

******It is quite typical of such medical dataset to be unbalanced. We’ll have to deal with it.

Like with PCA, KNN is quite sensitve to the scale of the variable. So it is important to first standardize the variables. This time we’ll do this using the preProcess funnction of the caret package.

library(caret)

param_preproc_df2 <- preProcess(df2[,2:9], method = c("scale", "center"))
df3_stdize <- predict(param_preproc_df2, df2[, 2:9])

summary(df3_stdize)

*********We can now see that all means are centered around 0. Now we reconstruct our df with the response variable and we split the df into a training and testing set.

df3_stdize <- bind_cols(diagnosis = df2$diagnosis_result, df3_stdize)

param_split<- createDataPartition(df3_stdize$diagnosis, times = 1, p = 0.8, 
                                      list = FALSE)
train_df3 <- df3_stdize[param_split, ]
test_df3 <- df3_stdize[-param_split, ]

################################We can check that we still have the same kind of split
prop.table(table(train_df3$diagnosis))

################################Nice to see that the proportion of Malign vs Benin has been conserved.
We use KNN with cross-validation (discussed in more details in this section 14.3 to train our model.

trnctrl_df3 <- trainControl(method = "cv", number = 10)

model_knn_df3 <- train(diagnosis ~., data = train_df3, method = "knn", 
                       trControl = trnctrl_df3, 
                       tuneLength = 10)

model_knn_df3

plot(model_knn_df3)

predict_knn_df3 <- predict(model_knn_df3, test_df3)

confusionMatrix(predict_knn_df3, test_df3$diagnosis, positive = "Malignant")