# library(data.table)
# train=fread("C:/data/train-allstate.csv",sep=",")
library(data.table)
library(h2o)
h2o.init(nthreads=3, max_mem_size = "3G")
# h2o.init(nthreads = -1, max_mem_size = "8G")
# Number of threads -1 means use all cores on your machine
# max mem size is the maximum memory to allocate to H2O


train =h2o.importFile("C:/data/train-allstate.csv", destination_frame = "train.hex")
#scale(train[,118:131],center=T,scale=T) #scale contiuous variable
test = h2o.importFile("C:/data/test-allstate.csv", destination_frame = "test.hex")

splits = h2o.splitFrame(
  data = train, 
  ratios = 0.8,
  destination_frames = c("train_1.hex", "valid_1.hex"))

train_xf_sp = splits[[1]]
valid_xf_sp = splits[[2]]


varimp=fread("C:/data/imp-allstate-rf.csv",sep=",")

features=varimp[1:65,2,with=F]
features=as.vector(features$variable)

response = "loss"
rm(varimp)

## Hyper-Parameter Search

## Construct hyper-parameter space
hidden.opt= list(c(30,30),c(30,20),c(30,10),c(20,10),
              c(20,20,10),c(12,6),
              c(30,30,10),c(40,20))
# distribution.opt=c("laplace","quantile","gamma")
#activation.opt=c("Rectifier","Maxout")
activation.opt=c("Maxout","Rectifier")
#distribution.opt=c("laplace","quantile")
#ncg.opt=c(T,F)
epochs.opt=c(8,10,20,30)

hyper_params = list( hidden = hidden.opt,
                     #distribution=distribution.opt,
                     activation=activation.opt,
                     #nesterov_accelerated_gradient=ncg.opt
                     #loss=loss.opt,
                     #stopping_metric=stopmetric.opt 
                     epochs=epochs.opt
                     )


## Search a random subset of these hyper-parmameters (max runtime and 
#max models are enforced, and the search will stop after we don't improve
#much over the best 5 random models)

search_criteria = list(strategy = "RandomDiscrete", 
                       max_models = 100, stopping_metric = "AUTO", 
                       stopping_rounds = 5, seed = 101)

nn.grid <- h2o.grid(algorithm = "deeplearning",
                     grid_id = "dl_grid",
                     x = features,
                     y = response,
                    use_all_factor_levels = T,
                     training_frame = train_xf_sp,
                     validation_frame = valid_xf_sp,
                     standardize=T,
                     nesterov_accelerated_gradient=T,
                    #diagnostics=T,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria )


## Sort by mae
grid=h2o.getGrid("dl_grid",sort_by="mae",decreasing=FALSE)

best_model = h2o.getModel(grid@model_ids[[1]])
all_pred_train=predict(best_model,train)
all_pred_test=predict(best_model,test)

for(i in 2:16)
{ best_model = h2o.getModel(grid@model_ids[[i]])
pred_train=predict(best_model,train)
all_pred_train=h2o.cbind(all_pred_train,pred_train)
pred_test=predict(best_model,test)
all_pred_test=h2o.cbind(all_pred_test,pred_test)
}

all_pred_train=as.data.table(all_pred_train)
colnames(all_pred_train)=paste("m",1:16,sep='')
write.csv(all_pred_train,"all_pred_train_16.csv",row.names = F)

all_pred_test=as.data.table(all_pred_test)
colnames(all_pred_test)=paste("m",1:16,sep='')
write.csv(all_pred_test,"all_pred_test_16.csv",row.names = F)

train_loss=as.matrix(train[,132])
avgPredTrain=(all_pred_train$m1+all_pred_train$m2+all_pred_train$m3+
                all_pred_train$m4+all_pred_train$m5
              # +all_pred_train$m6
              # +all_pred_train$m7+all_pred_train$m8
              # +all_pred_train$m9+all_pred_train$m10
              # +all_pred_train$m11+all_pred_train$m12
              #     +all_pred_train$m13+all_pred_train$m14+
              #       all_pred_train$m15+all_pred_train$m16
              #+all_pred_train$m17
              #     +all_pred_train$m18+all_pred_train$m19+all_pred_train$m20
)/5
avgMEA=mean(abs(avgPredTrain-train_loss))

all_pred_test=as.data.table(all_pred_test)
avgPredTest2=(all_pred_test$m1+all_pred_test$m2+all_pred_test$m3+
               all_pred_test$m4+all_pred_test$m5
             # +all_pred_test$m6
             # +all_pred_test$m7+all_pred_test$m8
             # +all_pred_test$m9
             # +all_pred_test$m10
             # +all_pred_test$m11+all_pred_test$m12
             #     +all_pred_test$m13+all_pred_test$m14+
             #       all_pred_test$m15+all_pred_test$m16
             #+all_pred_test$m17
             #     +all_pred_test$m18+all_pred_test$m19+all_pred_test$m20
)/5
submission <- as.matrix(test[,1])
submission=as.data.table(submission)
submission=cbind(submission,avgPredTest)
colnames(submission)=c("id","loss")

write.csv(submission,"all_pred_test_5_submission.csv",row.names = F)