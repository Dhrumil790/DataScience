
# Linear Regression

#importing Libraries
library(DAAG) #for cross validation
library(caret) #RMSE
library(ggplot2)
library(corrplot)# To check multi collinearity

#importing dataset
path = "c:/Users/HP/Downloads/concrete.csv"
conc = read.csv(path,header = T)

View(conc)
dim(conc)
summary(conc)

str(conc)

#EDA on the dataset
checkAnamalies = function(df,cols,ctype)
{
  #checktype :- ctype = z(zero),na (null check)
  
  if(ctype %in% c('z','NA'))
  {
    ctr = c()
    features= c()
    
    for(i in cols)
    {
      if(ctype == 'z')
      {
        ctr = append(ctr,length(df[i][df[i]==0]))
      }
      else
      {
        ctr = append(ctr,length(df[i][is.na(df[i])]))
      }
      features = append(features,i)
    }
    d= data.frame(feature = features,count= ctr)
  }
  else
  {
    d = "invalid type specified"
  }
  
  return(d)
  
}
cols = colnames(conc)
cols

#check for zero
checkAnamalies(conc,cols,"z")

#check for null
checkAnamalies(conc,cols,"NA")


#Plotting Chart for more intutions
#Distribution, Multicollinearity, Outlier

plotChart = function(df,cols,ctype)
{
  # h : Histogram, c : multicollinearity, b : boxplot
  if(ctype %in% c('h','c','b'))
  {
    #correlation plot
    if(ctype == 'c')
    {
      corr = cor(df[cols])
      corrplot(corr,type = "lower",method = "number")
      
    }else
    {
      for(c in cols)
      {
        if(ctype == 'b')
        {
          boxplot(df[c],horizontal = T,col = 'red',main =c)
          
        }
        else
        {
          hist(unlist(df[c]),main=c,col = 'blue')
        }
      }
    }
    msg = "Success"
  }
  else
  {
    msg = "invalid chart type"
    
  }
  return(msg)
  
}

plotChart(conc,cols,"c")
plotChart(conc,cols,"b")
plotChart(conc,cols,"h")


#split the data in train and test
totalrows = nrow(conc); totalrows

ss = sample(seq(1,totalrows),0.7*totalrows)
train = conc[ss,]
test = conc[-ss,]

View(train)
dim(train)
dim(test)

dimnames(conc)

#---------------------------------------------------------------------
#     MOdel Building
#---------------------------------------------------------------------
#Bulid the linear regression model
# m1 = lm(Y~X)
# . indicates all (x) features

m1 = lm(CCS ~ .,data = train)
summary(m1)

#validating the assumption of Linear Regression

#1) Mean residual is 0
mean(m1$residuals)

#2) residuals have a constant varience (homoscedasticity)
#see plot 'Residual vs Fitted'
plot(m1)

#Hypothesis testing for heteroscedasticity
# 1) Breusch-pagan test
library(lmtest)

# H0 : Homoscedasticity
# H1: Heteroscedasticity

ret = bptest(m1)
ret$p.value
if(ret$p.value < 0.05)
  print("Hetroscedastic model") else
    print("Homoscedastic model")

# 2) NCV test (non-constant varience test)
#install.packages("car")
library(car)

ret = ncvTest(m1)
ret$p
if(ret$p < 0.05)
  print("Hetroscedastic model") else
    print("Homoscedastic model")

#3)residual have a normal distribution
#check the 'Normal Q-Q' plot
#for normal distribution all points should be on the dotted line
plot(m1)

#4) rows > columns
dim(train)

#----------------------------------------------------------------------
#Cross - validation

#k-fold cross validation
tc = trainControl(method = "cv",number = 5)
cv1 = train(CCS~., data=train, trControl = tc, method = 'lm')
cv1
#Average error from CV = 10.05573

#(Actual) predict on test data
p1 = predict(m1,test)
p1[1:10]


#create a dataframe to store actual ccs and predicted ccs
df1 = data.frame(actual = test$CCS,
                 predicted = round(p1,2),
                 sqerr = round((test$CCS - p1)^2,2))
df1

#check if the predicted RMSE matches with the CV RMSE
#Calculate rmse, Aic, BIC
rmse1 = RMSE(df1$actual,df1$predicted)
rmse1
aic1 = AIC(m1)
aic1
bic1 = BIC(m1)
bic1

#plot the Actual and predicted values
ggplot(df1,aes(x=actual,y=predicted))+
  geom_point()+
  geom_smooth(method = "lm",color = 'red',se=F)


#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
#Build next model such that RMSE decreases
#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
#1) Convert hetero to homosc
#2) Remove insignificant feature
#3) Data Transformation



#Generic Functions

printResults = function(rmse,aic,bic,title="")
{
  cat("Model Measures :",title,"\n\t AIC : ",aic,"\n\t BIC : ",bic,"\n\t RMSE : ",rmse)
  
}
printResults(rmse1,aic1,bic1,"Model 1")

#----------------------------------------------------------------------------------------------
# 1) Box Cox Transformation
#----------------------------------------------------------------------------------------------
library(MASS)

bc1 = boxcox(CCS ~ .,data = train)

#To find the best value of Gamma
print(bc1)

#Find maximum Y; then its corresponding x
gamma = bc1$x[which(bc1$y == max(bc1$y))]
print(gamma)

gamma_ub = bc1$x[bc1$x > gamma][1]
#bc1$y[which(bc1$x == qw)]
gamma_lb = sort(bc1$x[bc1$x < gamma],decreasing = T)[1]

list_of_gamma = c(gamma_lb,gamma,gamma_ub)
list_of_gamma

#Create a function to built and predict CCS on the BoxCox transformed value of CSS

boxcoxLR = function(train,test,y,list_of_gamma)
{
  for(g in list_of_gamma)
  {
    # create the train and test data
    tr = train
    te = test
    
    #transform the Y- Variable int BoxCox format
    tr['bcY'] = tr[y]^g
    te['bcY'] = te[y]^g
    
    #Removing the original Y variable
    tr[y] = NULL 
    te[y] = NULL
    
    #BUild the model
    model = lm(bcY~., data = tr)
    pred = predict(model,te) # Prediction are in the BoxCox Format
    pred_conv = pred^(1/g) #Converting BoxCox format to original format
    
     #Create dataframe to store actual and predicted data
    df = data.frame(actual = test[y],predicted = pred_conv)
    View(df)
    
    rmse = RMSE(df$CCS,df$predicted)
    aic = AIC(model)
    bic = BIC(model)
    
    
    #cat("Gamma : ",g, "RMSE : ",rmse,"\n")
    printResults(rmse1,aic1,bic1,paste("Gamma",g))
  }
  
}
boxcoxLR(train,test,"CCS",list_of_gamma)


#BoxCox (In this case, failed to give us a better model)
#----------------------------------------------------------------------------------------------
# 2) Feature Selection
#----------------------------------------------------------------------------------------------

#Removal of feature that are not significant
summary(m1)

buildModel = function(df,y)
{
  lov = list()
  
  #Split data into train/test
  totalrows = nrow(df); totalrows
  ss = sample(seq(1,totalrows),0.7*totalrows)
  train = df[ss,]
  test = df[-ss,]
  
  #Build the formula
  form = as.formula(paste(y,"~."))
  
  #Build the model and predict
  model = lm(form, data = train)
  pred_data = predict(model,test)
  
  aic = AIC(model)
  bic = BIC(model)
  rmse = RMSE(unlist(test[y]),unlist(pred_data))
  
  
  lov[2] = list(aic)
  lov[3] = list(bic)
  lov[1] = list(rmse)
  
  
  return(lov) #rmse/aic/bic
}

buildModel(conc,"CCS")


#------------------------------------------------
#summary(m1)


#Remove feature and build the model

newLRModel = function(df,removeFeature)
{
  df[removeFeature] = NULL
  ret =  buildModel(df,"CCS")
  printResults(unlist(ret[1]),unlist(ret[2]),unlist(ret[3]),"Remove Features")
}

#Build a new model with feature 'coraseaggr' and 'finraggr'
ftr = c("coraseaggr","finraggr","superplastisizer")
ftr = c("finraggr")
newLRModel(conc,ftr)




#----------------------------------------------------------------------------------------------
# 3) Transform the X(feature) and build model
#----------------------------------------------------------------------------------------------
# natural log(nl), log with base value(log), sqroot(sqrt), inverse(inv), minmax, zscore(z) etc... transformation

minmax = function(x) return( (x-min(x))/(max(x)-min(x)) )



TransformBuildModel = function(df,y,tr_type,base=0)
{
  #Get all the feature (X column)
  pos = which(names(df) == y)  
  cols = names(conc)[-pos]
  
  if(tr_type == "z")
    df[cols] = apply(df[cols],2,scale) else
  if(tr_type == "minmax" )
    df[cols] = lapply(df[cols],minmax) else
  if(tr_type == "inv")
    df[cols] = 1/(df[cols]+1) else
  if(tr_type == "sqrt")
    df[cols] = sqrt(df[cols]) else
  if(tr_type == "nl")
    df[cols] = log(df[cols]+1)  else
  if(tr_type == "log")
  {
    if(base == 0)
      return('Invalid value for base for log()')
    else
      df[cols] = log(df[cols]+1,base)
  } else
    return(paste(tr_type," : Invalid type Specified"))
  
  #Build the model on the transformed dataset
  r = buildModel(df,y)
 printResults(unlist(r[1]),unlist(r[2]),unlist(r[3]),tr_type)
  
  return(r)
}
r=TransformBuildModel(conc,"CCS","z")
r=TransformBuildModel(conc,"CCS","minmax")
r=TransformBuildModel(conc,"CCS","inv")
r=TransformBuildModel(conc,"CCS","sqrt")
r=TransformBuildModel(conc,"CCS","nl")
r=TransformBuildModel(conc,"CCS","log",base =2)
r=TransformBuildModel(conc,"CCS","log",base =3)
r=TransformBuildModel(conc,"CCS","log",base =4)
r=TransformBuildModel(conc,"CCS","log",base =10)