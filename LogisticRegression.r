#logistic Regression



library(caret)
library(corrplot)
library(pROC)


path="E:/Dataset/ML/hr_emp.csv"
hr = read.csv(path,header=T, stringsAsFactors = T)


str(hr)

#------------Common Function------------------------------
#Function split cols
splitcols = function(df)
{
  cols = list()
  
  nc = names(df)[sapply(df,is.numeric)]
  fc = names(df)[sapply(df,is.factor)]
  
  cols[1] = list(nc)
  cols[2] = list(fc)
  return (cols)
  
}

# Finding unique values
printFactors = function(df,fc)
{
  for (c in fc)
  {
    print(paste("factor column :",c))
    print(levels(factor(unlist(df[c]))))
    cat("\n")
  }
  
}


# Check for 0 and Nulls

checkAnomalies = function(df,cols,ctype)
{
  # (check type )ctype = z (zero), na (null check)
  
  if (ctype %in% c('z','na'))
  {
    ctr = c()
    features = c()
    
    for(c in cols)
    {
      if (ctype == 'z')
        ctr = append(ctr,length(df[c][df[c]==0])) else
          ctr = append(ctr,length(df[c][is.na(df[c])]))
        
        features = append(features,c)
    }
    
    d = data.frame(feature=features,count=ctr)
  } 
  else
  {
    d = 'Invalid Type specified'    
  }
  
  return(d)
}

#Function : plotchart
# distributions, multicollinearity, outliers
# (histogram,heatmap,boxplot)
plotChart = function(df,cols,ctype)
{
  # h: histogram, c: multicollinearity, b: boxplot
  if (ctype %in% c('h','c','b'))
  {
    # correlation plot
    if (ctype == 'c')
    {
      corr = cor(df[cols])
      corrplot(corr,type='lower',method='number')
    }
    else
    {
      for (c in cols)
      {
        if (ctype == 'b')
          boxplot(df[c],horizontal=T,col='red',main=c) 
        else
          hist(unlist(df[c]),main=c,col='blue')
      }
    }
    msg = 'Success'
  }
  else
  {
    msg = 'Invalid Chart type'
  }
  
  return(msg)
}

#function : singularity

singularity = function(df,fc)
{
  D = data.frame()
  for(c in fc)
  {
    dd = data.frame(feature = c, prop.table(table(df[c])))
    D = rbind(D,dd)
  }
  
  names(D) = c('feature','value','percentage')
  return(D)
  
}

# Build Model
#Build : Logistic Regression Model
buildModel = function(df,y,ratio =0.7,verbose = FALSE)
{
  #Split the data
  totalrows = nrow(df)
  ss = sample(seq(1,totalrows),ratio*totalrows)
  traindata = df[ss,]
  testdata = df[-ss,]
  
  
  
  #initialize the formula
  form = as.formula(paste(y,"~."))
  
  #Build the Logistic Regression model
  model = glm(form, binomial(link = "logit"), data = traindata)
  
  #Print : Summary
  if(verbose)
      print(summary(model))
  
  #predict
  preds = predict(model,testdata,type = "response" )
  
  #return Values
  return(list(train = traindata,test = testdata, model= model, predictions = preds))
  
}




#--------------------------------------------------------------

cols = splitcols(hr)
nc = unlist(cols[1]);nc
fc = unlist(cols[2]);fc

#check if any numeric data has to be converted to factor
cols_as_fact = c("educ","env_satisf",'jobinv','joblvl','job_satisf','perf_rating',
                 'rel_satisf','stockopt','wk_life_bal','attr_value')

#convert to factor
hr[cols_as_fact] = lapply(hr[cols_as_fact],as.factor)
str(hr[cols_as_fact])


#check attrition and attr_value
head(hr[,c('attrition','attr_value' )],30)

#remove features
cols_to_remove = c('empnum','attrition')
hr[cols_to_remove] = NULL

#refresh the cols 
cols = splitcols(hr)
nc = unlist(cols[1]);nc
fc = unlist(cols[2]);fc


# Print all the unique value of factor columns

printFactors(hr,fc)


#Check Anomalies
cols = colnames(hr)
checkAnomalies(hr,nc,"z") # check 0's
checkAnomalies(hr,nc,"na") # check nulls

#Check for distribution/ outliers/ etc
plotChart(hr,nc,'c')
plotChart(hr,nc,'h')
plotChart(hr,nc,'b')


#singularities
sing = singularity(hr,fc)
sing[sing$percentage > 0.8,]

#Model building

lov = buildModel(hr2,"attr_value",verbose = T)
m1 = lov$model 
summary(m1)

#convert the probabilites into actual classes 
p1 = unlist(lov$predictions)

print(p1)

#convert the probabilites into classes; based on cutoff
ProbtoClass = function(test, y,preds,pos,cutoff)
{
  pp = ifelse(preds<=cutoff,0,1)
  
  #Confusion Matrix
  print(confusionMatrix(as.factor(unlist(test[y])),
                        as.factor(pp),
                        positive = pos))
  
  return(pp)
}
cutoff = 0.3
predy = ProbtoClass(lov$test, "attr_value",p1,'1',cutoff)


# ROC/AUC Curve
roc(test$attr_value,p1,plot=T, col='green',print.auc=T)

hr2 = hr[, !names(hr) %in% c('educ','educ_fld')]




















