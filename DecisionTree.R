# Decision Tree


library(caret)
library(corrplot)
library(rpart)
library(rpart.plot)


path = "E:/Dataset/ML/heart.csv"
heart = read.csv(path,stringsAsFactors = T)


View(heart)
dim(heart)
str(heart)

#Change the columns to type 'category'
cols_as_fact = c('sex','cp','fbs','restecg','exang','target')
heart[cols_as_fact] = lapply(heart[cols_as_fact], as.factor)

str(heart)


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

#ctrl-> values for hyperparameter tuning for decision tree
buildModel = function(df,y,posclass,ratio=0.7,viewtree=T,ctrl)
{
  #Split the data
  totalrows = nrow(df)
  ss = sample(seq(1,totalrows),ratio*totalrows)
  traindata = df[ss,]
  testdata = df[-ss,]
  
    #initialize the formula
  form = as.formula(paste(y,"~."))
  
  #Build the Decision Tree model based on 'ctrl' param
  
  #if length(ctrl)==0 --> no hyperparameters
  #if length(ctrl)>0 --> hyperparameters
  
  
  if(length(ctrl) == 0)
    model = rpart(formula,data = train, method='class')  else
      model = rpart(fromula,data=train,method = 'class', control = ctrl)
  
  #plot the decision tree 
  if(viewtree)
    rpart.plot(model,type = 4,extra =101)
  
  #predict
  pred = predict(model,test,type='class')
  
  # confusion matrix
  print(confusionMatrix(as.factor(unlist(test[y])),as.factor(pred),positive = posclass))
  
    return(list(train=train,test=test,model =model, predictions=pred))
  
}




#--------------------------------------------------------------

#split columns

cols = splitcols(heart)
nc = unlist(cols[1])
fc = unlist(cols[2])

print(nc)
print(fc)

#Check for Anamolies

#Print Factor
printFactors(heart,fc)



heart[heart$thal == '1',]
heart[heart$thal == '2',]

#Assume that the cases are normal
#set ' thal value to normal

heart$thal = as.character(heart$thal)
str(heart$thal)

heart$thal[heart$thal %in% c('1','2')] = 'normal'
table(heart$thal)

#Converting to 'thal' col factor 
heart$thal = as.factor(heart$thal)
str(heart$thal)

#print factor
printFactors(heart,fc)


# Check distribution of y-variable
table(heart$target)

#check anamolies
checkAnomalies(heart,colnames(heart),'na')
checkAnomalies(heart,fc,'z')

#oldpeak -> cannot take 0
#impute 'oldpeak' 

#find the rows where 'oldpeak' =0
rows = as.integer(rownames(heart[heart$oldpeak == 0 ,]))
length(rows)

#Check if the rows have 0
heart$oldpeak[rows]

mini = min(heart$oldpeak[heart$oldpeak>0])
maxi = max(heart$oldpeak[heart$oldpeak>0])

randomnum = runif(length(rows),mini,maxi)
randomnum = round(randomnum,1)

heart$oldpeak[rows] = randomnum

checkAnomalies(heart,nc,'z')

#ca -> 0 is valid

#Check correlation
plotChart(heart,nc,'c')

#Singularity

sing = singularity(heart,fc)
sing[sing$percentage>0.85,]

#Building the model
Y = 'target'
pos= "1"

# 1) Without doing hyperparameter tuning
lov1 = buildModel(df =heart,y= Y,posclass = pos,ctrl = list())
















