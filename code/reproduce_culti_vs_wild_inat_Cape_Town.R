require(magrittr)
require(rinat)
require(raster)
require(rgeos)
require(rgbif)
require(ggplot2)
require(sf)
require(ggmap)
require(leaflet)
require(rpart)
require(glmnet)
require(randomForest)
require(jsonlite)
require(dplyr)

# Reproducing map Figures requires registering to stadiamaps
# using register_stadiamaps() 

prefix="/home/botella/pcloud/"
main=paste0(prefix,"boulot/data/invasion iNat CapeTown/")
saveDir = paste0(prefix,"boulot/data/invasion iNat CapeTown/dataset/")
figDir = paste0(prefix,"boulot/data/invasion iNat CapeTown/Figures/")
alienDir = paste0(prefix,"boulot/data/invasion iNat CapeTown/Peninsula list/")
admDir = paste0(prefix,"boulot/data/invasions iNaturalist/administrative borders SA/")
appScriptPath = paste0(prefix,"boulot/data/invasion iNat CapeTown/ShinyApp_annotated/")
landUse = paste0(prefix,"boulot/data/invasion iNat CapeTown/land use/")
landUse2 = paste0(prefix,"boulot/data/global land cover/globCover/")

SApoly <- getData("GADM", country = "ZA", level = 1)  # Level 1 corresponds to country boundaries
WCpoly=SApoly[SApoly$NAME_1=="Western Cape",,drop=F]
save(WCpoly,file=paste0(main,'dataset/WCpoly.Rdata'))

#####
# Functions
#####

cellsSelection = c(3913,3914,3988,3989,4063)

getParcel2 = function(coos){
  pt = st_point(x=as.numeric(coos[1,]),dim="XY")
  pt = st_sfc(pt,crs = 4326)
  sburb =suppressMessages(suppressWarnings(
    as.character(st_intersection(suburbs,pt)$NAME)
  ))
  if(length(sburb)>0 && sburb%in%tab$OFC_SBRB_NAME){
    cd=tab$OFC_SBRB_NAME==sburb
    exactProp = suppressMessages(suppressWarnings(
      as.character(st_intersection(sbs[sburb][[1]],pt)$PRTY_NMBR)
    ))
    classes= as.character(tab$ZONING[cd & tab$PRTY_NMBR%in%exactProp])
  }else{
    classes= NULL
  }
  return(classes)
}

toScale = c("positional_accuracy",
            "lnObs",
            'lnSp',
            'visu_ctx_anthropized','visu_ctx_intothewild',
            'visu_ctx_uniform_background_macro_dissection','visu_ctx_with_human_body_presence',
            'visu_for_amphibian','visu_for_bird',
            'visu_for_fish','visu_for_humanmade',
            'visu_for_invertebrate','visu_for_landscape',
            'visu_for_mammal','visu_for_mushroom',
            'visu_for_reptile','visu_for_bark',
            'visu_for_branch','visu_for_bud',
            'visu_for_flower','visu_for_fruit',
            'visu_for_habit','visu_for_leaf',
            'visu_for_roots','visu_for_seeds',
            'visuOld_ctx_garden_park','visuOld_ctx_indoor',
            'visuOld_ctx_into_the_wild','visuOld_ctx_landscape',
            'visuOld_ctx_terrace_balcon','visuOld_ctx_uniform_background',
            'visuOld_ctx_urban_building_road','visuOld_ctx_urban_ground_wall',
            'visuOld_for_hand','visuOld_for_humanmade')


multiplot <- function(plots=NULL, file, cols=1, layout=NULL){
  library(grid)
  
  numPlots = length(plots)
  print(numPlots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

models=c('tag',
         'glmnet',
         'glmnet_soft',
         'glm_selec',
         'glm_selec_soft',
         'glm_full',
         'glm_full_weighted',
         'glm_full_soft',
         'glm_full_soft_weighted',
         "RF",
         "RF_doubt",
         "RF_doubt_weighted",
         "RF_doubt_weighted2")

colors = c('darkorchid4','red4','red3','chocolate4',
                        'chocolate2','seagreen4','chartreuse4',
                        'seagreen3',
                        'chartreuse3',
                        'royalblue4',
                        'royalblue3',
                        'royalblue1',"skyblue2")
                        


sets=c('base',
       'inatTag',
       'inat',
       'geo','inat_geo',
       'visu',
       'visu_geo',
       'inat_visu',
       'all')

# var sets
if(T){
  var_visual=c('visu_ctx_anthropized','visu_ctx_intothewild',
               'visu_ctx_uniform_background_macro_dissection','visu_ctx_with_human_body_presence',
               'visu_for_amphibian','visu_for_bird',
               'visu_for_fish','visu_for_humanmade',
               'visu_for_invertebrate','visu_for_landscape',
               'visu_for_mammal','visu_for_mushroom',
               'visu_for_reptile','visu_for_bark',
               'visu_for_branch','visu_for_bud',
               'visu_for_flower','visu_for_fruit',
               'visu_for_habit','visu_for_leaf',
               'visu_for_roots','visu_for_seeds')
  
  var_visuOld=c('visuOld_ctx_garden_park',
                'visuOld_ctx_indoor',
                'visuOld_ctx_into_the_wild',
                'visuOld_ctx_landscape',
                'visuOld_ctx_terrace_balcon','visuOld_ctx_uniform_background',
                'visuOld_ctx_urban_building_road','visuOld_ctx_urban_ground_wall',
                'visuOld_for_hand','visuOld_for_humanmade')
  
  var_obs_basic=c("positional_accuracy",
                  "lnObs",
                  'lnSp')
  
  var_obs_inat = c("iNat_tag",
                   "nReviews",
                   "AgreeRate",
                   "tagged",
                   "tagFreq")
  
  var_geo = c("protecArea",
              #"alienControl",
              "dToRiverM",
              "dToCanalM",
              "parcel_resid",
              "parcel_business",
              "parcel_road",
              "parcel_agri",
              "parcel_open",
              "simple_lc")
  
}

varSets=list(
  base=c(var_obs_basic),
  inat=c(var_obs_basic,var_obs_inat),
  visu=c(var_obs_basic,var_visual),
  geo=c(var_obs_basic,var_geo),
  inat_visu=c(var_obs_basic,var_obs_inat,var_visual),
  inat_geo=c(var_obs_basic,var_obs_inat,var_geo),
  visu_geo=c(var_obs_basic,var_geo,var_visual),
  all=c(var_obs_basic,var_obs_inat,var_geo,var_visual)
)

# var scaling
if(T){
  transf_full = c('I(dToRiverM^2)',
                  'I(dToCanalM^2)',
                  'I(tagFreq^2)',
                  'I(positional_accuracy^2)')
  toScale = c("positional_accuracy",
              "lnObs",
              'lnSp',
              'visu_ctx_anthropized','visu_ctx_intothewild',
              'visu_ctx_uniform_background_macro_dissection','visu_ctx_with_human_body_presence',
              'visu_for_amphibian','visu_for_bird',
              'visu_for_fish','visu_for_humanmade',
              'visu_for_invertebrate','visu_for_landscape',
              'visu_for_mammal','visu_for_mushroom',
              'visu_for_reptile','visu_for_bark',
              'visu_for_branch','visu_for_bud',
              'visu_for_flower','visu_for_fruit',
              'visu_for_habit','visu_for_leaf',
              'visu_for_roots','visu_for_seeds',
              'visuOld_ctx_garden_park','visuOld_ctx_indoor',
              'visuOld_ctx_into_the_wild','visuOld_ctx_landscape',
              'visuOld_ctx_terrace_balcon','visuOld_ctx_uniform_background',
              'visuOld_ctx_urban_building_road','visuOld_ctx_urban_ground_wall',
              'visuOld_for_hand','visuOld_for_humanmade')
  
}

# all model fitting function
compute.model.metrics=function(metricas,
                               df,
                               folds,
                               modTypes,
                               varSet,
                               covar=NULL,
                               transf=NULL){
  nSa=dim(df)[1]
  for(i in 1:length(folds)){
    cat('\n Rep ',i,' \n')
    # Fit using glm
    trainSamp = !(1:nSa)%in%folds[[i]]
    train = df[trainSamp,]
    valid = df[!trainSamp,]
    for(j in colnames(valid)){
      if(!is.numeric(valid[,j])){
        valid[,j]=factor(valid[,j],levels=levels(factor(df[,j])))
      }
    }
    nValid=sum(!trainSamp)
    noD = train$annotation!="doubtful"
    vnoD=valid$annotation!="doubtful"
    truTrain = as.numeric(train$annotation[noD]=="managed") 
    for(modo in modTypes){
      print(modo)
      if(modo=='glm_full'){
        # GLM full 
        fu=paste0("~",paste(c(covar,transf),collapse = "+"))
        mod=glm(as.formula(paste0('y',fu)),
                family=binomial(link = "logit"),
                data=train[noD,])
        validX=model.matrix(as.formula(fu),data=valid)
        pred= (validX%*%mod$coefficients) %>%
          as.vector %>%
          sapply(function(lp){if(is.infinite(exp(lp))){1}else{exp(lp)/(1+exp(lp))}})
        predF = pred
        predTrain= (model.matrix(as.formula(fu),train[noD,])%*%mod$coefficients) %>%
          as.vector %>%
          sapply(function(lp){if(is.infinite(exp(lp))){1}else{exp(lp)/(1+exp(lp))}}) %>%
          sapply(function(pp)pp>.5)
        trainAcc=sum((predTrain-truTrain)==0)/sum(noD)
      }
      else if(modo=='glm_full_weighted'){
        # GLM full without intercept
        weights=sapply(train$annotation,function(ano)if(ano=="wild"){1/sum(train$annotation=="wild")}else if(ano=="managed"){1/sum(train$annotation=="managed")}else{1/sum(train$annotation=="doubtful")})
        fu=paste0("~",paste(c(covar,transf),collapse = "+"))
        mod=glm(as.formula(paste0('y',fu)),
                family=binomial(link = "logit"),
                data=train[noD,],weights = weights[noD])
        validX=model.matrix(as.formula(fu),data=valid)
        pred= (validX%*%mod$coefficients) %>%
          as.vector %>%
          sapply(function(lp){if(is.infinite(exp(lp))){1}else{exp(lp)/(1+exp(lp))}})
        predW=pred
        predTrain= (model.matrix(as.formula(fu),train[noD,])%*%mod$coefficients) %>%
          as.vector %>%
          sapply(function(lp){if(is.infinite(exp(lp))){1}else{exp(lp)/(1+exp(lp))}})%>%
          sapply(function(pp)pp>.5)
        trainAcc=sum((predTrain-truTrain)==0)/sum(noD)
      }
      else if(modo=="glmnet"){
        # GLM L1-regularized full 
        fu=paste0("~",paste(c(covar,transf),collapse = "+"))
        yMat=matrix(c(1-train$y,train$y),sum(trainSamp),2)
        des=sparse.model.matrix(as.formula(fu),data=train)
        lambdas=cv.glmnet(x=des[noD,],y=yMat[noD,],family="binomial",nfolds=3)[c('lambda.min','lambda','glmnet.fit')]
        coefs=lambdas$glmnet.fit$beta[,lambdas$lambda==lambdas$lambda.min]
        coefs[1]=as.numeric(lambdas$glmnet.fit$a0[lambdas$lambda==lambdas$lambda.min])
        validX=model.matrix(as.formula(fu),data=valid)
        pred= (validX%*%coefs) %>%
          as.vector %>%
          (function(lp){exp(lp)/(1+exp(lp))})
        selectedVariables = names(coefs)[coefs!=0]
        predTrain= (des[noD,,drop=F]%*%coefs) %>%
          as.vector %>%
          (function(lp){exp(lp)/(1+exp(lp))}) %>%
          sapply(function(pp)pp>.5)
        trainAcc=sum((predTrain-truTrain)==0)/sum(noD)
      }
      else if(modo=='glm_selec'){
        fu=paste0("~",paste(c(covar,transf),collapse = "+"))
        des=model.matrix(as.formula(fu),data=train)
        des=des[,selectedVariables,drop='F']
        mod=glm.fit(des[noD,],train$y[noD],family=binomial(link = "logit"))
        validX= as.formula(fu) %>%
          model.matrix(data=valid) %>%
          subset(select=selectedVariables)
        pred= (validX%*%mod$coefficients) %>%
          as.vector %>%
          (function(lp){exp(lp)/(1+exp(lp))})
        predS = pred
        predTrain= (des[noD,,drop=F]%*%mod$coefficients) %>%
          as.vector %>%
          (function(lp){exp(lp)/(1+exp(lp))}) %>%
          sapply(function(pp)pp>.5)
        trainAcc=sum((predTrain-truTrain)==0)/sum(noD)
      }
      else if(modo=='glm_full_soft'){
        # GLM full 
        fu=paste0("~",paste(c(covar,transf),collapse = "+"))
        mod=glm(as.formula(paste0('y',fu)),
                family=binomial(link = "logit"),
                data=train)
        validX=model.matrix(as.formula(fu),data=valid)
        pred= (validX%*%mod$coefficients) %>%
          as.vector %>%
          sapply(function(lp){if(is.infinite(exp(lp))){1}else{exp(lp)/(1+exp(lp))}})
        predF = pred
        predTrain= (model.matrix(as.formula(fu),train[noD,])%*%mod$coefficients) %>%
          as.vector %>%
          sapply(function(lp){if(is.infinite(exp(lp))){1}else{exp(lp)/(1+exp(lp))}}) %>%
          sapply(function(pp)pp>.5)
        trainAcc=sum((predTrain-truTrain)==0)/sum(noD)
      }
      else if(modo=='glm_full_soft_weighted'){
        # GLM full without intercept
        weights=sapply(train$annotation,function(ano)if(ano=="wild"){1/sum(train$annotation=="wild")}else if(ano=="managed"){1/sum(train$annotation=="managed")}else{1/sum(train$annotation=="doubtful")})
        fu=paste0("~",paste(c(covar,transf),collapse = "+"))
        mod=glm(as.formula(paste0('y',fu)),
                family=binomial(link = "logit"),
                data=train,weights = weights)
        validX=model.matrix(as.formula(fu),data=valid)
        pred= (validX%*%mod$coefficients) %>%
          as.vector %>%
          sapply(function(lp){if(is.infinite(exp(lp))){1}else{exp(lp)/(1+exp(lp))}})
        predW=pred
        predTrain= (model.matrix(as.formula(fu),train[noD,])%*%mod$coefficients) %>%
          as.vector %>%
          sapply(function(lp){if(is.infinite(exp(lp))){1}else{exp(lp)/(1+exp(lp))}}) %>%
          sapply(function(pp)pp>.5)
        trainAcc=sum((predTrain-truTrain)==0)/sum(noD)
      }
      else if(modo=="glmnet_soft"){
        # GLM L1-regularized full 
        fu=paste0("~",paste(c(covar,transf),collapse = "+"))
        yMat=matrix(c(1-train$y,train$y),sum(trainSamp),2)
        des=sparse.model.matrix(as.formula(fu),data=train)
        lambdas=cv.glmnet(x=des,y=yMat,family="binomial",nfolds=3)[c('lambda.min','lambda','glmnet.fit')]
        coefs=lambdas$glmnet.fit$beta[,lambdas$lambda==lambdas$lambda.min]
        coefs[1]=as.numeric(lambdas$glmnet.fit$a0[lambdas$lambda==lambdas$lambda.min])
        validX=model.matrix(as.formula(fu),data=valid)
        pred= (validX%*%coefs) %>%
          as.vector %>%
          (function(lp){exp(lp)/(1+exp(lp))})
        selectedVariables = names(coefs)[coefs!=0]
        predTrain= (des[noD,,drop=F]%*%coefs) %>%
          as.vector %>%
          (function(lp){exp(lp)/(1+exp(lp))}) %>%
          sapply(function(pp)pp>.5)
        trainAcc=sum((predTrain-truTrain)==0)/sum(noD)
      }
      else if(modo=='glm_selec_soft'){
        fu=paste0("~",paste(c(covar,transf),collapse = "+"))
        des=model.matrix(as.formula(fu),data=train)
        des=des[,selectedVariables,drop=F]
        mod=glm.fit(des,train$y,family=binomial(link = "logit"))
        validX= as.formula(fu) %>%
          model.matrix(data=valid) %>%
          subset(select=selectedVariables)
        pred= (validX%*%mod$coefficients) %>%
          as.vector %>%
          sapply(function(lp){if(is.infinite(exp(lp))){1}else{exp(lp)/(1+exp(lp))}})
        predS = pred
        predTrain= (des[noD,,drop=F]%*%mod$coefficients) %>%
          as.vector %>%
          sapply(function(lp){if(is.infinite(exp(lp))){1}else{exp(lp)/(1+exp(lp))}}) %>%
          sapply(function(pp)pp>.5)
        trainAcc=sum((predTrain-truTrain)==0)/sum(noD)
      }
      else if(modo=="RF"){
        nVar=length(c(covar,transf))
        fu=paste0("~",paste(c(covar,transf),collapse = "+"))
        des=model.matrix(as.formula(fu),data=train)
        mtries=seq(1,min(nVar,25),3)
        nodesizes=seq(1,maxNodeSize,3)
        Err=matrix(NA,length(mtries),length(nodesizes))
        for(pili in mtries){
          for(nono in nodesizes){
            RF = randomForest(x=des[noD,],y=factor(train$annotation[noD]),
                              ntree=300,
                              nodesize = nono,
                              mtry= pili)
            Err[which(mtries==pili),which(nodesizes==nono)]=mean(RF$err.rate[,'OOB'])
          }
        }
        BestMtry=which.min(sapply(1:nrow(Err),function(k)min(Err[k,])))
        BestSize=which.min(sapply(1:ncol(Err),function(l)min(Err[,l])))
        
        RF = randomForest(x=des[noD,],y=factor(train$annotation[noD]),
                          ntree=500,
                          nodesize = BestSize,
                          mtry= BestMtry)
        
        validX=model.matrix(as.formula(fu),data=valid)
        predMat=RF %>%
          predict(validX,"prob")
        pred = as.numeric(predMat[,"managed"])/as.numeric(rowSums(predMat[,c('wild',"managed")]))
        predRF=pred
        predTrain= as.character(RF$predicted)=="managed"
        trainAcc=sum((predTrain-truTrain)==0)/sum(noD)
        gc(reset=T)
      }
      else if(modo=="RF_doubt"){
        nVar=length(c(covar,transf))
        fu=paste0("~",paste(c(covar,transf),collapse = "+"))
        des=model.matrix(as.formula(fu),data=train)
        mtries=seq(1,min(nVar,25),3)
        nodesizes=seq(1,maxNodeSize,3)
        Err=matrix(NA,length(mtries),length(nodesizes))
        for(pili in mtries){
          for(nono in nodesizes){
            RF = randomForest(x=des,y=factor(train$annotation),
                              ntree=300,
                              nodesize = nono,
                              mtry= pili)
            Err[which(mtries==pili),which(nodesizes==nono)]=mean(RF$err.rate[,'OOB'])
          }
        }
        BestMtry=which.min(sapply(1:nrow(Err),function(k)min(Err[k,])))
        BestSize=which.min(sapply(1:ncol(Err),function(l)min(Err[,l])))
        
        RF = randomForest(x=des,y=factor(train$annotation),
                          ntree=500,
                          nodesize = nodesizes[BestSize],
                          mtry= mtries[BestMtry])
        
        validX=model.matrix(as.formula(fu),data=valid)
        predMat=RF %>%
          predict(validX,"prob")
        pred = as.numeric(predMat[,"managed"])/as.numeric(rowSums(predMat[,c('wild',"managed")]))
        predRFD=pred
        predTrain= as.character(RF$predicted[noD])=="managed"
        trainAcc=sum((predTrain-truTrain)==0)/sum(noD)
      }
      else if(modo=="RF_doubt_weighted"){
        weights=sapply(train$annotation,function(ano)if(ano=="wild"){1/sum(train$annotation=="wild")}else if(ano=="managed"){1/sum(train$annotation=="managed")}else{1/sum(train$annotation=="doubtful")})
        nVar=length(c(covar,transf))
        fu=paste0("~",paste(c(covar,transf),collapse = "+"))
        des=model.matrix(as.formula(fu),data=train)
        mtries=seq(1,min(nVar,25),3)
        nodesizes=seq(1,maxNodeSize,3)
        Err=matrix(NA,length(mtries),length(nodesizes))
        for(pili in mtries){
          for(nono in nodesizes){
            RF = randomForest(x=des,y=factor(train$annotation),
                              ntree=300,weights = weights,
                              nodesize = nono,
                              mtry= pili)
            Err[which(mtries==pili),which(nodesizes==nono)]=mean(RF$err.rate[,'OOB'])
          }
        }
        BestMtry=which.min(sapply(1:nrow(Err),function(k)min(Err[k,])))
        BestSize=which.min(sapply(1:ncol(Err),function(l)min(Err[,l])))
        
        RF = randomForest(x=des,y=factor(train$annotation),
                          ntree=500,weights = weights,
                          nodesize = nodesizes[BestSize],
                          mtry= mtries[BestMtry])
        validX=model.matrix(as.formula(fu),data=valid)
        predMat=RF %>%
          predict(validX,"prob")
        pred = as.numeric(predMat[,"managed"])/as.numeric(rowSums(predMat[,c('wild',"managed")]))
        predRFD=pred
        predTrain= as.character(RF$predicted[noD])=="managed"
        trainAcc=sum((predTrain-truTrain)==0)/sum(noD)
      }
      else if(modo=="tag"){
        pred= as.numeric(valid$iNat_tag=="cultivated")
        predTrain= as.numeric(train$iNat_tag[noD]=="cultivated")
        trainAcc=sum((predTrain-truTrain)==0)/sum(noD)
      }
      truth = valid$annotation[vnoD]
      predHard = pred[vnoD]
      cd = metricas$rep==i & metricas$model==modo & metricas$set==varSet
      trW = truth=="wild"
      prW = predHard<=.5 
      trM = truth=="managed"
      prM = predHard>.5
      metricas$train_accuracy[cd]= trainAcc
      metricas$accuracy[cd] = sum((trW & prW)|(trM & prM))/sum(trW | trM)
      metricas$prWilVstrWil[cd] = sum(trW & prW)/sum(trW)
      metricas$prManVstrMan[cd] = sum(trM & prM)/sum(trM)
      truth = valid$annotation
      trW = truth=="wild"
      trM = truth=="managed"
      trD = truth=="doubtful"
      if(modo%in%c("RF_doubt","RF_doubt_weighted")){
        prW = sapply(1:dim(predMat)[1],function(i)colnames(predMat)[predMat[i,]==max(predMat[i,])][1]=="wild")
        prM = sapply(1:dim(predMat)[1],function(i)colnames(predMat)[predMat[i,]==max(predMat[i,])][1]=="managed")
        prD = sapply(1:dim(predMat)[1],function(i)colnames(predMat)[predMat[i,]==max(predMat[i,])][1]=="doubtful")
        metricas$accuracy_doubt40[cd] = sum((trW & prW)|(trM & prM)|(trD & prD))/length(pred)
        metricas$accuracy_doubt20[cd] = metricas$accuracy_doubt40[cd]
        metricas$accuracy_doubt10[cd] = metricas$accuracy_doubt40[cd]
      }else if(modo=="tag"){
        metricas$accuracy_doubt10[cd] = metricas$accuracy[cd]*sum(vnoD)/dim(DF)[1]
        metricas$accuracy_doubt20[cd] = metricas$accuracy_doubt10[cd]
        metricas$accuracy_doubt40[cd] = metricas$accuracy_doubt10[cd]
      }else{
        prW = pred<=.1
        prM = pred>=.9
        prD = pred>.1 & pred<.9
        metricas$accuracy_doubt40[cd] = sum((trW & prW)|(trM & prM)|(trD & prD))/length(pred)
        prW = pred<=.3
        prM = pred>=.7
        prD = pred>.3 & pred<.7
        metricas$accuracy_doubt20[cd] = sum((trW & prW)|(trM & prM)|(trD & prD))/length(pred)
        prW = pred<=.4
        prM = pred>=.6
        prD = pred>.4 & pred<.6
        metricas$accuracy_doubt10[cd] = sum((trW & prW)|(trM & prM)|(trD & prD))/length(pred)
      }
      gc(reset=T)
    }
  }
  return(metricas)
}

#####
# Extract iNaturalist records around Cape Town
# using rinat package
#####

r = raster(nrows=50, ncols=50, 
           xmn=18.106630, 
           xmx=19.102129, 
           ymn=-34.467458, 
           ymx=-33.757545,
           crs= CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))  
r[] = 1:ncell(r)
pts = rasterToPoints(r)
years = 2000:2021
extractions = as.data.frame(expand.grid(year=years,cell=pts[,3]))
extractions$status = "todo"
occ  = NULL
for(i in 1:dim(extractions)[1]){
  cell = extractions$cell[i]
  year = extractions$year[i]
  
  bounds = as.numeric(c(pts[pts[,3]==cell,2]-res(r)[2]/2,
                        pts[pts[,3]==cell,1]-res(r)[1]/2,
                        pts[pts[,3]==cell,2]+res(r)[2]/2,
                        pts[pts[,3]==cell,1]+res(r)[1]/2))
  
  if(year==min(years)){
    cd = extractions$cell==cell
    
    tmp=NULL
    while(is.null(tmp)){
      tmp = try( get_inat_obs(taxon_name="Plantae",
                              bounds = bounds,
                              maxresults = 9999),silent = T)
      Sys.sleep(.2)
    }
    extractions$status[cd] = "done"
    
    if(!is.character(tmp)){
      cat('found ',dim(tmp)[1],' obs at cell ',cell,' \n ')
      if(dim(tmp)[1]==9999){extractions$status[cd]="saturated"}
      if(is.null(occ)){occ = tmp}else{occ = rbind(occ,tmp)}
    }
  }
  
  if(i/200==round(i/200)){
    flush.console()
    cat('\r Processed ',round(1000*i/dim(extractions)[1])/10,'%... \n')
    setwd(saveDir)
    save(r,i,extractions,occ,file="saveExtr_iNat_CT")
  }
}

setwd(saveDir)
save(r,i,extractions,occ,file="saveExtr_iNat_CT")

setwd(saveDir)
load(file="saveExtr_iNat_CT")


#####
# Study area 
#####

setwd(admDir)
country = readRDS('gadm36_ZAF_0_sp.rds')
regions = readRDS('gadm36_ZAF_1_sp.rds')

# Load PRECIS
setwd(saveDir)
load(file="raster_SA")
centers = rasterToPoints(rSA)
colnames(centers) = c('x','y','cell')
centers = as.data.frame(centers)
centers = centers[centers$cell%in%cellsSelection,]
reso = .25
cellsPoly = list()
for(i in 1:dim(centers)[1]){
  x0 = centers$x[i]
  y0 = centers$y[i]
  tmp = data.frame(x=c(x0-reso/2,x0-reso/2,x0+reso/2,x0+reso/2,x0-reso/2),
                   y=c(y0+reso/2,y0-reso/2,y0-reso/2,y0+reso/2,y0+reso/2))
  tmp = as.matrix(tmp)
  cellsPoly[[i]] = st_polygon(list(tmp))
}
names(cellsPoly) = centers$cell

sfTest = st_sf(data.frame(cell=centers$cell,
                          geometry=st_sfc(cellsPoly)),
               crs=4326)  

# get subregion SpatialPolygonsDataFrame
tmp = regions[regions@data$NAME_1=="Western Cape",]
WC_CT = st_as_sf(tmp)

p = ggplot()+geom_sf(data=WC_CT)+
  geom_sf(data=sfTest,alpha=.05)+
  scale_x_continuous(limits=c(18.2,18.75))+
  scale_y_continuous(limits=c(-34.5,-33.75))

setwd(saveDir)
png('study_area_QtCells.png',height=800,width=1000)
print(p)
dev.off()

#####
# Clean iNaturalist records
#####

setwd(saveDir)
load(file="saveExtr_iNat_CT")

cols = c("usageKey",
         "rank","scientificName",
         "matchType","confidence",
         "synonym","status",
         "canonicalName","species",
         "speciesKey")
sps = data.frame(initName=unique(occ$scientific_name))
for(i in 1:dim(sps)[1]){
  tmp = as.data.frame(name_backbone(sps$initName[i],kingdom = "plantae"))  
  if(tmp$rank%in%c('SPECIES','SUBSPECIES','VARIETY')){
    tmp = tmp[,cols]
    if(i==1){
      toAdd=matrix(NA,dim(sps)[1],length(cols))
      colnames(toAdd)=cols
      toAdd=as.data.frame(toAdd)
      toAdd[1,] = tmp[1,]
    }else{toAdd[i,]= tmp[1,]}
  }
  cat(' \r Processed ',round(1000*i/dim(sps)[1])/10,'%')
}
sps = cbind(sps,toAdd)

setwd(saveDir)
write.table(sps,'iNat_gbif_matching.csv',sep=";",row.names=F,col.names=T)

occ = merge(occ,sps,by.x='scientific_name',by.y="initName",all.x=T)
occ = occ[!is.na(occ$species),c('species','longitude','latitude','time_observed_at','captive_cultivated','image_url','url','user_login','num_identification_agreements','num_identification_disagreements','quality_grade','positional_accuracy')]
occ$genus = sapply(strsplit(occ$species,split = ' '),function(el)el[1])

setwd(saveDir)
write.table(occ,'occ_CT_clean.csv',sep=";",row.names=F,col.names=T)

load(file="raster_SA")
occ$cellSA = extract(rSA,occ[,c('longitude','latitude')])
occ = occ[occ$cellSA%in%cellsSelection,colnames(occ)!="cellSA",]

write.table(occ,'occ_CT_inStudyArea.csv',sep=";",row.names=F,col.names=T)

######
# Raster of study area cells 
######

setwd(saveDir)
occ=read.csv('occ_CT_inStudyArea.csv',sep=";",header=T)
load(file="raster_SA")

grid = rasterToPoints(rSA) 
grid = grid[grid[,3]%in%cellsSelection,]
xmin = min(grid[,1])-res(rSA)[1]/2
xmax = max(grid[,1])+res(rSA)[1]/2
ymin = min(grid[,2])-res(rSA)[2]/2
ymax = max(grid[,2])+res(rSA)[2]/2
ext=extent(c(xmin,xmax,ymin,ymax))

rStu = raster(resolution=c(res(rSA)[1]/(2*57),res(rSA)[2]/(3*46)),ext=ext)

rTerreM = 6371000
ResY = pi*rTerreM*res(rStu)[2]/180
ResX = cos(pi*mean(grid[,2])/180)*rTerreM*pi*res(rStu)[1]/180

rStu[] = 1:ncell(rStu)
occ$cell = extract(rStu,occ[,c('longitude','latitude')])

occ$AgreeRate = occ$num_identification_agreements / (1+occ$num_identification_agreements+occ$num_identification_disagreements)
occ$trustable = (occ$captive_cultivated=="true" & occ$AgreeRate>=2/3) | occ$quality_grade=="research"

write.table(occ,'occ_with_cell.csv',sep=";",row.names=F,col.names=T)
saveRDS(object = rStu,file = 'raster_Study')

######
# match Rebello list with GBIF
######

setwd(saveDir)
occ=read.csv('occ_CT_clean.csv',sep=";",header=T)


setwd(alienDir)
rebList = read.csv('peninsula_invasives.csv',sep=";",header=T,stringsAsFactors = F)

cols = c("usageKey",
         "rank","scientificName",'family','class',
         "matchType","confidence",
         "synonym","status",
         "canonicalName","species",
         "speciesKey")
sps = data.frame(initName=rebList$Name)
for(i in 1:dim(sps)[1]){
  tmp = as.data.frame(name_backbone(sps$initName[i],kingdom = "plantae"))  
  if(tmp$rank%in%c('SPECIES','SUBSPECIES','VARIETY')){
    tmp = tmp[,cols]
    if(i==1){
      toAdd=matrix(NA,dim(sps)[1],length(cols))
      colnames(toAdd)=cols
      toAdd=as.data.frame(toAdd)
      toAdd[1,] = tmp[1,]
    }else{toAdd[i,]= tmp[1,]}
  }
  cat(' \r Processed ',round(1000*i/dim(sps)[1])/10,'%')
}
sps = cbind(sps,toAdd)

table(sps$matchType)

sps[!is.na(sps$matchType) & sps$matchType!="EXACT",c('matchType','initName','species','synonym')]

sps$species[is.na(sps$matchType)] = c('Sieruela monophylla','Dysphania ambrosioides',
                                      NA,'Glebionis segetum',
                                      'Crataegus gracilior','Lotus pedunculatus',
                                      NA,'Cylindropuntia imbricata',
                                      'Phelipanche ramosa','Plantago elongata',
                                      'Sagina procumbens','Setaria parviflora',NA)

sps[is.na(sps$matchType),c('initName','species')]

setwd(saveDir)
write.table(sps,'rebList_gbif_matching.csv',sep=";",row.names=F,col.names=T)

#######
# Integrate annotated records
####### 

setwd(saveDir)
occ=read.csv('occ_with_cell.csv',sep=";",header=T)

spDone = c('Centranthus ruber',
           'Schinus terebinthifolia',
           'Hypericum canariense',
           'Echium candicans',
           'Ficus carica',
           'Tecoma stans',
           'Spartium junceum',
           'Callistemon viminalis',
           'Duranta erecta',
           'Solanum lycopersicum',
           'Psidium guajava',
           'Phoenix canariensis') 

persons = c('Sjirk',
            'Christophe',
            'Luke',
            'Luke',
            'Luke',
            'Luke',
            'Mlu',
            'Christophe',
            'Sjirk',
            'Cang',
            'Cang',
            'Cang')

results = list(
  list(nDone=NA,# Sjirk Centranthus ruber
       managed=c(
         # RG
         11058360, 39538441,10952281,22994105,11208760,11264464, 45756178, 11201908, 11058347, 76542323, 67618397, 23355680, 11251895 ,43735612, 23134299, 43330329,75836318, 76547994, 24155090, 11208503, 11208500,11205969, 11205922, 34953345, 23627797,10912946,11205859, 8633688,11207442,
         # tagged cultivated
         46720894,23323714,33938364,24317404),
       doubt=c(
         # RG
         67130881, 11067601, 76069859, 11085637,76972219, 10959867, 66567754, 43078661, 11252112, 24022478, 66197959,11276120, 19936743, 65683684, 
         11251891, 11252112,11251877,76324131,60760704,8705649,23142536,10912811, 11066420, 11252111,11202005,11252122,56125484,22994450, 76248129, 11231355, 23101332,11201980, 11201995, 61306393,35253809,11208837, 77771221, 63153186,11201926, 17692118, 33608274,65146173, 11207464, 11208483, 35975864, 50410975,
         # tagged cultivated
         23101293,23101310,23101177,23101421, 23101140)),
  list(nDone=NA,# Christophe Schinus terebinthifolia
       managed=c(
         # RG
         22992688,83793142, 92078130,43284343,92910929,31053361,
         23340206, 43056375, 23157454, 35730494, 29270250, 83793241, 43609139,
         76969898, 76907678, 41858375, 63021509,93189633, 43212154, 43041669, 24154903, 43062011, 43632634, 35130124, 23384672, 84828037, 23586585,
         43119055, 43278738, 23108576, 66818840,
         23327654, 23592179, 23631191, 66825348, 23691368,
         19764933, 44610825, 86262628, 45339570, 23325093, 23879491, 21279084, 23391328, 11285001, 43281722,
         #tagged culti
         23591108, 38022895, 23942951, 76935108, 23607947, 95238913, 48825340, 43733466, 76468007, 23328074, 76920878, 23605569, 23643129,23395411),
       doubt=c(
         #RG
         62484808, 43888603, 43050061, 23379475,
         23917826, 43926454, 44104263, 24877750, 48772847, 43707546, 43608082, 75781574, 43323275,
         43117527, 76914570, 43321947, 87248759, 76881098, 43959298, 43294201, 23890785, 43900509, 24446381, 43885558, 36212941, 43077172, 43580779, 43277912, 75785314, 23895000, 43035291, 43294709, 43579073, 76901700, 43277940,
         46282291, 24877752, 23626337, 83269605, 46695313,43641456, 43096217, 76481852, 35730597, 41592846, 43058704, 43058706, 93233149, 24070097, 43278259, 43893473, 43565522, 43566635, 76072931, 75763543, 43873004, 35156543,
         24044781, 43883721, 43704870, 43900167, 43583029, 43944992, 76143222, 43306163, 76912221, 43278088, 89292756, 43853702, 75811810, 26235675, 43932735, 43860553, 44459646, 43462120, 23614364, 43282421, 43969196, 23613269, 43324443, 43041076, 23691368,
         76216187, 43878145, 80474419, 76835655, 43284442, 43054084, 43073330, 82550114, 24156594, 10888425, 43163226, 43619623, 21516228, 43054796, 75832392, 23874974, 88819413,  43561184, 61101629, 43580546, 43308864, 43113029, 43611955,
         # tagged culti
         74912880, 76447868, 23633553, 76958235, 23101142, 44109052)),
  list(nDone=NA,# Luke Hypericum canariense
       managed=c(11066361),
       doubt=c(10828581, 73770456, 37994441, 11045496, 68106662, 30627973, 11667520, 35739195, 43313862)),
  list(nDone=NA,# Luke Echium candicans
       managed=c(
         #RG
         76927931, 43586289, 43869106,
         #tagged cultivated
         17038521, 43630794, 23494178, 31406950, 94813288, 33608269),
       doubt=c(
         #RG
         37746533, 62770806, 33080542, 76544611, 60052613, 96360890, 28441071, 55022809, 33896957, 76453664, 55795166
         #tagged culti
       )),
  list(nDone=NA,# Luke Ficus carica
       managed=c(
         #RG
         76644220, 43618739, 76528663, 43594096, 43936401, 43593957, 43270095, 76156825, 23108017, 43304545,
         #tag culti
         43855996, 43913980, 43448979, 23344860, 24390080, 76113637, 96212882),
       doubt=c(
         #RG
         76554268, 44002207, 43733250, 43322125, 76883254, 76580978, 76980872, 24036607, 23584510, 23893740, 43885670, 39573218, 23889468, 23333299, 43970802, 76198438, 19764977, 76064296,
         #tag culti
         76920475)),
  list(nDone=NA,# Luke Tecoma stans
       managed=c(
         #RG
         22803215, 51371342,
         #tag culti
         26235410, 10912912, 23607714),
       doubt=c(#RG
         11268581, 35167673, 43974714, 43903247, 43906658, 24277422, 24275042, 75785126, 26235302, 77597730, 23901975, 76562129, 76072666, 23327457, 23359244, 43298540, 62576467, 42564745, 43277969, 10897443, 23941132,
         #tag culti
         66336074, 23335351, 23334187)),
  list(nDone=NA,# Mlu Spartium junceum
       managed=c(32976458, 39571474, 75587988, 75588003, 75587986, 11046135, 75587982, 75588000, 75587990, 75587976, 75587980, 37618170, 56786783, 11067543, 85893966, 10893804, 62233718),
       doubt=c(61206479, 62223928, 11284916, 39571471, 66918266, 77080497, 39571473, 38293756, 8713621, 92093106, 64585936, 19764984, 39571483, 63488035, 23439795, 75587978, 11284942, 66871552, 9382128, 28949398, 66057269, 11268754, 24013059, 76109927, 65265010, 19403768, 62486092, 35539871, 63308675, 76089104, 35253808, 70517350, 65426677, 46871475, 64436682, 76547684 , 11058373, 11095070, 64624105, 10840928, 11067525, 23753181, 76990973, 64029029, 34868635, 72453249, 86958062, 62032942, 34110456, 34111897, 35053560, 10854610, 62822641)),
  list(nDone=NA,#Christophe Callistemon viminalis
       managed=c(#RG
         38473119, 43646718, 10887854, 77075079, 23156841, 59671591, 23342714, 24135580,
         #tag culti
         23933310, 43572355, 45341882),
       doubt=c(#RG
         43089674, 76950536, 23117166, 30545754, 43905854, 44567826, 43269446, 10996136, 62482788, 23611272, 43117345, 22993277, 44101552, 43890538, 43269664, 43972035, 94712315, 43849912, 68057153, 43878530, 22993301, 77967420, 23344755, 23742969, 23328256, 76165931,19073500, 24071332,
         #tag culti
         43620751, 44100809, 44100823, 24160934)),
  list(nDone=NA,#Sjirk Duranta erecta
       managed=c(#RG
         43299320, 76238297, 76452736, 43132815, 76549675, 76891751,  23936023, 75778415,23586814, 77771241, 23324016, 39927060, 21553047, 67242369, 76471068, 45508330, 42473925, 76105003,  25792675, 23341149,  76562663,67961064,25792786,27286260,
         #tag culti
         43132810, 43640329, 43893742, 43274832, 39975310,74914468,43614686, 43274811),
       doubt=c(#RG
         38097999, 75826108,35914037,27285253,44107046,23890914,43633768, 42964778,51073038, 23320205,76891751, 23597258,23887493,38468738,25112371, 76924314, 70613906, 60498518,79918618, 43650015, 43889520, 44295417,
         #tag culti
         46228745, 76953750, 83319887, 23895453, 76973249,77591536, 23639090,43868015,23895392,44401599)),
  list(nDone=NA,# Cang Solanum lycopersicum
       managed=c(#RG
         43101800, 44373269, 76527442, 76467363, 43121816, 43534189, 23601865, 23116661, 43579366, 31169158, 43049733, 43566631, 23116308, 73843038, 46276746, 43299771, 23884902, 23337534, 24165215, 44361940, 43189643, 43093856, 43292672, 43041024, 22604589, 24272347, 23117224, 43296246, 23117482, 43294430, 49630308, 22904558, 41925944, 23615614, 43332745, 23117795,
         #tag culti 
         56314665, 43575292, 43593816, 44599659, 43277149, 43860411, 43116229, 43296130, 76953091),
       doubt=c(#RG
         23889102,
         #tag culti
         44563529, 43277745)),
  list(nDone=NA,# Cang Psidium guajava
       managed=c(#RG
         52612825, 43919908, 43868770, 43569269, 43324988, 76520354, 43861592, 43856213, 43861158, 43580541, 11067572, 43855496, 76135238, 23602097, 76520591, 43652071, 43297866, 43598164, 43861143, 43859284, 43268927, 43762836, 43861972, 43762836, 42227481, 10888572, 43594811, 43970660, 43603309, 42227549, 23656587, 36215930, 75903414, 23584054, 23116244, 43298006, 75821701, 43357620, 43294205, 43312863, 75831863, 23595674, 76141800, 43313753, 23348379, 55788531, 43311573,
         #tag culti
         43046029, 35771903, 43096676, 43108934),
       doubt=c(#RG
         24356260, 24507846,
         #tag culti
         23893575, 43910011, 43269816, 43269816, 43579949, 43579949, 43097152, 43269799)),
  list(nDone=NA,# Cang Phoenix canariensis
       managed=c(#RG
         94243682, 43599904, 59671812, 76165903, 39520981, 43282689, 42961203, 43059049, 43610298, 70510595, 43855820, 43686360, 43337629, 87705595, 43928950, 43927521, 75828733, 43865701, 42038685, 23864957, 75802428, 52612920, 43928280, 39520971, 75802089, 23644319, 43592930, 75584720, 43559965, 36139084, 23345534, 43599129, 23993531, 73039065, 52612885, 43311391, 10895798, 43860842, 43870398, 75584658, 43893696, 43052034, 23671447, 75584703, 55081710, 54922555, 23907940, 86902445, 43062369, 27227315, 43087598, 43630084, 43427301, 43865739, 43089643, 92909920, 43859813, 23649350, 23881375, 43121097, 43595646, 43567262, 71488584, 71488594, 24135564, 34526398, 43880776,
         #tag culti
         43338707, 44124250, 23865977, 23682860, 11202114, 24681126, 24135640, 44400671, 12824626, 27242274, 11294213, 24062271, 23599867, 23406774, 24137948, 23979046, 23404653, 24156587, 23319557, 60940591, 23594530, 24007517, 33286560, 24013499, 24164970, 19029544, 23980074, 43616336, 23910048, 43071675, 23587009, 11109866),
       doubt=c(#RG
         19073841, 23447238, 11100939, 23627858, 56878383, 48762376, 44151903, 51818828, 39520984,92372090, 10686107, 43283059, 43632958, 48756907 , 10889559, 50291505, 38097978, 23436718,
         #tag culti
         43291835, 24501553, 43921626))
)
names(results)=spDone

setwd(saveDir)
labelled_dataset = list(results,persons)
save(labelled_dataset,file="labelled_dataset")

dfLab = occ[occ$species%in%names(labelled_dataset[[1]]) & ((occ$captive_cultivated=="true" & occ$trustable ) | occ$quality_grade=="research"),]
dfLab$annotation = "wild"
for(i in 1:length(labelled_dataset[[1]])){
  cultiURLs = paste('https://www.inaturalist.org/observations/',labelled_dataset[[1]][[i]]$managed,sep="")
  dfLab$annotation[dfLab$species==names(labelled_dataset[[1]])[i] & dfLab$url%in%cultiURLs] = "managed"
  doubtURLs = paste('https://www.inaturalist.org/observations/',labelled_dataset[[1]][[i]]$doubt,sep="")
  dfLab$annotation[dfLab$species==names(labelled_dataset[[1]])[i] & dfLab$url%in%doubtURLs] = "doubtful"
}
colnames(dfLab)[colnames(dfLab)=="captive_cultivated"]="iNat_tag"
dfLab$iNat_tag=as.character(dfLab$iNat_tag)
dfLab$iNat_tag[dfLab$iNat_tag=="true"]="cultivated"
dfLab$iNat_tag[dfLab$iNat_tag=="false"]="wild"
write.table(dfLab,'occ_labelled.csv',sep=";",row.names=F,col.names=T)

#######
# Extract protection status
#######

setwd(landUse)
types = read.csv("biodiversity network/Biodiversity_network_table.csv",sep=";",header=T)
types = unique(types[,c('CBA_Catego','Descriptio','Managed','Significan','Objective','Subtype','ACTION','Compat_Act','Legislatio')])
types$alienControl = c(F,T,F,
                       T,T,T,
                       T,T,F,
                       T,T,F,
                       F)
write.table(types,'biodiversity network/protected_types.csv',sep=";",row.names=F,col.names=T)

protec <- st_read("biodiversity network/SL_BDVR_NTWR.shp")
alien = protec[protec$CBA_Catego%in%as.character(types$CBA_Catego[types$alienControl]),]

dfLab$protecArea = NA
dfLab$alienControl = NA
for(i in 1:dim(dfLab)[1]){
  coos = data.frame(x=dfLab$longitude[i],y=dfLab$latitude[i])
  pt= coos[1,] %>% 
    as.numeric %>% 
    st_point(dim="XY") %>% 
    st_sfc(crs=4326) %>%
    st_transform(st_crs(protec))
  dfLab$protecArea[i] = dim(st_intersection(protec,pt))[1]>0
  dfLab$alienControl[i] = dim(st_intersection(alien,pt))[1]>0
  if(i/10==round(i/10)){
    flush.console()
    cat('\r Processed...',round(1000*i/dim(dfLab)[1])/10,'%')
    setwd(saveDir)
    write.table(dfLab,'test.csv',sep=";",row.names=F,col.names=T)
  }
}
setwd(saveDir)
write.table(dfLab,'occ_labelled_protec.csv',sep=";",row.names=F,col.names=T)

#dfLab$count=1
#aggregate(count~protecArea+alienControl+annotation,data=dfLab,FUN=sum)

#######
# Extract (distance to) watercourses
#######

setwd(saveDir)
dfLab=read.csv('occ_labelled_protec.csv',sep=";",header=T,stringsAsFactors = F)

setwd(landUse)
water <- st_read("watercourses/Open_Watercourses.shp")
types = data.frame(
  LU_TYPE_CD=c(31,32,30,34,33,35),
  label=c('Canal Lined',
          'Channel Open',
          'Canal Composite',
          'Natural River/Streams',
          'Gap Connector/Flow Path',
          'Stream Extension'),
  artificial=c(T,T,T,F,T,T))
rivers = water[water$LU_TYPE_CD%in%types$LU_TYPE_CD[!types$artificial],]
canals = water[water$LU_TYPE_CD%in%types$LU_TYPE_CD[types$artificial],]
rm(water);gc(reset=T)
#ggplot(data=water[5,,drop=F])+geom_sf()

dfLab$dToRiverM = NA
dfLab$dToCanalM = NA
for(i in 1:dim(dfLab)[1]){
  coos = data.frame(x=dfLab$longitude[i],y=dfLab$latitude[i])
  pt= coos[1,] %>% 
    as.numeric %>% 
    st_point(dim="XY") %>% 
    st_sfc(crs=4326) %>%
    st_transform(st_crs(rivers))
  dfLab$dToRiverM[i] = min(as.vector(st_distance(rivers,pt)))
  dfLab$dToCanalM[i] = min(as.vector(st_distance(canals,pt)))
  if(i/10==round(i/10)){
    flush.console()
    cat('\r Processed...',round(1000*i/dim(dfLab)[1])/10,'%')
    setwd(saveDir)
    write.table(dfLab,'occ_labelled_protec_eau.csv',sep=";",row.names=F,col.names=T)
  }
}
setwd(saveDir)
write.table(dfLab,'occ_labelled_protec_eau.csv',sep=";",row.names=F,col.names=T)

#dfLab$count=1
#aggregate(count~protecArea+alienControl+annotation,data=dfLab,FUN=sum)

#######
# Extract land parcel status
#######

setwd(saveDir)
dfLab=read.csv('occ_labelled_protec_eau.csv',sep=";",header=T,stringsAsFactors = F)

setwd(landUse)
parcel <- st_read(
  "land parcel/SL_LAND_PRCL.shp")
parcel$PRTY_NMBR = as.character(parcel$PRTY_NMBR)
parcel$LU_LGL_STS= as.character(parcel$LU_LGL_STS)

suburbs = st_read("suburbs/SL_OFC_SBRB.shp")

tab = read.csv('land parcel/Land_Parcels.csv',sep=",",header=T)
tab$OFC_SBRB_NAME = as.character(tab$OFC_SBRB_NAME)
tab$PRTY_NMBR = as.character(tab$PRTY_NMBR)

if(F){
  classes = c("General Residential 1 : Group Housing",
              "General Residential 2",
              "General Residential 3",# Various types of residential plots including house with gardens 
              "Single Residential 1 : Conventional Housing",#Residences
              "Single Residential 2 : Incremental Housing",#Shacks (not kept)
              "General Residential 5",# in construction, big building (not kept)
              "General Residential 4",# Family houses
              "General Residential 6")# Very rare
  
  i=4
  tmp = tab[tab$ZONING==classes[i],,drop=F]
  suburb = data.frame(x=NA)
  suburb = suburb[-1,,drop=F]
  tmpParcels = suburb
  while(dim(suburb)[1]==0 & dim(tmpParcels)[1]==0){
    id = sample(1:dim(tmp)[1],1)
    suburb = suburbs[suburbs$NAME==tmp$OFC_SBRB_NAME[id],,drop=F]
    tmpParcels = parcel[parcel$PRTY_NMBR==tmp$PRTY_NMBR[id],,drop=F]
  }
  idFound= NULL
  for(k in 1:dim(suburb)[1]){
    inSub = st_intersects(suburb[k,,drop=F],tmpParcels,sparce=F)
    if(length(inSub[[1]])>0){
      idFound = inSub[[1]][1]
    }
    print(idFound)
  }
  parcelo = tmpParcels[idFound,,drop=F]
  cat(mean(st_bbox(parcelo)[c(2,4)]),',',mean(st_bbox(parcelo)[c(1,3)]))
  
}

dfLab$landParcel = NA
for(i in 1:dim(dfLab)[1]){
  coos = data.frame(x=dfLab$longitude[i],y=dfLab$latitude[i])
  clacla = getParcel2(coos)
  if(length(clacla)>0){
    dfLab$landParcel[i] = paste(clacla,collapse = ",")
  }
  if(i/10==round(i/10)){
    flush.console()
    cat('\r Processed...',round(1000*i/dim(dfLab)[1])/10,'%')
    setwd(saveDir)
    save(dfLab,file = 'tmp_occ_with_parcel.Rdata')
  }
}
setwd(saveDir)
save(dfLab,file='tmp_occ_with_parcel.Rdata')

dfLab$landParcel_clean = NA
for(i in which(!is.na(dfLab$landParcel))){
  seqo = dfLab$landParcel[i] 
  vec=unique(strsplit(seqo,',')[[1]])
  vec = vec[order(vec)]
  vec=vec[vec!=""]
  if(length(vec)>0){
    dfLab$landParcel_clean[i] = paste(vec,collapse = ",")
  }
}
setwd(saveDir)
save(dfLab,file='tmp_occ_with_parcel.Rdata')

resid = c("General Residential 1 : Group Housing",#Residences
          "General Residential 2", 
          "General Residential 3", 
          "Single Residential 1 : Conventional Housing",
          "General Residential 4",# "Family" houses
          "Single Residential 2 : Incremental Housing",#Shacks 
          "General Residential 5",# big buildings in construction
          "General Residential 6")# Quite rare
road = c('Transport 2 : Public Road and Public Parking')
agri = c('Agricultural')
open = c('Open Space')
business = c("Local Business",
             "General Business",
             "General Industrial")

dfLab$parcel_resid = F
for(i in 1:length(resid)){
  dfLab$parcel_resid[!is.na(dfLab$landParcel_clean) & regexpr(resid[i],dfLab$landParcel_clean)>0] = T
}
dfLab$parcel_business = F
for(i in 1:length(business)){
  dfLab$parcel_business[!is.na(dfLab$landParcel_clean) & regexpr(business[i],dfLab$landParcel_clean)>0] = T
}
dfLab$parcel_road = !is.na(dfLab$landParcel_clean) & regexpr(road,dfLab$landParcel_clean)>0
dfLab$parcel_agri = !is.na(dfLab$landParcel_clean) & regexpr(agri,dfLab$landParcel_clean)>0
dfLab$parcel_open = !is.na(dfLab$landParcel_clean) & regexpr(open,dfLab$landParcel_clean)>0

dfLab$nParcelType = sapply(1:dim(dfLab)[1],
                           function(i) dfLab$parcel_resid[i] + 
                             dfLab$parcel_road[i] + 
                             dfLab$parcel_agri[i] + 
                             dfLab$parcel_open[i] + 
                             dfLab$parcel_business[i])
table(dfLab$nParcelType[cd])

setwd(saveDir)
write.table(dfLab,'occ_labelled_protec_eau_parcel.csv',sep=";",row.names=F,col.names=T)

#######
# Extract land cover class
#######

setwd(main)
df = read.csv('dataset/occ_labelled_protec_eau_parcel.csv',sep=";",header=T)
setwd(landUse)
clasTab = read.csv("land cover/land_cover_classes.csv",sep=",",header=T)
colnames(clasTab)[1]="landCover"
r=raster("land cover/SA_NLC_2020_GEO.tif")
spatial_extent=extent(c(18,19.2,-34.5,-33.7))
r_landCover=raster::crop(r,spatial_extent)
writeRaster(r_landCover,
            filename="land cover/lc_croped.tif",
            format="GTiff")
r_landCover=raster("land cover/lc_croped.tif")
df$landCover=extract(r_landCover,df[,c('longitude','latitude')])
df=merge(df,clasTab[,c('landCover','simple_lc')],by="landCover",all.x=T)
df$simple_lc=as.character(df$simple_lc)
setwd(saveDir)
write.table(df,'occ_labelled_protec_eau_parcel_lc.csv',sep=";",row.names=F,col.names=T)

#######
# Extract observer variables
#######

setwd(saveDir)
df = read.csv('occ_labelled_protec_eau_parcel_lc.csv',sep=";",header=T)
occ=read.csv('occ_with_cell.csv',sep=";",header=T,stringsAsFactors = F)
nSa=dim(df)[1]
df$nReviews=sapply(1:nSa,function(i)df$num_identification_agreements[i]+df$num_identification_disagreements[i])

obs = data.frame(obs=unique(df$user_login))
obs$tagged = sapply(obs$obs,function(obso)sum(occ$captive_cultivated=="true" & occ$user_login==obso)>0)
obs$tagFreq = sapply(obs$obs,function(obso)sum(occ$captive_cultivated=="true" & occ$user_login==obso)/sum(occ$user_login==obso))
obs$nObs = sapply(obs$obs,function(obso)sum(occ$user_login==obso))
obs$nSp = sapply(obs$obs,function(obso)length(unique(occ$species[occ$user_login==obso & occ$trustable])))
df=merge(df,obs,
         by.x="user_login",
         by.y="obs",all.x=T)
df$lnObs=log(df$nObs)
df$lnSp=log(df$nSp)
df$positional_accuracy[is.na(df$positional_accuracy)]=mean(df$positional_accuracy,na.rm=T)
setwd(saveDir)
write.table(df,'occ_labelled_protec_eau_parcel_lc_obs.csv',sep=";",row.names=F,col.names=T)

#######
# Figure 1: Map of 99K iNaturalist trusted records
#######

setwd(main)
occ=read.csv('dataset/occ_with_cell.csv',sep=";",header=T)
r_landCover=raster("land use/land cover/lc_croped.tif")
occ$landCover=raster::extract(r_landCover,occ[,c('longitude','latitude')])
rebList=read.csv('dataset/rebList_gbif_matching.csv',sep=";",header=T)
invList = unique(rebList$species[!is.na(rebList$species)])
boda=read.csv(paste0(main,'input data/species_in_PRECIS_cells.csv'),sep=";",header=T)
nativeSp = unique(boda$species)
nativeSp = setdiff(nativeSp,invList)
occFilt=occ%>%  # onLand & species exist & position accurate
  filter(!landCover%in%c(16,17) & (is.na(positional_accuracy) | positional_accuracy<100) & species%in%c(invList,nativeSp) & trustable)
write.table(occFilt,'dataset/occ_with_cell_filtered.csv',sep=";",row.names=F,col.names=T)

setwd(main)
occFilt=read.csv('dataset/occ_with_cell.csv',sep=";",header=T)

load(file="dataset/raster_SA")
centers = rasterToPoints(rSA)
colnames(centers) = c('x','y','cell')
centers = as.data.frame(centers)
centers = centers[centers$cell%in%cellsSelection,]
reso = .25
cellsPoly = list()
for(i in 1:dim(centers)[1]){
  x0 = centers$x[i]
  y0 = centers$y[i]
  tmp = data.frame(x=c(x0-reso/2,x0-reso/2,x0+reso/2,x0+reso/2,x0-reso/2),
                   y=c(y0+reso/2,y0-reso/2,y0-reso/2,y0+reso/2,y0+reso/2))
  tmp = as.matrix(tmp)
  cellsPoly[[i]] = st_polygon(list(tmp))
}
names(cellsPoly) = centers$cell

sfTest = st_sf(data.frame(cell=centers$cell,
                          geometry=st_sfc(cellsPoly)),
               crs=4326)  

rStu = readRDS(file = 'dataset/raster_Study')
grid = as.data.frame(rasterToPoints(rStu))
colnames(grid)[3]="cell"
# Filter cells in sea
load(file='dataset/WCpoly.Rdata')
pts <- SpatialPoints(grid[,c('x','y')], proj4string = CRS(proj4string(WCpoly)))
isIn=sp::over(pts,WCpoly)
grid=grid[which(!is.na(isIn$NAME_1)),]
#ggplot()+geom_sf(data=st_as_sf(WCpoly))+geom_point(data=gridTmp,aes(x=x,y=y))

toPlot=occFilt%>%  # count per cell
  group_by(cell)%>%
  summarise(nOcc=n())%>%
  filter(nOcc>0)%>%
  merge(grid[!is.na(grid$x),],by='cell',all.x=T)%>%
  filter(!is.na(x))

breaks = c(.9,stats::quantile(x = toPlot$nOcc ,probs=c(.5,.75,.95),na.rm=T),max(toPlot$nOcc,na.rm=T)+1)
toPlot$class=cut(toPlot$nOcc,breaks)
palette = colorRampPalette(c('goldenrod','darkorchid4'))
legend=data.frame(
  class=levels(toPlot$class),
  name=c('[1,3]','[4,10]','[11,46]','[47,765]'),
  color = palette(length(levels(toPlot$class)))
  )
toPlot=toPlot%>%
  merge(legend,by="class",all.x=T)

map <- get_stadiamap(  bbox = c(left = 18.25, bottom = -34.5, right = 18.75, top = -33.75), 
                       zoom = 11, 
                       maptype = "stamen_toner_lite")
#map <- get_stamenmap( 
#  bbox = c(left = 18.25, bottom = -34.5, right = 18.75, top = -33.75), 
#  zoom = 11, 
#  maptype = "toner-lite")

p <- ggmap(map)+ geom_sf(data=sfTest,alpha=.02, inherit.aes = FALSE)+
  geom_tile(data=toPlot,aes(x=x,y=y,fill=class),alpha=.7)+
  scale_fill_manual(name="Number of \niNaturalist \nrecords",values=legend$color)+
  xlab('Longitude')+ylab('Latitude')+theme(text=element_text(size=30))

png('Figure_1.png',width=2000,height=2000)
print(p)
dev.off()

######
# Classif model testing 
######

setwd(saveDir)
DF=read.csv('occ_labelled_protec_eau_parcel_lc_obs_visu_glob.csv',sep=";",header=T)
nSa=dim(DF)[1]
DF$y = sapply(DF$annotation,function(ano)if(ano=="managed"){1}else if(ano=="doubtful"){.5}else{0})
DF$simple_lc=factor(DF$simple_lc)
DF[,toScale] = scale(DF[,toScale],center = T,scale=T)

nFold = 20
modTypes = c('glmnet',
             'glm_selec',
             'glm_selec_soft',
             'glmnet_soft',
             'glm_full',
             'glm_full_weighted',
             'glm_full_soft',
             'glm_full_soft_weighted',
             'RF',
             'RF_doubt',
             'RF_doubt_weighted',
             'RF_doubt_weighted2')

metrics=as.data.frame(
  expand.grid(model=modTypes,
              rep=c(1:nFold),
              set=names(varSets)))
metrics$accuracy = NA
metrics$train_accuracy = NA
metrics$prWilVstrWil = NA
metrics$prManVstrMan = NA
metrics$accuracy_doubt10 = NA
metrics$accuracy_doubt20 = NA
metrics$accuracy_doubt40 = NA

# iNat tag ONLY 
metrico = metrics[1,,drop=F]
metrico$model='tag';metrico$set="inatTag"
tru = DF$annotation=="managed" 
wild=DF$annotation=="wild"
pred= DF$iNat_tag=="cultivated"
noD = DF$annotation!="doubtful"
metrico$accuracy=sum(as.numeric(pred[noD])==as.numeric(tru[noD]))/length(pred[noD])
metrico$prWilVstrWil=sum(!pred[wild])/sum(wild)
metrico$prManVstrMan=sum(pred[tru])/sum(tru)
metrico$accuracy_doubt10 = metrico$accuracy*length(pred[noD])/dim(DF)[1]
metrico$accuracy_doubt20 = metrico$accuracy_doubt10
metrico$accuracy_doubt40 = metrico$accuracy_doubt10

set.seed(32)

maxNodeSize=15
# Shuffle dataset
SH=DF[sample(nrow(DF)),]
# Create nFold folds
bdWidth=round(dim(DF)[1]/nFold)
folds=lapply(1:(nFold-1),function(i)((i-1)*bdWidth+1):(i*bdWidth))
folds[[nFold]]=((nFold-1)*bdWidth+1):dim(SH)[1]
# Other methods
for(varSet in names(varSets)){
  print(varSet)
  df=SH[,c('y',"annotation",varSets[varSet][[1]])]
  covar=varSets[varSet][[1]]
  transf=transf_full[sapply(transf_full,function(tra)sum(sapply(covar,function(va)regexpr(va,tra)>0))>0)]
  for(i in 1:nFold){
    cat('\n Rep ',i,' \n')
    # Fit using glm
    trainSamp = !(1:nSa)%in%folds[[i]]
    train = df[trainSamp,]
    valid = df[!trainSamp,]
    nValid=sum(!trainSamp)
    noD = train$annotation!="doubtful"
    vnoD=valid$annotation!="doubtful"
    truTrain = as.numeric(train$annotation[noD]=="managed") 
    for(modo in modTypes){
      print(modo)
      if(modo=='glm_full'){
        # GLM full 
        fu=paste0("~",paste(c(covar,transf),collapse = "+"))
        mod=glm(as.formula(paste0('y',fu)),
                family=binomial(link = "logit"),
                data=train[noD,])
        validX=model.matrix(as.formula(fu),data=valid)
        pred= (validX%*%mod$coefficients) %>%
          as.vector %>%
          (function(lp){exp(lp)/(1+exp(lp))})
        predF = pred
        predTrain= (model.matrix(as.formula(fu),train[noD,])%*%mod$coefficients) %>%
          as.vector %>%
          (function(lp){exp(lp)/(1+exp(lp))}) %>%
          sapply(function(pp)pp>.5)
        trainAcc=sum((predTrain-truTrain)==0)/sum(noD)
      }
      else if(modo=='glm_full_weighted'){
        # GLM full without intercept
        weights=sapply(train$annotation,function(ano)if(ano=="wild"){1/sum(train$annotation=="wild")}else if(ano=="managed"){1/sum(train$annotation=="managed")}else{1/sum(train$annotation=="doubtful")})
        fu=paste0("~",paste(c(covar,transf),collapse = "+"))
        mod=glm(as.formula(paste0('y',fu)),
                family=binomial(link = "logit"),
                data=train[noD,],weights = weights[noD])
        validX=model.matrix(as.formula(fu),data=valid)
        pred= (validX%*%mod$coefficients) %>%
          as.vector %>%
          (function(lp){exp(lp)/(1+exp(lp))})
        predW=pred
        predTrain= (model.matrix(as.formula(fu),train[noD,])%*%mod$coefficients) %>%
          as.vector %>%
          (function(lp){exp(lp)/(1+exp(lp))}) %>%
          sapply(function(pp)pp>.5)
        trainAcc=sum((predTrain-truTrain)==0)/sum(noD)
      }
      else if(modo=="glmnet"){
        # GLM L1-regularized full 
        fu=paste0("~",paste(c(covar,transf),collapse = "+"))
        yMat=matrix(c(1-train$y,train$y),sum(trainSamp),2)
        des=sparse.model.matrix(as.formula(fu),data=train)
        lambdas=cv.glmnet(x=des[noD,],y=yMat[noD,],family="binomial",nfolds=3)[c('lambda.min','lambda','glmnet.fit')]
        coefs=lambdas$glmnet.fit$beta[,lambdas$lambda==lambdas$lambda.min]
        coefs[1]=as.numeric(lambdas$glmnet.fit$a0[lambdas$lambda==lambdas$lambda.min])
        validX=model.matrix(as.formula(fu),data=valid)
        pred= (validX%*%coefs) %>%
          as.vector %>%
          (function(lp){exp(lp)/(1+exp(lp))})
        selectedVariables = names(coefs)[coefs!=0]
        predTrain= (des[noD,,drop=F]%*%coefs) %>%
          as.vector %>%
          (function(lp){exp(lp)/(1+exp(lp))}) %>%
          sapply(function(pp)pp>.5)
        trainAcc=sum((predTrain-truTrain)==0)/sum(noD)
      }
      else if(modo=='glm_selec'){
        fu=paste0("~",paste(c(covar,transf),collapse = "+"))
        des=model.matrix(as.formula(fu),data=train)
        des=des[,selectedVariables,drop='F']
        mod=glm.fit(des[noD,],train$y[noD],family=binomial(link = "logit"))
        validX= as.formula(fu) %>%
          model.matrix(data=valid) %>%
          subset(select=selectedVariables)
        pred= (validX%*%mod$coefficients) %>%
          as.vector %>%
          (function(lp){exp(lp)/(1+exp(lp))})
        predS = pred
        predTrain= (des[noD,,drop=F]%*%mod$coefficients) %>%
          as.vector %>%
          (function(lp){exp(lp)/(1+exp(lp))}) %>%
          sapply(function(pp)pp>.5)
        trainAcc=sum((predTrain-truTrain)==0)/sum(noD)
      }
      else if(modo=='glm_full_soft'){
        # GLM full 
        fu=paste0("~",paste(c(covar,transf),collapse = "+"))
        mod=glm(as.formula(paste0('y',fu)),
                family=binomial(link = "logit"),
                data=train)
        validX=model.matrix(as.formula(fu),data=valid)
        pred= (validX%*%mod$coefficients) %>%
          as.vector %>%
          (function(lp){exp(lp)/(1+exp(lp))})
        predF = pred
        predTrain= (model.matrix(as.formula(fu),train[noD,])%*%mod$coefficients) %>%
          as.vector %>%
          (function(lp){exp(lp)/(1+exp(lp))}) %>%
          sapply(function(pp)pp>.5)
        trainAcc=sum((predTrain-truTrain)==0)/sum(noD)
      }
      else if(modo=='glm_full_soft_weighted'){
        # GLM full without intercept
        weights=sapply(train$annotation,function(ano)if(ano=="wild"){1/sum(train$annotation=="wild")}else if(ano=="managed"){1/sum(train$annotation=="managed")}else{1/sum(train$annotation=="doubtful")})
        fu=paste0("~",paste(c(covar,transf),collapse = "+"))
        mod=glm(as.formula(paste0('y',fu)),
                family=binomial(link = "logit"),
                data=train,weights = weights)
        validX=model.matrix(as.formula(fu),data=valid)
        pred= (validX%*%mod$coefficients) %>%
          as.vector %>%
          (function(lp){exp(lp)/(1+exp(lp))})
        predW=pred
        predTrain= (model.matrix(as.formula(fu),train[noD,])%*%mod$coefficients) %>%
          as.vector %>%
          (function(lp){exp(lp)/(1+exp(lp))}) %>%
          sapply(function(pp)pp>.5)
        trainAcc=sum((predTrain-truTrain)==0)/sum(noD)
      }
      else if(modo=="glmnet_soft"){
        # GLM L1-regularized full 
        fu=paste0("~",paste(c(covar,transf),collapse = "+"))
        yMat=matrix(c(1-train$y,train$y),sum(trainSamp),2)
        des=sparse.model.matrix(as.formula(fu),data=train)
        lambdas=cv.glmnet(x=des,y=yMat,family="binomial",nfolds=3)[c('lambda.min','lambda','glmnet.fit')]
        coefs=lambdas$glmnet.fit$beta[,lambdas$lambda==lambdas$lambda.min]
        coefs[1]=as.numeric(lambdas$glmnet.fit$a0[lambdas$lambda==lambdas$lambda.min])
        validX=model.matrix(as.formula(fu),data=valid)
        pred= (validX%*%coefs) %>%
          as.vector %>%
          (function(lp){exp(lp)/(1+exp(lp))})
        selectedVariables = names(coefs)[coefs!=0]
        predTrain= (des[noD,,drop=F]%*%coefs) %>%
          as.vector %>%
          (function(lp){exp(lp)/(1+exp(lp))}) %>%
          sapply(function(pp)pp>.5)
        trainAcc=sum((predTrain-truTrain)==0)/sum(noD)
      }
      else if(modo=='glm_selec_soft'){
        fu=paste0("~",paste(c(covar,transf),collapse = "+"))
        des=model.matrix(as.formula(fu),data=train)
        des=des[,selectedVariables,drop=F]
        mod=glm.fit(des,train$y,family=binomial(link = "logit"))
        validX= as.formula(fu) %>%
          model.matrix(data=valid) %>%
          subset(select=selectedVariables)
        pred= (validX%*%mod$coefficients) %>%
          as.vector %>%
          (function(lp){exp(lp)/(1+exp(lp))})
        predS = pred
        predTrain= (des[noD,,drop=F]%*%mod$coefficients) %>%
          as.vector %>%
          (function(lp){exp(lp)/(1+exp(lp))}) %>%
          sapply(function(pp)pp>.5)
        trainAcc=sum((predTrain-truTrain)==0)/sum(noD)
      }
      else if(modo=="RF"){
        nVar=length(c(covar,transf))
        fu=paste0("~",paste(c(covar,transf),collapse = "+"))
        des=model.matrix(as.formula(fu),data=train)
        mtries=seq(1,min(nVar,25),3)
        nodesizes=seq(1,maxNodeSize,3)
        Err=matrix(NA,length(mtries),length(nodesizes))
        for(pili in mtries){
          for(nono in nodesizes){
            RF = randomForest(x=des[noD,],y=factor(train$annotation[noD]),
                              ntree=300,
                              nodesize = nono,
                              mtry= pili)
            Err[which(mtries==pili),which(nodesizes==nono)]=mean(RF$err.rate[,'OOB'])
          }
        }
        BestMtry=which.min(sapply(1:nrow(Err),function(k)min(Err[k,])))
        BestSize=which.min(sapply(1:ncol(Err),function(l)min(Err[,l])))
        
        RF = randomForest(x=des[noD,],y=factor(train$annotation[noD]),
                          ntree=500,
                          nodesize = BestSize,
                          mtry= BestMtry)
        
        validX=model.matrix(as.formula(fu),data=valid)
        predMat=RF %>%
          predict(validX,"prob")
        pred = as.numeric(predMat[,"managed"])/as.numeric(rowSums(predMat[,c('wild',"managed")]))
        predRF=pred
        predTrain= as.character(RF$predicted)=="managed"
        trainAcc=sum((predTrain-truTrain)==0)/sum(noD)
        gc(reset=T)
      }
      else if(modo=="RF_doubt"){
        nVar=length(c(covar,transf))
        fu=paste0("~",paste(c(covar,transf),collapse = "+"))
        des=model.matrix(as.formula(fu),data=train)
        mtries=seq(1,min(nVar,25),3)
        nodesizes=seq(1,maxNodeSize,3)
        Err=matrix(NA,length(mtries),length(nodesizes))
        for(pili in mtries){
          for(nono in nodesizes){
            RF = randomForest(x=des,y=factor(train$annotation),
                              ntree=300,
                              nodesize = nono,
                              mtry= pili)
            Err[which(mtries==pili),which(nodesizes==nono)]=mean(RF$err.rate[,'OOB'])
          }
        }
        BestMtry=which.min(sapply(1:nrow(Err),function(k)min(Err[k,])))
        BestSize=which.min(sapply(1:ncol(Err),function(l)min(Err[,l])))
        
        RF = randomForest(x=des,y=factor(train$annotation),
                          ntree=500,
                          nodesize = nodesizes[BestSize],
                          mtry= mtries[BestMtry])
        
        validX=model.matrix(as.formula(fu),data=valid)
        predMat=RF %>%
          predict(validX,"prob")
        pred = as.numeric(predMat[,"managed"])/as.numeric(rowSums(predMat[,c('wild',"managed")]))
        predRFD=pred
        predTrain= as.character(RF$predicted[noD])=="managed"
        trainAcc=sum((predTrain-truTrain)==0)/sum(noD)
      }
      else if(modo=="RF_doubt_weighted"){
        weights=sapply(train$annotation,function(ano)if(ano=="wild"){1/sum(train$annotation=="wild")}else if(ano=="managed"){1/sum(train$annotation=="managed")}else{1/sum(train$annotation=="doubtful")})
        nVar=length(c(covar,transf))
        fu=paste0("~",paste(c(covar,transf),collapse = "+"))
        des=model.matrix(as.formula(fu),data=train)
        mtries=seq(1,min(nVar,25),3)
        nodesizes=seq(1,maxNodeSize,3)
        Err=matrix(NA,length(mtries),length(nodesizes))
        for(pili in mtries){
          for(nono in nodesizes){
            RF = randomForest(x=des,y=factor(train$annotation),
                              ntree=300,weights = weights,
                              nodesize = nono,
                              mtry= pili)
            Err[which(mtries==pili),which(nodesizes==nono)]=mean(RF$err.rate[,'OOB'])
          }
        }
        BestMtry=which.min(sapply(1:nrow(Err),function(k)min(Err[k,])))
        BestSize=which.min(sapply(1:ncol(Err),function(l)min(Err[,l])))
        
        RF = randomForest(x=des,y=factor(train$annotation),
                          ntree=500,weights = weights,
                          nodesize = nodesizes[BestSize],
                          mtry= mtries[BestMtry])
        validX=model.matrix(as.formula(fu),data=valid)
        predMat=RF %>%
          predict(validX,"prob")
        pred = as.numeric(predMat[,"managed"])/as.numeric(rowSums(predMat[,c('wild',"managed")]))
        predRFD=pred
        predTrain= as.character(RF$predicted[noD])=="managed"
        trainAcc=sum((predTrain-truTrain)==0)/sum(noD)
      }
      else if(modo=="RF_doubt_weighted2"){
        nVar=length(c(covar,transf))
        fu=paste0("~",paste(c(covar,transf),collapse = "+"))
        des=model.matrix(as.formula(fu),data=train)
        
        # Set weights through classwt by "ignoring" doubtful
        labels=factor(train$annotation)
        class_counts <- table(labels)
        total_samples <- sum(class_counts)
        num_classes <- length(class_counts)
        cwt <- total_samples / (num_classes * class_counts)
        names(cwt) <- levels(labels)
        cwt[1:3]=c(.01,2,1)
        
        mtries=seq(1,min(nVar,25),3)
        nodesizes=seq(1,maxNodeSize,3)
        Err=matrix(NA,length(mtries),length(nodesizes))
        for(pili in mtries){
          for(nono in nodesizes){
            RF = randomForest(x=des,y=factor(train$annotation),
                              ntree=300,classwt = cwt,
                              nodesize = nono,
                              mtry= pili)
            Err[which(mtries==pili),which(nodesizes==nono)]=mean(RF$err.rate[,'OOB'])
          }
        }
        BestMtry=which.min(sapply(1:nrow(Err),function(k)min(Err[k,])))
        BestSize=which.min(sapply(1:ncol(Err),function(l)min(Err[,l])))
        
        RF = randomForest(x=des,y=factor(train$annotation),
                          ntree=500,classwt = cwt,
                          nodesize = nodesizes[BestSize],
                          mtry= mtries[BestMtry])
        validX=model.matrix(as.formula(fu),data=valid)
        predMat=RF %>%
          predict(validX,"prob")
        pred = as.numeric(predMat[,"managed"])/as.numeric(rowSums(predMat[,c('wild',"managed")]))
        predRFD=pred
        predTrain= as.character(RF$predicted[noD])=="managed"
        trainAcc=sum((predTrain-truTrain)==0)/sum(noD)
      }
      truth = valid$annotation[vnoD]
      predHard = pred[vnoD]
      cd = metrics$rep==i & metrics$model==modo & metrics$set==varSet
      trW = truth=="wild"
      prW = predHard<=.5 
      trM = truth=="managed"
      prM = predHard>.5
      metrics$train_accuracy[cd]= trainAcc
      metrics$accuracy[cd] = sum((trW & prW)|(trM & prM))/sum(trW | trM)
      metrics$prWilVstrWil[cd] = sum(trW & prW)/sum(trW)
      metrics$prManVstrMan[cd] = sum(trM & prM)/sum(trM)
      truth = valid$annotation
      trW = truth=="wild"
      trM = truth=="managed"
      trD = truth=="doubtful"
      if(modo=="RF_doubt"){
        prW = sapply(1:dim(predMat)[1],function(i)colnames(predMat)[predMat[i,]==max(predMat[i,])][1]=="wild")
        prM = sapply(1:dim(predMat)[1],function(i)colnames(predMat)[predMat[i,]==max(predMat[i,])][1]=="managed")
        prD = sapply(1:dim(predMat)[1],function(i)colnames(predMat)[predMat[i,]==max(predMat[i,])][1]=="doubtful")
        metrics$accuracy_doubt40[cd] = sum((trW & prW)|(trM & prM)|(trD & prD))/length(pred)
        metrics$accuracy_doubt20[cd] = metrics$accuracy_doubt40[cd]
        metrics$accuracy_doubt10[cd] = metrics$accuracy_doubt40[cd]
      }else{
        prW = pred<=.1
        prM = pred>=.9
        prD = pred>.1 & pred<.9
        metrics$accuracy_doubt40[cd] = sum((trW & prW)|(trM & prM)|(trD & prD))/length(pred)
        prW = pred<=.3
        prM = pred>=.7
        prD = pred>.3 & pred<.7
        metrics$accuracy_doubt20[cd] = sum((trW & prW)|(trM & prM)|(trD & prD))/length(pred)
        prW = pred<=.4
        prM = pred>=.6
        prD = pred>.4 & pred<.6
        metrics$accuracy_doubt10[cd] = sum((trW & prW)|(trM & prM)|(trD & prD))/length(pred)
      }
      gc(reset=T)
    }
    setwd(saveDir)
    #save(metrics,file='metrics_results_all_combo_LOCAL.Rdata')
  }
}

metrics=rbind(metrico,metrics)

setwd(saveDir)
save(metrics,file='metrics_results_all_combo_LOCAL.Rdata')

#save(metrics,file='metrics_results_all_combo_with_RF_doubt_weighted2.Rdata')

load(file='metrics_results_all_combo_LOCAL.Rdata')

######
# Compute Taxonomic transferability
#####

setwd(saveDir)
DF=read.csv('occ_labelled_protec_eau_parcel_lc_obs_visu_glob.csv',sep=";",header=T)
nSa=dim(DF)[1]
DF$y = sapply(DF$annotation,function(ano)if(ano=="managed"){1}else if(ano=="doubtful"){.5}else{0})
DF$simple_lc=factor(DF$simple_lc)
DF[,toScale] = scale(DF[,toScale],center = T,scale=T)
maxNodeSize=15

sps=unique(DF$species)
nFold = length(sps)
modTypes = c('glmnet',
             'glmnet_soft',
             'glm_selec',
             'glm_selec_soft',
             'glm_full',
             'glm_full_weighted',
             'glm_full_soft',
             'glm_full_soft_weighted',
             'RF',
             'RF_doubt',
             'RF_doubt_weighted')
# Create species folds
folds=lapply(1:nFold,function(i)which(DF$species==sps[i]))

# metrics table
metrics=as.data.frame(
  expand.grid(model=modTypes,
              rep=c(1:nFold),
              set=names(varSets)))
metrics$accuracy = NA
metrics$train_accuracy = NA
metrics$prWilVstrWil = NA
metrics$prManVstrMan = NA
metrics$accuracy_doubt10 = NA
metrics$accuracy_doubt20 = NA
metrics$accuracy_doubt40 = NA

# iNat tag ONLY 
metrico=data.frame(model="tag",
              rep=c(1:nFold),
              set="inatTag")
for(col in setdiff(colnames(metrics),colnames(metrico))){
  eval(parse(text=paste0('metrico$',col,'=NA')))
}
metrico=compute.model.metrics(metrico,DF,folds,"tag",
                      varSet="inatTag")

set.seed(32)

# Other methods
for(varSet in names(varSets)){
  print(varSet)
  df=DF[,c('y',"annotation",varSets[varSet][[1]])]
  covar=varSets[varSet][[1]]
  transf=transf_full[sapply(transf_full,function(tra)sum(sapply(covar,function(va)regexpr(va,tra)>0))>0)]
  metrics=compute.model.metrics(metrics,
                                df,
                                folds,
                                modTypes,
                                varSet,
                                covar,
                                transf)
  setwd(saveDir)
  save(metrics,file='metrics_taxo_transferability.Rdata')
}

metrics=rbind(metrico,metrics)

setwd(saveDir)
save(metrics,file='metrics_taxo_transferability.Rdata')

#####
# Table 2
#####

setwd(saveDir)
DF=read.csv('occ_labelled_protec_eau_parcel_lc_obs_visu_glob.csv',sep=";",header=T)
tabi=DF%>%
  group_by(species,annotation)%>%
  summarise(n=n())%>%
  tidyr::pivot_wider(id_cols=c('species'),
                     names_from="annotation",
                     values_from = "n")%>%
  mutate(total=doubtful+managed+wild)%>%
  mutate(pwild=100*wild/total,
         pculti=100*managed/total,
         pdoubt=100*doubtful/total)%>%
  arrange(species)

tabi[,c('species','total','pwild','pculti','pdoubt')]

colMeans(tabi[,c('total','pwild','pculti','pdoubt')])

tabRG=DF%>%
  filter(quality_grade=="research")%>%
  group_by(species,annotation)%>%
  summarise(n=n())%>%
  tidyr::pivot_wider(id_cols=c('species'),
                     names_from="annotation",
                     values_from = "n")%>%
  mutate(total=doubtful+managed+wild)%>%
  mutate(pwild=100*wild/total,
         pculti=100*managed/total,
         pdoubt=100*doubtful/total)%>%
  arrange(species)

tabRG[,c('species','total','pwild','pculti','pdoubt')]

colSums(tabRG[,c('total','wild','managed','doubtful')])

colMeans(tabRG[,c('total','pwild','pculti','pdoubt')])


#####
# Figure 4 & 5 Performance analysis (strict, uncertain)
#####

setwd(saveDir)
load(file='metrics_results_all_combo.Rdata')
final=metrics
load(file='metrics_results_all_combo_LOCAL.Rdata')
final=rbind(final,metrics)

load(file='metrics_results_all_combo_with_RF_doubt_weighted2.Rdata')
final=metrics%>%
  filter(!model%in%"RF_doubt_weighted2")

final$model=factor(final$model,levels=models[models!="RF_doubt_weighted2"])
final$set=factor(final$set,levels=sets)


legend_title="Algorithm"
text_size=28
# Full plot STRICT
p1=ggplot(final,aes(x=set,y=accuracy,fill=model))+
  geom_boxplot()+
  scale_fill_manual(values=colors)+
  scale_y_continuous(limits=c(.45,1),breaks=seq(0,1,0.05),minor_breaks = seq(0,1,0.02))+
  ylab('Accuracy (culti. vs wild)')+
  xlab('Predictor set')+
  theme_bw()+
  theme(text=element_text(size=text_size))
p2=ggplot(final,aes(x=set,y=prManVstrMan,fill=model))+
  geom_boxplot()+
  scale_fill_manual(legend_title,values=colors)+
  scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.05),minor_breaks = seq(0,1,0.02))+
  ylab('Sensitivity to cultivametrics_taxo_transferability.Rdatated')+
  xlab('Predictor set')+
  theme_bw()+
  theme(text=element_text(size=text_size))
p3=ggplot(final,aes(x=set,y=prWilVstrWil,fill=model))+
  geom_boxplot()+
  scale_fill_manual(values=colors)+
  scale_y_continuous(limits=c(.4,1),breaks=seq(0,1,0.05),minor_breaks = seq(0,1,0.02))+
  ylab('Sensitivity to wild')+
  xlab('Predictor set')+
  theme_bw()+
  theme(text=element_text(size=text_size))

png('full_strict.png',width=1550,height=2100)
#png('full_strict_with_RF_doubt_weighted2.png',width=1550,height=2100)
multiplot(list(p1,p2,p3),cols = 1)
dev.off()

### Plot uncertainty-based-accuracy vs margin
subSelectedModels=models[c(2:9)]
cd=final$model%in%subSelectedModels & final$set=="all"
final$model=as.character(final$model)
for(marg in c(10,20,40)){
  tmp=final[cd,c('model','rep',paste0('accuracy_doubt',marg))]
  colnames(tmp)[3]="accuracy"
  tmp$margin=marg
  if(marg==10){toPlot=tmp
  }else{toPlot=rbind(toPlot,tmp)}
}
toPlot=toPlot%>%
  group_by(margin,model)%>%
  summarise(accuracy=mean(accuracy))
all=toPlot%>%
  group_by(margin)%>%
  summarise(accuracy=mean(accuracy))%>%
  mutate(model="average")%>%
  select(margin,model,accuracy)%>%
  rbind(toPlot)
all$model=factor(all$model,
                 levels=c(subSelectedModels,'average'))
colors2=c(colors[models%in%subSelectedModels],
          'black')
          
p=ggplot(all,aes(x=margin,y=accuracy,color=model))+
  geom_point(size=4)+geom_line(size=1.5)+
  scale_color_manual(values=colors2)+
  scale_y_continuous(limits=c(.4,.8),minor_breaks = seq(0,1,0.05))+
  xlab('Predictor set')+
  theme_bw()+
  theme(text=element_text(size=25))

png('accuracy_vs_margin.png',width=1550,height=700)
print(p)
dev.off()

### Full plot UNCERTAIN
final$model=factor(final$model,levels=models[models!="RF_doubt_weighted2"])
p1=ggplot(final,aes(x=set,y=accuracy_doubt10,fill=model))+
  geom_boxplot()+
  scale_fill_manual(legend_title,values=colors)+
  scale_y_continuous(limits=c(.33,1),breaks=seq(0,1,0.05),minor_breaks = seq(0,1,0.02))+
  ylab('Accuracy (3 classes)')+
  xlab('Predictor set')+
  theme_bw()+
  theme(text=element_text(size=25))

png('full_uncertain.png',width=1550,height=700)
#png('full_uncertain_test.png',width=1550,height=700)
print(p1)
dev.off()

# Metrics for the best method only
if(F){
  tmp=metrics%>%
    group_by(model,set)%>%
    summarise(accuracy=mean(accuracy,na.rm=T),
              train_accuracy=mean(train_accuracy,na.rm=T),
              prWilVstrWil=mean(prWilVstrWil,na.rm=T),
              prManVstrMan=mean(prManVstrMan,na.rm=T),
              accuracy_doubt10=mean(accuracy_doubt10,na.rm=T),
              accuracy_doubt20=mean(accuracy_doubt20,na.rm=T),
              accuracy_doubt40=mean(accuracy_doubt40,na.rm=T))%>%
    arrange(set)
  
  metr=c('accuracy','train_accuracy','prWilVstrWil','prManVstrMan',
         'accuracy_doubt10','accuracy_doubt20','accuracy_doubt40')
  
  toPlot=data.frame(set=unique(metrics$set),bestMod=NA)
  for(met in metr){
    eval(parse(text=paste0('toPlot$',met,'=NA')))
  }
  for(set in unique(metrics$set)){
    tmp2=tmp[tmp$set==set,]
    bestMod=tmp2$model[which.max(tmp2$accuracy)]
    toPlot$bestMod[toPlot$set==set]=bestMod
    for(met in metr){
      eval(parse(text=paste0('toPlot$',met,'[toPlot$set==set]=tmp2$',met,'[tmp2$model==bestMod]')))
    }
  }
  colnames(toPlot)[c(3,4,5,6,7:9)]=c('acc','tr_acc','senWil','senCul','acc_dou10','acc_dou20','acc_dou40')
  print(toPlot,digits=2)
}

#####
# Figure taxonomic transferability
#####

setwd(saveDir)
load(file='metrics_taxo_transferability.Rdata')
final=metrics
final$model=factor(final$model,levels=models[models!="RF_doubt_weighted2"])
final$set=factor(final$set,levels=sets)

# Full plot STRICT
p1=ggplot(final,aes(x=set,y=accuracy,fill=model))+
  geom_boxplot()+
  scale_fill_manual(values=colors)+
  scale_y_continuous(limits=c(.2,1),minor_breaks = seq(0,1,0.05))+
  ylab('Accuracy (cultivated vs wild)')+
  theme_bw()+
  theme(text=element_text(size=25))
print(p1)
p2=ggplot(final,aes(x=set,y=accuracy_doubt10,fill=model))+
  geom_boxplot()+
  scale_fill_manual(values=colors)+
  scale_y_continuous(limits=c(.1,1),minor_breaks = seq(0,1,0.05))+
  ylab('Accuracy (3 classes)')+
  theme_bw()+
  theme(text=element_text(size=25))
print(p2)

png('taxoTransferability_accuracy.png',width=1050,height=1500)
multiplot(list(p1,p2),cols = 1)
dev.off()


#####
# Figure 6. Variable importances
#####

setwd(saveDir)
DF=read.csv('occ_labelled_protec_eau_parcel_lc_obs_visu_glob.csv',sep=";",header=T)
nSa=dim(DF)[1]
DF$y = sapply(DF$annotation,function(ano)if(ano=="managed"){1}else if(ano=="doubtful"){.5}else{0})
DF$simple_lc=factor(DF$simple_lc)
# var scaling
if(T){
  transf_full = c('I(dToRiverM^2)',
                  'I(dToCanalM^2)',
                  'I(tagFreq^2)',
                  'I(positional_accuracy^2)')
  toScale = c("positional_accuracy",
              "lnObs",
              'lnSp',
              'visu_ctx_anthropized','visu_ctx_intothewild',
              'visu_ctx_uniform_background_macro_dissection','visu_ctx_with_human_body_presence',
              'visu_for_amphibian','visu_for_bird',
              'visu_for_fish','visu_for_humanmade',
              'visu_for_invertebrate','visu_for_landscape',
              'visu_for_mammal','visu_for_mushroom',
              'visu_for_reptile','visu_for_bark',
              'visu_for_branch','visu_for_bud',
              'visu_for_flower','visu_for_fruit',
              'visu_for_habit','visu_for_leaf',
              'visu_for_roots','visu_for_seeds',
              'visuOld_ctx_garden_park','visuOld_ctx_indoor',
              'visuOld_ctx_into_the_wild','visuOld_ctx_landscape',
              'visuOld_ctx_terrace_balcon','visuOld_ctx_uniform_background',
              'visuOld_ctx_urban_building_road','visuOld_ctx_urban_ground_wall',
              'visuOld_for_hand','visuOld_for_humanmade')
  DF[,toScale] = scale(DF[,toScale],center = T,scale=T)
}

maxNodeSize=15
varSet='all'
df=DF[,c('y',"annotation",varSets[varSet][[1]])]
covar=varSets[varSet][[1]]
transf=transf_full[sapply(transf_full,function(tra)sum(sapply(covar,function(va)regexpr(va,tra)>0))>0)]
nVar=length(c(covar,transf))
fu=paste0("~",paste(c(covar,transf),collapse = "+"))
des=model.matrix(as.formula(fu),data=df)

Err=matrix(NA,nVar,maxNodeSize)
for(pili in 1:nVar){
  print(pili)
  for(nono in 1:maxNodeSize){
    RF = randomForest(x=des,y=factor(df$annotation),
                      ntree=300,
                      nodesize = nono,
                      mtry= pili)
    Err[pili,nono]=mean(RF$err.rate[,'OOB'])
  }
}
BestMtry=which.min(sapply(1:nrow(Err),function(k)min(Err[k,])))
BestSize=which.min(sapply(1:ncol(Err),function(l)min(Err[,l])))

plot(1:nVar,Err[,BestSize])
plot(1:maxNodeSize,Err[32,])


RF = randomForest(x=des,y=factor(df$annotation),
                  ntree=500,
                  mtry= BestMtry,
                  nodesize = BestSize,
                  importance=T)

print(RF$importance[order(RF$importance[,'MeanDecreaseAccuracy'],decreasing = T),
                    c('MeanDecreaseAccuracy',
                      'MeanDecreaseGini')],digits=3)

setwd(saveDir)
save(RF,file = 'randomForest_variable_importance.Rdata')
png('Variable_Importance.png',width=1000,height=800)
varImpPlot(RF)
dev.off()


#####
# On all trusted iNat records:
# Fit & predict with best model (RF_doubt_weighted)
#####

setwd(saveDir)
df=readRDS(file = paste0(main,'input data/all_trusted_records.rds'))
anot=read.csv('occ_labelled_protec_eau_parcel_lc_obs_visu_glob.csv',sep=";",header=T)[,c('url','annotation')]
df$iNat_tag="wild";df$iNat_tag[df$captive_cultivated=="true"]="cultivated"
df$captive_cultivated[]
df$simple_lc=factor(df$simple_lc)
toScaleTmp=toScale[toScale%in%colnames(df)]
df[,toScaleTmp] = scale(df[,toScaleTmp],center = T,scale=T)
varSet="all"
covar=varSets[varSet][[1]]
transf=transf_full[sapply(transf_full,function(tra)sum(sapply(covar,function(va)regexpr(va,tra)>0))>0)]


### Fit RF_doubt_weighted on ALL annotated records
trainSamp = df$url%in%anot$url
train=df[trainSamp,c('url',varSets[varSet][[1]])]
train=train%>%
  merge(anot,by='url',all.x=T)%>%
  filter(complete.cases(.))

maxNodeSize=15

weights=sapply(train$annotation,function(ano)if(ano=="wild"){1/sum(train$annotation=="wild")}else if(ano=="managed"){1/sum(train$annotation=="managed")}else{1/sum(train$annotation=="doubtful")})
nVar=length(c(covar,transf))
fu=paste0("~",paste(c(covar,transf),collapse = "+"))
des=model.matrix(as.formula(fu),data=train)
Err=matrix(NA,min(nVar,25),maxNodeSize)
for(pili in 1:min(nVar,25)){
  cat('mtry: ',pili,'\n')
  for(nono in 1:maxNodeSize){
    cat('nodeSize: ',nono,'\n')
    RF = randomForest(x=des,y=factor(train$annotation),
                      ntree=300,weights = weights,
                      nodesize = nono,
                      mtry= pili)
    Err[pili,nono]=mean(RF$err.rate[,'OOB'])
  }
}
BestMtry=which.min(sapply(1:nrow(Err),function(k)min(Err[k,])))
BestSize=which.min(sapply(1:ncol(Err),function(l)min(Err[,l])))
cat('Fit final model')
RF = randomForest(x=des,y=factor(train$annotation),
                  ntree=500,weights = weights,
                  nodesize = BestSize,
                  mtry= BestMtry)
predTrain= as.character(RF$predicted)
#trainAcc=sum((predTrain-truTrain)==0)/sum(noD)
#cat('Train accuracy:')

# Predict on ALL records
tmp=df[complete.cases(df[,covar]),]
predX=model.matrix(as.formula(fu),data=tmp)
predMat=RF %>%
  predict(predX,"prob")

setwd(saveDir)
save(tmp,predMat,file="pred_allTrusted_RF_doubt_weighted_fullfit.Rdata")

idUrls=tmp$url%in%train$url[is.na(RF$predicted)]
cbind(predMat[idUrls,],anot=sapply(tmp$url[idUrls],function(urlo)train$annotation[train$url==urlo]))

#####
# Figures 7 and 8: Maps of wild frequency and wild introduced frequency
#####

setwd(saveDir)
rebList=read.csv('rebList_gbif_matching.csv',sep=";",header=T)
invList = unique(rebList$species[!is.na(rebList$species)])

load(file="pred_allTrusted_RF_doubt_weighted_fullfit.Rdata")
occ=tmp
rStu = readRDS(file = 'raster_Study')
grid = as.data.frame(rasterToPoints(rStu))
colnames(grid)[3]="cell"
# Filter cells in sea
load(file='WCpoly.Rdata')
pts <- SpatialPoints(grid[,c('x','y')], proj4string = CRS(proj4string(WCpoly)))
isIn=sp::over(pts,WCpoly)
grid=grid[which(!is.na(isIn$NAME_1)),]

# Estimated frequency of wild recorded plants
#ggplot()+geom_sf(data=st_as_sf(WCpoly))+geom_point(data=gridTmp,aes(x=x,y=y))

occ$probWild=predMat[,3]

toPlot=occ%>%
  group_by(cell)%>%
  summarise(frqWild=mean(probWild))%>%
  merge(grid[!is.na(grid$x),],by='cell',all.x=T)%>%
  filter(!is.na(x))

breaks = c(0,stats::quantile(x = toPlot$frqWild ,probs=c(.2,.5,.8,.9),na.rm=T),1)
Levels = cut(breaks[1:(length(breaks)-1)]+.001,breaks,dig.lab=5)
palette = colorRampPalette(c("#E69F00","#56B4E9",'chartreuse4'))
colorsToPlot = palette(length(Levels))

toPlot$class=cut(toPlot$frqWild,breaks)

map <- get_stadiamap(
  bbox = c(left = 18.25, 
           bottom = -34.5, 
           right = 18.75, 
           top = -33.75),
  zoom = 11,
  color = c("bw"),
  messaging=T, maptype = "stamen_toner_lite")

p <- ggmap(map)+ geom_sf(data=sfTest,alpha=.02, inherit.aes = FALSE)+
  geom_tile(data=toPlot,aes(x=x,y=y,fill=class),alpha=.7)+
  scale_fill_manual(name="Estimated frequency of \n wild plant records",values=colorsToPlot)+
  xlab('Longitude')+ylab('Latitude')+theme(text=element_text(size=30))

setwd(main)
png('Figure_frequency_wild.png',width=2000,height=2000)
print(p)
dev.off()

# Estimated frequency of wild recorded introduced plants 
toPlot=occ%>%
  filter(species%in%invList)%>%
  group_by(cell)%>%
  summarise(nOcc=n(),frqWild=mean(probWild))%>%
  merge(grid[!is.na(grid$x),],by='cell',all.x=T)%>%
  filter(!is.na(x))

breaks = c(0,stats::quantile(x = toPlot$frqWild ,probs=c(.2,.5,.8,.9),na.rm=T),1)
Levels = cut(breaks[1:(length(breaks)-1)]+.001,breaks,dig.lab=5)
palette = colorRampPalette(c('chartreuse4',"#E69F00","orangered"))
colorsToPlot = palette(length(Levels))

toPlot$class=cut(toPlot$frqWild,breaks)

p <- ggmap(map)+
  geom_tile(data=toPlot,aes(x=x,y=y,fill=class),alpha=.7)+
  scale_fill_manual(name="Estimated frequency of wild \n introduded plant records",values=colorsToPlot)+
  xlab('Longitude')+ylab('Latitude')+theme(text=element_text(size=30))

setwd(main)
png('Figure_frequency_wild_introduced.png',width=2000,height=2000)
print(p)
dev.off()


# Filter cells in sea
