# Custom Analysis functions for App

# Generate Missing analysis 
# and create GGPlot containing values
analyze_miss <- function(tbl_name){
  castbl <- defCasTable(conn, table=tbl_name)
  
  tbl <- cas.simple.distinct(castbl)$Distinct[,c('Column', 'NMiss')]
  
  tbl$PctMiss <- tbl$NMiss/nrow(castbl)
  
  plot <- ggplot(tbl, aes(Column, PctMiss)) +
    geom_col(fill = 'blue') +
    ggtitle('Pct Missing Values') +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(plot)
}


# Analyze carninality on a CAS table
# this will generate cardinality for each variable
# and return a plot containing the information
analyze_cardinality <- function(tbl_name) {
  loadActionSet(conn, "cardinality")
  cas.cardinality.summarize(conn, table={name=tbl_name}, cardinality=list(caslib="casuser", name="cardinality", replace = TRUE))
  tbl <- get_table("cardinality")
  
  plot <- ggplot(tbl, aes(`_VARNAME_`, `_CARDINALITY_`)) +
    geom_col(fill = 'blue') +
    ggtitle('Cardinality') +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(plot)
}


# Generate features
# uses featureMachine cas action
# it will analyze:
#     MISSING
#     ENTROPY
#     HIGH-CARDINALITY
#     SKEWNESS
#     CURTOSIS
#     OUTLIER
# and will apply transformations accordingly

feature_engineering <- function(tbl_name, target, a, b, c, d, e, f, g) {
  loadActionSet(conn, "dataSciencePilot")
  target <<- target
  tbl_name <<- tbl_name
  features <- cas.dataSciencePilot.featureMachine(
    conn,
    table                 = list(name =tbl_name, caslib="casuser"),
    target                = target,
    explorationPolicy     = list(),
    screenPolicy          = list(),
    copyVars = list(target),
    transformationPolicy  = list(missing = a, cardinality = b,
                                 entropy = c, iqv = d,
                                 skewness = e, kurtosis = f, Outlier = g),
    transformationOut     = list(name= "TRANSFORMATION_OUT", replace = TRUE),
    featureOut            = list(name= "FEATURE_OUT", replace = TRUE),
    casOut                = list(name= paste(tbl_name,"_TRANSFORMED",sep=""), replace = TRUE),
    saveState             = list(name= "ASTORE_OUT", replace = TRUE)
  )
  return(get_table("FEATURE_OUT"))
}


# Partition the dataset into 
#   70% training
#   30% test  
partition <- function(tbl) {
  loadActionSet(conn, "sampling")
  cas.sampling.srs(conn,
                   table = list(name=tbl, caslib="casuser"),
                   samppct = 30,
                   partind = TRUE,
                   output = list(casOut = list(name = paste(tbl,'_part', sep=""), replace = T, caslib="casuser"), copyVars = 'ALL')
  )
}


# Runs autoML
# It trains multiple ML models:
#   * Decision Tree
#   * Random Forest
#   * Gradient Boosting
#   * Neural Networks
# Then, scores on the 30% dataset
# and runs assessment

auto_ml <- function(dt, rf, gb, nn) {
  loadActionSet(conn, "decisionTree")
  loadActionSet(conn, "neuralNet")
  
  partition(paste(tbl_name, "_TRANSFORMED", sep =""))
  
  model_tbl <<- paste(tbl_name, "_TRANSFORMED_part", sep ="")
  
  tbl <- defCasTable(conn, model_tbl)
  colinfo <- head(cas.table.columnInfo(conn, table = tbl)$ColumnInfo, -1)
  target <<- colinfo$Column[1]
  inputs <<- colinfo$Column[-1]
  nominals <<- c(target, subset(colinfo, Type == 'varchar')$Column)
  
  models <- vector()
  scores <- vector()
  model_names <- vector()
  
  if(dt) {
    cas.decisionTree.dtreeTrain(conn,
                                table = list(name = model_tbl, caslib = "casuser", where = "_PartInd_ = 0"),
                                target = target,
                                inputs = inputs,
                                nominals = nominals,
                                casOut = list(name = "dt_model", caslib="casuser", replace = TRUE))
    
    #print(defCasTable(conn, "dt_model", caslib="casuser"))
    models <- c(models, 'dt')
    scores <- c(scores, cas.decisionTree.dtreeScore)
    model_names <- c(model_names, 'Decision Tree')
  }
  
  if(rf) {
    cas.decisionTree.forestTrain(conn,
                                 table = list(name = model_tbl, caslib = "casuser", where = '_PartInd_ = 0'),
                                 target = target,
                                 inputs = inputs,
                                 nominals = nominals,
                                 casOut = list(name = 'rf_model', caslib="casuser", replace = TRUE)
    )
    #print(defCasTable(conn, 'rf_model'))
    models <- c(models, 'rf')
    scores <- c(scores, cas.decisionTree.forestScore)
    model_names <- c(model_names, 'Random Forest')
  }
  if(gb) {
    cas.decisionTree.gbtreeTrain(conn,
                                 table = list(name = model_tbl, caslib = "casuser", where = '_PartInd_ = 0'),
                                 target = target,
                                 inputs = inputs,
                                 nominals = nominals,
                                 casOut = list(name = 'gbt_model', caslib="casuser", replace = TRUE)
    )
    models <- c(models, 'gbt')
    scores <- c(scores, cas.decisionTree.gbtreeScore)
    model_names <- c(model_names, 'Gradient Boosting')
  }
  if(nn) {
    cas.neuralNet.annTrain(conn,
                           table = list(name = model_tbl, caslib = "casuser", where = '_PartInd_ = 0'),
                           target = target,
                           inputs = inputs,
                           nominals = nominals,
                           casOut = list(name = 'nn_model', caslib="casuser", replace = TRUE)
    )
    models <- c(models, 'nn')
    scores <- c(scores, cas.neuralNet.annScore)
    model_names <- c(model_names, 'Neural Network')
  }
  
  return(measure_models(models, scores, model_names))
  
}


# Submethod that calculates
# ROC curve on all models selected
# above

measure_models <- function(models, scores, model.names) {
  loadActionSet(conn, "percentile")
  
  names(scores) <- models
  
  score.params <- function(model){return(list(
    object       = defCasTable(conn, model_tbl),
    modelTable   = list(name = paste0(model, '_model')),
    copyVars     = list(target, '_PartInd_'),
    assessonerow = TRUE,
    casOut       = list(name = paste0(model, '_scored'), replace = T)
  ))}
  lapply(models, function(x) {do.call(scores[[x]], score.params(x))})
  
  assess.model <- function(model){
    cas.percentile.assess(conn,
                          table    = list(name = paste(model,'_scored', sep=""), 
                                          where = '_PartInd_ = 1', caslib="casuser"),
                          inputs   = paste0('_', model, '_P_           1'),
                          response = target,
                          event    = '1')
  }
  
  
  roc.df <- data.frame()
  for (i in 1:length(models)){
    tmp <- (assess.model(models[i]))$ROCInfo
    tmp$Model <- model.names[i] 
    roc.df <- rbind(roc.df, tmp)
  }
  
  compare <- subset(roc.df, round(roc.df$CutOff, 2) == 0.5)
  rownames(compare) <- NULL
  compare[,c('Model','TP','FP','FN','TN')]
  
  compare$Misclassification <- 1 - compare$ACC
  miss <<- compare[order(compare$Misclassification), c('Model','Misclassification')]
  rownames(miss) <- NULL
  
  roc.df$Models <- paste(roc.df$Model, round(roc.df$C, 3), sep = ' - ')
  
  plot <- ggplot(data = roc.df[c('FPR', 'Sensitivity', 'Models')],
                 aes(x = as.numeric(FPR), y = as.numeric(Sensitivity), colour = Models)) +
    geom_line() + scale_y_continuous( limits = c(0,1), expand = c(0,0) ) +
    labs(x = 'False Positive Rate', y = 'True Positive Rate')
  
  return(plot)
}


get_miss <- function(){
  return(miss)
}