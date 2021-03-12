

### First tests of loading data from S3 storage
library(terra)
library(raster)
library(dismo)
library(foreach)
library(doParallel)


# load a test file file
load('/data-s3/thoval/sdm_outputs/moth/rf/SDMs_Adscita_geryon.rdata')

# get predictions -- doesn't work
preds <- model_output$quantile_range
plot(preds)



loc_gam <- list.files('/data-s3/thoval/sdm_outputs/moth/rf', full.names = TRUE, pattern = 'meanpred.grd')
test <- raster(loc_gam[1])
plot(test)
plot(test, xlim = c(450000, 540000), ylim = c(450000, 500000))

t2 <- rast(loc_gam[6], ncol = 7000, nrow = 12500)
plot(t2, xlim = c(450000, 540000), ylim = c(450000, 500000))
## terra does something really weird- it seems to affect the resolution of the raster?


# get location of each model output
loc_rf <- list.files('/data-s3/thoval/sdm_outputs/moth/rf',
                     pattern = '.grd', 
                     full.names = TRUE)
loc_lr <- list.files('/data-s3/thoval/sdm_outputs/moth/lr',
                     pattern = '.grd', 
                     full.names = TRUE)
loc_gam <- list.files('/data-s3/thoval/sdm_outputs/moth/gam',
                      pattern = '.grd', 
                      full.names = TRUE)

out_df <- vector('list', length = length(names))




#### function to read files
#' _input_:
#' taxa = 'butterfly', 'moth', 'orthoptera
#' model = 'lr', 'rf', 'gam' - later 'me'
#' 
#' _output_: raster stack of all the different model outputs
#' 'meanpred', 'quantilemaxmin', 'quantilerange' 


model = c('rf', 'lr', 'gam')
taxa = 'moth'

all_mods_out <- list()

for(i in 1:length(model)){
  print(model[i])
  
  ## get the species names to loop over
  names <- unique(gsub(pattern="_meanpred.grd|_quantilemaxmin.grd|_quantilerange.grd", replacement = '', 
                       x = list.files(paste0('/data-s3/thoval/sdm_outputs/', taxa, '/', model[i]), pattern = '.grd')))
  names
  
  all_rasts_out <- list()
  
  for(n in 1:length(names)){
    print(names[n])
    
    ## read mean_preds
    mpred <- raster(list.files(paste0('/data-s3/thoval/sdm_outputs/', taxa, '/', model[i]), 
                               pattern = paste0(names[n], "_meanpred.grd"),
                               full.names = TRUE))
    names(mpred) <- paste0(model[i],'_mean_pred') ### change all to like this!!
    
    # quantile min/max
    qminmax <- raster::stack(list.files(paste0('/data-s3/thoval/sdm_outputs/', taxa, '/', model[i]), 
                                        pattern = paste0(names[n], "_quantilemaxmin.grd"),
                                        full.names = TRUE))
    names(qminmax) <- c('min', 'max')
    
    # quantile range
    qrange <- raster::stack(list.files(paste0('/data-s3/thoval/sdm_outputs/', taxa, '/', model[i]), 
                                       pattern = paste0(names[n], "_quantilerange.grd"),
                                       full.names = TRUE))
    names(qrange) <- 'quantile_range'
    
    all_rasts <- raster::stack(mpred, qminmax, qrange)
    
    all_rasts_out[[n]] <- all_rasts
    
  }
  
  all_rasts_out[[i]] <- all_rasts_out
  
}

#                              pattern = paste0(pattern, '.grd')), replacement = '')
# names <- gsub(pattern = '.grd', x = names, replacement = '')
# names


## direct R to the different files 









doParallel::registerDoParallel(detectCores()-1)
registerDoSEQ()

# choose thing to load
list.files('/data-s3/thoval/sdm_outputs/moth/rf')

pattern = 'quantilerange'

out_df <- foreach(i = 1:length(names)) %do% {
  
  print(i)
  
  # get name of species and item of interest
  names <- gsub(pattern = 'rf_SDMs_', x = list.files('/data-s3/thoval/sdm_outputs/moth/rf', 
                                                     pattern = paste0(pattern, '.grd')), replacement = '')
  names <- gsub(pattern = '.grd', x = names, replacement = '')
  names
  
  # rf
  rf <- loc_rf[grepl(pattern = paste(names[i]), x = loc_rf)]
  
  if(length(rf)==1){
    load(rf)
    rf_auc <- model_output$sdm_output$AUC
    rm(model_output)
  } else {
    rf_auc <- NA
  }
  
  # lr
  lr <- loc_lr[grepl(pattern = names[i], x = loc_lr)]
  
  if(length(lr)==1){
    load(lr)
    lr_auc <- model_output$sdm_output$AUC
    rm(model_output)
  }else {
    lr_auc <- NA
  }
  
  # gam
  gam <- loc_gam[grepl(pattern = names[i], x = loc_gam)]
  
  if(length(gam)==1){
    load(gam)
    gam_auc <- model_output$sdm_output$AUC
    rm(model_output)
  } else {
    gam_auc <- NA
  }
  
  return(data.frame(species = rep(names[i], 3), 
                    auc = c(rf_auc, lr_auc, gam_auc),
                    model = c('rf', 'lr', 'gam')))
  
  
}
out_df
