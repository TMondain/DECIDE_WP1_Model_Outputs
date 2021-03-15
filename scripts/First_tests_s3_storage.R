

### First tests of loading data from S3 storage
# library(terra)
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

# get a list of all the species that appear in the outputs
spp_names_lr <- unique(gsub(pattern="lr_SDMs_|_meanpred.grd|_quantilemaxmin.grd|_quantilerange.grd", replacement = '', 
                            x = list.files(paste0('/data-s3/thoval/sdm_outputs/', taxa, '/lr'), pattern = '.grd')))

spp_names_rf <- unique(gsub(pattern="rf_SDMs_|_meanpred.grd|_quantilemaxmin.grd|_quantilerange.grd", replacement = '', 
                            x = list.files(paste0('/data-s3/thoval/sdm_outputs/', taxa, '/rf'), pattern = '.grd')))

spp_names_gam <- unique(gsub(pattern="gam_SDMs_|_meanpred.grd|_quantilemaxmin.grd|_quantilerange.grd", replacement = '', 
                            x = list.files(paste0('/data-s3/thoval/sdm_outputs/', taxa, '/gam'), pattern = '.grd')))

spp_names <- unique(c(spp_names_lr, spp_names_rf, spp_names_gam))

# sdm outputs for each species
species_stack <- list()

# error outputs
error_out <- list()

for(i in 1:length(names)){
  
  print(names[i])
  
  # initiate model list within for loop so that it gets replaced when starting a new species
  # otherwise we might get some weird overlaps
  model_stack <- list()
  errored_models <- list()
  
  for(m in 1:length(model)){
    
    check_models <- list.files(paste0('/data-s3/thoval/sdm_outputs/', taxa, '/', model[m]), 
                               pattern = paste0(names[i]),
                               full.names = TRUE)
    
    if(length(check_models)<=1){
      
      print(paste('!!!   model', model[m], 'failed for species', names[i], '  !!!'))
      
      errored_models[[m]] <- data.frame(taxa = taxa, 
                                        species = names[i], 
                                        model = model[m])
      
      next
    }
    
    # mean predictions
    mp <- list.files(paste0('/data-s3/thoval/sdm_outputs/', taxa, '/', model[m]), 
                     pattern = paste0(names[i], "_meanpred.grd"),
                     full.names = TRUE)
    
      mod_preds <- raster::stack(mp)
      names(mod_preds) <- paste0(names[i], '_', model[m],'_mean_pred')
    
    
    
    # quantile min/max
    mm <- list.files(paste0('/data-s3/thoval/sdm_outputs/', taxa, '/', model[m]), 
                     pattern = paste0(names[i], "_quantilemaxmin.grd"),
                     full.names = TRUE)
    
      qminmax <- raster::stack(mm)
      names(qminmax) <- c(paste0(names[i], '_', model[m],'_min'), paste0(names[i], '_', model[m],'_max'))
    
    
    # quantile range
    qr <- list.files(paste0('/data-s3/thoval/sdm_outputs/', taxa, '/', model[m]), 
                     pattern = paste0(names[i], "_quantilerange.grd"),
                     full.names = TRUE)
    
     qrange <- raster::stack(qr)
    names(qrange) <- paste0(names[i], '_', model[m], '_quantile_range')
    
    
    # stack all from one model together
    model_stack[[m]] <- raster::stack(mod_preds, qminmax, qrange)
    
  }
  
  # model_stack[sapply(model_stack,is.null)] <- raster(nrow=12500, 
  #                                                    ncol=7000,
  #                                                    crs="+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
  
  # To combine them together need to remove the NULL raster layers (i.e. if a model hasn't worked)
  model_stack <- model_stack[!sapply(model_stack,is.null)]
  
  species_stack[[i]] <- raster::stack(model_stack)
  
  # Output the models that failed too
  error_out[[i]] <- do.call('rbind', errored_models) 
  
}


