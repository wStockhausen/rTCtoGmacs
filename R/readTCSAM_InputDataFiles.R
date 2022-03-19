#' 
#' @title Read TCSAM02 input data files
#' 
#' @description Function to read TCSAM02 input data files.
#' 
#' @param mcFN - TCSAM02 model configuration file name
#' @param mcObj - model configuration object from call to [readTCSAM_ModelConfiguration]
#' 
#' @return list (see [@details])
#' 
#' @details Either \code{mcFN} or \code{mcObj} can be given as input (\code{mcObj} is used if both
#' are non-null). The returned list has elements:
#' \itemize{
#' \item{mc - list with model configuration information}
#' \item{ds - list with dataset names}
#' \item{bio - list with biological data}
#' \item{fsh - list with data from fisheries}
#' \item{srv - list with data from surveys}
#' \item{grw - list with growth (molt increment) data}
#' \item{mat - list with maturity ogive data}
#' }
#' 
#' @export
#' 
readTCSAM_InputDataFiles<-function(mcFN="./inst/testExample/M21.22a.MCI.inp",
                                   mcObj=NULL){
  #--set global variables
  setGlobals();

  if (is.null(mcObj)){
    mc = readTCSAM_ModelConfiguration(mcFN);
  } else {
    mc = mcObj;
  }
  
  #--read fn_datasets
  topDir = dirname(fn_mc);
  cat(paste0("--topDir = '",topDir,"'\n"));
  ds = readTCSAM_DatasetNames(file.path(topDir,mc$fn_datasets));
  
  #--read datasets
  #----read biological data file
  bd = readTCSAM_BioData(file.path(topDir,ds[["fn_BioInfo"]]));

  #----read fishery datasets
  fds = list();
  if (ds$nFshs>0) for (i in 1:ds$nFshs) {
    fnp = ds[["fn_Fshs"]][i];
    fds[[fnp]] = readTCSAM_FleetData(file.path(topDir,fnp),verbose=TRUE);
  }

  #----read survey datasets
  sds = list();
  if (ds$nSrvs>0) for (i in 1:ds$nSrvs) {
    fnp = ds[["fn_Srvs"]][i];
    sds[[fnp]] = readTCSAM_FleetData(file.path(topDir,fnp));
  }

  #----read molt increment data files
  mids = list();
  if (ds$nMIDs>0) for (i in 1:ds$nMIDs) {
    fnp = ds[["fn_MIDs"]][i];
    mids[[fnp]] = readTCSAM_GrowthData(file.path(topDir,fnp));
  }

  #----read chela height data files
  # mids = list();
  # if (ds$nMIDs>0) for (i in 1:ds$nMIDs) {
  #   fnp = ds[["fn_MIDs"]][i];
  #   mids[[fnp]] = readTCSAM_ChelaHeightData(file.path(topDir,fnp));
  # }

  #--read maturity ogive data files
  mods = list();
  if (ds$nMODs>0) for (i in 1:ds$nMODs) {
    fnp = ds[["fn_MODs"]][i];
    mods[[fnp]] = readTCSAM_MaturityOgiveData(file.path(topDir,fnp));
  }
  return(list(mc=mc,ds=ds,bio=bd,fsh=fds,srv=sds,grw=mids,mat=mods));
}

  
  
