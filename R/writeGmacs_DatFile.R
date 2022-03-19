#' 
#' @title Write TCSAM data to a Gmacs dat file
#' 
#' @description Function to write TCSAM data to a Gmacs dat file.
#' 
#' @param lst - list returned by [readTCSAM_InputDataFiles]
#' @param fnInp - file path for input Gmacs information on seasons
#' @param fnOut - file path for output Gmacs dat file (or "" to write to console)
#' @param verbose - flag to print debugging info
#' 
#' @details 
#' 
#' @export
#' 
writeGmacs_DatFile<-function(lst,
                             fnInp="gmacs_SeasonInfo.inp",
                             fnOut="testing_GmacsDatFile.dat",
                             verbose=FALSE){
  #--set global variables
  setGlobals();
  
  #--define local functions
  cat0<-function(...){if (verbose) {i=list(...); base::cat(paste(i),"\n",sep="");}}
  write<-function(...){i=list(...); base::cat(paste(i),"\n",file=conn)}
  
  #--start processing
  cat0(paste0("rTCtoGmacs::writeGmacs_DatFile: Writing to '",fnOut,"'"));
  conn = "";
  if (fnOut!="") conn = base::file(description=fnOut,open="w")
  write("#========================================================================================================");
  write("#  Gmacs Main	Data	File	Version	1.1:	TANNER CRAB");																			
  write("#","FLEET_INDEX","DESCRIPTION",sep="\t");
  nFlts = lst$mc$nFsh + lst$mc$nSrv;
  flt = 0;
  for (i in 1:lst$mc$nFsh){
    flt = flt+1;
    fsh = lst$fsh[[i]];
    write("#\t",flt,": ",fsh$name,sep="")
  }
  for (i in 1:lst$mc$nSrv){
    flt = flt+1;
    srv = lst$srv[[i]];
    write("#\t",flt,": ",srv$name,sep="")
  }
  write("#========================================================================================================");
  
  write(lst$mc$mnYr,"\t","#--Model start year");
  write(lst$mc$mxYr,"\t","#--Model end year");
  write("??","\t","#--Number of seasons");
  write(nFlts,"\t","#--Number of fleets (fishing fleets + surveys)");
  write(.nSXs,"\t","#--Number of model sexes");
  write(.nSCs,"\t","#--Number of model shell conditions");
  write(.nMTs,"\t","#--Number of model maturity states");
  write(lst$mc$nZBs,"\t","#--Number of model size classes");
  write("??","\t","#--Season in which recruitment occurs");
  write("??","\t","#--Season in which molting and growth occurs");
  write("??","\t","#--Season in which to calculate SSB");
  write("??","\t","#--Season for N output");
  write("#\t indices for max size class (males, then females)");
  for (i in 1:.nSXs) {
    mxZ = lst$mc$mxZsByX[i];
    idx = sum(mxZ <= lst$mc$zCs) - 1;
    write(idx,"\t","#--index of max size bin for",names(.SXs)[i+1]);
  }
  write("#--cutpoints for size bins (number of size bins + 1)")
  write(mc$zCs);
  
  write("#--input type for seasonal M' information's (1: vector by season; 2: matrix by season/year)")
  if (fnOut!="") close(conn);
}
