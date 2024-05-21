#' Run SWAT+
#'
#' This function allows to run a SWAT+ project in R. Basic
#' settings for the SWAT run such as the simulation period or the time interval
#' for the outputs can be done directly. SWAT simulation outputs can be
#' defined that are returned in a 'tidy' format in R. Functionality such as model
#' parametrization, parallel execution of simulations, or incremental saving of
#' simulation runs is provided.
#'
#' @param project_path  Character string that provides the path to the SWAT project
#'   folder (i.e. TxtInOut).
#' @param output Define the output variables to extract from the SWAT model
#'   runs. See function \code{\link{define_output}} help file to see how to
#'   define simulation outputs.
#' @param parameter (optional) SWAT model parameters either provided as named
#'   vector or a tibble. The parameter changes provided with \code{parameter}
#'   are performed during the model execution accordingly. To learn how to
#'   modify parameters see the \href{https://Mike-Fuchs.github.io/SWATandR/articles/SWATandR.html}{Get started} page of \code{SWATandR}.
#' @param start_date (optional) Start date of the SWAT simulation. Provided as
#'   character string in any ymd format (e.g. 'yyyy-mm-dd') or in Date format
#'   project are located.
#' @param end_date (optional) End date of the SWAT simulation. Provided as
#'   character string in any ymd format (e.g. 'yyyy-mm-dd') or in Date format
#'   project are located
#' @param output_interval (optional) Time interval in which the SWAT model
#'   outputs are written. Provided either as character string ("d" for daily,
#'   "m" for monthly, or "y" for yearly, and "a" for average annual)
#' @param years_skip (optional) Integer value to define the number of simulation
#'   years that are skipped before writing SWAT model outputs.
#' @param run_index (optional) Numeric vector (e.g.\code{run_index = c(1:100,
#'   110, 115)}) to run a subset of the provided \code{parameter} sets. If NULL
#'   all provided parameter sets are used in the simulation.
#' @param run_path (optional) Character string that provides the path where the
#'   '.model_run' folder is written and the SWAT models are executed. If NULL
#'   '.model_run' is built in the project folder.
#' @param n_thread (optional) Number of threads to be used for the parallel
#'   model run. If not provided models are run on single core. The parameter is
#'   ineffective for single simulations.
#' @param save_path (optional) Character string to define the path where the
#'   model runs are saved if \code{save_file} is defined. If \code{save_path = NULL}
#'   the \code{save_file} is saved in the project_path.
#' @param save_file (optional) Character string to define the name of the folder
#'   where data bases are generated that store the simulations incrementally.
#' @param return_output (optional) Logical. Whether outputs should be returned
#'   or not. Set \code{return_out = FALSE} and provide \code{save_file} if
#'   outputs should only be saved on the hard drive and not be returned in R.
#'   '\code{Default = TRUE}
#' @param add_date (optional) Logical. If \code{add_date = TRUE} a date column
#'   is added to every simulation output table.  \code{Default = TRUE}
#' @param add_parameter (optional) Logical. If \code{add_parameter = TRUE}, the
#'   values of the parameter changes and information on the changes are saved
#'   and/or returned together with the model outputs. \code{Default = TRUE}
#' @param refresh (optional) Logical. \code{refresh = TRUE} always forces that
#'   '.model_run' is newly written when SWAT run ins started. \code{Default =
#'   TRUE}
#' @param keep_folder (optional) Logical. If \code{keep_folder = TRUE}
#'   '.model_run' is kept and not deleted after finishing model runs. In this
#'   case '.model_run' is reused in a new model run if \code{refresh = FALSE}.
#'   \code{Default = FALSE}
#' @param quiet (optional) Logical. If \code{quiet = TRUE} no messages are
#'   written.  \code{Default = FALSE}
#' @param revision (optional) Numeric. If \code{revision} is defined
#' \code{run_swatplus()} uses the input revision number (e.g. \code{revision = 59.3}.
#' Otherwise the revision number is acquired from the SWAT executable.
#'
#' @section Examples:
#'   To learn the basics on how to use \code{SWATandR} see the
#'   \href{https://Mike-Fuchs.github.io/SWATandR/articles/SWATandR.html#first-swat-model-runs}{Get started}
#'   page on the package's github page.
#' @return Returns the simulation results for the defined output variables as a
#'   tibble. If more than one parameter set was provided a list of tibbles is
#'   returned where each column is a model run and each list entry is an output
#'   variable.
#'
#' @importFrom doSNOW registerDoSNOW
#' @importFrom dplyr %>%
#' @importFrom foreach foreach %dopar%
#' @importFrom lubridate now
#' @importFrom parallel detectCores makeCluster parSapply stopCluster
#' @importFrom processx run
#' @importFrom purrr map
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @export
run_swatplus <- function(project_path, output, parameter = NULL,
                         start_date = NULL, end_date = NULL,
                         output_interval = NULL, years_skip = NULL,
                         run_index = NULL, run_path = NULL,
                         n_thread = NULL, save_path = NULL,
                         save_file = NULL, return_output = TRUE,
                         add_parameter = TRUE, add_date = TRUE,
                         refresh = TRUE, keep_folder = FALSE,
                         quiet = FALSE, revision = NULL,singel_plant = TRUE) {

#-------------------------------------------------------------------------------

  # Check input parameters for additional inputs
  # Not implemented currently, might be required if soft calibration is
  # implemented
  # add_input <- as.list(match.call(expand.dots=FALSE))[["..."]]

  # Check settings before starting to set up '.model_run'
  ## General function input checks
  stopifnot(is.character(project_path))
  stopifnot(is.character(run_path)|is.null(run_path))
  stopifnot(is.numeric(n_thread)|is.null(n_thread))
  stopifnot(is.numeric(years_skip)|is.null(years_skip))
  stopifnot(is.logical(add_parameter))
  stopifnot(is.logical(add_date))
  stopifnot(is.logical(return_output))
  stopifnot(is.logical(refresh))
  stopifnot(is.logical(keep_folder))
  stopifnot(is.logical(quiet))

  ## Check if all parameter names exist in cal_parms.cal
  if (!is.null(parameter)) {
    values <- parameter
    names(values) <- unlist(lapply(strsplit(names(parameter),"\\|"), `[[`, 1))
    aaa<-unlist(lapply(strsplit(names(parameter),"\\|"), `[[`, 1))
    bbb<-unlist(lapply(strsplit(names(parameter),"\\|"), `[[`, 1))
    ccc<-unlist(lapply(strsplit(names(parameter),"\\|"), `[[`, 2))
    ddd<-unlist(lapply(strsplit(names(parameter),"\\|"), `[[`, 3))
    definition <- tibble(
      par_name = aaa,
      parameter = bbb,
      file_name = ccc,
      change = ddd
    )
    parameter <- list(values=values,definition=definition)
  }
  
  ## Check values provided with run_index and prepare run_index for simulation
  if(!is.null(run_index)){
    run_index <- check_run_index(run_index, parameter$values)
  } else {
    run_index <- 1:max(nrow(parameter$values), 1)
  }

  ## Convert output to named list in case single unnamed output was defined
  output <- check_output(output, "plus")

  ## Define save_path and check if planned simulations already exist in save file
  if(!is.null(save_file)) {
    save_path <- set_save_path(project_path, save_path, save_file)
    check_saved_data(save_path, parameter, output, run_index)
  }


  ## Check if soft_calibration was triggered by screen methods. If TRUE it
  ## forces the model setup to also write the average annula balances.
  ## Not yet implemented thus set FALSE by default
  # if("soft_calibration" %in% names(add_input)){
  #   soft_cal <- eval(add_input$soft_calibration)
  # } else {
  #   soft_cal <- FALSE
  # }
  soft_cal <- FALSE

  ## Read and modify the projects' files defining simulation period years to
  ## skip, interval, etc.
  model_setup <- setup_swatplus(project_path, parameter, output,
                                start_date, end_date,
                                output_interval, years_skip, soft_cal, unit_cons)

  # Check if weather inputs accord with start and end date
  check_dates(project_path, model_setup)

#-------------------------------------------------------------------------------
  # Build folder structure where the model will be executed
  ## Identify the required number of parallel threads to build.
  n_thread <- min(max(nrow(parameter$values),1),
                  max(n_thread,1),
                  max(length(run_index),1),
                  detectCores())

  ## Set the .model_run folder as the run_path
  run_path <- ifelse(is.null(run_path), project_path, run_path)%//%".model_run"

  ## Identify operating system and find the SWAT executable in the project folder
  os <- get_os()

  ## Manage the handling of the '.model_run' folder structure.
  swat_exe <- manage_model_run(project_path, run_path, n_thread, os,
                               "plus", refresh, quiet)
  if(is.null(revision)){
    revision <- check_revision(project_path, run_path, os, swat_exe)
  }
  # cat("SWAT revision is ",swat_rev,"\n")
  output <- translate_outfile_names(output, model_setup$output_interval, revision)
  if(any(grepl("pest",output))){
    if(output_interval=="d"){
      model_setup$print.prt[48] <- "pest                         y             n             n             n  "
    }else if(output_interval=="m"){
      model_setup$print.prt[48] <- "pest                         n             y             n             n  "
    }else{
      model_setup$print.prt[48] <- "pest                         n             n             y             n  "
    }
  }
  
#-------------------------------------------------------------------------------
  # Write files
  ## Write model setup: Files that define the time range etc. of the SWAT
  ## simulation
  write_swatplus_setup(run_path, model_setup)

  ## Initialize the save_file if defined
  if(!is.null(save_file)) {
    initialize_save_file(save_path, parameter, model_setup)
  }
  #-------------------------------------------------------------------------------
  # Initiate foreach loop to run SWAT models
  ## make and register cluster, create table that links the parallel worker
  ## with the created parallel thread folders in '.model_run'
  cl <- makeCluster(n_thread)
  worker <- tibble(worker_id = parSapply(cl, 1:n_thread,
                                         function(x) paste(Sys.info()[['nodename']],
                                                           Sys.getpid(), sep = "-")),
                   thread_id = dir(run_path) %>% .[grepl("thread_",.)])

  registerDoSNOW(cl)
  #-------------------------------------------------------------------------------
  # Start parallel SWAT model execution with foreach

  ## If not quiet a function for displaying the simulation progress is generated
  ## and provided to foreach via the SNOW options
  n_run <- length(run_index)
  if(!quiet) {
    cat("Performing", n_run, "simulation"%&%plural(n_run),"on", n_thread,
        "core"%&%plural(n_thread)%&%":", "\n")
    t0 <- now()
    progress <- function(n){
      display_progress(n, n_run, t0, "Simulation")
    }
    opts <- list(progress = progress)
  } else {
    opts <- list()
  }
#
  sim_result <- foreach(i_run = 1:n_run,
  .packages = c("dplyr", "lubridate", "processx", "stringr"), .options.snow = opts) %dopar% {
    # for(i_run in 1:max(nrow(parameter), 1)) {
    ## Identify worker of the parallel process and link it with respective thread
    worker_id <- paste(Sys.info()[['nodename']], Sys.getpid(), sep = "-")
    thread_id <- worker[worker$worker_id == worker_id, 2][[1]]
    thread_path <- run_path%//%thread_id
    # thread_path <- run_path%//%"thread_1"

        ## Modify model parameters if parameter set was provided and write
    ## calibration file. If no parameters provided write empty calibration file
    if(is.null(parameter)) {
      if(file.exists(thread_path%//%"calibration.cal")) {
        file.remove(thread_path%//%"calibration.cal")
      }
    } else {
	lookup <- read.csv(paste0(thread_path,"/","lookup.lkp"), header = T, sep = ",", colClasses = c("character", "character"))
	if (!file.exists(thread_path %//% "lookup.lkp")) {
	 stop("lookup.lkp doesn't exist!")
	}
	in_cal_parms <- parameter$definition$parameter %in% lookup[,1]
	if (any(!in_cal_parms)) {
	 stop("Parameters" %&&% paste(parameter$definition$par_name[!in_cal_parms],
								  collapse = ", ") %&&% "not defined in 'lookup.lkp'")
	}
	lkp <- parameter$definition[c(1,3,4)]
	lkp$file <- rep("", nrow(lkp))
	for(k in 1:nrow(lkp)){
	 lkp$file[k] <- lookup[which(lookup[,1]==unlist(lkp[k,1])),2]
	}
	lkp$value <- as.numeric(unlist(parameter$values[run_index[i_run],]))
	lfi <- unlist(unique(lkp[,4]))
	# for loop over all files
	for(i in 1:length(lfi)){
	 lsu <- lkp[which(lkp$file==lfi[i]),]
	 if(lfi[i]=="plants.plt" & singel_plant){                         # for plant parameters
	   test <- read.csv(paste0(project_path,"/",lfi[i]), sep = "", skip = 1, header = T)
	   for(j in 1:nrow(lsu)){
		 if(lsu[j,3]=="pctchg"){
		   test[which(test$name==as.character(lsu[j,2])),grep(lsu[j,1],names(test))] <- test[which(test$name==as.character(lsu[j,2])),grep(lsu[j,1],names(test))]*((100+as.numeric(lsu[j,5]))/100)
		 }
		 if(lsu[j,3]=="relchg"){
		   test[which(test$name==as.character(lsu[j,2])),grep(lsu[j,1],names(test))] <- test[which(test$name==as.character(lsu[j,2])),grep(lsu[j,1],names(test))]*(1+as.numeric(lsu[j,5]))
		 }
		 if(lsu[j,3]=="abschg"){
		   test[which(test$name==as.character(lsu[j,2])),grep(lsu[j,1],names(test))] <- test[which(test$name==as.character(lsu[j,2])),grep(lsu[j,1],names(test))]+as.numeric(lsu[j,5])
		 }
		 if(lsu[j,3]=="absval"){
		   test[which(test$name==as.character(lsu[j,2])),grep(lsu[j,1],names(test))] <- as.numeric(lsu[j,5])
		 }
	   }
	   sink(paste0(thread_path,"/",lfi[i]))
	   writeLines(c("written by SWAT+ editor and modiefied for calibration"))
	   write.table(test, quote = F, sep = " ", row.names = F)
	   sink()
	 }else if(lfi[i]=="soils.sol"){
	   soil <- read.fwf(file = paste0(project_path,"/",lfi[i]),
						widths = c(4,31,18,14,14,14,9,32,14,14,14,14,14,14,14,14,14,14,14,14,14),
						skip = 1,
						colClass = rep("character",21))
	   soil <- data.frame(lapply(as.data.frame(lapply(soil, trimws)), as.character), stringsAsFactors=FALSE)
	   names(soil) <- soil[1,]
	   soil <- soil[-1,]
	   soil[c(1,2,4:21)] <- lapply(soil[c(1,2,4:21)], as.numeric)
	   for(j in 1:nrow(lsu)){
		 if(lsu[j,3]=="pctchg"){
		   soil[,grep(lsu[j,1],names(soil))] <- soil[,grep(lsu[j,1],names(soil))]*((100+as.numeric(lsu[j,5]))/100)
		 }
		 if(lsu[j,3]=="relchg"){
		   soil[,grep(lsu[j,1],names(soil))] <- soil[,grep(lsu[j,1],names(soil))]*(1+as.numeric(lsu[j,5]))
		 }
		 if(lsu[j,3]=="abschg"){
		   for(k in 1:nrow(soil)){
			 if(is.na(soil[k,grep(lsu[j,1],names(soil))])){
			 }else{
			   soil[k,grep(lsu[j,1],names(soil))] <- soil[k,grep(lsu[j,1],names(soil))]+as.numeric(lsu[j,5])
			 }
		   }
		 }
		 if(lsu[j,3]=="absval"){
		   for(k in 1:nrow(soil)){
			 if(is.na(soil[k,grep(lsu[j,1],names(soil))])){
			 }else{
			   soil[k,grep(lsu[j,1],names(soil))] <- as.numeric(lsu[j,5])
			 }
		   }
		 }
	   }
	   col_names <- paste0(c(sprintf(c("%-4s", "%31s", "%18s", "%14s", "%14s", "%14s", "%9s", "%32s", "%14s", "%14s", "%14s", "%14s", "%14s", "%14s", "%14s", "%14s", "%14s", "%14s", "%14s", "%14s", "%14s"), names(soil)),"  "), collapse = "")
	   x<-c()
	   for(i in 1:nrow(soil)){
		 if(is.na(soil[i,1])){
		   x[i] <- sprintf("%-4s","")
		 }else{
		   x[i] <- sprintf("%-4s",soil[i,1])
		 }
	   }
	   soil[,1] <- x
	   for(i in 1:nrow(soil)){
		 if(is.na(soil[i,2])){
		   x[i] <- sprintf("%31s","")
		 }else{
		   x[i] <- sprintf("%31.0f",soil[i,2])
		 }
	   }
	   soil[,2] <- x
	   soil[,3] <- sprintf(rep("%18s",nrow(soil)),soil[,3])
	   for(i in 1:nrow(soil)){
		 if(is.na(soil[i,4])){
		   x[i] <- sprintf("%14s","")
		 }else{
		   x[i] <- sprintf("%14.5f",soil[i,4])
		 }
	   }
	   soil[,4] <- x
	   for(i in 1:nrow(soil)){
		 if(is.na(soil[i,5])){
		   x[i] <- sprintf("%14s","")
		 }else{
		   x[i] <- sprintf("%14.5f",soil[i,5])
		 }
	   }
	   soil[,5] <- x
	   for(i in 1:nrow(soil)){
		 if(is.na(soil[i,6])){
		   x[i] <- sprintf("%14s","")
		 }else{
		   x[i] <- sprintf("%14.5f",soil[i,6])
		 }
	   }
	   soil[,6] <- x
	   for(i in 1:nrow(soil)){
		 if(is.na(soil[i,7])){
		   x[i] <- sprintf("%9s","")
		 }else{
		   x[i] <- sprintf("%9.0f",soil[i,7])
		 }
	   }
	   soil[,7] <- x
	   for(i in 1:nrow(soil)){
		 if(is.na(soil[i,8])){
		   x[i] <- sprintf("%32s","")
		 }else{
		   x[i] <- sprintf("%32.5f",soil[i,8])
		 }
	   }
	   soil[,8] <- x
	   for(j in 9:21){
		 for(i in 1:nrow(soil)){
		   if(is.na(soil[i,j])){
			 x[i] <- sprintf("%14s","")
		   }else{
			 x[i] <- sprintf("%14.5f",soil[i,j])
		   }
		 }
		 soil[,j] <- x
	   }
	   soil$ad <- "  "
	   sink(paste0(thread_path,"/soils.sol"))
	   writeLines(c("written by SWAT+ editor and modiefied for calibration"))
	   writeLines(col_names)
	   write.table(soil, quote = F, sep = "", row.names = F, col.names = F)
	   sink()
	 }else{                                            # all other parameters
	   test <- read.csv(paste0(project_path,"/",lfi[i]), sep = "", skip = 1, header = T)
	   for(j in 1:nrow(lsu)){
		 if(lsu[j,3]=="pctchg"){
		   test[,grep(lsu[j,1],names(test))] <- test[,grep(lsu[j,1],names(test))]*((100+as.numeric(lsu[j,5]))/100)
		 }
		 if(lsu[j,3]=="relchg"){
		   test[,grep(lsu[j,1],names(test))] <- test[,grep(lsu[j,1],names(test))]*(1+as.numeric(lsu[j,5]))
		 }
		 if(lsu[j,3]=="abschg"){
		   test[,grep(lsu[j,1],names(test))] <- test[,grep(lsu[j,1],names(test))]+as.numeric(lsu[j,5])
		 }
		 if(lsu[j,3]=="absval"){
		   test[,grep(lsu[j,1],names(test))] <- as.numeric(lsu[j,5])
		 }
	   }
	   sink(paste0(thread_path,"/",lfi[i]))
	   writeLines(c("written by SWAT+ editor and modiefied for calibration"))
	   write.table(test, quote = F, sep = " ", row.names = F)
	   sink()
	   
	   # urban debugging 
	   sink(paste0(thread_path,"/debug.txt"))
	   print(test)	   
	   sink()
	   
	 }
	}
	if (file.exists(thread_path %//% "calibration.cal")) {
	 file.remove(thread_path %//% "calibration.cal")
	}
	}


    ## Execute the SWAT exe file located in the thread folder
    for(lll in 1:10){
		msg <- run(run_os(swat_exe, os), wd = thread_path, error_on_status = FALSE)
		if(file.exists("success.fin")) break	
	}

	## modify header of pest files
	if(any(grepl("pest",output))){
	 z <- unique(unlist(as.data.frame(output)[1,grep("pest",as.data.frame(output))]))
	 for(i in 1:length(z)){
	  x<-readLines(paste0(thread_path,"/",z[i]))
	  y<-c(x[1],stringr::str_replace_all(stringr::str_replace_all(x[2],"_kg","   "),"/ha","   "),"                                                                                                                   ",x[3:length(x)])
	  writeLines(y, paste0(thread_path,"/",z[i]))
	 }
	}

    if(nchar(msg$stderr) == 0) {
      ## Read defined model outputs
      model_output <- read_swatplus_output(output, thread_path, revision) %>%
        extract_output(output, .)

      if(!is.null(save_path)) {
        save_run(save_path, model_output, parameter, run_index, i_run, thread_id)
      }
    } else {
      err_msg <- str_split(msg$stderr, '\r\n|\r|\n', simplify = TRUE)
      out_msg <- str_split(msg$stdout, '\r\n|\r|\n', simplify = TRUE) %>%
        .[max(1, length(.) - 10):length(.)]
      err_msg <- c('Last output:', out_msg, 'Error:', err_msg)
      model_output <- err_msg
      if(!is.null(save_path)) {
        save_error_log(save_path, model_output, parameter, run_index, i_run)
      }
    }

    if(return_output) {
      return(model_output)
    }
  }

  ## Stop cluster after parallel run
  stopCluster(cl)

  ## Show total runs and elapsed time in console if not quiet
  if(!quiet) {
    finish_progress(n_run, t0, "simulation")
    ## Delete the time stamp t0 created for the progress estimation
    rm(t0)
  }

  ##Tidy up results if return_output is TRUE
  if(return_output) {
    ## Create date vector from the information in model_setup
    date <- get_date_vector(model_setup)
    ## Tidy up the simulation results and arrange them in clean tibbles before
    ## returning them
    sim_result <- tidy_results(sim_result, parameter, date, add_parameter,
                               add_date, run_index)

  }
  ## Delete the parallel threads if keep_folder is not TRUE
  if(!keep_folder)unlink(run_path, recursive = TRUE)

  ## ...and return simulation results if return_output is TRUE
  if(return_output) return(sim_result)
}
