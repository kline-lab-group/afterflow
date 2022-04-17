#' @include package-functions.R

#' @title S4 AfterFlowBase (Super) Class
#'
#' @description
#' This handles storage and manipulation of the FCS files and MFI values.
#' Subclassed by [AfterFlowFrame] and [AfterFlowBase]
#'
#' @slot fcs_orig Original FCS file(s)
#' @slot fcs_active Processed FCS file(s)
#' @slot metadata tibble with metadata information, by event
#' @slot markers_exclude List of excluded markers for analysis
#' @slot comp_key Compensation/spillover matrix key (only set when applied)
#' @slot transform_method Transformation method (empty character for none)
#' @slot batch_covar Batch correction covariable
#' @slot batch_bool Whether batch correction was applied to current active data
#' @slot clust Vector of clustering methods run (corresponding to metadata columns)
#' @slot dimred Vector of dimensional reduction methods run (corresponding to metadata columns)
#' @slot seed Seed value

setClass(
  "AfterFlowBase",
  slots = list(
    "metadata" = "data.frame",
    "comp_key" = "character",
    "transform_method" = "character",
    "markers_exclude" = "character",
    "batch_covar" = "character",
    "batch_bool" = "logical",
    "clust" = "character",
    "dimred" = "character",
    "seed" = "numeric"
  )
)

#' @title S4 AfterFlowFrame (Sub) Class
#'
#' @description
#' Contains the original and processed [flowCore::flowFrame]
#' Subclass of [AfterFlowBase]
#'
#' @slot fcs_orig Original flowFrame
#' @slot fcs_active Processed flowFrame

setClass(
  "AfterFlowFrame",
  contains = "AfterFlowBase",
  slots = list(
    "fcs_active" = "flowFrame",
    "fcs_orig" = "flowFrame"
  )
)

#' @title S4 AfterFlowSet (Sub) Class
#'
#' @description
#' Contains the original and processed [flowCore::flowSet]
#' Subclass of [AfterFlowBase]
#'
#' @slot fcs_orig Original flowSet
#' @slot fcs_active Processed flowSet

setClass(
  "AfterFlowSet",
  contains = "AfterFlowBase",
  slots = list(
    "fcs_active" = "list",
    "fcs_orig" = "list"
  )
)

#' @title S4 AfterFlow (Virtual) Class
#'
#' @description
#' Virtual class that can be either an [AfterFlowFrame] or [AfterFlowSet],
#' both subclass [AfterFlowBase]

setClassUnion("AfterFlow", c("AfterFlowFrame", "AfterFlowSet"))

setMethod(
  f = "initialize",
  signature = "AfterFlow",
  definition = function(.Object, fcs_orig, fcs_active,
                        metadata, seed, verbose) {
    .Object@fcs_orig <- fcs_orig
    .Object@seed <- seed
    .Object <- afterflow_reset(.Object)
    if (is.data.frame(metadata)) {
      .Object@metadata <- metadata
    } else {
      .Object@metadata <- data.table::data.table(id = seq_len(nrow(exprs(.Object))))
      .Object <- afterflowframe_add_metadata(.Object, metadata)
    }
    .Object@fcs_active <- fcs_active
    return(.Object)
  }
)

#' @title afterflowframe
#'
#' @description
#' This handles storage and manipulation of the FCS files and MFI values.
#'
#' @param fcs_file Path of the FCS file to be processed
#' @param list_metadata Named vector of metadata variables (example: c(sample = "1"))
#' @param seed Seed to use for all random calculations, default 5841
#' @param verbose Whether to print status messages, default TRUE
#'
#' @return AfterFlow object
#' @importFrom data.table as.data.table
#' @importFrom flowCore read.FCS exprs
#' @export
#'
#' @examples
#' AfterFlow("fcs_location", c(batch = "1")) -> returns an AfterFlow object

afterflowframe <- function(fcs_file, list_metadata,
                            seed = 5841, verbose = TRUE) {
  if (!is.character(fcs_file)) {
    if (is.list(fcs_file) | any(grepl("FileHandler", class(fcs_file))))
      return(afterflowset(fcs_file, list_metadata, seed, verbose))
    show_message("Not a valid string, can't parse file path")
  }
  if (!file.exists(fcs_file))
    show_message("Not a valid file path", final = "stop")
  fcs <- read.FCS(fcs_file, truncate_max_range = FALSE)
  if (verbose)
    show_message("FCS file loaded", basename(fcs_file))
  if (is.null(names(list_metadata)))
    show_message("Metadata must be a named vector or list for initialization",
                 final = "stop")
  return(new("AfterFlowFrame", fcs, fcs, list_metadata, seed, verbose))
}

#' @title afterflowset
#'
#' @description
#' This handles storage and manipulation of the FCS files and MFI values.
#'
#' @param af_fh Path of the FCS file to be processed
#' @param markers_exclude Markers to exclude for analysis, defaults defined with
#'                        AF-A (autofluorescence) and dead
#' @param seed Seed to use for all random calculations, default 5841
#' @param verbose Whether to print status messages, default TRUE
#'
#' @return AfterFlow object
#' @importFrom data.table as.data.table
#' @importFrom flowCore flowSet
#' @importFrom dplyr bind_rows
#' @export
#'
#' @examples
#' AfterFlow("fcs_location", c(batch = "1")) -> returns an AfterFlow object

afterflowset <- function(af_fh, markers_exclude = c("AF-A", "dead"),
                          seed = 5841, verbose = TRUE) {
  if (is.list(af_fh)) {
    if (all(sapply(
      seq_len(length(af_fh)),
      function(i)
        any(grepl("AfterFlowFrame", class(af_fh[[i]]))))
    )) {
      transform_methods <- sapply(af_fh, function(af) af@transform_method)
      if (length(unique(transform_methods)) > 1)
        show_message(paste0("Can't merge, not all provided AfterFlowFrames ",
                            "were transformed the same way"), final = "stop")
      comps <- lapply(af_fh, function(af) af@comp_key)
      names(comps) <- names(af_fh)
      if ((length(unlist(comps)) != 0) &
          (length(unlist(comps)) != length(comps)))
        show_message(paste0("Can't merge, inconsistent compensation across ",
                            "provided AfterFlowFrames"), final = "stop")
      list_fcs_orig <- lapply(af_fh, function(af) af@fcs_orig)
      names(list_fcs_orig) <- names(af_fh)
      list_fcs_active <- lapply(af_fh, function(af) af@fcs_active)
      names(list_fcs_active) <- names(af_fh)
      afs <- new("AfterFlowSet",
                 list_fcs_orig, list_fcs_active,
                 bind_rows(lapply(af_fh, function(af) af@metadata)),
                 seed, verbose)
      afs@transform_method <- unlist(unique(transform_methods))
      afs@comp_key <- unlist(comps)
      afs@markers_exclude <- markers_exclude
      return(afs)
    } else {
      show_message("Not a list of AfterFlow objects", final = "stop")
    }
  } else if (any(grepl("FileHandler", class(af_fh)))) {
    list_fcs <- filehandler_get_fcs(af_fh)
    fcs_main <- list_fcs[["main"]]
    fcs_pop <- list_fcs[names(list_fcs) != "main"]
    list_af <- lapply(names(fcs_main), function(exp) {
      af <- afterflowframe(fcs_main[exp],
                            filehandler_get_meta(af_fh, exp = exp),
                            seed, verbose)
      af_fcs_pop <- sapply(names(fcs_pop), function(pop) fcs_pop[[pop]][exp])
      names(af_fcs_pop) <- names(fcs_pop)
      af <- afterflowframe_add_metadata(af, population = af_fcs_pop,
                                   verbose = verbose)
    })
    names(list_af) <- names(fcs_main)
    return(afterflowset(list_af, markers_exclude))
  } else {
    show_message(paste0("af_fh must be a named list of AfterFlow objects or a ",
                        "FileHandler object"), final = "stop")
  }
}

#' @title AfterFlow exprs
#'
#' @description
#' Resets AfterFlow to the original imported FCS file
#'
#' @param af AfterFlow object
#'
#' @return AfterFlow object
#' @export exprs
#'
#' @rdname AfterFlow-class
#'
#' @examples
#' exprs(af) -> returns the MFI matrix of the active flow object

setMethod(
  "exprs",
  signature = "AfterFlow",
  definition = function(object) {
    af_class <- afterflow_check(object, FALSE)
    if (af_class == "AfterFlowFrame") {
      return(as.data.frame(flowCore::exprs(object@fcs_active)))
    } else {
      return(
        dplyr::bind_rows(
          lapply(
            unique(object@metadata[["exp"]]),
            function(exp)
              return(as.data.frame(flowCore::exprs(object@fcs_active[[exp]])))
          )
        )
      )
    }
  }
)

#' @title AfterFlow Check
#'
#' @description
#' Makes sure the passed object is an AfterFlow object
#'
#' @param af AfterFlow object
#'
#' @return Returns the primary class of the AfterFlow object
#'
#' @examples
#' afterflow_check(af) -> If an AfterFlow object, nothing happens

afterflow_check <- function(af, invis = TRUE) {
  af_classes <- class(af)[grepl("AfterFlow", class(af))]
  if (length(af_classes) == 0)
    show_message("af parameter is not an AfterFlow object", final = "stop")
  flowFrame_check <- function(fcs) {
    return(any(grepl("flowFrame", class(fcs))))
  }
  if (any(grepl("AfterFlowFrame", af_classes))) {
    if (!flowFrame_check(af@fcs_orig) | !flowFrame_check(af@fcs_active))
      show_message("Incorrectly set fcs flowFrame", final = "stop")
    if (invis)
      invisible(return())
    else
      return("AfterFlowFrame")
  } else if (any(grepl("AfterFlowSet", af_classes))) {
    if (!any(grepl("list", class(af@fcs_orig))) |
        !any(grepl("list", class(af@fcs_active))))
      show_message("Incorrectly set fcs list", final = "stop")
    if (!all(c(sapply(af@fcs_orig, flowFrame_check)),
               sapply(af@fcs_active, flowFrame_check)))
      show_message("Incorrectly set fcs list contents", final = "stop")
    if (invis)
      invisible(return())
    else
      return("AfterFlowSet")
  }
  show_message("Not an AfterFlowFrame or AfterFlowSet",
               paste(af_classes, collapse = ", "), final = "stop")
}

#' @title AfterFlow Reset
#'
#' @description
#' Resets AfterFlow to the original imported FCS file
#'
#' @param af AfterFlow object
#' @param verbose Boolean for verbose message printing, default TRUE
#'
#' @return AfterFlow object
#' @export
#'
#' @examples
#' afterflow_reset(af) -> returns a reset AfterFlow object

afterflow_reset <- function(af, verbose = TRUE) {
  afterflow_check(af)
  af@fcs_active <- af@fcs_orig
  af@comp_key <- character()
  af@transform_method <- character()
  af@batch_bool <- FALSE
  af@batch_covar <- character()
  af@clust <- character()
  af@dimred <- character()
  if (verbose)
    show_message("AfterFlow object set to defaults")
  return(af)
}

#' @title AfterFlowFrame Add Metadata
#'
#' @description
#' Adds metadata to AfterFlowFrame object
#'
#' @param af AfterFlow object
#' @param metadata Named vector of additional/modified metadata - will overwrite any matching names
#' @param population Named vector of filename/path of FCS file with subpopulation names
#' @param verbose Boolean for verbose message printing, default TRUE
#'
#' @return AfterFlow object
#' @importFrom flowCore read.FCS exprs
#' @importFrom data.table as.data.table setnafill
#' @importFrom dplyr left_join pull
#' @export
#'
#' @examples
#' afterflowframe_add_metadata(af) -> returns an AfterFlow object with updated metadata

afterflowframe_add_metadata <- function(af, metadata = c(), population = c(),
                                   verbose = TRUE) {
  if (afterflow_check(af, FALSE) == "AfterFlowSet")
    show_message("Cannot add metadata to AfterFlowSet", final = "stop")
  existing_meta <- names(metadata)[names(metadata) %in% colnames(af@metadata)]
  if (length(existing_meta) > 0)
    show_message(
      "Metadata being overwritten",
      paste(existing_meta, collapse = ", "),
      "warning"
    )
  invisible(sapply(
    names(metadata),
    function(meta)
      af@metadata[[meta]] <<- metadata[meta]
  ))
  invisible(sapply(
    names(population),
    function(pop) {
      exprs_pop <- as.data.table(
        exprs(read.FCS(population[pop], truncate_max_range = FALSE))
      )
      pop_name <- sprintf("POP_%s", pop)
      exprs_pop[[pop_name]] <- TRUE
      af@metadata[[pop_name]] <<- ifelse(is.na(left_join(
        as.data.table(exprs(af@fcs_orig)),
        exprs_pop,
        by = as.vector(colnames(exprs(af)))
      )[[pop_name]]), FALSE, TRUE)
    }
  ))
  if (verbose)
    show_message("Metadata added to AfterFlow",
                 paste(c(names(metadata), names(population)), collapse = ", "))
  return(af)
}

#' @title AfterFlow Markers
#'
#' @description
#' Applies the compensation matrix to the AfterFlow object if available
#'
#' @param af AfterFlowFrame object
#' @param verbose Boolean for verbose message printing, default TRUE
#'
#' @return AfterFlow object
#' @importFrom flowCore markernames getChannelMarker
#' @export afterflow_compensate
#'
#' @examples
#' afterflow_markers(af) -> returns a compensated AfterFlow object

afterflow_markers <- function(af, filtered = FALSE, verbose = TRUE) {
  af_class <- afterflow_check(af, FALSE)
  fcs <- if (af_class == "AfterFlowFrame") af@fcs_active else af@fcs_active[[1]]
  markers <- sapply(markernames(fcs), function(m) getChannelMarker(fcs, m)[[2]])
  if (filtered) {
    markers <- markers[!(names(markers) %in% af@markers_exclude)]
    markers <- markers[!(markers %in% af@markers_exclude)]
  }
  return(markers)
}

#' @title AfterFlow Compensation
#'
#' @description
#' Applies the compensation matrix to the AfterFlow object if available
#'
#' @param af AfterFlowFrame object
#' @param verbose Boolean for verbose message printing, default TRUE
#'
#' @return AfterFlow object
#' @importFrom flowCore compensate keyword
#' @export afterflow_compensate
#'
#' @examples
#' afterflow_compensate(af) -> returns a compensated AfterFlow object

afterflow_compensate <- function(af, verbose = TRUE) {
  af_class <- afterflow_check(af, FALSE)
  if (!identical(af@comp_key, character()))
    show_message("Previously compensated from key",
                 paste(unique(af@comp_key), collapse = ", "), "stop")
  comp_keys <- c("SPILL", "SPILLOVER", "COMP")
  comp_keys <- c(comp_keys, paste0("$", comp_keys))
  comp_keys <- c(
    comp_keys[grepl("SPILL", comp_keys)],
    comp_keys[!grepl("SPILL", comp_keys)]
  )
  comp_fcs <- function(fcs, exp = character()) {
    keywords <- names(keyword(fcs))
    comp_key <- (comp_keys[comp_keys %in% keywords])[1]
    if (identical(comp_key, character())) {
      show_message("No compensation matrix found", exp, final = "stop")
    }
    fcs <- compensate(
      fcs,
      keyword(fcs)[[comp_key]]
    )
    if (verbose)
      show_message(
        "Compensation matrix applied from key",
        {
          if (identical(exp, character()))
            comp_key
          else
            paste0(exp, ": ", comp_key)
        }
      )
    return(list(fcs = fcs, comp_key = comp_key))
  }
  if (af_class == "AfterFlowFrame") {
    list_comp <- comp_fcs(af@fcs_active)
    af@fcs_active <- list_comp[["fcs"]]
    af@comp_key <- list_comp[["comp_key"]]
  } else if (af_class == "AfterFlowSet") {
    list_comp <- lapply(names(af@fcs_active),
                        function(fcs)
                          comp_fcs(af@fcs_active[[fcs]], fcs))
    fcs_active <- lapply(list_comp, function(lc) lc[["fcs"]])
    names(fcs_active) <- names(af@fcs_active)
    af@fcs_active <- fcs_active
    comp_keys <- lapply(list_comp, function(lc) lc[["comp_key"]])
    names(comp_keys) <- names(af@fcs_active)
    af@comp_key <- unlist(comp_keys)
  }
  return(af)
}

#' @title AfterFlow Transformation
#'
#' @description
#' Applies the transformation specified to the AfterFlow object
#'
#' @param af AfterFlow object
#' @param transform_method Options include arcsinh, lgcl_ct, lgcl_fc,
#'                         log, biexp, none. Defaults to arcsinh.
#' @param verbose Boolean for verbose message printing, default TRUE
#'
#' @return AfterFlow object
#' @importFrom flowCore markernames transformList arcsinhTransform estimateLogicle
#' @importFrom flowCore logicleTransform logTransform biexponentialTransform transform
#' @export afterflow_transform
#'
#' @examples
#' afterflow_transform(af) -> returns a transformed AfterFlow object with arcsinh

afterflow_transform <- function(af, transform_method = "arcsinh",
                                verbose = TRUE) {
  af_class <- afterflow_check(af, FALSE)
  method_list <- c("arcsinh", "lgcl", "log", "biexp")
  if (!(transform_method %in% method_list))
    show_message("Invalid transformation method", transform_method, "stop")
  if (!identical(af@transform_method, character()))
    show_message("Already transformed", af@transform_method, final = "warning")
  af@transform_method <- transform_method
  markers_transform <- names(afterflow_markers(af, verbose = verbose))
  transform_fcs <- function(fcs, exp = character()) {
    transform_list <- switch(
      af@transform_method,
      "arcsinh" = transformList(markers_transform, arcsinhTransform()),
      "lgcl" = transformList(
        markers_transform,
        lapply(
          markers_transform,
          function(marker)
            return(tryCatch(
              expr = estimateLogicle(fcs, marker)@transforms[[marker]]@f,
              error = function(err) {
                show_message(paste0("Estimation of logicle failed with instrument ",
                                    "values, trying data-derived values"),
                             exp, final = "warning")
                return(tryCatch(
                  expr = estimateLogicle(fcs, marker, type = "data")@transforms[[marker]]@f,
                  error = function(err) {
                    show_message(paste0("Estimation of logicle failed with data-",
                                        "derived values, using default logicle"),
                                 exp, final = "warning")
                    return(logicleTransform())
                  }
                ))
              }
            ))
        )
      ),
      "log" = transformList(markers_transform, logTransform()),
      "biexp" = transformList(markers_transform, biexponentialTransform())
    )
    fcs <- transform(fcs, transform_list)
    if (verbose)
      show_message(
        "Transformation method applied",
        {
          if (identical(exp, character()))
            af@transform_method
          else
            paste0(exp, ": ", af@transform_method)
        }
      )
    return(fcs)
  }
  if (af_class == "AfterFlowFrame") {
    af@fcs_active <- transform_fcs(af@fcs_active)
  } else if (af_class == "AfterFlowSet") {
    fcs_active <- lapply(names(af@fcs_active),
                         function(fcs)
                           transform_fcs(af@fcs_active[[fcs]], fcs))
    names(fcs_active) <- names(af@fcs_active)
    af@fcs_active <- fcs_active
  }
  return(af)
}


#' @title AfterFlow Data
#'
#' @description
#' Gets the active MFI values with the existing metadata
#'
#' @param af AfterFlow object
#'
#' @return AfterFlow object
#' @importFrom dplyr bind_cols
#' @export
#'
#' @examples
#' afterflow_data(af) -> returns a current data

afterflow_data <- function(af) {
  afterflow_check(af)
  data <- exprs(af)[names(afterflow_markers(af))]
  data <- bind_cols(data, af@metadata)
  return(data)
}

#' @title AfterFlowSet Batch Correct
#'
#' @description
#' Gets the active MFI values with the existing metadata
#'
#' @param af AfterFlow object
#'
#' @return AfterFlow object
#' @importFrom cyCombine batch_correct
#' @export
#'
#' @examples
#' afterflow_data(af) -> returns a current data

afterflowset_batch <- function(af, cv = NULL, verbose = TRUE) {

  #'   tib_fcs_corrected <- cyCombine::correct_data(
  #'     tib_fcs,
  #'     cyCombine::create_som(
  #'       cyCombine::normalize(
  #'         tib_fcs,
  #'         markers = markers_include,
  #'         norm_method = "scale"
  #'       ),
  #'       markers = markers_include,
  #'       seed = seed
  #'     ),
  #'     covar = batch_covar,
  #'     markers = markers_include
  #'   )


  afterflow_check(af)
  data <- afterflow_data(af)
  markers_include = names(afterflow_markers(af, TRUE))
  data <- batch_correct(data, markers = markers_include,
                        seed = af@seed, covar = cv)
  return(afterflow_set_expr(af, data, verbose))
}

#' @title AfterFlow Set MFIs
#'
#' @description
#' Set MFI values manually
#'
#' @param af AfterFlow object
#' @param expr data.frame of MFI values, requires an "exp" variable if a Set
#'
#' @return AfterFlow object
#' @importFrom cyCombine batch_correct
#' @export
#'
#' @examples
#' afterflow_data(af) -> returns a current data

afterflow_set_expr <- function(af, expr, verbose = TRUE) {
  af_class <- afterflow_check(af, FALSE)
  markers <- names(afterflow_markers(af))
  if (!all(markers %in% colnames(expr)))
    show_message("Missing markers for assignment from expr data.frame",
                 paste(markers[!(markers %in% colnames(expr))], collapse = ", "),
                 final = "stop")

  fcs_assign <- function(fcs, mfi) {
    for (marker in markers)
      fcs@exprs[, marker] <- mfi[[marker]]
    return(fcs)
  }

  if (af_class == "AfterFlowFrame") {
    af@fcs_active <- fcs_assign(af@fcs_active, expr)
  } else {
    exps <- names(af@fcs_active)
    af@fcs_active <- lapply(
      exps,
      function(expt)
        fcs_assign(af@fcs_active[[expt]], dplyr::filter(expr, exp == expt))
    )
    names(af@fcs_active) <- exps
  }

  return(af)
}


#'   tib_fcs_corrected <- cyCombine::correct_data(
#'     tib_fcs,
#'     cyCombine::create_som(
#'       cyCombine::normalize(
#'         tib_fcs,
#'         markers = markers_include,
#'         norm_method = "scale"
#'       ),
#'       markers = markers_include,
#'       seed = seed
#'     ),
#'     covar = batch_covar,
#'     markers = markers_include
#'   )
#'
#'   batch_covar <- ifelse(is.null(batch_covar), "none", batch_covar)
#'
#'   plot_density(
#'     dplyr::bind_rows(
#'       dplyr::mutate(tib_fcs, Type = "Uncorrected"),
#'       dplyr::mutate(dplyr::select(tib_fcs_corrected, -1, -2), Type = "Corrected")
#'     ),
#'     markers = markers_include,
#'     sprintf("%s/BatchCorrection_Covar_%s_Densities.png", dir_output, batch_covar),
#'     covar = batch_covar
#'   )
#'
#'   cowplot::save_plot(
#'     sprintf("%s/BatchCorrection_Covar_%s_DimRed.png", dir_output, batch_covar),
#'     cowplot::plot_grid(
#'       cyCombine::plot_dimred(
#'         tib_fcs_transformed, 'Uncorrected',
#'         markers = markers_include, type = 'pca'
#'       ),
#'       cyCombine::plot_dimred(
#'         tib_fcs_corrected, 'Corrected',
#'         markers = markers_include, type = 'pca'
#'       ),
#'       cyCombine::plot_dimred(
#'         tib_fcs_transformed, 'uncorrected', seed = seed,
#'         markers = markers_include, type = 'umap'
#'       ),
#'       cyCombine::plot_dimred(
#'         tib_fcs_corrected, 'corrected', seed = seed,
#'         markers = markers_include, type = 'umap'
#'       ),
#'       align = "v", scale = 0.9, ncol = 2),
#'     base_width = 12,
#'     base_height = 6
#'   )
#'
