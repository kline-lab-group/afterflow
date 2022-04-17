#' @include package-functions.R

#' @title S4 FileHandler Class
#'
#' @description
#' This handles all directory and file operations, custom directories passed
#' during initialization
#'
#' @slot input Input directory
#' @slot output Output directory
#' @slot fcs List of files, grouped by population, with vectors named by exp
#' @slot root Root population
#' @slot metadata Metadata for each file in data.frame form

setClass(
  "FileHandler",
  slots = list(
    "input" = "character",
    "output" = "character",
    "fcs" = "list",
    "root" = "character",
    "metadata" = "data.frame"
  )
)

setMethod(
  f = "initialize",
  signature = "FileHandler",
  definition = function(.Object, input, output, fcs, root, metadata) {
    .Object@input <- input
    .Object@output <- output
    .Object@fcs <- fcs
    .Object@root <- root
    .Object@metadata <- metadata
    return(.Object)
  }
)

#' @title FileHandler Constructor
#'
#' @description
#' Constructor for the FileHandler object
#'
#' @param dir_main Primary working directory
#' @param dir_flow Directory with flow data
#' @param dir_input Directory with input data
#' @param dir_output Directory with output data
#' @param file_meta Metadata file
#' @param fcs_prefix Prefix for FCS files (default "export")
#' @param fcs_suffix Suffix for FCS files set in FlowJo (default "")
#' @param fcs_root Specifying the primary population
#' @param verbose Whether to print status messages, default TRUE
#'
#' @return FileHandler object
#' @importFrom stringr str_to_lower str_split_fixed str_remove str_remove_all
#' @importFrom readxl excel_sheets read_xls read_xlsx
#' @importFrom readr read_csv read_tsv
#' @importFrom tidyr tibble
#' @importFrom dplyr bind_rows
#' @export
#'
#' @examples
#' FileHandler() -> returns an FileHandler object after searching in working dir

FileHandler <- function(dir_main = NA_character_, dir_flow = "flow",
                        dir_input = "input", dir_output = "output",
                        dir_export = character(), file_meta = "metadata",
                        fcs_prefix = "export", fcs_suffix = "",
                        fcs_root = "CD3+", verbose = TRUE) {
  if (is.na(dir_main)) {
    dir_main <- getwd()
    if (verbose)
      show_message("Main directory is the current working directory", dir_main)
  } else if (dir.exists(dir_main)) {
    dir_main <- str_remove(dir_main, "\\/$")
    if (verbose)
      show_message("Main directory found", dir_main)
  } else {
    show_message("Main directory provided does not exist", dir_main, "stop")
  }

  dir_flow <- ifelse(
    dir.exists(dir_flow),
    dir_flow,
    sprintf("%s/%s", dir_main, dir_flow)
  )
  if (!dir.exists(dir_flow)) {
    show_message("Flow directory does not exist", dir_flow, "stop")
  } else if (verbose) {
    show_message("Flow directory found", dir_flow)
  }


  input <- ifelse(
    dir.exists(dir_input),
    dir_input,
    sprintf("%s/%s", dir_main, dir_input)
  )
  if (!dir.exists(input)) {
    dir.create(input)
    if (verbose)
      show_message("Input directory created", input)
  } else if (verbose) {
    show_message("Input directory found", input)
  }

  output <- ifelse(
    dir.exists(dir_output),
    dir_output,
    sprintf("%s/%s", dir_main, dir_output)
  )
  if (length(dir(output)) > 0) {
    timestamp <- format(file.info(output)$mtime, ".%Y%m%d.%H%M%S")
    old_output <- sprintf("%s%s", output, timestamp)
    file.rename(output, old_output)
    if (verbose)
      show_message("Output directory not empty, moved", details = old_output)
  }
  if (!dir.exists(output)) {
    dir.create(output)
    if (verbose)
      show_message("Output directory created", details = output)
  } else {
    if (verbose)
      show_message("Output directory found", details = output)
  }


  meta_filetypes <- c("xlsx", "xls", "csv", "tsv")
  meta_isfile <- any(sapply(paste0(meta_filetypes, "$"), grepl, file_meta))
  file_meta <- paste0(
    if (file.exists(file_meta)) file_meta else sprintf("%s/%s", input, file_meta),
    if (meta_isfile) "" else meta_filetypes
  )
  metadata <- tidyr::tibble()
  # tib_meta_var <- NULL
  if (!any(file.exists(file_meta))) {
    show_message("No metadata exists, will not be able to perform batch correction or perform by-variable analyses.
            If needed, please provide a metadata file in the input folder (can be xlsx, xls, csv, or tsv).", final = "warning")
    file_meta <- NA_character_
  } else {
    file_meta <- file_meta[file.exists(file_meta)][1]
    if (verbose)
      show_message("Metadata file found", file_meta)
    meta_filetype <- str_to_lower(strsplit(basename(file_meta), split = "\\.")[[1]][-1])
    if (grepl("xls", meta_filetype)) {
      meta_sheets <- excel_sheets(file_meta)
      if (!("meta" %in% meta_sheets))
        show_message("No metadata in provided metadata Excel file--please name a sheet 'meta' for correct processing", file_meta, final = "stop")
    }
    metadata <- switch(
      meta_filetype,
      "xlsx" = read_xlsx(file_meta, sheet = "meta"),
      "xls" = read_xls(file_meta, sheet = "meta"),
      "csv" = read_csv(file_meta),
      "tsv" = read_tsv(file_meta)
    )
    if (is.null(metadata[["exp"]]))
      show_message("Metadata missing experiment column - please name with 'exp'", file_meta, final = "stop")
  }


  fcs_ext <- "\\.(fcs|FCS)$"
  fcs_filenames <- list.files(path = dir_flow, pattern = "^.*\\.fcs",
                              full.names = TRUE, recursive = TRUE)
  if (length(fcs_filenames) == 0)
    show_message("No .fcs files with given pattern found in flow directory",
                 fcs_regex, "stop")
  exps <- str_remove_all(basename(fcs_filenames), fcs_ext)
  if (!identical(exps, unique(exps)))
    show_message("Flow directory has duplicate FCS files", final = "stop")
  exps <- unique(exps)
  names(fcs_filenames) <- exps

  if (nrow(metadata) == 0)
    metadata <- tibble(exp = exps)
  if (!all(exps %in% metadata[["exp"]])) {
    show_message(paste0("Metadata missing some experiment annotations, ",
                        "removing files with no metadata"),
                 paste(exps[!(exps %in% metadata[["exp"]])], collapse = ", "),
                 final = "warning")
    exps <- metadata[["exp"]]
    fcs_filenames <- fcs_filenames[exps]
  }

  fcs <- list("main" = fcs_filenames)
  root <- character()

  if (!identical(dir_export, character())) {
    dir_export <- ifelse(
      dir.exists(dir_export),
      dir_export,
      sprintf("%s/%s", dir_main, dir_export)
    )
    if (!dir.exists(dir_export)) {
      show_message("Exported FCS directory does not exist", dir_export, "stop")
    } else if (verbose) {
      show_message("Exported FCS directory found", dir_export)
    }

    fcs_prefix <- sprintf("%s\\_", fcs_prefix)
    fcs_suffix <- ifelse(fcs_suffix == "", "", sprintf("\\_%s", fcs_suffix))
    fcs_regex <- sprintf("^%s.*%s%s", fcs_prefix, fcs_suffix, fcs_ext)
    fcs_export_filenames <- list.files(path = dir_export, pattern = fcs_regex,
                                       full.names = TRUE, recursive = TRUE)
    if (length(fcs_export_filenames) == 0)
      show_message("No .fcs files with given pattern found in export directory",
                   fcs_regex, "stop")
    if (!all(grepl("\\_", fcs_export_filenames)))
      show_message(paste0("Exported .fcs files not named correctly, expected ",
                          "naming is prefix_SAMPLE_POPULATION(_suffix).fcs"),
                   final = "stop")

    fcs_exps_pops <- str_split_fixed(
      str_remove_all(
        basename(fcs_export_filenames),
        sprintf("^%s|%s%s", fcs_prefix, fcs_suffix, fcs_ext)
      ),
      "\\_", 2
    )
    fcs_pop_exps <- fcs_exps_pops[, 1]
    names(fcs_pop_exps) <- fcs_export_filenames
    pop_exps <- unique(fcs_pop_exps)
    if (!all(exps %in% pop_exps))
      show_message("Missing experiments in exported data",
                   paste(exps[!(exps %in% pop_exps)], collapse = ", "),
                   final = "stop")
    pops <- unique(fcs_exps_pops[, 2])

    root <- ifelse(fcs_root %in% pops, fcs_root, pops[1])
    if (root != fcs_root)
      show_message(paste0("Specified root export population not found, using ",
                   "first one available"), fcs_root, final = "warning")

    fcs_exported <- lapply(
      pops,
      function(pop) {
        if (verbose)
          show_message("Getting .fcs files by population", pop)
        pop_fn <- grepl(pop, basename(fcs_export_filenames), fixed = TRUE)
        fcs_fn <- fcs_export_filenames[pop_fn]
        names(fcs_fn) <- fcs_pop_exps[fcs_fn]
        return(fcs_fn)
      }
    )
    names(fcs_exported) <- pops

    fcs <- c(fcs, fcs_exported)
  }



  return(new("FileHandler", input, output, fcs, root, metadata))
}

#' @title FileHandler Check
#'
#' @description
#' Makes sure the passed object is a FileHandler object
#'
#' @param fh FileHandler object
#'
#' @return Nothing
#'
#' @examples
#' filehandler_check(fh) -> If an FileHandler object, nothing happens

filehandler_check <- function(fh) {
  if (!any(grepl("FileHandler", class(fh))))
    show_message("fh parameter is not an FileHandler object", final = "stop")
  invisible(return())
}

#' @title FCS Filenames
#'
#' @description
#' Returns a list of vectors of FCS filenames split by populations
#'
#' @param fh FileHandler object
#' @param pop Optional population subset, default empty character
#' @param exp Optional experiment subset, default empty character
#'
#' @return List of vectors of filenames
#'
#' @examples
#' filehandler_get_fcs(fh) -> Returns all fcs filenames by population
#' filehandler_get_fcs(fh, pop = "CD3+") -> Returns only CD3+ fcs filenames
#' filehandler_get_fcs(fh, exp = "Exp1") -> Returns only Exp1 fcs filenames across populations

filehandler_get_fcs <- function(fh, pop = character(), exp = character()) {
  filehandler_check(fh)
  pop_exist <- pop %in% names(fh@fcs)
  if (!all(pop_exist))
    show_message("Population not found in FCS file list",
                 paste(pop[!pop_exist], collapse = ", "), "stop")
  exp_exist <- exp %in% fh@metadata$exp
  if (!all(exp_exist))
    show_message("Experiment not found in FCS file list",
                 paste(exp[!exp_exist], collapse = ", "), "stop")
  fcs_list <- if (identical(pop, character())) fh@fcs else fh@fcs[pop]
  if (!identical(exp, character()))
    fcs_list <- lapply(fcs_list, function(fcs) fcs[exp])
  return(fcs_list)
}

#' @title Metadata
#'
#' @description
#' Returns all metadata or by exp filter
#'
#' @param fh FileHandler object
#'
#' @return List of filenames
#'
#' @examples
#' filehandler_get_fcs_exp(fh) -> If CD3+ exists, return list of filenames

filehandler_get_meta <- function(fh, exp = character()) {
  filehandler_check(fh)
  if (identical(exp, character()))
    return(fh@metadata)
  else
    return(unlist(as.list(fh@metadata[fh@metadata$exp == exp, ])))
}

#' getAfterFlow <- function(dir_main = NA_character_, dir_flow = "flow",
#'                          dir_input = "input", dir_output = "output",
#'                          file_meta = "metadata",
#'                          fcs_prefix = "export", fcs_suffix = "",
#'                          fcs_transform = c("arcsinh", "lgcl", "lgcl_estim", "lgcl_med", "log", "biexp"),
#'                          fcs_root = "CD3+", markers_exclude = c(),
#'                          marker_label = "Marker", batch_covar = NULL,
#'                          seed = 5841, verbose = TRUE, test = FALSE) {
#'
#'   if (is.na(dir_main)) {
#'     dir_main <- getwd()
#'     if (verbose)
#'       show_message("Main directory is the current working directory", dir_main)
#'   } else if (dir.exists(dir_main)) {
#'     dir_main <- str_remove(dir_main, "\\/$")
#'     if (verbose)
#'       show_message("Main directory found", dir_main)
#'   } else {
#'     show_message("Main directory provided does not exist", dir_main, "stop")
#'   }
#'
#'   dir_flow <- ifelse(
#'     dir.exists(dir_flow),
#'     dir_flow,
#'     sprintf("%s/%s", dir_main, dir_flow)
#'   )
#'   if (!dir.exists(dir_flow)) {
#'     show_message("Flow directory does not exist", dir_flow, "stop")
#'   } else if (verbose) {
#'     show_message("Flow directory found", dir_flow)
#'   }
#'
#'   dir_input <- ifelse(
#'     dir.exists(dir_input),
#'     dir_input,
#'     sprintf("%s/%s", dir_main, dir_input)
#'   )
#'   if (!dir.exists(dir_input)) {
#'     if (!test)
#'       dir.create(dir_input)
#'     if (verbose)
#'       show_message("Input directory created", dir_input)
#'   } else if (verbose) {
#'     show_message("Input directory found", dir_input)
#'   }
#'
#'   meta_filetypes <- c("xlsx", "xls", "csv", "tsv")
#'   meta_isfile <- any(sapply(paste0(meta_filetypes, "$"), grepl, file_meta))
#'   file_meta <- paste0(
#'     if (file.exists(file_meta)) file_meta else sprintf("%s/%s", dir_input, file_meta),
#'     if (meta_isfile) "" else meta_filetypes
#'   )
#'   tib_meta <- NULL
#'   tib_meta_var <- NULL
#'   if (!any(file.exists(file_meta))) {
#'     show_message("No metadata exists, will not be able to perform batch correction or perform by-variable analyses.
#'             If needed, please provide a metadata file in the input folder (can be xlsx, xls, csv, or tsv).", final = "warning")
#'     file_meta <- NA_character_
#'   } else {
#'     file_meta <- file_meta[file.exists(file_meta)][1]
#'     if (verbose)
#'       show_message("Metadata file found", file_meta)
#'     meta_filetype <- str_to_lower(strsplit(basename(file_meta), split="\\.")[[1]][-1])
#'     if (grepl("xls", meta_filetype)) {
#'       meta_sheets <- excel_sheets(file_meta)
#'       if (!("meta" %in% meta_sheets))
#'         show_message("No metadata in provided metadata Excel file--please name a sheet 'meta' for correct processing", file_meta, final = "stop")
#'       if ("varMeta" %in% meta_sheets) {
#'         if (verbose)
#'           show_message("Variable annotations found in provided metadata Excel file", file_meta)
#'         tib_meta_var <- switch(
#'           meta_filetype,
#'           "xlsx" = readxl::read_xlsx(file_meta, sheet = "varMeta"),
#'           "xls" = readxl::read_xls(file_meta, sheet = "varMeta")
#'         )
#'       }
#'     }
#'     tib_meta <- switch(
#'       meta_filetype,
#'       "xlsx" = readxl::read_xlsx(file_meta, sheet = "meta"),
#'       "xls" = readxl::read_xls(file_meta, sheet = "meta"),
#'       "csv" = readr::read_csv(file_meta),
#'       "tsv" = readr::read_tsv(file_meta)
#'     )
#'     if (is.null(tib_meta[["exp"]]))
#'       show_message("Metadata missing experiment column - please name with 'exp'", file_meta, final = "stop")
#'     if (is.null(tib_meta_var))
#'       tib_meta_var <- tidyr::tibble()
#'     if (is.null(tib_meta_var[["rownames"]])) {
#'       tib_meta_var[["rownames"]] <- colnames(tib_meta)
#'       if (verbose)
#'         show_message("Metadata variable names missing, autopopulated")
#'     }
#'     if (is.null(tib_meta_var[["labelDescription"]])) {
#'       tib_meta_var[["labelDescription"]] <- colnames(tib_meta)
#'       if (verbose)
#'         show_message("Metadata variable descriptions missing, autopopulated")
#'     }
#'
#'   }
#'
#'   dir_output <- ifelse(
#'     dir.exists(dir_output),
#'     dir_output,
#'     sprintf("%s/%s", dir_main, dir_output)
#'   )
#'   if (length(dir(dir_output)) > 0) {
#'     timestamp <- format(file.info(dir_output)$mtime, ".%Y%m%d.%H%M%S")
#'     old_output <- sprintf("%s%s", dir_output, timestamp)
#'     if (!test)
#'       file.rename(dir_output, old_output)
#'     if (verbose)
#'       show_message("Output directory not empty, moved", details = old_output)
#'   }
#'   if (!dir.exists(dir_output)) {
#'     if (!test)
#'       dir.create(dir_output)
#'     if (verbose)
#'       show_message("Output directory created", details = dir_output)
#'   } else {
#'     if (verbose)
#'       show_message("Output directory found", details = dir_output)
#'   }
#'
#'   fcs_prefix <- sprintf("%s\\_", fcs_prefix)
#'   fcs_suffix <- ifelse(fcs_suffix == "", "", sprintf("\\_%s", fcs_suffix))
#'   fcs_ext <- "\\.(fcs|FCS)$"
#'   fcs_regex <- sprintf("^%s.*%s%s", fcs_prefix, fcs_suffix, fcs_ext)
#'
#'   fcs_filenames <- list.files(path = dir_flow, pattern = fcs_regex,
#'                               full.names = TRUE, recursive = TRUE)
#'   if (length(fcs_filenames) == 0) {
#'     show_message("No .fcs files with given pattern found in flow directory",
#'             fcs_regex, "stop")
#'   }
#'   if (!all(grepl("\\_", fcs_filenames))) {
#'     show_message(".fcs files not named correctly, expected naming is prefix_SAMPLE_POPULATION(_suffix).fcs",
#'             final = "stop")
#'   }
#'
#'   GetExpPop <- function(filename, ep) {
#'     stringr::str_split_fixed(
#'       stringr::str_remove_all(
#'         basename(filename),
#'         sprintf("^%s|%s%s", fcs_prefix, fcs_suffix, fcs_ext)
#'       ),
#'       "\\_", 2
#'     )[, ifelse(ep == "e", 1, 2)]
#'   }
#'
#'   exps <- unique(GetExpPop(fcs_filenames, "e"))
#'   if (is.null(tib_meta)) {
#'     tib_meta <- tidyr::tibble(exp = exps, name = exps)
#'     tib_meta_var <- tidyr::tibble(rownames = c("name"), labelDescription = c("name"))
#'   }
#'   if (!all(exps %in% tib_meta[["exp"]]))
#'     show_message("Metadata missing some experiment annotations", exps[!(exps %in% tib_meta[["exp"]])], final = "stop")
#'   if (is.null(tib_meta[["name"]])) {
#'     tib_meta[["name"]] <- tib_meta[["exp"]]
#'     tib_meta_var <- dplyr::bind_rows(tib_meta_var, tidyr::tibble(rownames = c("name"), labelDescription = c("name")))
#'   }
#'
#'   populations <- unique(GetExpPop(fcs_filenames, "p"))
#'   if (!(fcs_root %in% populations)) {
#'     fcs_root <- populations[1]
#'     show_message("Specified root population not found, using first one available", fcs_root, final = "warning")
#'   }
#'   if (verbose)
#'     show_message("Processing .fcs files by population")
#'   fcs <- lapply(
#'     populations,
#'     function(pop) {
#'       if (verbose)
#'         show_message("Importing .fcs files", pop, lvls = 1)
#'       fcs_fn <- fcs_filenames[grepl(pop, basename(fcs_filenames), fixed = TRUE)]
#'       fcs_files <- lapply(fcs_fn, flowCore::read.FCS, truncate_max_range = FALSE)
#'       names(fcs_files) <- GetExpPop(fcs_fn, "e")
#'       return(fcs_files)
#'     }
#'   )
#'   names(fcs) <- populations
#'   list_pop_meta <- lapply(exps, function(exp) {
#'     dplyr::bind_cols(
#'       lapply(
#'         populations[fcs_root != populations],
#'         function(pop) {
#'           root_exprs <- tidyr::as_tibble(flowCore::exprs(fcs[[fcs_root]][[exp]]))
#'           pop_exprs <- tidyr::as_tibble(flowCore::exprs(fcs[[pop]][[exp]]))
#'           pop_member <- tidyr::replace_na(
#'             dplyr::pull(
#'               dplyr::left_join(
#'                 root_exprs,
#'                 dplyr::mutate(pop_exprs, member = TRUE),
#'                 by = colnames(root_exprs)
#'               ),
#'               member
#'             ),
#'             FALSE
#'           )
#'           tib_member <- tidyr::tibble(a = pop_member)
#'           colnames(tib_member) <- pop
#'           return(tib_member)
#'         }
#'       )
#'     )
#'   })
#'   names(list_pop_meta) <- exps
#'
#'
#'   if (verbose)
#'     show_message("Compensating and transforming each sample", pop, lvls = 1)
#'
#'   fcs <- lapply(
#'     exps,
#'     function(exp) {
#'       comp_keys <- c("SPILL", "SPILLOVER", "COMP")
#'       comp_keys <- c(comp_keys, paste0("$", comp_keys))
#'       ff <- fcs[[fcs_root]][[exp]]
#'       comp <- NULL
#'       for (ckey in comp_keys) {
#'         comp <- flowCore::keyword(ff)[[key]]
#'         if (!is.null(comp)) {
#'           ff <- flowCore::compensate(ff, comp)
#'           if (fcs_transform == "none")
#'             return(ff)
#'           if (length(fcs_transform) > 1)
#'             fcs_transform <- fcs_transform[1]
#'           MakeTransformList <- function(tform) {
#'             return(flowCore::transformList(
#'               colnames(comp),
#'               rep(list(tform), dim(comp)[2])
#'             ))
#'           }
#'           transform_list <- switch(
#'             fcs_transform,
#'             "arcsinh" = {
#'               MakeTransformList(flowCore::arcsinhTransform())
#'             },
#'             "lgcl" = {
#'               autoLgcl(ff_comp, colnames(comp), fcs_file)
#'             },
#'             "lgcl_estim" = {
#'               flowCore::estimateLogicle(ff_comp, colnames(comp))
#'             },
#'             "lgcl_med" = {
#'               flowCore::estimateMedianLogicle(ff_comp, colnames(comp))
#'             },
#'             "log" = {
#'               MakeTransformList(flowCore::logTransform())
#'             },
#'             "biexp" = {
#'               MakeTransformList(flowCore::biexponentialTransform())
#'             }
#'           )
#'           return(flowCore::transform(ff, transform_list))
#'         }
#'       }
#'       show_message("Missing compensation matrix", exp, final = "stop")
#'     }
#'   )
#'   names(fcs) <- exps
#'
#'   tib_fcs <- dplyr::bind_rows(
#'     lapply(
#'       exps,
#'       function(exp) {
#'         df_cols <- flowCore::pData(flowCore::parameters(fcs[[exp]]))
#'         tib_exprs <- tidyr::as_tibble(flowCore::exprs(fcs[[exp]]))
#'         colnames(tib_exprs) <- RenameMarkers(dplyr::case_when(
#'           is.na(df_cols[["desc"]]) ~ df_cols[["name"]],
#'           TRUE ~ df_cols[["desc"]]
#'         ))
#'         tib_exprs[["exp"]] <- exp
#'         dplyr::bind_cols(
#'           dplyr::left_join(tib_exprs, tib_meta, by = "exp"),
#'           list_pop_meta[[exp]]
#'         )
#'       }
#'     )
#'   )
#'
#'   markers_include <- colnames(tib_fcs)[
#'     !(colnames(tib_fcs) %in% RenameMarkers(c(markers_flowjo, markers_exclude))) &
#'       grepl(sprintf("^%s", marker_label), colnames(tib_fcs))
#'   ]
#'
#'   non_markers <- c()
#'
#'   # cyCombine::detect_batch_effect_express(
#'   #   tib_fcs[c(markers_include, "batch")],
#'   #   dir_output
#'   # )
#'
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
#'   if (verbose)
#'     show_message("Successfully loaded and corrected required flow data")
#'   FH <- new(
#'     "AfterFlow",
#'     list_fcs,
#'     file_meta,
#'     dirs_io = c(input = dir_input, output = dir_output))
#'   return(FH)
#' }
#'
#'
#'
#' #' Density plot
#' #'
#' #' Makes a ridge-density plot
#' #'
#' #' @param df Data.frame with data
#' #' @param markers Marker list
#' #' @param filename Filename
#' #' @param yvar Y variable
#' #' @param colorvar Color variable
#' #' @param dataset_names Dataset names
#' #' @param ncol Number of columns
#' #' @param covar Covariable
#' #'
#' #' @return ggplot
#' #' @export
#' #'
#' #' @examples
#' #' ggplot
#' plot_density <- function(df, markers, filename = NULL, yvar = "batch",
#'                          colorvar = "Type", dataset_names = NULL, ncol = 6,
#'                          covar = NULL) {
#'   if ("AfterFlow" %in% class(df))
#'     df <- df@fcs
#'   df <- dplyr::select(df, dplyr::all_of(c(markers, yvar, colorvar)))
#'   p <- lapply(
#'     markers,
#'     function(marker)
#'       ggplot2::ggplot(
#'         df,
#'         ggplot2::aes_string(
#'           x = paste0("`", marker, "`"),
#'           y = yvar
#'         )
#'       ) +
#'       ggridges::geom_density_ridges(
#'         ggplot2::aes_string(color = paste0("`", colorvar, "`"), fill = paste0("`", colorvar, "`")),
#'         alpha = 0.4
#'       ) +
#'       ggplot2::labs(y = yvar) +
#'       ggplot2::theme_bw()
#'   )
#'   legend <- cowplot::get_legend(p[[1]] +
#'                                   ggplot2::theme(
#'                                     legend.box.margin = ggplot2::margin(0, 0, 0, 12)
#'                                   ))
#'   p <- lapply(p, function(pl) pl + ggplot2::theme(legend.position = "none"))
#'   p <- cowplot::plot_grid(
#'     cowplot::plot_grid(plotlist = p, ncol = ncol),
#'     legend,
#'     rel_widths = c(2 * ncol, 1)
#'   )
#'   if (is.null(covar))
#'   #   covar <- "none"
#'   # title <-
#'   #   p <- cowplot::plot_grid(
#'   #     cowplot::ggdraw() +
#'   #       cowplot::draw_label(
#'   #         sprintf("Densities by %s (Covar: %s)", yvar, covar),
#'   #         fontface = 'bold',
#'   #         x = 0,
#'   #         hjust = 0
#'   #       ) +
#'   #       ggplot2::theme(
#'   #         plot.margin = margin(0, 0, 0, 7)
#'   #       ), p,
#'   #     ncol = 1,
#'   #     rel_heights = c(0.1, ceiling(length(markers) / ncol))
#'   #   )
#'
#'   if (!is.null(filename))
#'     cowplot::save_plot(filename, p, base_width = 20, base_height = length(markers)/1.3)
#'   else
#'     return(p)
#' }
