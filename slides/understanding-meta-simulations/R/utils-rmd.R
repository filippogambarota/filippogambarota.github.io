# Utils functions used in the paper and supplementary materials -----------

#' svg_to_pdf
#' @description convert all svg to pdf
svg_to_pdf <- function(){
    all_svg <- list.files(here::here("docs", "paper-draft", "img"), 
                          pattern = ".svg", full.names = TRUE, recursive = TRUE)
    out_names <- gsub(".svg", ".pdf", all_svg)
    purrr::walk2(all_svg, out_names, rsvg::rsvg_pdf)
}

#' get_caption
#' @description create text reference for captions
get_caption <- function() {
    chunk <- knitr::opts_current$get("label")
    sprintf("(ref:%s)", chunk)
}

#' get_funs
#' @description extract all functions as text from a \code{.R} file
#' @param file path of the file
#'
#' @return named list
#' @export
#'
get_funs <- function(file){
    file <- suppressWarnings(readLines(file))
    cutpoints <- grep("<- function", file)
    cutpoints[length(cutpoints) + 1] <- length(file)
    
    out <- vector(mode = "list", length = length(cutpoints)-1)
    fun_names <- vector(mode = "character", length = length(cutpoints)-1)
    
    for(i in 1:(length(cutpoints) - 1)){
        if(i == length(cutpoints) - 1){
            out[[i]] <- file[cutpoints[i]:(cutpoints[i + 1])]
        }else{
            out[[i]] <- file[cutpoints[i]:(cutpoints[i + 1] - 1)]
        }
        fun_names[i] <- stringr::str_extract(out[[i]][1], ".+?(?=<-)")
    }
    fun_names <- gsub(" ", "", fun_names)
    names(out) <- fun_names
    out <- lapply(out, function(x) x[!grepl("#'", x)]) # remove roxygen
    return(out)
}

#' print_fun
#'
#' @param fun character vector with the function text
#'
#' @export
#'
print_fun <- function(fun){
    cat("```r\n", fun, sep = "\n", "```\n")
}

#' rma_tidy
#'
#' @param res model fitted with \code{metafor::rma.uni()}
#'
#' @return a list
#'
rma_tidy <- function(res){
    res_sum <- broom::tidy(res, conf.int = TRUE)
    i2 <- sprintf("$I^2 = %.3f%%$", res$I2)
    
    if(!res$int.only){
        tau2 <- sprintf("$\\tau^2_r = %.3f$ ($SE = %.3f$)", res$tau2, res$se.tau2)
    }else{
        tau2 <- sprintf("$\\tau^2 = %.3f$ ($SE = %.3f$)", res$tau2, res$se.tau2)
    }
    
    r2 <- sprintf("$R^2 = %.3f%%$", res$R2)
    k <- sprintf("$k = %s$", res$k)
    
    if(res$method != "REML"){
        notes <- list(k = k)
    }else{
        if(res$int.only){
            notes <- list(k = k, tau2 = tau2, i2 = i2)
        }else{
            notes <- list(k = k, tau2 = tau2, r2 = r2, i2 = i2)
        }
    }
    
    notes <- lapply(notes, as_tex)
    
    res_sum$ci <- sprintf("$[%.3f, %.3f]$", res_sum$conf.low, res_sum$conf.high)
    res_sum$beta <- sprintf("%.3f (SE = %.3f)", res_sum$estimate, res_sum$std.error)
    res_sum$ps <- ifelse(res_sum$p.value < 0.001, "< 0.001", sprintf("%.3f", res_sum$p.value))
    
    list(res = res_sum, notes = notes)
}

#' rma_table
#'
#' @param res_tidy the first element of the \code{rma_tidy()} output
#' @param notes the second element of the \code{rma_tidy()} output
#' @param caption the table caption
#'
#' @export
#'
rma_table <- function(res_tidy, notes, caption){
    col_names <- c("", "$\\beta$", "95\\% CI", "z", "p")
    res_tidy |> 
        select(term, beta, ci, statistic, ps) |> 
        kable(digits = 3,
              align = "c",
              caption = caption,
              col.names = col_names, 
              escape = FALSE, 
              format = "latex",
              booktabs = TRUE) |> 
        kable_styling(full_width = FALSE, 
                      position = "center",
                      latex_options = "HOLD_position",
                      font_size = 9) |> 
        add_footnote(notes, notation = "none", escape = FALSE)
}

#' trim_df
#'
#' @param data a dataframe
#' @param prows how many rows to display
#'
#' @return a dataframe
#' @export
#'
trim_df <- function(data, prows = 4){
    data <- mutate(data, across(where(is.factor), as.character))
    dots <- data[1, ]
    dots[1, ] <- "..."
    nrows <- nrow(data)
    trimmed <- rbind(
        data[1:prows,],
        dots,
        data[(nrows-(prows - 1)):nrows, ]
    )
    rownames(trimmed) <- NULL
    return(trimmed)
}

#' as_tex
#'
#' @param x a character vector
#'
#' @export
#'
as_tex <- function(x){
    gsub("%", "\\\\%", x)
}


#' check_sim
#'
#' @param fixed named list of population level parameters
#' @param results named list of simulation results
#'
#' @return NULL
#' @import cli
#' @export
#'
check_sim <- function(fixed, results){
    results <- results[names(fixed)] # arrange
    diff <- abs(unlist(results) - unlist(fixed))
    out <- sprintf("%s: Fixed = %.3f, Simulated = %.3f --> %s = %.3f",
                   cli::col_blue(names(results)), fixed, results, cli::col_magenta("|Fixed - Simulated|"), diff)
    cat(out, sep = "\n\n")
}

get_packages <- function(){
    options(renv.verbose = FALSE)
    pkgs <- renv::dependencies()
    pkgs <- unique(pkgs$Package)
    pkgs <- pkgs[!pkgs %in% c("R", "shiny")]
    pkgs_data <- data.frame(
        pkg = pkgs
    )
    
    pkgs_data$version <- sapply(pkgs_data$pkg, 
                                function(x) paste(packageVersion(x), collapse = "."))
    pkgs_data
}
