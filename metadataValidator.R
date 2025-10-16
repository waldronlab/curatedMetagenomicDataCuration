library(ontologyIndex)
library(xml2)

t <- get_ontology(file = "dictionaries/cmd_age_group.owl")
t <- read_xml("dictionaries/cmd_age_group.owl")

diet_dict <- read.csv("dictionaries/cmd_dietary_restriction.csv")
color_dict <- c("red", "green", "blue", "yellow")
validate_age <- function(x) {
    if (abs(x)-round(x) != 0) {
        return(FALSE)
    }
    if (x < 0 | x > 120) {
       return(FALSE)
    }

    return(TRUE)
}

template <- list(id = NA,
                 dietary_restriction = diet_dict$label,
                 color = color_dict,
                 age = validate_age,
                 species = is.character)

ddata <- data.frame(id = c(1, 2, 3, 4, 5),
                    color = c("red", "green", "blue", "blue", "red"),
                    dietary_restriction = c("vegan", NA, "gluten", 1, "gluten"),
                    age = c(27, 48, 34, 23.5, 199),
                    country = rep("Ukraine", ))

validate_column <- function(validation_type, allowed, data) {
    if (validation_type == "character") {
        results <- unlist(lapply(data, function(x) x %in% allowed))
    } else if (validation_type == "function") {
        results <- unlist(lapply(data, function(x) allowed(x)))
    }

    rejected_ids <- which(!results)
    rejected <- data[rejected_ids]
    report <- list(rejected_ids = rejected_ids,
                   rejected_values = rejected)
    return(report)
}

validateMetadata <- function(data_table, template_list) {
    ## Validate input
    # data_table

    # template_list

    ## Compare columns
    unvalidated_cols <- setdiff(colnames(data_table), names(template_list))
    missing_cols <- setdiff(names(template_list), colnames(data_table))

    effective_template <- template_list
    effective_template[missing_cols] <- NULL

    ## Get validation classes
    template_classes <- lapply(effective_template, class)

    ## Validate columns
    # Non-matching columns
    nm_cols <- c(unvalidated_cols, missing_cols)
    nm_results <- sapply(nm_cols, function(x) list(rejected_values = NA,
                                                   rejected_ids = NA),
                         simplify = FALSE)

    # NA columns
    na_cols <- effective_template[names(which(is.na(effective_template)))]
    na_results <- sapply(na_cols, function(x) list(rejected_values = as.character(),
                                                   rejected_ids = as.integer()),
                         simplify = FALSE)

    # character columns
    character_cols <- effective_template[names(which(template_classes == "character"))]
    character_results <- sapply(names(character_cols), function(x) {
        validate_column("character", character_cols[[x]], data_table[,x])
    },
    simplify = FALSE)

    # function columns
    function_cols <- effective_template[names(which(template_classes == "function"))]
    function_results <- sapply(names(function_cols), function(x) {
        validate_column("function", function_cols[[x]], data_table[,x])
    },
    simplify = FALSE)

    ## Create report
    all_results <- c(nm_results, na_results, character_results, function_results)
    report <- data.frame(column = c(names(template_list), unvalidated_cols)) %>%
        rowwise() %>%
        dplyr::mutate(template = column %in% names(template_list),
                      data = column %in% colnames(data_table)) %>%
        dplyr::mutate(rejected_values = list(all_results[[column]]$rejected_values),
                      rejected_ids = list(all_results[[column]]$rejected_ids))

    return(report)
}
