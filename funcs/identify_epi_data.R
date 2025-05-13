identify_epi_data <- function(data, counts = NULL, groups = NULL) {
  # Check input
  if (!is.data.frame(data)) stop("Input must be a data frame or tibble")
  
  # Load quietly
  suppressPackageStartupMessages({
    if (!requireNamespace("cli", quietly = TRUE)) install.packages("cli")
    if (!requireNamespace("tibble", quietly = TRUE)) install.packages("tibble")
    if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
    library(cli)
    library(tibble)
    library(dplyr)
  })
  
  # Convert to tibble
  data <- as_tibble(data)
  
  # MANUAL MODE (when counts specified)
  if (!is.null(counts)) {
    if (length(counts) != 2) stop("Please specify exactly two column names for counts")
    if (!all(counts %in% names(data))) {
      stop("Specified count columns not found in data: ",
           paste(setdiff(counts, names(data)), collapse = ", "))
    }
    
    if (all(data[[counts[1]]] <= data[[counts[2]]], na.rm = TRUE)) {
      event_col <- counts[1]
      n_col <- counts[2]
    } else if (all(data[[counts[2]]] <= data[[counts[1]]], na.rm = TRUE)) {
      event_col <- counts[2]
      n_col <- counts[1]
    } else {
      stop("Specified count columns don't satisfy event <= n relationship")
    }
    
    cli_h1("Epidemiological Data Identification (Manual Mode)")
    cli_alert_success(paste(
      "Using specified columns: {.field {event_col}} as cases/events",
      "with {.field {n_col}} as population"
    ))
    
    # Handle groups in manual mode
    group_vars <- NULL
    if (!is.null(groups)) {
      if (!all(groups %in% names(data))) {
        stop("Specified group columns not found in data: ", 
             paste(setdiff(groups, names(data)), collapse = ", "))
      }
      group_vars <- groups
    } else {
      # Auto-detect grouping variables
      potential_groups <- names(data)[sapply(data, function(x) is.factor(x) | is.character(x))]
      potential_groups <- setdiff(potential_groups, c(event_col, n_col))
      
      if (length(potential_groups) > 0) {
        cli_h2("Available Grouping Variables")
        cli_ol(potential_groups)
        selection <- readline(prompt = "Enter numbers of groups to include (space-separated, 0 for none): ")
        selected_indices <- as.numeric(strsplit(selection, " ")[[1]])
        
        if (!all(selected_indices %in% seq_along(potential_groups))) {
          stop("Invalid group selection - please enter numbers corresponding to the list")
        }
        
        if (!0 %in% selected_indices) {
          group_vars <- potential_groups[selected_indices]
        }
      }
    }
    
    result <- tibble(
      event = data[[event_col]],
      n = data[[n_col]]
    )
    
    if (length(group_vars) > 0) {
      cli_alert_success(paste("Including groups:", paste(group_vars, collapse = ", ")))
      result <- bind_cols(result, select(data, all_of(group_vars)))
    }
    
    return(result)
  }
  
  # AUTOMATIC MODE WITH INTERACTIVE SELECTION
  
  # Find valid count pairs
  int_cols <- names(data)[sapply(data, function(x) is.numeric(x) && all(x %% 1 == 0, na.rm = TRUE))]
  if (length(int_cols) < 2) stop("Need at least two integer columns")
  
  find_valid_pairs <- function(cols) {
    pairs <- expand.grid(event = cols, n = cols, stringsAsFactors = FALSE) %>%
      filter(event != n) %>%
      rowwise() %>%
      mutate(
        valid = all(data[[event]] <= data[[n]], na.rm = TRUE)
      ) %>%
      ungroup() %>%
      filter(valid) %>%
      mutate(
        prefix_match = ifelse(
          sub("_.*", "", event) == sub("_.*", "", n), 
          "✅", "❌")
      )
  }
  
  valid_pairs <- find_valid_pairs(int_cols)
  
  if (nrow(valid_pairs) == 0) stop("No valid event/n pairs found where all(event <= n)")
  
  cli_h1("Epidemiological Data Identification")
  
  # Interactive count pair selection
  if (nrow(valid_pairs) > 1) {
    cli_h2("Multiple Valid Pairs Found")
    cli_ol()
    for (i in 1:nrow(valid_pairs)) {
      pair <- valid_pairs[i, ]
      cli_li(paste0(
        "[", i, "] ", pair$prefix_match, " ",
        "{.field ", pair$event, "} → {.field ", pair$n, "}"
      ))
    }
    cli_end()
    
    selection <- readline(prompt = "Enter the number of your preferred pair: ")
    selection <- as.numeric(selection)
    
    if (is.na(selection) || !selection %in% seq_len(nrow(valid_pairs))) {
      stop("Invalid selection - please enter a number from the list")
    }
    
    best_pair <- valid_pairs[selection, ]
    cli_alert_success(paste(
      "Selected: {.field ", best_pair$event, "} as cases/events",
      "with {.field ", best_pair$n, "} as population"
    ))
  } else {
    best_pair <- valid_pairs[1, ]
    cli_h2("Selected Pair")
    cli_alert_success(paste(
      "Column {.field ", best_pair$event, "} as cases/events",
      "with column {.field ", best_pair$n, "} as population"
    ))
  }
  
  # Interactive group selection
  group_vars <- NULL
  if (is.null(groups)) {
    potential_groups <- names(data)[sapply(data, function(x) is.factor(x) | is.character(x))]
    potential_groups <- setdiff(potential_groups, c(best_pair$event, best_pair$n))
    
    if (length(potential_groups) > 0) {
      cli_h2("Available Grouping Variables")
      cli_ol(potential_groups)
      selection <- readline(prompt = "Enter numbers of groups to include (space-separated, 0 for none): ")
      selected_indices <- as.numeric(strsplit(selection, " ")[[1]])
      
      if (length(selected_indices) == 1 && selected_indices == 0) {
        cli_alert_info("No grouping variables selected")
      } else if (!all(selected_indices %in% seq_along(potential_groups))) {
        stop("Invalid group selection - please enter numbers corresponding to the list")
      } else {
        group_vars <- potential_groups[selected_indices]
        cli_alert_success(paste("Selected groups:", paste(group_vars, collapse = ", ")))
      }
    }
  } else {
    # Use manually specified groups
    if (!all(groups %in% names(data))) {
      stop("Specified group columns not found in data: ", 
           paste(setdiff(groups, names(data)), collapse = ", "))
    }
    group_vars <- groups
  }
  
  # Create result with selected groups
  result <- tibble(
    event = data[[best_pair$event]],
    n = data[[best_pair$n]]
  )
  
  if (length(group_vars) > 0) {
    result <- bind_cols(result, select(data, all_of(group_vars)))
  }
  
  attr(result, "all_valid_pairs") <- valid_pairs
  attr(result, "selected_groups") <- group_vars
  invisible(result)
}