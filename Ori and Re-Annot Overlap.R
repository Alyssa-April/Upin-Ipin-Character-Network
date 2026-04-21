# install libraries
library(readxl)
library(dplyr)

#__________________________________________________________________________________

# read in the Excel sheets (original and new annotations for the series and film respectively)
file_path <- "E:/PhD Year 1/Upin Ipin/BMMSS Data, Code, Figures/BMMSS Code/Re-Annotation.xlsx"

orig_series <- read_excel(file_path, sheet = "Original Series")
new_series  <- read_excel(file_path, sheet = "New Annot Series")

orig_film <- read_excel(file_path, sheet = "Original Film")
new_film  <- read_excel(file_path, sheet = "New Annot Film")

#___________________________________________________________________________________

# function to compute metrics (Jaccard edge overlap, spearman agreement between weights and weighted Jaccard overlap)
compute_metrics <- function(orig, new) {
  
  # keep only edges with Sum > 0 (the edge exists)
  orig <- orig %>% filter(Sum > 0)
  new  <- new %>% filter(Sum > 0)
  
  # create Edge column to show directed edges
  orig <- orig %>% mutate(Edge = paste(From, To, sep = "->"))
  new  <- new  %>% mutate(Edge = paste(From, To, sep = "->"))
  
  # ____________________________
  # Jaccard overlap shows edge existence in the original and new annotations 
  # ____________________________
  intersection <- length(intersect(orig$Edge, new$Edge))
  union <- length(union(orig$Edge, new$Edge))
  jaccard <- intersection / union
  
  # ____________________________
  # Weight agreement between common edges using Spearman
  # ____________________________
  common_edges <- intersect(orig$Edge, new$Edge)
  
  # keep only edges that exist in both the original and new annotations
  # So, weight similarty is only between common edges
  orig_common <- orig %>% filter(Edge %in% common_edges)
  new_common  <- new  %>% filter(Edge %in% common_edges)
  
  # align order by making both original and new tables have the same order of edges
  orig_common <- orig_common[match(common_edges, orig_common$Edge), ]
  new_common  <- new_common[match(common_edges, new_common$Edge), ]
  
  # get the spearman correlation between weights in the original and new annotations
  weight_corr <- cor(orig_common$Sum, new_common$Sum, method = "spearman")
  
  # ____________________________
  # obtain the edges that have different weights
  # ____________________________
  
  # create comparison table
  comparison <- data.frame(
    Edge = common_edges,
    Orig_Weight = orig_common$Sum,
    New_Weight  = new_common$Sum
  )
  
  # find edges where weights differ
  weight_diff <- comparison %>%
    filter(Orig_Weight != New_Weight)
  
  weight_diff <- weight_diff %>%
    mutate(Diff = abs(Orig_Weight - New_Weight))
  
  weight_diff
  
  # percentage of overlapping edges with changed weights
  print("Changed Weights")
  print(nrow(weight_diff) / nrow(comparison))
  
  # ____________________________
  # Weighted Jaccard
  # ____________________________
  all_edges <- union(orig$Edge, new$Edge)
  
  orig_all <- data.frame(Edge = all_edges) %>%
    left_join(orig[, c("Edge", "Sum")], by = "Edge") %>%
    mutate(Sum = ifelse(is.na(Sum), 0, Sum))
  
  new_all <- data.frame(Edge = all_edges) %>%
    left_join(new[, c("Edge", "Sum")], by = "Edge") %>%
    mutate(Sum = ifelse(is.na(Sum), 0, Sum))
  
  weighted_jaccard <- sum(pmin(orig_all$Sum, new_all$Sum)) /
    sum(pmax(orig_all$Sum, new_all$Sum))
  
  # return results
  list(
    jaccard = jaccard,
    spearman_weight = weight_corr,
    weighted_jaccard = weighted_jaccard
  )
}

# ______________________________________________________________________________
# run the function
series_results <- compute_metrics(orig_series, new_series)
film_results   <- compute_metrics(orig_film, new_film)

series_results
film_results

