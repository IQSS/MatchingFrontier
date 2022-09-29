checkArgs <- function(QOI, metric, ratio, outcome = NULL, ...){
    if (!is.null(outcome)) {
        customWarning("the 'outcome' argument is defunct and will be ignored. See ?estimateEffects.", 'makeFrontier()')
    }

    if (length(QOI) != 1 || !is.character(QOI)) {
        customStop("'QOI' must be a string.")
    }
    QOI <- try(match_arg(toupper(QOI), c("SATE", "FSATE", "SATT", "FSATT")), silent = TRUE)
    if (inherits(QOI, "try-error")){
        customStop("'QOI' must be either 'SATE', 'FSATE', 'SATT', or 'FSATT'.", 'makeFrontier()')
    }

    acceptable_dist <- c("FSATE", "FSATT")
    acceptable_bin <- c("FSATE", "SATT")
    acceptable_energy <- c("SATE", "FSATE", "SATT")

    acceptable_combinations <- list(
        dist = acceptable_dist,
        l1 = acceptable_bin,
        l2 = acceptable_bin,
        energy = acceptable_energy
    )

    bad_combination <- !QOI %in% acceptable_combinations[[metric]]

    if (bad_combination) {
        msg <- sprintf('%s frontiers are only presently calculable for the %s.',
                      switch(metric, "dist" = "pair distance-based",
                             "energy" = "energy distance-based", "bin-based"),
                             word_list(acceptable_combinations[[metric]]))
        customStop(msg, 'makeFrontier()')
    }

    if (!is.null(ratio)) {
      if (metric == "dist") {
        if (!is.numeric(ratio) || length(ratio) != 1 || is.na(ratio) || ratio <= 0 ||
            abs(round(ratio) - ratio) > .Machine$double.eps) {
          customStop("'ratio' must be a single positive integer when used with pair distance-based frontiers.",
                     "makeFrontier()")
        }
      }
      else {
        if (!QOI %in% c("FSATE", "SATE")) {
          customWarning(sprintf("'ratio' is ignored for %s frontiers when QOI = '%s'.",
                                switch(metric, "dist" = "pair distance-based",
                                       "energy" = "energy distance-based", "bin-based"),
                                QOI),
                        "makeFrontier()", immediate = TRUE)
        }
        if (!is.numeric(ratio) || length(ratio) != 1 || is.na(ratio) || ratio <= 0) {
          customStop("'ratio' must be a single positive number.", "makeFrontier()")
        }
      }
    }

}
