#' Switch Between Parameter Spaces
#'
#' @description
#' This R6 object manages two related parameter spaces: the Optimization Space
#' (for optimization) and the Interpretation Space (for easier interpretation).
#'
#' In the Optimization Space, parameters are stored as a \code{numeric}
#' \code{vector}, the standard format for numerical optimizers. Parameters in
#' this space are typically identified.
#'
#' In the Interpretation Space, parameters are stored as a \code{list} and can
#' take different formats (e.g., \code{matrix}). Parameters here do not need to
#' be identified.
#'
#' The user can define transformation functions (not necessarily bijective) to
#' switch between these two spaces via the \code{$o2i()} and \code{$i2o()}
#' methods.
#'
#' @examples
#' ### Log-likelihood function of two-class Gaussian mixture model with
#' ### parameter vector `theta` that consists of
#' ### - `mu`, mean vector of length 2
#' ### - `sd`, standard deviation vector of length 2, must be positive
#' ### - `lambda`, class probability of length 1, must be between 0 and 1
#'
#' normal_mixture_llk <- function(theta, data) {
#'   mu <- theta[1:2]
#'   sd <- exp(theta[3:4])
#'   lambda <- plogis(theta[5])
#'   c1 <- lambda * dnorm(data, mu[1], sd[1])
#'   c2 <- (1 - lambda) * dnorm(data, mu[2], sd[2])
#'   sum(log(c1 + c2))
#' }
#'
#' ### define parameter spaces
#' ### - `mu` needs no transformation
#' ### - `sd` needs to be real in optimization space and positive in
#' ###    interpretation space
#' ### - `lambda` needs to be real and of length `1` in optimization space, and
#' ###    a probability vector of length `2` in interpretation space
#'
#' normal_mixture_spaces <- ParameterSpaces$
#'   new(
#'     parameter_names = c("mu", "sd", "lambda"),
#'     parameter_lengths_in_o_space = c(2, 2, 1)
#'   )$
#'   o2i(
#'     "mu" = function(x) x,
#'     "sd" = function(x) exp(x),
#'     "lambda" = function(x) c(plogis(x), 1 - plogis(x))
#'   )$
#'   i2o(
#'     "mu" = function(x) x,
#'     "sd" = function(x) log(x),
#'     "lambda" = function(x) qlogis(x[1])
#'   )
#'
#' ### switch between parameter spaces
#'
#' par <- list(                             # parameters in interpretation space
#'   "mu" = c(2, 4),
#'   "sd" = c(0.5, 1),
#'   "lambda" = c(0.4, 0.6)
#' )
#' (x <- normal_mixture_spaces$switch(par)) # switch to optimization space
#' normal_mixture_llk(
#'   theta = x, data = datasets::faithful$eruptions
#' )
#' normal_mixture_spaces$switch(x)          # switch back
#'
#' @export

ParameterSpaces <- R6::R6Class(

  classname = "ParameterSpaces",

  public = list(

    #' @description
    #' Initializes a new \code{ParameterSpaces} object.
    #'
    #' @param parameter_names \[`character()`\]\cr
    #' Unique names for the parameters.
    #'
    #' @param parameter_lengths_in_o_space \[`integer()`\]\cr
    #' The length of each parameter in the optimization space.
    #'
    #' @return
    #' A new \code{ParameterSpaces} object.

    initialize = function(
      parameter_names, parameter_lengths_in_o_space
    ) {

      ### input checks
      oeli::input_check_response(
        check = checkmate::check_names(parameter_names, type = "strict"),
        var_name = "parameter_names"
      )
      oeli::input_check_response(
        check = checkmate::check_integerish(
          parameter_lengths_in_o_space, lower = 0,
          len = length(parameter_names), any.missing = FALSE
        ),
        var_name = "parameter_lengths_in_o_space"
      )

      ### set internal parameters
      private$.parameter_names <- p <- parameter_names
      private$.parameter_lengths_in_o_space <- n <- parameter_lengths_in_o_space
      private$.o2i <- {
        o2i <- replicate(length(n), function(x) x)
        names(o2i) <- p
        o2i
      }
      private$.i2o <- {
        i2o <- replicate(length(n), function(x) as.vector(x))
        names(i2o) <- p
        i2o
      }

    },

    #' @description
    #' Print an overview of the parameter spaces.
    #'
    #' @param show_transformer \[`logical(1)`\]\cr
    #' Show transformer functions in the output?

    print = function(show_transformer = FALSE) {

      ### input checks
      oeli::input_check_response(
        check = checkmate::check_flag(show_transformer),
        var_name = "show_transformer"
      )

      ### optimization space
      cli::cli_h1("Optimization Space {.cls numeric({private$.o_space_length()})}")
      print.data.frame(
        structure(
          data.frame(private$.format_ranges(), private$.parameter_names),
          names = NULL
        ),
        row.names = FALSE, right = FALSE
      )
      if (show_transformer) {
        cli::cli_h3("Transformation to Interpretation Space:")
        print.data.frame(
          structure(
            data.frame(
              private$.parameter_names,
              sapply(private$.o2i, oeli::function_body)
            ),
            names = NULL
          ),
          row.names = FALSE, right = FALSE
        )
      }

      ### interpretation space
      cli::cli_h1("Interpretation Space {.cls list({private$.number_parameters()})}")
      print.data.frame(
        structure(
          data.frame(
            sapply(seq_along(private$.parameter_names), function(i) sprintf("[[%d]]", i)),
            private$.parameter_names),
          names = NULL
        ),
        row.names = FALSE, right = FALSE
      )
      if (show_transformer) {
        cli::cli_h3("Transformation to Optimization Space:")
        print.data.frame(
          structure(
            data.frame(
              private$.parameter_names,
              sapply(private$.i2o, oeli::function_body)
            ),
            names = NULL
          ),
          row.names = FALSE, right = FALSE
        )
      }

      cat("\n")
      invisible(self)
    },

    #' @description
    #' Switch between Optimization Space and Interpretation Space.
    #'
    #' @param x \[`numeric()` | `list()`\]\cr
    #' The parameters, either as a `numeric vector` (will be switched to
    #' Interpretation Space), or as a `list()` (will be switched to Optimization
    #' Space).
    #'
    #' @param to \[`character(1)` | `NULL`\]\cr
    #' Explicitly switch to a specific space, either
    #'
    #' - `"o"`: Optimization Space
    #' - `"i"`: Interpretation Space
    #'
    #' If `NULL`, the function will switch to the other space.

    switch = function(x, to = NULL) {

      oeli::input_check_response(
        check = checkmate::check_choice(to, c("o", "i"), null.ok = TRUE),
        var_name = "to"
      )

      if (oeli::test_numeric_vector(x)) {

        ### input check
        oeli::input_check_response(
          check = oeli::check_numeric_vector(
            x, any.missing = FALSE, len = private$.o_space_length()
          ),
          var_name = "x"
        )

        ### stay in Optimization Space?
        if (checkmate::test_choice(to, c("o"), null.ok = FALSE)) {
          return(x)
        }

        ### transform to Interpretation Space
        out <-  setNames(vector("list", private$.number_parameters()), private$.parameter_names)
        ranges <- private$.get_ranges()
        x_split <- lapply(ranges, function(start_end) {
          if (is.null(start_end)) {
            NULL
          } else {
            x[start_end[1]:start_end[2]]
          }
        })
        names(x_split) <- private$.parameter_names
        for (parameter in names(out)) {
          out[[parameter]] <- private$.o2i[[parameter]](x_split[[parameter]])
        }
        return(out)

      } else if (checkmate::test_list(x)) {

        ### input check
        oeli::input_check_response(
          check = checkmate::check_names(
            names(x), permutation.of = private$.parameter_names
          ),
          var_name = "x"
        )

        ### stay in Interpretation Space?
        if (checkmate::test_choice(to, c("i"), null.ok = FALSE)) {
          return(x)
        }

        ### transform to Optimization Space
        out <- numeric(private$.o_space_length())
        i2o_parts <- list()
        for (i in seq_along(private$.parameter_names)) {
          par_name <- private$.parameter_names[i]
          part <- private$.i2o[[par_name]](x[[par_name]])
          oeli::input_check_response(
            check = oeli::check_numeric_vector(
              part, len = private$.parameter_lengths_in_o_space[i],
              any.missing = FALSE, null.ok = TRUE
            ),
            var_name = paste0("x[[\"", par_name, "\"]]"),
            prefix = "Transforming {.var {var_name}} is bad:"
          )
          i2o_parts[[i]] <- part
        }
        return(unlist(i2o_parts))

      } else {

        ### bad input type
        cli::cli_abort(
          "Input {.code x} must be a {.cls numeric vector} or a {.cls list},
          but is of class {.cls {class(x)}}.",
          call = NULL
        )
      }

    },

    #' @description
    #' Define transformation functions when switching from Optimization Space to
    #' Interpretation Space.
    #'
    #' @param ... \[`function`\]\cr
    #' One or more transformation functions, named according to the parameters.
    #'
    #' Transformers from Optimization Space to Interpretation Space (o2i)
    #' **must receive** a `numeric`. The default is the identity.

    o2i = function(...) {
      input <- list(...)
      oeli::input_check_response(
        check = checkmate::check_list(input, types = "function")
      )
      oeli::input_check_response(
        check = checkmate::check_names(
          names(input), subset.of = private$.parameter_names
        )
      )
      private$.o2i <- oeli::merge_lists(input, private$.o2i)[private$.parameter_names]
      invisible(self)
    },

    #' @description
    #' Define transformation functions when switching from Interpretation Space
    #' to Optimization Space.
    #'
    #' @param ... \[`function`\]\cr
    #' One or more transformers functions, named according to the parameters.
    #'
    #' Transformers from Interpretation Space to Optimization Space (i2o)
    #' **must return** a `numeric`. The default is `as.vector()`.

    i2o = function(...) {
      input <- list(...)
      oeli::input_check_response(
        check = checkmate::check_list(input, types = "function")
      )
      oeli::input_check_response(
        check = checkmate::check_names(
          names(input), subset.of = private$.parameter_names
        )
      )
      private$.i2o <- oeli::merge_lists(input, private$.i2o)[private$.parameter_names]
      invisible(self)
    }

  ),

  private = list(

    .parameter_names = character(),
    .parameter_lengths_in_o_space = 0,
    .o2i = list(),
    .i2o = list(),

    .number_parameters = function(names = private$.parameter_names) {
      length(names)
    },

    .o_space_length = function(lengths = private$.parameter_lengths_in_o_space) {
      sum(lengths)
    },

    .get_ranges = function(lengths = private$.parameter_lengths_in_o_space) {
      lapply(seq_along(lengths), function(i) {
        if (lengths[i] == 0) NULL else {
          start <- if (i == 1) 1 else sum(lengths[1:(i - 1)]) + 1
          end <- start - 1 + lengths[i]
          c(start, end)
        }
      })
    },

    .format_ranges = function(ranges = private$.get_ranges()) {
      sapply(ranges, function(x) {
        if (is.null(x)) "[NULL]" else sprintf("[%d:%d]", x[1], x[2])
      })
    }

  )

)
