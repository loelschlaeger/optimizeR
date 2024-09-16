#' Parameter Spaces
#'
#' @description
#' TODO
#'
#' @examples
#' # TODO
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
    #' @param show_transformer \[`logical(1)`\]\cr
    #' Show transformer functions in the output?

    print = function(show_transformer = FALSE) {

      ### input checks
      oeli::input_check_response(
        check = checkmate::check_flag(show_transformer),
        var_name = "show_transformer"
      )

      ### optimization space
      cli::cli_h1("Optimization Space {.cls numeric({sum(private$.parameter_lengths_in_o_space)})}")
      print.data.frame(
        structure(
          data.frame(private$.get_ranges(), private$.parameter_names),
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
      cli::cli_h1("Interpretation Space {.cls list({length(private$.parameter_names)})}")
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
      return(self)
    },

    #' @description
    #' Switch between optimization and interpretation space.
    #'
    #' @param x \[`numeric()` | `list()`\]\cr
    #' The parameters, either as `numeric()` (will be switched to interpretation
    #' space), or as `list()` (will be switched to optimization space).

    switch = function(x) {

      if (oeli::test_numeric_vector(x)) {

        ### transform to Interpretation Space
        # TODO

      } else if (checkmate::test_list(x)) {

        ### transform to Optimization Space
        out <- numeric(sum(private$.parameter_lengths_in_o_space))
        i2o_parts <- list()
        for (i in seq_along(private$.parameter_names)) {
          par_name <- private$.parameter_names[i]
          part <- private$.i2o[[par_name]](x[[par_name]])
          if (!oeli::test_numeric_vector(
            part, len = private$.parameter_lengths_in_o_space[i], any.missing = FALSE
          )) {
            stop()
          }
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
        check = checkmate::check_names(
          names(input), subset.of = private$.parameter_names
        )
      )
      private$.o2i <- oeli::merge_lists(input, private$.o2i)[private$.parameter_names]
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
        check = checkmate::check_names(
          names(input), subset.of = private$.parameter_names
        )
      )
      private$.i2o <- oeli::merge_lists(input, private$.i2o)[private$.parameter_names]
    }

  ),

  private = list(

    .parameter_names = character(),
    .parameter_lengths_in_o_space = 0,
    .o2i = list(),
    .i2o = list(),

    .get_ranges = function(
      lengths = private$.parameter_lengths_in_o_space
    ) {
      sapply(seq_along(lengths), function(i) {
        if (lengths[i] == 0) "[NULL]" else {
          start <- if (i == 1) 1 else sum(lengths[1:(i - 1)]) + 1
          end <- start - 1 + lengths[i]
          sprintf("[%d:%d]", start, end)
        }
      })
    }

  )

)
