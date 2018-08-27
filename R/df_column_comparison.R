


#' @export
df_col_comparison = function(object, examples = 3) {

  for (i in seq_along(object)) {

    object[[i]] = data_frame(column = names(object[[i]]),
                             class = sapply(object[[i]],
                                            function(x) {
                                              paste(class(x), collapse = ", ")
                                            }),
                             examples = sapply(object[[i]],
                                               function(x) {
                                                 paste(sample(x,
                                                              pmin(examples,
                                                                   length(x))),
                                                       collapse = ", ")
                                               }))
    names(object[[i]])[2] = paste0("class_df", i)
    names(object[[i]])[3] = paste0("examples_df", i)

    if (i > 1) {
      object[[1]] = full_join(object[[1]], object[[i]], "column")
    }

  }

  object[[1]][c("column",
                paste0("class_df", seq_along(object)),
                paste0("examples_df", seq_along(object)))]

}
