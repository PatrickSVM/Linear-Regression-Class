#' Linear Regression Reference Class
#'
#' Definition of the Reference Class linreg for the linear regression model
#' in Lab #4.
#'
#'
#' @field formula formula for building the model
#' @field data dataset that contains feature and target values
#' @field beta values of the model's coefficients
#' @field predictions values of the dependent variable predicted by the model
#' @field residuals values of the residuals
#' @field df degrees of freedom
#' @field res_var variance of the residuals
#' @field beta_var variance of the regression coefficients
#' @field t_value t-values for each coefficient
#' @field p_value p-values for each coefficient
#'
#'
#' @import ggplot2
#' @importFrom dplyr %>% summarise group_by
#' @import gridExtra
#'
#'
#' @export linreg
#' @exportClass linreg

linreg <- setRefClass(Class = "linreg",

                      fields = list(formula = "formula",
                                    data = "data.frame",
                                    beta = "matrix",
                                    predictions = "matrix",
                                    residuals = "matrix",
                                    df = "numeric",
                                    res_var = "matrix",
                                    beta_var = "matrix",
                                    t_value = "matrix",
                                    p_value = "matrix",
                                    name_dataset = "character",
                                    col_names = "character"),


                      methods = list(

                        initialize = function(formula, data){

                          if (!(inherits(formula, "formula") & is.data.frame(data))) {
                            stop("Wrong input!")
                          }

                          stopifnot("invalid input" = all(all.vars(formula) %in% colnames(data)))


                          X <- model.matrix(formula, data)
                          #convert y to matrix, otherwise the computations won't work.
                          y <- as.matrix(data[all.vars(formula)[1]])

                          .self$col_names <<- colnames(X)
                          .self$formula <<- formula
                          .self$data <<- data
                          .self$name_dataset <<- deparse(substitute(data))
                          .self$beta <<- solve((t(X) %*% X)) %*% t(X) %*% y
                          .self$predictions <<- X %*% beta
                          .self$residuals <<- y - predictions
                          .self$df <<- nrow(X) - ncol(X)
                          .self$res_var <<- (t(residuals) %*% residuals)/df
                          .self$beta_var <<- as.numeric(res_var) * solve(t(X) %*% X)
                          .self$t_value <<- beta/sqrt(diag(beta_var))
                          .self$p_value <<- 2 * pt(q=-abs(t_value), df=df)


                          return(.self)
                        },

                        # print function: object$print()
                        print = function(){

                          to_print <- sprintf("linreg(formula = %s, data = iris)", format(.self$formula))
                          coef_output <- beta
                          rownames(coef_output) <- .self$col_names

                          cat("Call:",
                              toString(to_print),
                              "\nCoefficients:\n")

                          ext_print(t(coef_output))
                        },


                        # show function is used to change what is shown when print(object) is called
                        show = function(){

                          to_print <- sprintf("linreg(formula = %s, data = iris)", format(.self$formula))
                          coef_output <- beta
                          rownames(coef_output) <- .self$col_names

                          cat("Call:",
                              toString(to_print),
                              "\nCoefficients:\n")

                          ext_print(t(coef_output))
                        },


                        resid = function(){
                          return(.self$residuals)
                        },


                        pred = function(){
                          return(.self$predictions)
                        },


                        coef = function(){
                          return(.self$beta)
                        },


                        summary = function(){

                          coef_df <- data.frame(.self$beta, sqrt(diag(beta_var)), .self$t_value, .self$p_value, unlist(lapply(p_value, FUN = significance)))
                          colnames(coef_df) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", " ")

                          ext_print(coef_df)
                          ext_print("")
                          ext_print(sprintf("Residual standard error: %f on %i degrees of freedom", sqrt(.self$res_var), .self$df))

                        }
                      )
                    )


