#' Train and then test a logistic regression model
#' @export
train = function(formula, data, prop = 0.7) {
  samp = sample(c(TRUE, FALSE), size = nrow(data), replace = TRUE,
                prob = c(1 - prop, prop))
  testing = data[samp, ]
  training = data[!samp, ]
  model = glm(formula, data = training, family = binomial(link = "logit"))
  predicted = tibble(phat = predict(model, testing, type = "response"),
                     response = testing[[as.character((formula)[[2]])]])
  return(predicted)
}
