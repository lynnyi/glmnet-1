plotCoefs = function(model, nonzero = T, ...) {
    stopifnot(require(ggplot2))
    coefs = coef(model)[,1]
    coef.labels = attr(coefs, "names")

    if (model$name == "Binomial Deviance") coefs = exp(coefs)
    coefs.df = data.frame(label = coef.labels, value = coefs)
    coefs.df = coefs.df[order(coefs.df$value),]
    coefs.df$label = factor(coefs.df$label, levels = coefs.df$label)

    coefs.df = subset(coefs.df, label != "(Intercept)")
    if (nonzero) coefs.df = droplevels(subset(coefs.df, value != 0))

    ggplot(coefs.df,
           aes(x = label, y = value, fill = value > 0)) +
      geom_bar(stat='identity') +
      coord_flip() +
      theme_grey(base_size = 25) + 
      xlab("Variable") + ylab("Value")
}

plotFactorCoefs = function(model, variable, ...) {
    stopifnot(require(ggplot2))
    variable.ch = deparse(substitute(variable))
    coefs = coef(model)[,1]
    coef.labels = attr(coefs, "names")
    variable.indices = grep(paste(variable.ch, ".*", sep=""), coef.labels)

    base = paste(variable.ch, levels(eval(substitute(variable), model$data))[1], sep="")
    labels = c(base, coef.labels[variable.indices])
    values = c(0, coefs[variable.indices])
    if (model$name == "Binomial Deviance") values = exp(values)
    coefs.df = data.frame(label = labels, value = values)
    coefs.df = coefs.df[order(coefs.df$value),]
    coefs.df$label = factor(coefs.df$label, levels = coefs.df$label)

    ggplot(coefs.df,
           aes(x = label, y = value, fill = value > 0)) +
      geom_bar(stat='identity') +
      coord_flip() +
      theme_grey(base_size = 25) + 
      xlab(variable.ch) + ylab("Value") + labs(title = variable.ch)
}

