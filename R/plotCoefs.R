plotCoefs = function(model, nonzero = T, subset.condition, ...) {
    stopifnot(require(ggplot2))
    coefs = coef(model)[,1]
    coef.labels = attr(coefs, "names")

    isLogistic = model$name == "Binomial Deviance"
    if (isLogistic) coefs = exp(coefs)
    coefs.df = data.frame(label = coef.labels, value = coefs)
    coefs.df = coefs.df[order(coefs.df$value),]
    coefs.df$label = factor(coefs.df$label, levels = coefs.df$label)

    coefs.df = subset(coefs.df, label != "(Intercept)")
    if (nonzero) coefs.df = droplevels(subset(coefs.df, value != 0))
    if (!missing(subset.condition)) coefs.df = droplevels(subset(coefs.df, eval(subset.condition, coefs.df)))

    if (isLogistic) {
      coefs.df$value_bin = ">1"
      coefs.df$value_bin[which(coefs.df$value == 1)] = "=1"
      coefs.df$value_bin[which(coefs.df$value < 1)] = "<1"
      coefs.df$value_bin = factor(coefs.df$value_bin, levels = c("<1", "=1", ">1"))
    } else {
      coefs.df$value_bin = ">0"
      coefs.df$value_bin[which(coefs.df$value == 0)] = "=0"
      coefs.df$value_bin[which(coefs.df$value < 0)] = "<0"
      coefs.df$value_bin = factor(coefs.df$value_bin, levels = c("<0", "=0", ">0"))
    }

    p = ggplot(coefs.df,aes(x = label, y = value, fill = value_bin))
    p +
      geom_bar(stat='identity') +
      coord_flip() +
      theme_grey(base_size = 25) + 
      xlab("Variable") + ylab(ifelse(isLogistic, "Odds Ratio Multiplier", "Value"))
}

plotFactorCoefs = function(model, variable, ...) {
    stopifnot(require(ggplot2))
    variable.ch = deparse(substitute(variable))
    coefs = coef(model)[,1]
    coef.labels = attr(coefs, "names")
    variable.levels = levels(eval(substitute(variable), model$data))
    #variable.indices = grep(paste("^",variable.ch, ".*", sep=""), coef.labels)
    variable.indices = which(coef.labels %in% paste(variable.ch, variable.levels, sep=""))

    base = paste(variable.ch, variable.levels[1], sep="")
    labels = c(coef.labels[variable.indices], base)
    values = c(coefs[variable.indices], 0)
    isLogistic = model$name == "Binomial Deviance"
    if (isLogistic) values = exp(values)
    coefs.df = data.frame(label = labels, value = values)
    n = nrow(coefs.df)
    coefs.df[1:(n-1),] = coefs.df[1:(n-1),][order(coefs.df$value[1:(n-1)], coefs.df$label[1:(n-1)]),]
    coefs.df$label = factor(coefs.df$label, levels = coefs.df$label)

    if (isLogistic) {
      coefs.df$value_bin = ">1"
      coefs.df$value_bin[which(coefs.df$value == 1)] = "=1"
      coefs.df$value_bin[which(coefs.df$value < 1)] = "<1"
      coefs.df$value_bin = factor(coefs.df$value_bin, levels = c("<1", "=1", ">1"))
    } else {
      coefs.df$value_bin = ">0"
      coefs.df$value_bin[which(coefs.df$value == 0)] = "=0"
      coefs.df$value_bin[which(coefs.df$value < 0)] = "<0"
      coefs.df$value_bin = factor(coefs.df$value_bin, levels = c("<0", "=0", ">0"))
    }

    p = ggplot(coefs.df,aes(x = label, y = value, fill = value_bin))
    p +
      geom_bar(stat='identity') +
      coord_flip() +
      theme_grey(base_size = 25) + 
      xlab(variable.ch) + ylab(ifelse(isLogistic, "Odds Ratio Multiplier", "Value")) + labs(title = variable.ch)
}

