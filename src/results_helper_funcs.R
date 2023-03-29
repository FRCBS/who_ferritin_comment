# Helpoer functions for ferritin_threshold_results.qmd

find_saddle <- function(fit, start = 0, stop = 50, interval_length = 10, max = TRUE) {


    # init values for while loop
    end <- start + interval_length
    i <- -1
    optim <- 9999
    while ((((end + i) - optim[[1]]) < 0.001) && i <= 40) {
        i <- i + 1
        optim <- optimize(Function(fit), interval = c(start + i, end + i), maximum = max)
    }

    if (i == 41) {
        # No saddle found between (start, stop)
        return(list(saddle.exists = F,
                    x = 0,
                    y = 0))
    } else {
        x <- optim[[1]]
        y <- optim[[2]]
        return(list(saddle.exists = T,
                    x = x,
                    y = y))
    }
}

boot_saddle <- function(data, n_knot, i) {
    sample <- data[i, ]
    resampled_fit <- ols(
        Hb ~ rcs(Ferritin, n_knot),
        data = sample,
        x = TRUE,
        y = TRUE
    )
    resampled_saddle <- find_saddle(resampled_fit, max = T)
    return(resampled_saddle$x)
}


plot_rcs <- function(data, fit, n_knot, zoom = T) {

    pred <- Predict(fit)

    # Check whether we are working with Hb or sTfR
    # We need this to know if we are maximising or minimizing
    # when searching for saddle points!
    response_var_name <- attributes(pred)$info$ylab

    if (zoom == TRUE) {
        if (response_var_name == "Hb") {
            ylims <- c(min(pred$yhat) - 10, max(pred$yhat) + 10)
        } else {
            ylims <- c(min(pred$yhat) - 1, max(pred$yhat) + 2)
        }
    } else {
        if (response_var_name == "Hb") {
            ylims <- c(min(data$Hb), max(data$Hb))
        } else {
            ylims <- c(min(data$sTfR), max(data$sTfR))
        }

    }

    # Test if saddle point exists
    start = 0
    stop = 50
    out <- find_saddle(fit, start = start, stop = stop, max = (response_var_name == "Hb"))
    saddle.exists <- out$saddle.exists
    inflection_x <- out$x
    inflection_y <- out$y

    if (saddle.exists) {
        subtitle <-
            paste0("Saddle point at: ", round(inflection_x, 2), " μg/L")
    } else {
        subtitle <- paste0("No saddle point between ", start, "-", stop, " μg/L.")
    }

    ggplot(data = data, aes(x = Ferritin, y = Hb)) +
        geom_point(alpha = 0.1) +
        geom_line(data = pred,
                  aes(x = Ferritin, y = yhat),
                  size = 0.6) +
        geom_ribbon(
            data = pred,
            aes(x = Ferritin, ymin = lower, ymax = upper),
            alpha = 0.2,
            inherit.aes = FALSE
        ) +
        geom_segment(
            x = inflection_x - 10,
            xend = inflection_x + 10,
            y = inflection_y,
            yend = inflection_y,
            size = 0.4,
            alpha = saddle.exists
        ) +
        geom_segment(
            x = inflection_x,
            xend = inflection_x,
            y = min(data$Hb),
            yend = inflection_y + 30,
            linetype = "dashed",
            size = 0.3,
            alpha = saddle.exists
        ) +
        scale_x_continuous(breaks = seq(0, 150, 25),
                           labels = seq(0, 150, 25)) +
        xlim(c(0, 150)) +
        ylim(ylims) +
        theme_minimal() +
        labs(title = paste0("Knots: ", n_knot, ifelse(zoom, " (zoomed)", "")),
             subtitle = subtitle)
}


plot_rcs_temp <- function(data, fit, n_knot, zoom = T) {

    pred <- Predict(fit)

    # Check whether we are working with Hb or sTfR
    # We need this to know if we are maximising or minimizing
    # when searching for saddle points!
    response_var_name <- attributes(pred)$info$ylab

    if (zoom == TRUE) {
        if (response_var_name == "hemoglobin") {
            ylims <- c(min(pred$yhat) - 10, max(pred$yhat) + 10)
        } else {
            ylims <- c(min(pred$yhat) - 1, max(pred$yhat) + 2)
        }
    } else {
        if (response_var_name == "hemoglobin") {
            ylims <- c(min(data$hemoglobin), max(data$hemoglobin))
        } else {
            ylims <- c(min(data$sTfR), max(data$sTfR))
        }

    }

    # Test if saddle point exists
    start = 0
    stop = 50
    out <- find_saddle(fit, start = start, stop = stop, max = (response_var_name == "hemoglobin"))
    saddle.exists <- out$saddle.exists
    inflection_x <- out$x
    inflection_y <- out$y

    if (saddle.exists) {
        subtitle <-
            paste0("Saddle point at: ", round(inflection_x, 2), " μg/L")
    } else {
        subtitle <- paste0("No saddle point between ", start, "-", stop, " μg/L.")
    }

    ggplot(data = data, aes(x = ferritin, y = hemoglobin)) +
        geom_point(alpha = 0.1) +
        geom_line(data = pred,
                  aes(x = ferritin, y = yhat),
                  size = 0.6) +
        geom_ribbon(
            data = pred,
            aes(x = ferritin, ymin = lower, ymax = upper),
            alpha = 0.2,
            inherit.aes = FALSE
        ) +
        geom_segment(
            x = inflection_x - 10,
            xend = inflection_x + 10,
            y = inflection_y,
            yend = inflection_y,
            size = 0.4,
            alpha = saddle.exists
        ) +
        geom_segment(
            x = inflection_x,
            xend = inflection_x,
            y = min(data$hemoglobin),
            yend = inflection_y + 30,
            linetype = "dashed",
            size = 0.3,
            alpha = saddle.exists
        ) +
        scale_x_continuous(breaks = seq(0, 150, 25),
                           labels = seq(0, 150, 25)) +
        xlim(c(0, 150)) +
        ylim(ylims) +
        theme_minimal() +
        labs(title = paste0("Knots: ", n_knot, ifelse(zoom, " (zoomed)", "")),
             subtitle = subtitle)
}
