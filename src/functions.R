# Helper

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

plot_rcs <- function(data, fit, n_knot, zoom = T, FIGUREPATH = FIGUREPATH) {

    pred <- Predict(fit)

    # Check whether we are working with Hb or sTfR
    # We need this to know if we are maximising or minimizing
    # when searching for saddle points!
    response_var_name <- attributes(pred)$info$ylab

    if (zoom == TRUE) {
        if (response_var_name == "Hemoglobin") {
            ylims <- c(min(pred$yhat) - 1, max(pred$yhat) + 2)
        } else {
            ylims <- c(min(pred$yhat) - 1, max(pred$yhat) + 2)
        }
    } else {
        if (response_var_name == "Hemoglobin") {
            ylims <- c(min(data$Hemoglobin), max(data$Hemoglobin))
        } else {
            ylims <- c(min(data$sTfR), max(data$sTfR))
        }

    }

    # Test if saddle point exists
    start = 0
    stop = 50
    out <- find_saddle(fit, start = start, stop = stop, max = (response_var_name == "Hemoglobin"))
    saddle.exists <- out$saddle.exists
    inflection_x <- out$x
    inflection_y <- out$y

    if (saddle.exists) {
        subtitle <-
            paste0("Saddle point at: ", round(inflection_x, 2), " μg/L")
    } else {
        subtitle <- paste0("No saddle point between ", start, "-", stop, " μg/L.")
    }

    if (response_var_name == "Hemoglobin") {
        p <- ggplot(data = data, aes(x = Ferritin, y = Hemoglobin)) +
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
                y = min(data$Hemoglobin),
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
    } else {
        p <- ggplot(data = data, aes(x = Ferritin, y = sTfR)) +
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
                y = min(data$sTfR),
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
    if (!is.null(FIGUREPATH)) {
        # Save plot to a PDF file
        ggsave(paste0(FIGUREPATH, response_var_name, "_rcs_knot_", n_knot, ".pdf"), p, width = 8, height = 6, device = cairo_pdf)
    }
    return(p)

}

investigate_RCS <- function(data, response, knots, zoom = F, FIGUREPATH = NULL) {
    fits <- vector("list", length = length(knots))
    for (i in 1:length(knots)) {
        fits[[i]] <- ols(as.formula(sprintf("%s ~ rcs(Ferritin, %s)", response, knots[i])),
                       data = data,
                       x = TRUE,
                       y = TRUE)
    }

    for (i in 1:length(knots)) {
        p <- plot_rcs(data, fits[[i]], n_knot = i + 2, zoom = zoom, FIGUREPATH = FIGUREPATH)
        print(p)
    }

    AICs <- unlist(lapply(fits, AIC))
    AIC_min <- min(AICs)
    AIC_mindiff <- AICs - AIC_min
    probs <- exp(-AIC_mindiff / 2) * 100
    AIC_df <- data.frame(knots = knots,
                            AIC_delta = AIC_mindiff,
                            probs = probs)

    AICp <- ggplot(data = AIC_df, aes(x = knots,
                                 y = AIC_delta)) +
        geom_point() +
        geom_hline(yintercept = 2,
                   linetype = "dashed",
                   color = "red") +
        geom_hline(yintercept = 4,
                   linetype = "dashed",
                   color = "orange") +
        geom_hline(yintercept = 7,
                   linetype = "dashed",
                   color = "yellow") +
        labs(x = "Number of knots in an RCS model",
             y = bquote(Delta[i] ~ " = " ~ AIC[min] ~ " - " ~ AIC[i])) +
        scale_x_continuous(breaks = knots) +
        scale_y_continuous(breaks = c(2, 4, 7), limits = c(0, 50)) +
        geom_segment(aes(x = knots, y = AIC_delta + 3, xend = knots, yend = AIC_delta + 8), alpha = 0.2) +
         geom_text(aes(label = paste0(round(probs, 2), "%")), nudge_y = 10, nudge_x = 0.1) +
        theme_minimal()

    ggsave(paste0(FIGUREPATH, response, "_rcs_AIC", ".pdf"), AICp, width = 8, height = 6, device = cairo_pdf)
    print(AICp)
}


boot_knot_selection <- function(data, response, knots, i) {
    fits <- vector("list", length = length(knots))
    for (j in 1:length(knots)) {
        fits[[j]] <- ols(as.formula(sprintf("%s ~ rcs(Ferritin, %s)", response, knots[j])),
                       data = data[i, ],
                       x = TRUE,
                       y = TRUE)
    }

    AICs <- unlist(lapply(fits, AIC))
    AIC_min <- min(AICs)
    AIC_mindiff <- AICs - AIC_min
    probs <- exp(-AIC_mindiff / 2) * 100
    AIC_df <- data.frame(knots = knots,
                            AIC_delta = AIC_mindiff,
                            probs = probs)

    selected_knot <- AIC_df %>%
        filter(probs > 5) %>%
        slice(1) %>%
        select(knots) %>%
        as.integer()

    return(selected_knot)
}

boot_saddle_estimate <- function(data, response, knot_n, i) {
    sample <- data[i, ]
    resampled_fit <- ols(
        as.formula(sprintf("%s ~ rcs(Ferritin, %s)", response, knot_n)),
        data = sample,
        x = TRUE,
        y = TRUE
    )
    resampled_saddle <- find_saddle(resampled_fit, max = (response == "Hemoglobin"))
    return(resampled_saddle$x)
}
