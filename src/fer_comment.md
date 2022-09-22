---
title: "WHO low ferritin comment"
author: "Esa Turkulainen"
date: "2022-09-20"
output: 
    html_document:
        keep_md: yes
---



# Intro

The World Health Organization currently recommends a ferritin cut-off of <15 μg/L for iron deficiency in all subpopulations, except children under 5 and pregnant individuals after the first trimester. [[1](https://www.who.int/publications/i/item/9789240000124)] Although these numbers were "updated" in 2020, WHO couldn't find strong evidence for using ferritin as a good marker for iron stores. They still recommend it, as there is high likelihood of it being better than not using it.

In 2021, to conjure some sense into this, Mei et al. [[2](https://www.sciencedirect.com/science/article/abs/pii/S235230262100168X)] (affl. CDC) attempted to tie ferritin into two different physiological measures: hemoglobin and soluble transferritin receptors (sTfR), both indicators of iron-deficient erythropoiesis (layman: "not enough iron to make red blood cells"). They seem to argue that if they can find a similar threshold value for both of these by looking at ferritin, they can "derive a physiological ferritin threshold for iron deficiency". Their cohort consists of "apparently healthy" children and non-pregnant women (15-49 yo) recorded in the US National Health and Nutrition Examination Survey (NHANES). For the purposes of their study, "apparently healthy" means filtering out individuals based on CRP, white blood cell counts, and possible liver disease.

To find this threshold, they chose a restricted cubic spline model with 5 knots. They do not report testing other numbers of knots, nor varying knot positions. In their Supplement they explain that they used the `rcs()` function from the R package `rms` for this. 5 equidistant knots is the default setting for this function, so I'll just assume this was ran as is.

In this report we'll focus on apparently healthy non-pregnant women (by recorded gender, pregnancy status and blood donor eligibility criteria available in the Health 2000 cohort), so here are the results for women in Mei et al. (reproduced without permission!):

<center>
![Serum ferritin threshold calculation for non-pregnant women](/home/esa/iron_deficiency_comment/fer_comment_files/figure-html/plot_women_mei_et_al.png)
</center>

Mei et al. determine the threshold by computing the zero derivatives of the resulting fits, implicitly arguing that the "plateau before the fall/rise" constitutes as the _de facto_ threshold. The threshold generated this way are 25.2 μg/L and 24.0 μg/L for hemoglobin and sTfR, respectively. These vary somewhat when stratifying by age. It is however immediately interesting that these thresholds settle around the same number for both hemoglobin and sTfR, and that they defy the visual inspection of the scatter plot / median line, which points closer to the WHO threshold.

Later, in 2022, they replicated their results with Retrovirus Epidemiology Donor Study-II Donor Iron Status Evaluation (REDS-RISE) blood donor data (this time Addo et al.). [[3](https://ashpublications.org/bloodadvances/article/6/12/3661/484681/Physiologically-based-serum-ferritin-thresholds)] 

<center>
![Concentration curve of serum ferritin against hemoglobin, and soluble transferrin receptor in a sample of women who are blood donors.](/home/esa/iron_deficiency_comment/fer_comment_files/figure-html/addo_et_al.png)
</center>

# Replication

We're now left with a curiosity: why exactly 5 knots? If we were to try to replicate this, we'd be interested in testing fits with more/less knots, or possibly entirely other approaches. We only have hemoglobin and ferritin to work with in the Health 200 cohort, but let's see how the data looks.

## Data visualized



<img src="fer_comment_files/figure-html/plot_data-1.png" style="display: block; margin: auto;" />

Our data scatters in a very similar manner to Mei & Addo et al. Visual inspection would suggest that the ferritin threshold for iron deficiency, as indicated by hemoglobin, might even be lower than what WHO suggests. Admittedly, visual inspection is insufficient in determining relative scatter densities and may underestimate.

What will the models show?

## Restricted cubic splines (5 knots)




```r
# Fit
rcs5 <- ols(Hemoglobin ~ rcs(Ferritin, 5), data = apparently_healthy, x = TRUE, y = TRUE)
# Plot fit
p5 <- ggplot(Predict(rcs5)) + 
    geom_vline(xintercept = 19.1053, linetype = "dashed") +
    annotate(geom = "text", label = "19.1 μg/L", x = 34, y = 140) +
    annotate(geom = "text", label = paste("AIC:", as.integer(AIC(rcs5))), x = 110, y = 144) + 
    scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120), limits = c(0, 120)) +
    theme_minimal() + 
    labs(title = "Restricted cubic spline, 5 knots",
         subtitle = "Apparently healthy non-pregnant women 20-49 y.o.",
         x = "Serum ferritin concentrations (μg/L)",
         y = "Haemoglobin concentration (g/L)")
p5
```

<img src="fer_comment_files/figure-html/rcs5-1.png" style="display: block; margin: auto;" />
The curve shape is somewhat similar to Mei & Addo et al. with their NHANES study. Our model specification ends up being

$$
\begin{eqnarray*}
\hat{y}=\\
& & 113.7922 \\
& & + 1.752375x - 0.00444937 (x - 5.1)_{+}^{3}  \\
& &  +0.007910383  (x-13.065)_{+}^{3}-0.003740607 (x-24.6)_{+}^{3}  \\
& &   +0.000282423  (x-41.235)_{+}^{3}-2.828606\!\times\!10^{-6}(x-100.453)_{+}^{3}  \\
\end{eqnarray*}
$$
$\text{where} \space (x)_{+}=x \space \text{if} \space x > 0, 0 \space \text{otherwise}$. The zero derivative can be computed to be **19.1053 μg/L**. This is closer to the current WHO reference value, than the suggested 25 μg/L, but this is just a curiosity at this point. We need to see what happens with other knot structures.

## 3 to 12 knots
<img src="fer_comment_files/figure-html/other_knots-1.png" style="display: block; margin: auto;" />

Which of these restricted cubic spline models is best? Out of these, the 11 knot version has the lowest AIC, but is this the global minimum? And does it differ "enough" from simpler models? We can compute the "probability that model *i* is as good as the one that got the lowest AIC" with
$$
\text{P} = e^{\frac{\text{AIC}_{min} - \text{AIC}_i}{2}}.
$$
The idea is that if the model that gets the minimum AIC does not improve significantly from another, we may want to not discriminate between them. Let us choose a significance level of 5\%.


```r
rcs8 <- ols(Hemoglobin ~ rcs(Ferritin, 8), data = apparently_healthy, x = TRUE, y = TRUE)
rcs9 <- ols(Hemoglobin ~ rcs(Ferritin, 9), data = apparently_healthy, x = TRUE, y = TRUE)
rcs10 <- ols(Hemoglobin ~ rcs(Ferritin, 10), data = apparently_healthy, x = TRUE, y = TRUE)
rcs12 <- ols(Hemoglobin ~ rcs(Ferritin, 12), data = apparently_healthy, x = TRUE, y = TRUE)
AIC_min <- min(c(AIC(rcs8), AIC(rcs9), AIC(rcs10), AIC(rcs11), AIC(rcs12)))
# The 11 knot version is indeed the global MIN, we do not need to check the P for the 12 knot version as it increases in complexity and AIC.

P7 <- compare_AIC(AIC_min, AIC(rcs7))
P8 <- compare_AIC(AIC_min, AIC(rcs8))
P9 <- compare_AIC(AIC_min, AIC(rcs9))
P10 <- compare_AIC(AIC_min, AIC(rcs10))
```

We find that the 11 knot model does not differ significantly (arbitrary 5\% threshold chosen by us) from 10 and 9 knot models (37\% and 7\% probability of being equally good as the 11 knot version, respectively). With this knowledge, we probably ought to go for the 9 knot model. 


```r
# Plot fit
p9 <- ggplot(Predict(rcs9)) + 
    geom_vline(xintercept = 10.2289, linetype = "dashed") +
    annotate(geom = "text", label = "10.2 μg/L", x = 19, y = 140) +
    annotate(geom = "text", label = paste("AIC:", as.integer(AIC(rcs9))), x = 110, y = 144) + 
    scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120), limits = c(0, 120)) +
    theme_minimal() + 
    labs(title = "Best restricted cubic spline: 9 knots",
         subtitle = "Apparently healthy non-pregnant women 20-49 y.o.",
         x = "Serum ferritin concentrations (μg/L)",
         y = "Haemoglobin concentration (g/L)")
p9
```

<img src="fer_comment_files/figure-html/plot_rcs9-1.png" style="display: block; margin: auto;" />

If this approach is sensible overall is a whole another matter. We arrive at a ferritin threshold greatly below even the WHO recommendation!

Measuring the accuracy of a model like this has its pitfalls. What we're actually interested in is: how well can we find the inflection point for this phenomenon? Restricted cubic splines are handy when we need to define inflection points mathematically, but scoring them greatly outside the point of interest may lead us to select a sub-optimal model. So, what if we only consider the space of 0 - 30 μg/L for serum ferritin?

## RCS on abridged data


<img src="fer_comment_files/figure-html/knots_abr-1.png" style="display: block; margin: auto;" />
We're skipping the documentation of AIC comparison here. This time, 7 knots is the global minimum. It also passes our significance test against the 6 knot model, so now we're left with an even smaller threshold of 8.4 μg/L.

## Let's transform

Does it help any to log-transform `Ferritin`?



<img src="fer_comment_files/figure-html/plot_log-1.png" style="display: block; margin: auto;" />


# References
1. World Health Organization. (2020). WHO guideline on use of ferritin concentrations to assess iron status in populations. World Health Organization. ISBN: 978-92-4-000012-4
2. Mei Z, Addo OY, Jefferds ME, Sharma AJ, Flores-Ayala RC, Brittenham GM. Physiologically based serum ferritin thresholds for iron deficiency in children and non-pregnant women: a US National Health and Nutrition Examination Surveys (NHANES) serial cross-sectional study. The Lancet Haematology. 2021;8(8):e572-e582. doi:10.1016/S2352-3026(21)00168-X
3. Addo OY, Mei Z, Hod EA, et al. Physiologically based serum ferritin thresholds for iron deficiency in women of reproductive age who are blood donors. Blood Advances. 2022;6(12):3661-3665. doi:10.1182/bloodadvances.2022007066
