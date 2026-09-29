## Promoters

The rate and percentage of germination of a seed lot can be affected by various promotive factors. In many cases, these are associated with seed dormancy and environmental conditions that reduce its depth and allow more seeds to germinate and to do so more rapidly (Finch-Savage and Leubner-Metzger, 2006; Bewley et al., 2013; Reed et al., 2022). For example, a plant hormone called gibberellin (GA) is involved in breaking dormancy and promoting germination. Adding GA to the imbibition media will generally increase germination speed and percentage. Often, the dose-response relationship with plant hormones is logarithmic:

`theta_GA = (log[GA] – log[GA_b(g)]) * t_g`

where `theta_GA` is the GA sensitivity time constant, `GA` is the GA concentration applied, `GA_b(g)` is the base GA concentration (minimum to elicit a germination response) for seed fraction `g` and `t_g` is the time to germination of fraction `g` of the seed population (Ni and Bradford, 1993; Bradford and Bello, 2022). The Data Input Options allow specifying whether the promoter dosages are linear or logarithmic.

The parameters calculated are:

- `theta_p` = promoter time constant, or `theta_GA`
- `P_b(50)` = base promoter concentration (or `GA_b(50)` for GA)
- `sigma` = the standard deviation of the `GA_b(g)` distribution

The graph shows the germination data along with the time courses for each concentration predicted by the model based upon the calculated parameters.
