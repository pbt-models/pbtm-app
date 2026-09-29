## Inhibitors

The rate and percentage of germination of a seed lot can be affected by various inhibitory factors. In many cases, these are associated with seed dormancy and environmental conditions that delay or inhibit germination (Finch-Savage and Leubner-Metzger, 2006; Bewley et al., 2013; Reed et al., 2022). For example, a plant hormone called abscisic acid (ABA) is involved in inducing dormancy and inhibiting germination. Adding ABA to the imbibition media will generally decrease germination speed and percentage. Often, the dose-response relationship with plant hormones is logarithmic:

`theta_ABA = (log[ABA_b(g)] - log[ABA]) * t_g`

where `theta_ABA` is the ABA sensitivity time constant, `ABA_b(g)` is the base ABA concentration (minimum to elicit a germination response) for seed fraction `g`, `ABA` is the ABA concentration applied, and `t_g` is the time to germination of fraction `g` of the seed population (Ni and Bradford, 1992; Ni and Bradford, 1993; Bradford and Bello, 2022). The Data Input Options allow specifying whether the promoter dosages are linear or logarithmic.

The parameters calculated are:

- `theta_I` = the inhibitor time constant, `theta_ABA` in this case
- `I_b(50)` = the base inhibitor concentration (or `ABA_b(50)` for ABA)
- `sigma` = the standard deviation of the `ABA_b(g)` distribution

The graph shows the germination data along with the time courses for each concentration predicted by the model based upon the calculated parameters.
