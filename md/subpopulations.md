# Subpopulations

It is generally assumed that a seed population can be characterized by a single distribution of thresholds for a given factor. However, it is common in the seed industry to blend lots from different productions, which may have different germination characteristics, into a single lot. Seed and environmental conditions during seed development can also have significant consequences for seed dormancy and quality (Donohue, 2009; Auge et al., 2017; Penfield and MacGregor, 2017). This can result in apparent non-normal distributions of thresholds and less than optimal fits of the models described above, which has spurred studies to test other statistical models that might better fit these non-normal distributions and debates about which models should be used (Gianinetti, 2020). However, in many cases the behavior of seed lots with complex germination behavior can be analyzed as being due to a mixture of distinct subpopulations comprising the total seed population (Bello and Bradford, 2016; Bello, 2020). Summing the germination percentages of these subpopulation fractions at each time point can then match the full germination time course. This can be modelled by combining multiple PBT subpopulations, as illustrated here for two subpopulations: 

```
D(g) =
  f1 * ( [X - (theta_x1 / t_g1) - X_b50_1] / sigma_X1 ) +
  (1 - f1) * ( [X – (theta_x2 / t_g2) – X_b50_2] / sigma_X2 )
```

where *D(g)* is the sensitivity distribution of the total population, *f* indicates the fraction of the total population and the subscripts indicate the subpopulation (1 or 2) of each parameter within the total population. This approach can use detailed germination time course data to deconvolute complex germination patterns by identifying underlying subpopulations whose mixture will result in the overall pattern (Bradford and Bello, 2022).

## References

- Auge, G.A., et al. (2017) Adjusting phenotypes via within- and across-generational plasticity. New Phytol. 216(2): 343–349. 10.1111/nph.14495

- Bello, P., Bradford, K.J. (2016) Single-seed oxygen consumption measurements and population-based threshold models link respiration and germination rates under diverse conditions. Seed Sci. Res. 26(3): 199–221. 10.1017/S0960258516000179
- Bello, P.H.N. (2020) Analytical and Computational Tools to Assess Seed Quality and Model Germination Rates. MS Thesis. University of California, Davis, CA USA. https://www.proquest.com/dissertations-theses/analytical-computational-tools-assess-seed/docview/2540474371/se-2
- Bradford, K.J., Bello, P. (2022) Applying population-based threshold models to quantify and improve seed quality attributes. In J Buitink, O Leprince, eds, Advances in Seed Science and Technology for More Sustainable Crop Production. Burleigh Dodds Science Publishing, Cambridge, UK
- Donohue, K. (2009) Completing the cycle: maternal effects as the missing link in plant life histories. Philosophical Transactions of the Royal Society B: Biological Sciences 364(1520): 1059–1074. 10.1098/rstb.2008.0291
- Gianinetti, A. (2020) Basic features of the analysis of germination data with generalized linear mixed models. Data 5(1): 6. 10.3390/data5010006
- Penfield, S., MacGregor, D.R. (2017) Effects of environmental variation during seed production on seed dormancy and germination. J. Exp. Bot. 68(4): 819–825. 10.1093/jxb/erw436
