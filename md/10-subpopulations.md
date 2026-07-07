## Subpopulations

It is generally assumed that a seed population can be characterized by a single distribution of thresholds for a given factor. However, it is common in the seed industry to blend lots from different productions, which may have different germination characteristics, into a single lot. Seed and environmental conditions during seed development can also have significant consequences for seed dormancy and quality (Donohue, 2009; Auge et al., 2017; Penfield and MacGregor, 2017). This can result in apparent non-normal distributions of thresholds and less than optimal fits of the models described above, which has spurred studies to test other statistical models that might better fit these non-normal distributions and debates about which models should be used (Gianinetti, 2020). However, in many cases the behavior of seed lots with complex germination behavior can be analyzed as being due to a mixture of distinct subpopulations comprising the total seed population (Bello and Bradford, 2016; Bello, 2020). Summing the germination percentages of these subpopulation fractions at each time point can then match the full germination time course. This can be modelled by combining multiple PBT subpopulations, as illustrated here for two subpopulations: 

```
D(g) =
  f1 * ( [X - (theta_x1 / t_g1) - X_b50_1] / sigma_X1 ) +
  (1 - f1) * ( [X – (theta_x2 / t_g2) – X_b50_2] / sigma_X2 )
```

where `D(g)` is the sensitivity distribution of the total population, `f` indicates the fraction of the total population and the subscripts indicate the subpopulation (1 or 2) of each parameter within the total population. This approach can use detailed germination time course data to deconvolute complex germination patterns by identifying underlying subpopulations whose mixture will result in the overall pattern (Bradford and Bello, 2022).
