## Hydrothermal Time

The hydrothermal time model assumes a data set with both temperature and water potential (MPa) at germination as treatment conditions. In the Data Input Options, you can select which temperatures or water potentials to include. You can also specify whether to use all of the original data, or remove points where the germination percentage does not increase since the previous observation time (“Cleaned” data). The hydrothermal time model is

`theta_HT = (psi - psi_b(g)) * (T – T_b) * t_g`

where `theta_HT` is the hydrothermal time constant, `psi` is the treatment water potential, `T` is the treatment temperature, `T_b` is the base temperature and `t_g` is the time to germination across different germination fractions `g`. The term `psi_b(g)` refers to the base water potential, a normal distribution of sensitivity thresholds for water potential that varies across the seed population, and sigma is the standard deviation of this distribution. This model is fit across the water potentials and temperatures indicated in the dataset and the predicted time courses will be plotted along with the data.

If you have treatments in addition to temperature and water potential in your dataset, the model will display those treatments and include them in the model calculation, which will result in incorrect results. Use the Additional Treatment Filters to select the treatments to be included in the plotting and calculations. For example, select all temperatures and water potentials, and do not select any other treatments.

The hydrothermal time model will be fit to the data, and results will be shown in the table under Model Results:

- `theta_HT` = hydrothermal time constant
- `psi_b(50)` = median base water potential
- `sigma` = standard deviation of the `psi_b(g)` distribution
- `R2` = the correlation value between the data and the predicted curves

It is also possible to hold some of these values constant and specify others to observe the effects of changes in these parameters on the predicted time courses.
