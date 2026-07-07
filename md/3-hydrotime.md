## Hydrotime

The hydrotime model assumes a data set with germination water potential (MPa) as a treatment condition. In the Data Input Options, you can select which water potentials to include. You can also specify whether to use all of the original data, or remove points where the germination percentage does not increase since the previous observation time (“Cleaned” data). The hydrotime model is

`theta_H = (psi – psi_b(g)) * t_g`

where `theta_H` is the hydrotime constant, `psi` is the treatment water potential and `t_g` is the time to germination across different germination fractions `g`. The term `psi_b(g)` refers to the base water potential, a normal distribution of sensitivity thresholds for water potential that varies across the seed population, and sigma is the standard deviation of this distribution. This model is fit across the water potentials indicated in the dataset and the predicted time courses will be plotted along with the data.

If you have additional treatments in your dataset, the model will display those treatments and include them in the model calculation, which will result in incorrect results. Use the Additional Treatment Filters to select the treatments to be included in the plotting and calculations. For example, if different temperatures are included as well as water potentials, and only data for germination at one temperature are desired (i.e., hydrotime), then only select the box for that temperature.

The hydrotime model will be fit to the data, and results will be shown in the table under Model Results:

- `theta_H` = thermal time constant
- `psi_b(50)` = median base water potential
- `sigma` = standard deviation of the `psi_b(g)` distribution
- `R2` = the correlation value between the data and the predicted curves

It is also possible to hold some of these values constant and specify others to observe the effects of changes in these parameters on the predicted time courses.
