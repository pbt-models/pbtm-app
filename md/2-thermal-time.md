## Thermal Time

The thermal time model assumes a data set with germination temperature as a treatment condition. In the Data Input Options, you can select which temperatures to include. You can also specify whether to use all of the original data, or remove points where the germination percentage does not increase since the previous observation time ("Cleaned" data). The thermal time model is:

`theta_T(g) = (T - T_b) * t_g`

where `theta_T(g)` is the thermal time constant, which varies normally across germination fractions `g` according to the standard deviation `sigma` calculated by the model. Thus, the term `T(g)` indicates that the time constant varies in a normal distribution across `g` values. `T` is the actual temperature and `T_b` is the base temperature, or the minimum at which germination can occur. In this case, this value is assumed to be constant across all seeds, but this is not always the case. The time to germination of fraction `g` is indicated by `t_g`. This model is fit across the temperatures indicated in the dataset and the predicted time courses will be plotted along with the data.

If you have additional treatments in your dataset, the model will display those treatments and include them in the model calculation, which will result in incorrect results. Use the Additional Treatment Filters to indicate which other parameters are included and select those to be included in the plotting and calculations. For example, if different water potentials are included as well as temperatures, and only data for germination in water (0 MPa) are desired (i.e., thermal time), then only select that box.

The thermal time model will be fit to the data, and results will be shown in the table under Model Results:

- `t_b` = base temperature
- `theta_T(50)` = thermal time constant
- `sigma` = standard deviation of `theta_T(50)` value
- `R2` = the correlation value between the data and the predicted curves

It is also possible to hold some of these values constant and specify others to observe the effects of changes in these parameters on the predicted time courses.
