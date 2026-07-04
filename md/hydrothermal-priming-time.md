# Hydrothermal Priming Time

> **[Editorial note]:** This does not seem to work for me, even though it shows a green OK on the left when the priming dataset is loaded.

Hydrothermal priming time is similar to hydropriming time, except that different priming temperatures can also be included, i.e., different priming water potentials at each of several different temperatures and durations. The approach is the same as for hydropriming time, except that the priming temperature (*T_P*) minus the minimum temperature for a priming effect (*T_min*) is multiplied by the priming duration (*t_P*) to include the thermal priming time accumulated during the treatment (Bradford and Haigh, 1994; Bradford and Bello, 2022):

***GR_50 = GR_i + (1/theta_HTP) (psi_P - psi_min)(T_P - T_min) * t_P***

In practice, a matrix of test priming treatments at two or three *T* and *psi_P* values and durations (*t_P*) are conducted, the seeds are then dried, and the *GR_50* values of the seed lots for each priming condition are determined in germination tests under standard conditions. With these data, the Aging Time analysis uses regression methods to estimate the effective values of *psi_min*, *T_min* and *theta_HTP* (hydrothermal priming time constant) for the seed lot.

As noted for hydropriming time, the practical value of applying the hydrothermal priming time model is that the effects on germination of different combinations of priming water potentials, temperatures and durations can be predicted for the tested seed lot. The values of these parameters can be modified, but so long as the accumulated hydrothermal priming time is the same, the effect on the germination performance will be similar as well. In particular, increasing temperature or water potential will shorten the priming duration, while lowering temperature and water potential will extend priming duration. Increasing temperature and lowering water potential will have offsetting effects for a given priming duration. Thus, the same effective priming treatment can be achieved by various combinations of temperature, water potential and duration that can be selected for convenience or economy. 

## References

- Bradford, K.J., Bello, P. (2022) Applying population-based threshold models to quantify and improve seed quality attributes. In J Buitink, O Leprince, eds, Advances in Seed Science and Technology for More Sustainable Crop Production. Burleigh Dodds Science Publishing, Cambridge, UK

- Bradford, K.J., Haigh, A.M. (1994) Relationship between accumulated hydrothermal time during seed priming and subsequent seed germination rates. Seed Sci. Res. 4(1–10).
