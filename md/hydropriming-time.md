# Hydropriming Time

Priming is a process in which seeds are hydrated to a level just below what is required for the completion of germination and held for a given period of time before redrying (Bradford, 1986). This initiates physiological repair and preparation for germination but prevents radicle emergence. When the primed seeds are rehydrated, the lag phase between imbibition and radicle emergence is shortened, advancing and synchronizing germination among seeds in the population (Corbineau et al., 2023). The advancement in germination achieved by priming is related to the water potential of the priming treatment and its duration, or the hydropriming time (Bradford and Haigh, 1994). Thus, if seeds are primed at different water potentials and for different durations at each water potential, then dried and subsequently tested for germination performance, the advancement in germination rates (speed) is increases in proportion to the amount of hydropriming time accumulated during the priming treatment. Water potentials are negative values, so the higher the water potential (closer to zero), the more hydropriming time is accumulated per unit of time. At a lower (more negative) water potential, a longer priming duration would be required to achieve the same effect. By fitting the hydropriming time model to the data, the duration of priming at a selected water potential to give an optimal effect can be calculated.

The priming sample data available in the PTBM app shows the expected data for this function. These include the water potential (and temperature) and duration of the priming treatment and the timing and fraction of germination observed in subsequent germination tests. If there are additional treatments in the spreadsheet (such as temperature), the Additional Treatment Filters option can be used to only use the data from one temperature. The data are plotted as the germination rate (1/time to 50% germination) versus the hydropriming time, or the priming water potential minus the minimum water potential (PsiMin) to give a priming effect times the priming duration. The advancement in median germination rates (*GR_50*) after priming a given and water potential (*psi_P*) and duration (*t_p*) on a common hydropriming scale can be shown as:

***theta_HP = (psi_P - psi_min) * t_p***

where *theta_HP* is a hydropriming time constant and *psi_min* is the minimum effective priming water potential. This can also be shown as a germination rate to 50% (*GR_50 = 1/time to 50% germination*) after priming as

***GR_50 = GR_i + [(psi_P - psi_min) * t_p] / theta_HP***

where *GR_i* is the initial median germination rate before priming(Bradford and Haigh, 1994). In the app, the *GR_50* values for different priming conditions are plotted as a function of the hydropriming time, resulting in a line with a slope of *1/theta_HP* and an intercept on the y-axis of *G_i*.

The practical value of the fitting the hydropriming time model is that the effects on germination of different combinations of priming water potentials and durations can be predicted for the tested seed lot. These water potentials and durations can be modified for convenience or other reasons, but so long as the accumulated hydropriming time is the same, the effect on the germination performance will be similar as well. 

## References

- Bradford, K.J. (1986) Manipulation of seed water relations via osmotic priming to improve germination under stress conditions. HortScience 21(5): 1105–1112.

- Bradford, K.J., Haigh, A.M. (1994) Relationship between accumulated hydrothermal time during seed priming and subsequent seed germination rates. Seed Sci. Res. 4(1–10).
- Corbineau, F., et al. (2023) Improvement of Seed Quality by Priming: Concept and Biological Basis. Seeds 2(1): 101–115.
