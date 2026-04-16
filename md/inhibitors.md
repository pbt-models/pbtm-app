# Inhibitors

The rate and percentage of germination of a seed lot can be affected by various inhibitory factors. In many cases, these are associated with seed dormancy and environmental conditions that reduce its depth and allow more seeds to germinate and to do so more rapidly (Finch-Savage and Leubner-Metzger, 2006; Bewley et al., 2013; Reed et al., 2022). For example, a plant hormone called abscisic acid (ABA) is involved in breaking dormancy and promoting germination. Adding ABA to the imbibition media will generally decrease germination speed and percentage. Often, the dose-response relationship with plant hormones is logarithmic:

***theta_ABA = [log(ABA_b(g)) - log(ABA)] * t_g***

where *ABA* is the ABA sensitivity time constant, *ABA* is the ABA concentration applied, *ABA_b(g)* is the base ABA concentration (minimum to elicit a germination response) for seed fraction *g* and *t_g* is the time to germination of fraction *g* of the seed population (Ni and Bradford, 1992; Ni and Bradford, 1993; Bradford and Bello, 2022). The Data Input Options allow specifying whether the promoter dosages are linear or logarithmic. The parameters calculated are ThetaI (the inhibitor time constant, *theta_ABA* in this case), Ib50 (*ABA_b(50)*), and Sigma (the standard deviation of the *ABA_b(g)* distribution). The graph shows the germination data along with the time courses for each concentration predicted by the model based upon the calculated parameters. 

## References

- Bewley, J.D., et al. (2013) Seeds: Physiology of Development, Germination and Dormancy, Ed Third. Springer, New York

- Bradford, K.J., Bello, P. (2022) Applying population-based threshold models to quantify and improve seed quality attributes. In J Buitink, O Leprince, eds, Advances in Seed Science and Technology for More Sustainable Crop Production. Burleigh Dodds Science Publishing, Cambridge, UK
- Finch-Savage, W.E., Leubner-Metzger, G. (2006) Seed dormancy and the control of germination. New Phytol. 171(3): 501–523. doi:10.1111/j.1469-8137.2006.01787.x
- Ni, B.R., Bradford, K.J. (1992) Quantitative models characterizing seed germination responses to abscisic acid and osmoticum. Plant Physiol. 98(3): 1057–1068. 10.1104/pp.98.3.1057
- Ni, B.R., Bradford, K.J. (1993) Germination and dormancy of abscisic acid-deficient and gibberellin-deficient mutant tomato seeds. Sensitivity of germination to abscisic acid, gibberellin, and water potential. Plant Physiol. 101(2): 607–617. 10.1104/pp.101.2.607
- Reed, R.C., et al. (2022) Seed germination and vigor: ensuring crop sustainability in a changing climate. Heredity 128(450–459). 10.1038/s41437-022-00497-2
