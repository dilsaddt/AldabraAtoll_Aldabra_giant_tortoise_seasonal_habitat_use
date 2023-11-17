# AldabraAtoll_Aldabra_giant_tortoise_seasonal_habitat_use
Analysis of seasonal habitat-use patterns of Aldabra giant tortoises of Aldabra Atoll (_Aldabrachelys gigantea_), Seychelles using temporary emigration models.

## Files
data/aldabra_tortoise_data_all.RData = data for running all veresions of models and predictions
*** Data here is representative just to make code running, not the actual data used in the analyses. ***

In all models lambda represents expected site-specific density and kept the same for all model versions:

_lambda ~ island_

We used availability probability part of the temporary emigration models (denoted as theta) as a proxy for habitat use.
We assumed that when a tortoise was available to be detected in a specific habitat type, then it was using this site at that time of the year. 
Consequently, whereas a higher availability probability indicated higher habitat use, a lower availability probability indicated higher temporary emigration, meaning that the tortoise was not using the habitat when the transect counts were conducted. 
Therefore, we refer to theta as the habitat-use probability, and changed this component of the model to investigate different combination of season and habitat type.

**To do so 5 different models were run:**

1. habitat_only = single effect of habitat type

2. season_only = single effect of season

3. habitat_season_additive(add) = additive effect of habitat type and season

4. habitat_season_interaction(int) = interaction effect of habitat type and season

5. habitat_season_interaction(int)_random_transect = interaction effect of habitat type and season together with random transect effect




**In each folder, file order is the same, as an example inside the habitat_only folder:**

- habitat_only_model_diags.R = code for running the habitat only model and its diagnostics

- habitat_only_pred.R = code for running predictions for the habitat only model

- L_I_T_H_Alt.txt = text file for the JAGS model


## Software
R version 4.2.1

JAGS (through R-package ‘jagsU’I)
