# dynamic-OD-estimation-railway-network
Code associated to the paper "Estimation of dynamic Origin-Destination matrices in a railway transportation network integrating ticket sales and passenger count data" authored by Greta Galliani, Piercesare Secchi, and Francesca Ieva

## Overview

This repository contains the code and resources associated with the research paper titled **Estimation of dynamic Origin-Destination matrices in a railway transportation network integrating ticket sales and passenger count data** authored by Greta Galliani, Piercesare Secchi, and Francesca Ieva.

In particular:

* Script <tt>1_estimation_trenord_od_matrices.R</tt> develops the code needed to estimate weekly OD matrices in the Trenord network, given ticket and counter data.
* Script <tt>2_dynamics_network_analysis.R</tt> applies some techniques of network and functional data analysis to perform anomaly detection in the temporal weighted directed network induced by the Trenord dynamic OD matrices.

The code is developed in R (version 4.2.1).

## Data and Resources
Due to confidentiality agreements, the Trenord data about tickets sold and passenger counts in 2022 is unavailable. We produced two artificial datasets, <tt>ticket.csv</tt> and <tt>train.csv</tt> for demonstrative purposes, which, however, have no relationship with the actual data about railway movements in the Trenord railway network in 2022.

## Citation

If you use this code or data in your work, please consider citing the original paper authored by Greta Galliani, Piercesare Secchi, and Francesca Ieva. 

For further details about the research and its findings, please refer to the associated paper.
