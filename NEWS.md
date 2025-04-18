### Package changes from previous qlifetable version 0.0.2-5

A new reference: 3.	Pavía, JM and Lledó, J (2025) "qlifetable: An R package for constructing quarterly life tables", PLoS ONE, 20(2), e0315937. DOI: https://doi.org/10.1371/journal.pone.0315937, 
which describes all the details of the package is included.

### Package changes from previous qlifetable version 0.0.2-4

The way grobs are defined in the method **plot** for objects of class SAI has been modified to accomodate some changes made in new versions of ggplot2 (>=3.5.0).

A bug in the function **annual2quaterly** has been fixed when its input annual life table is defined using death rates (mx).

### Package changes from previous qlifetable version 0.0.1-15

The previous version (0.0.1-15) of qlifetable just computes from microdata the summary statistics to build quarterly life tables. This new version includes two new sets of functions. On the one hand, `qlifetable` now incorporates a bunch of functions to construct from summary statistics Seasonal-ageing indexes (SAIs) and quarterly life tables and, on the other hand, the new version also has new functions to estimate SAIs approximations as detailed in Pavía and Lledó (2023) <doi:10.1017/asb.2023.16>.

The list of new functions includes:

* **annual2quarterly**. This function allows to derive the four quarterly life tables associated with an annual life table by employing a set of estimated SAIs that have been obtained using either the new function **compute_SAI** or **SAI_shortcut_1**.

* **compute_SAI**. This function computes the seasonal-ageing index (SAIs) estimates linked to a set of quarterly crude rates of mortality, attained using either **crude_mx**, **crude_mx_sh2** or **crude_mx_sh3**, corresponding to several years.

**crude_mx**. This function computes quarterly crude rates of mortality given (i) a set of quarterly datasets of time of expositions at risk and (ii) a dataset of quarterly deaths.

**crude_mx_sh2**. This function computes, by applying equation (2.7) in Pavía and Lledó (2023), quarterly crude rates of mortality given (i) a couple of integer-age stock of population datasets and (ii) a dataset of quarterly deaths.

**crude_mx_sh3**. This function computes, by applying equation (2.9) in Pavía and Lledó (2023), quarterly crude rates of mortality given (i) a couple of integer-age stock of population datasets, (ii) a dataset of quarterly deaths, (iii) a dataset of quarterly entries and (iv) a dataset of quarterly exits.

**plot.SAI**. This function is a method for plotting objects of the class `SAI` attained using either the function **compute_SAI** or **SAI_shortcut_1**.

**SAI_shortcut_1**. This function estimates a set of SAIs by applying equation (2.5) in Pavía and Lledó (2023) given a set of datasets of quarterly deaths.
