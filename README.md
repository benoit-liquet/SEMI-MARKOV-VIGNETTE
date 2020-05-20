# BIOMETRICS-SEMI-MARKOV-VIGNETTE

Here we provide a comprehensible detailed vignette to run SEMI-MARKOV model using R software packages.

We illustrate the inference of the semi-Markov model on two real datasets : _Stanford Heart Transplant data_ and _asthma control data_

This vignette reproduce the results of our application section **Semi-Markov application in practice** from our submitted articles:

_Estimation of Semi-Markov Multistate Models: A Comparison between the
Sojourn Times and Transition Intensities Approaches_ A. Asanjarani, B. Liquet and Y. Nazarathy. (2020).


## Getting Started

In this vignette we used the following R packages

```
library("p3state.msm")
library("mstate")
library(SemiMarkov)
library(flexsurv)
library(xtable)
library(eha)
library(mpr)
library(mstate)
```

