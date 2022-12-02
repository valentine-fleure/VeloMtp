
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Velo Mtp

Vous avez toujours revé de decouvrir les secrets des Velomagg de
Montpellier? Vous etes au bon endroit!

<!-- badges: start -->
<!-- badges: end -->

![](https://france3-regions.francetvinfo.fr/image/p8H--eEjA7Zax7bM2UBg2k_S4EI/600x400/regions/2020/06/09/5edee9cc63fa9_velo_18.jpg)

Ici vous trouverez : - la distance et la durée d’utilsiation moyenne -
le lien entre distance, temperature et durée d’utilsiation pour un mois
de votre choix
(param$month) - Les caracteristiques des sites de depart et d'arrivé des velos - Le domaine vital d'un utilisateur au choix (param$person)

Les données Velomagg sont directement telechargés depuis le site de la
ville de Montpellier

### Content

This repository is structured as follow:

- [`data/`](https://github.com/valentine-fleure/VeloMtp/tree/master/data):
  contains all raw data required to perform analyses

- [`analyses/`](https://github.com/valentine-fleure/VeloMtp/tree/master/analyses/):
  contains R scripts to run each step of the workflow

- [`outputs/`](https://github.com/valentine-fleure/VeloMtp/tree/master/outputs):
  contains all the results created during the workflow

- [`figures/`](https://github.com/valentine-fleure/VeloMtp/tree/master/figures):
  contains all the figures created during the workflow

- [`R/`](https://github.com/valentine-fleure/VeloMtp/tree/master/R):
  contains R functions developed especially for this project

- [`man/`](https://github.com/valentine-fleure/VeloMtp/tree/master/man):
  contains help files of R functions

- [`DESCRIPTION`](https://github.com/valentine-fleure/VeloMtp/tree/master/DESCRIPTION):
  contains project metadata (author, date, dependencies, etc.)

- [`make.R`](https://github.com/valentine-fleure/VeloMtp/tree/master/make.R):
  main R script to run the entire project by calling each R script
  stored in the `analyses/` folder
