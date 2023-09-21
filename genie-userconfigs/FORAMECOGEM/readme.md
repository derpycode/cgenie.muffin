This is model documentation for ForamEcoGENIE 1.1 covering Pliocene/LGM/Pre-industrial/Future

# About the model

## Aim
The model won't be perfect. It aims to solve three things: (1) the essential foram trait; (2) the biogeogprahy; (3) useful in the past.

Currently I intend to keep the foraminifera biomass overestimated than the observation. Because currently no model including those species distribution model can fullfil all three aspects of foram observation: distribution, biomass and export production.

Other models: 
ForamClim has individual-based implementation, but use empirical (i.e., fixed) growth rate curve;
PLAFOM has similarily overestimated annual mean biomass (N. Pachyderma > 0.1 mmol C/m3) but has mechanistic setting
PLANKTOM has foram but only designed for solving carbonate pump. Similarily use Lombard et al. 2009 measured growth curve.
Statistic models (Waterson et al. 2016; Zaric et al. 2005; Knecht et al. 2023) are not counted here.


## Traits

### Feeding behaviour
Symbiont-barren non-spinose foram (e.g., N. pacheyderma):   *herbivory* generalist with increased food range (2x)
Symbiont-barren spinose foram (e.g., G. bulloides):         *herbivory* with increased grazing rate (reduced half-saturation constant)
Symbiont-facultative non-spinose foram (e.g., N. dutertei): *herbivory* generalist with increased food range (2x)
Symbiont-obligate spinose foram (e.g., G. ruber):           *carnivory* with increased grazing rate (reduced half-saturation constant)
Same predator-prey size ratio: 10/1

### Calcification/Spine trait trade-offs
- non-spinose adult: 20-40% growth rate reduction, 20-50% mortality reduction
- spinose     adult: 20-60% growth rate reduction, 12–55% mortality reduction

Reference of 0D model test: Grigoratou et al. (2019) Biogeoscience; Grigoratou et al. (2022) Marine Micropaleotonlogy;

respiration is incorporated because the mismatch of model-data biogeography; This should be

### Symbiosis
- Independent symbiont size: 1% foraminifera shell size
- Low autotrophy efficiency: Great amount of excessive carbon from symbionts (theoretical growth rate = 0.89 d-1, >> ~0.2 d-1 in observation) 
- Low heterotrophy efficiency: mixotrophy modelling convention (Ward and Follows. 2016 PNAS)

Tested implementation: directly transfer biomass from phytoplankton, or mixotroph;


## Limitation
+ only adult foram are incldued
+ no 3D water column ecosystem;
+ cGENIE has little primary production in the tropical oceans  
+ fixed zooplankton Q10 (the respiration of foram has high Q10 (>3, Lombard et al. 2009 L&O), Photo:Respir = 2-10


# Model Runs
## pre-industrial spinup (0-10000)

```
qsub -j y -o cgenie_log -V -S /bin/bash runmuffin.sh muffin.CBE.worlg4.BASESFeTDTL FORAMECOGEM muffin.CBE.worlg4.BASESFeTDTL.SPIN 10000
```

## Historical (1765-2022) 

```sh
qsub -j y -o cgenie_log -V -S /bin/bash runmuffin.sh muffin.CBE.worlg4.BASESFeTDTL FORAMECOGEM muffin.CBE.worlg4.BASESFeTDTL.historical 257 muffin.CBE.worlg4.BASESFeTDTL.SPIN
```

## Future (2022-2100)

```sh
qsub -j y -o cgenie_log -V -S /bin/bash runmuffin.sh muffin.CBE.worlg4.BASESFeTDTL FORAMECOGEM muffin.CBE.worlg4.BASESFeTDTL.2100.[XXX]deg 78 muffin.CBE.worlg4.BASESFeTDTL.historical
```

## LGM

```sh
qsub -j y -o cgenie_log -V -S /bin/bash runmuffin.sh muffin.CBE.GIteiiva.BASESFeTDTL_rb FORAMECOGEM muffin.CBE.GIteiiva.BASESFeTDTL_rb.SPIN 10000
```

## Holocene

```sh
qsub -j y -o cgenie_log -V -S /bin/bash runmuffin.sh muffin.CBE.GIteiiaa.BASESFeTDTL_rb FORAMECOGEM muffin.CBE.GIteiiaa.BASESFeTDTL_rb.SPIN 10000
```

## Pliocene
muffin.CB.umQ00p0a.BASES (modern)
muffin.CB.umQ04p5a.BASES (Pliocene open)
muffin.CB.umQ04p5b.BASES (Pliocene close)

``` sh
## Pliocene open, 400 ppm
qsub -j y -o cgenie_log -V -S /bin/bash runmuffin.sh muffin.CB.umQ04p5a.BASES FORAMECOGEM 20220104.CB.umQ04p5a.BASES.CASopen.400_0p5.SPIN 10000

## Modern CAS closed, 280 ppm
qsub -j y -o cgenie_log -V -S /bin/bash runmuffin.sh muffin.CB.umQ00p0a.BASES FORAMECOGEM 20220104.CB.umQ00p0a.BASES.CASclosed.280_0p2.SPIN 10000

## Pliocene closed, 400 ppm
qsub -j y -o cgenie_log -V -S /bin/bash runmuffin.sh muffin.CB.umQ04p5b.BASES FORAMECOGEM 20220104.CB.umQ04p5b.BASES.CASclosed.400_0p2.SPIN 10000

## Pliocene closed, 280 ppm
qsub -j y -o cgenie_log -V -S /bin/bash runmuffin.sh muffin.CB.umQ04p5b.BASES FORAMECOGEM 20220104.CB.umQ04p5b.BASES.CASclosed.280_0p2.SPIN 10000
```


> Full effect:4.5 Ma (open,   400 ppm) - 0 Ma (closed, 280 ppm)
20220104.CB.umQ04p5a.BASES.CASopen.400_0p5.SPIN
20220104.CB.umQ00p0a.BASES.CASclosed.280_0p2.SPIN

> CO2 effect: 4.5 Ma (closed, 400 ppm) - 4.5 Ma (closed, 280 ppm)
20220104.CB.umQ04p5b.BASES.CASclosed.400_0p2.SPIN
20220104.CB.umQ04p5b.BASES.CASclosed.280_0p2.SPIN

> CAS effect: 4.5 Ma (open,   400 ppm) - 4.5 Ma (closed, 400 ppm)
20220104.CB.umQ04p5a.BASES.CASopen.400_0p5.SPIN
20220104.CB.umQ04p5b.BASES.CASclosed.400_0p2.SPIN

## K-Pg
