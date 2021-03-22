################################################################
### readme.txt #################################################
################################################################

For:
'Temperature controls carbon cycling and biological evolution in the ocean twilight zone'
Flavia Boscolo-Galazzo, Katherine A. Crichton, Andy Ridgwell, Elaine M. Mawbey, Bridget S. Wade, Paul N. Pearson

################################################################
21/01/26 -- README.txt file creation 
################################################################

Provided are all the configuration files and forcing files necessary to run the model experiments presented in the paper.

The intention is to provide an opportunity to question the assumptions and interpretation through re-analysis and the creation of new and different experiments.
(Plus, to provide a means to replicate results.)

The experiments are grouped into 2 series -- with ('Tdep') and without ('stnd') temeprature-dependent biological export and remineralization.
These will reproduce the 2 series of experiments summerized in Figures 3 and 5.

The '???Ma' in the file-string refers to the time-interval:
0Ma     ->  0 Ma (late Holocene)
2_5Ma   ->  2.5 Ma
4_5Ma   ->  4.5 Ma
7_5Ma   ->  7.5 Ma
10Ma    ->  10.0 Ma
12_5Ma  ->  12.5 Ma
15Ma    ->  15.0 Ma

The final two values in the file-string (before '.SPIN') refer to the atmospheric pCO2 value (ppm), and freshwater flux adjustment (Sv), respectively.

All experiments are run for 10,000 years from 'cold' (no restart).

### model experiments -- 'standard' ############################

./runmuffin.sh muffin.CB.umQ00p0a MS/boscologalazzoetal.2021 0Ma_stnd_280_0.2.SPIN 10000
./runmuffin.sh muffin.CB.umQ02p5a MS/boscologalazzoetal.2021 2_5Ma_stnd_400_0.3.SPIN 10000
./runmuffin.sh muffin.CB.umQ04p5a MS/boscologalazzoetal.2021 4_5Ma_stnd_400_0.5.SPIN 10000
./runmuffin.sh muffin.CB.umQ07p5a MS/boscologalazzoetal.2021 7_5Ma_stnd_800_0.4.SPIN 10000
./runmuffin.sh muffin.CB.umQ10p0a MS/boscologalazzoetal.2021 10Ma_stnd_800_0.3.SPIN 10000
./runmuffin.sh muffin.CB.umQ12p5a MS/boscologalazzoetal.2021 12_5Ma_stnd_1120_0.2.SPIN 10000
./runmuffin.sh muffin.CB.umQ15p0a MS/boscologalazzoetal.2021 15Ma_stnd_1120_0.0.SPIN 10000

### model experiments -- T-dependent ###########################

./runmuffin.sh muffin.CB.umQ00p0a MS/boscologalazzoetal.2021 0Ma_Tdep_280_0.2.SPIN 10000
./runmuffin.sh muffin.CB.umQ02p5a MS/boscologalazzoetal.2021 2_5Ma_Tdep_400_0.3.SPIN 10000
./runmuffin.sh muffin.CB.umQ04p5a MS/boscologalazzoetal.2021 4_5Ma_Tdep_400_0.5.SPIN 10000
./runmuffin.sh muffin.CB.umQ07p5a MS/boscologalazzoetal.2021 7_5Ma_Tdep_800_0.4.SPIN 10000
./runmuffin.sh muffin.CB.umQ10p0a MS/boscologalazzoetal.2021 10Ma_Tdep_800_0.3.SPIN 10000
./runmuffin.sh muffin.CB.umQ12p5a MS/boscologalazzoetal.2021 12_5Ma_Tdep_1120_0.2.SPIN 10000
./runmuffin.sh muffin.CB.umQ15p0a MS/boscologalazzoetal.2021 15Ma_Tdep_1120_0.0.SPIN 10000

################################################################
################################################################
 