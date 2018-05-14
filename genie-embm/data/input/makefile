
#
# Preprocess input datasets which have previously been downloaded to
# ../download/ to facilitate the use of the datasets by the model
#
# this makefile requires NetCDF Operators (NCO;
# http://nco.sourceforge.net) to be installed on the system.
#

# NCO commands
NCKS=ncks
NCWA=ncwa
NCAP2=ncap2

PREPROCESSED_DATASETS_NCEP-DOE_R2_surface=NCEP-DOE_Reanalysis_2_ltaa_surface_T_q.nc
PREPROCESSED_DATASETS_NCEP-DOE_R2_1000mb=NCEP-DOE_Reanalysis_2_ltaa_1000mb_T_r.nc
DATASETS_NCEP-DOE_R2_surface=air.2m.mon.mean.nc shum.2m.mon.mean.nc
DATASETS_NCEP-DOE_R2_pressure=air.mon.mean.nc rhum.mon.mean.nc

vpath %.mon.mean.nc ../download

PREPROCESSED_DATASETS=$(PREPROCESSED_DATASETS_NCEP-DOE_R2_surface) $(PREPROCESSED_DATASETS_NCEP-DOE_R2_1000mb)

all: $(PREPROCESSED_DATASETS)

.PHONY: all cleanall

cleanall:
	\rm -f $(PREPROCESSED_DATASETS)

# Preprocess previously downloaded datasets of monthly 2m air
# temperature and monthly 2m specific humidity from the NCEP/DOE 2
# Reanalysis [Kanamitsu et al., 2002] by computing long term annual
# averages (ltaa) over 31 years from 1979 to 2009, removing obsolete
# dimension level, and collating the resulting two fields into one
# file. The preprocessing uses monthly fields derived from the
# NCEP/DOE 2 Ranalysis data provided by the NOAA/OAR/ESRL PSD,
# Boulder, Colorado, USA, from their Web site at
# http://www.esrl.noaa.gov/psd/.
#
# Reference:
#
# Kanamitsu M., W. Ebisuzaki, J. Woollen, S-K Yang, J.J. Hnilo,
# M. Fiorino, and G. L. Potter (2002), NCEP-DOE AMIP-II Reanalysis
# (R-2), Bul. Atmos. Met. Soc., 1631-1643.
#
$(PREPROCESSED_DATASETS_NCEP-DOE_R2_surface): $(DATASETS_NCEP-DOE_R2_surface)
# i) compute length of months in hours using the boundaries of the time
# axis
# ii) restrict data to only include 372 months (1979 to the end of 2009)
# iii) remove dimension 'level'
# iv) compute weighted average using length of months as weights
# v) collate variables into one file
	\rm -f ./$@
	for file in $^; do \
	    filename=`basename $${file}`; \
	    varname=$${filename%%.*}; \
	    $(NCAP2) -O -s "nhour[time]=time_bnds(:,1)-time_bnds(:,0)" $$file ./$@.tmp; \
	    $(NCKS) -O -d time,0,371 ./$@.tmp ./$@.tmp; \
	    $(NCWA) -O -a level ./$@.tmp ./$@.tmp; \
	    $(NCWA) -O -a time -v $$varname -w nhour ./$@.tmp ./$@.tmp; \
	    $(NCKS) -A -v $$varname ./$@.tmp ./$@; \
	    \rm ./$@.tmp; \
	done

# Preprocess previously downloaded datasets of monthly air temperature
# and monthly relative humidity on various pressure levels from the
# NCEP/DOE 2 Reanalysis [Kanamitsu et al., 2002] by computing long
# term annual averages (ltaa) over 31 years from 1979 to 2009,
# selecting the 1000mb layer only, and collating the resulting two
# fields into one file. The preprocessing uses monthly fields derived
# from the NCEP/DOE 2 Ranalysis data provided by the NOAA/OAR/ESRL
# PSD, Boulder, Colorado, USA, from their Web site at
# http://www.esrl.noaa.gov/psd/.
#
# Reference:
#
# Kanamitsu M., W. Ebisuzaki, J. Woollen, S-K Yang, J.J. Hnilo,
# M. Fiorino, and G. L. Potter (2002), NCEP-DOE AMIP-II Reanalysis
# (R-2), Bul. Atmos. Met. Soc., 1631-1643.
#
$(PREPROCESSED_DATASETS_NCEP-DOE_R2_1000mb): $(DATASETS_NCEP-DOE_R2_pressure)
# i) compute length of months in hours using the boundaries of the time
# axis
# ii) restrict data to only include 372 months (1979 to the end of 2009)
# iii) extract 1000mb layer
# iv) compute weighted average using length of months as weights
# v) collate variables into one file
	\rm -f ./$@
	for file in $^; do \
	    filename=`basename $${file}`; \
	    varname=$${filename%%.*}; \
	    $(NCAP2) -O -s "nhour[time]=time_bnds(:,1)-time_bnds(:,0)" $$file ./$@.tmp; \
	    $(NCKS) -O -d time,0,371 ./$@.tmp ./$@.tmp; \
	    $(NCKS) -O -d level,0,0 ./$@.tmp ./$@.tmp; \
	    $(NCWA) -O -a level ./$@.tmp ./$@.tmp; \
	    $(NCWA) -O -a time -v $$varname -w nhour ./$@.tmp ./$@.tmp; \
	    $(NCKS) -A -v $$varname ./$@.tmp ./$@; \
	    \rm ./$@.tmp; \
	done
