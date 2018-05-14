
# Makefile to manage the downloading of external datasets required for
# the interpolation of model-evaluation datasets onto the model grid
# from original dataservers. This makefile requires 'wget' to be
# installed on the system

# The datasets 'air.2m.mon.mean.nc', 'shum.2m.mon.mean.nc',
# 'air.mon.mean.nc', and 'rhum.mon.mean.nc' represent monthly 2m air
# temperature, 2m relative humidity, air temperature at various
# pressure levels, and relative humidity at various pressure levels,
# respectively, and are from the NCEP/DOE 2 Reanalysis [Kanamitsu et
# al., 2002]. This makefile downloads monthly fields derived from the
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

WGET=wget

LOGFILE=./download_datasets.log

DATASETS_NCEP-DOE_R2_surface=air.2m.mon.mean.nc shum.2m.mon.mean.nc
DATASETS_NCEP-DOE_R2_pressure=air.mon.mean.nc rhum.mon.mean.nc

DATASETS=$(DATASETS_NCEP-DOE_R2_surface) $(DATASETS_NCEP-DOE_R2_pressure)

all: download_datasets

.PHONY: all cleanall force_cleanall force_download download_datasets

cleanall:
	@echo "Not removing previously downloaded datasets (if any)!"

force_cleanall:
	\rm -f $(DATASETS)
	\rm -rf ./tmp
	\rm -f $(LOGFILE)

force_download:
	$(MAKE) force_cleanall
	$(MAKE) all

download_datasets: $(DATASETS)

$(DATASETS_NCEP-DOE_R2_surface):
	@echo
	@echo "downloading file "$@":"
	@touch $(LOGFILE)
	@echo >> ${LOGFILE}
	@echo >> ${LOGFILE}
	@echo "================================================================================" >> $(LOGFILE)
	@echo `date` >> ${LOGFILE}
	@echo "downloading file "$@ >> ${LOGFILE}
	@echo "--------------------------------------------------------------------------------" >> $(LOGFILE)
	@if [ ! -d ./tmp ]; then mkdir ./tmp; fi
	$(WGET) --progress=dot:mega -c ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis2.derived/gaussian_grid/$@ -a $(LOGFILE) -P ./tmp
	@mv ./tmp/$@ ./

$(DATASETS_NCEP-DOE_R2_pressure):
	@echo
	@echo "downloading file "$@":"
	@touch $(LOGFILE)
	@echo >> ${LOGFILE}
	@echo >> ${LOGFILE}
	@echo "================================================================================" >> $(LOGFILE)
	@echo `date` >> ${LOGFILE}
	@echo "downloading file "$@ >> ${LOGFILE}
	@echo "--------------------------------------------------------------------------------" >> $(LOGFILE)
	@if [ ! -d ./tmp ]; then mkdir ./tmp; fi
	$(WGET) --progress=dot:mega -c ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis2.derived/pressure/$@ -a $(LOGFILE) -P ./tmp
	@mv ./tmp/$@ ./
