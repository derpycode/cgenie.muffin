
# makefile to manage the downloading of external datasets required for
# the interpolation of model-evaluation datasets onto the model grid
# from original dataservers. This makefile requires 'wget' to be
# installed on the system

# datasets 't00an1.nc' and 's00an1.nc': objectively-analysed
# annual-mean climatologies of ocean temperature [Locarnini et al.,
# 2006] and objectively-analysed annual-mean climatology of ocean
# salinity [Antonov et al., 2006] of the World Ocean Atlas 2005
# (WOA05; http://www.nodc.noaa.gov/OC5/WOA05/pr_woa05.html) from the
# National Oceanographic Data Center (NODC;
# http://www.nodc.noaa.gov/).

#
# References:
#
# Locarnini R. A., A. V. Mishonov, J. I. Antonov, T. P. Boyer, and
# H. E. Garcia (2006), World Ocean Atlas 2005, Volume 1:
# Temperature. S. Levitus, Ed. NOAA Atlas NESDIS 61, U.S. Government
# Printing Office, Washington, D.C., 182 pp.
#
# Antonov J. I., R. A. Locarnini, T. P. Boyer, A. V. Mishonov, and
# H. E. Garcia (2006), World Ocean Atlas 2005, Volume 2:
# Salinity. S. Levitus, Ed. NOAA Atlas NESDIS 62, U.S. Government
# Printing Office, Washington, D.C., 182 pp.
#

WGET=wget

LOGFILE=./download_datasets.log

DATASETS_WOA05=t00an1.nc s00an1.nc

DATASETS=$(DATASETS_WOA05)

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

$(DATASETS_WOA05):
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
	$(WGET) --progress=dot:mega -c ftp://ftp.nodc.noaa.gov/pub/data.nodc/woa/WOA05nc/annual/$@ -a $(LOGFILE) -P ./tmp
	@mv ./tmp/$@ ./
