# ==== RULES FOR AUTOMATIC DOCUMENTATION =====

LATEX=latex
DVIPS=dvips
PS2PDF=ps2pdf
DOXYGEN=doxygen

PS2PDF_ARGS=-dMaxSubsetPct=100 -dCompatibilityLevel=1.2 \
            -dSubsetFonts=true -dEmbedAllFonts=true \
            -dAutoFilterColorImages=false \
            -dAutoFilterGrayImages=false \
            -dColorImageFilter=/FlateEncode \
            -dGrayImageFilter=/FlateEncode \
            -dModoImageFilter=/FlateEncode -sPAPERSIZE=a4

CHECK_FOR_LATEX= @ $(SHELL) -ec 'which $(LATEX) > /dev/null'
CHECK_FOR_DVIPS= @ $(SHELL) -ec 'which $(DVIPS) > /dev/null'
CHECK_FOR_PS2PDF= @ $(SHELL) -ec 'which $(PS2PDF) > /dev/null'
CHECK_FOR_PERL= @ $(SHELL) -ec 'which $(PERL) > /dev/null'
CHECK_FOR_DOXYGEN= @ $(SHELL) -ec 'which $(DOXYGEN) > /dev/null'

USER_MANUAL=user-manual
GENIE_USER_MANUAL=genie-user-manual
GENIE_HOWTO=genie-howto
GENIE_TUTORIAL=genie-tutorial

all : code-doc user-manual genie-user-manual genie-howto genie-tutorial

code-doc :
	$(CHECK_FOR_DOXYGEN)
	cd ../..; $(DOXYGEN) genie-main/doc/Doxyfile

user-manual :
	$(CHECK_FOR_LATEX)
	$(CHECK_FOR_DVIPS)
	$(CHECK_FOR_PS2PDF)
	$(LATEX) $(USER_MANUAL).tex
	$(LATEX) $(USER_MANUAL).tex
	$(DVIPS) -o $(USER_MANUAL).ps $(USER_MANUAL).dvi
	$(PS2PDF) $(PS2PDF_ARGS) $(USER_MANUAL).ps $(USER_MANUAL).pdf

genie-user-manual :
	$(CHECK_FOR_LATEX)
	$(CHECK_FOR_DVIPS)
	$(CHECK_FOR_PS2PDF)
	$(LATEX) $(GENIE_USER_MANUAL).tex
	$(LATEX) $(GENIE_USER_MANUAL).tex
	$(DVIPS) -o $(GENIE_USER_MANUAL).ps $(GENIE_USER_MANUAL).dvi
	$(PS2PDF) $(PS2PDF_ARGS) $(GENIE_USER_MANUAL).ps $(GENIE_USER_MANUAL).pdf

genie-howto :
	$(CHECK_FOR_LATEX)
	$(CHECK_FOR_DVIPS)
	$(CHECK_FOR_PS2PDF)
	$(LATEX) $(GENIE_HOWTO).tex
	$(LATEX) $(GENIE_HOWTO).tex
	$(DVIPS) -o $(GENIE_HOWTO).ps $(GENIE_HOWTO).dvi
	$(PS2PDF) $(PS2PDF_ARGS) $(GENIE_HOWTO).ps $(GENIE_HOWTO).pdf

genie-tutorial :
	$(CHECK_FOR_LATEX)
	$(CHECK_FOR_DVIPS)
	$(CHECK_FOR_PS2PDF)
	$(LATEX) $(GENIE_TUTORIAL).tex
	$(LATEX) $(GENIE_TUTORIAL).tex
	$(DVIPS) -o $(GENIE_TUTORIAL).ps $(GENIE_TUTORIAL).dvi
	$(PS2PDF) $(PS2PDF_ARGS) $(GENIE_TUTORIAL).ps $(GENIE_TUTORIAL).pdf

clean:
	\rm -rf html latex
	\rm -f $(USER_MANUAL).dvi $(USER_MANUAL).ps $(USER_MANUAL).pdf
	\rm -f $(USER_MANUAL).log $(USER_MANUAL).aux $(USER_MANUAL).toc $(USER_MANUAL).outsvn revert

clean-genie:
	\rm -f genie-*.dvi genie-*.ps genie-*.pdf
	\rm -f genie-*.log genie-*.aux genie-*.toc genie-*.out
