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
CGENIE_USER_MANUAL=cgenie-user-manual
CGENIE_HOWTO=cgenie-howto
CGENIE_EXAMPLES=cgenie-examples

all : code-doc user-manual cgenie-user-manual cgenie-howto cgenie-examples

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

cgenie-user-manual :
	$(CHECK_FOR_LATEX)
	$(CHECK_FOR_DVIPS)
	$(CHECK_FOR_PS2PDF)
	$(LATEX) $(CGENIE_USER_MANUAL).tex
	$(LATEX) $(CGENIE_USER_MANUAL).tex
	$(DVIPS) -o $(CGENIE_USER_MANUAL).ps $(CGENIE_USER_MANUAL).dvi
	$(PS2PDF) $(PS2PDF_ARGS) $(CGENIE_USER_MANUAL).ps $(CGENIE_USER_MANUAL).pdf

cgenie-howto :
	$(CHECK_FOR_LATEX)
	$(CHECK_FOR_DVIPS)
	$(CHECK_FOR_PS2PDF)
	$(LATEX) $(CGENIE_HOWTO).tex
	$(LATEX) $(CGENIE_HOWTO).tex
	$(DVIPS) -o $(CGENIE_HOWTO).ps $(CGENIE_HOWTO).dvi
	$(PS2PDF) $(PS2PDF_ARGS) $(CGENIE_HOWTO).ps $(CGENIE_HOWTO).pdf

cgenie-examples :
	$(CHECK_FOR_LATEX)
	$(CHECK_FOR_DVIPS)
	$(CHECK_FOR_PS2PDF)
	$(LATEX) $(CGENIE_EXAMPLES).tex
	$(LATEX) $(CGENIE_EXAMPLES).tex
	$(DVIPS) -o $(CGENIE_EXAMPLES).ps $(CGENIE_EXAMPLES).dvi
	$(PS2PDF) $(PS2PDF_ARGS) $(CGENIE_EXAMPLES).ps $(CGENIE_EXAMPLES).pdf

clean:
	\rm -rf html latex
	\rm -f $(USER_MANUAL).dvi $(USER_MANUAL).ps $(USER_MANUAL).pdf
	\rm -f $(USER_MANUAL).log $(USER_MANUAL).aux $(USER_MANUAL).toc $(USER_MANUAL).outsvn revert

clean-cgenie:
	\rm -f cgenie-*.dvi cgenie-*.ps cgenie-*.pdf
	\rm -f cgenie-*.log cgenie-*.aux cgenie-*.toc cgenie-*.out
