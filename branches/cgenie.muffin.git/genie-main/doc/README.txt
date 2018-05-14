README
------


Documentation can be generated from the TEX source in both postscript (.ps) and Adobe (.pdf) formats.


To build documentation from the TEX source - from this (~/genie-main/doc) directory issue at the commentd line ($):

$ make user-manual
Builds the general GENIE user manual. [CURRENTLY VERY OUT-OF-DATE ...]

$ make genie-user-manual
Builds the user manual specific to the genie trunk of GENIE-1.

$ make genie-tutorial
Builds documentation describing various tutorials and example model configurations and experimental designs.

$ make genie-howto
The HOW-TO documentation (potted explanations of how to get stuff done).


To clean up previously build documentation:

$ make clean
Cleans the built user-manual documentation files.

$ make clean-genie
Cleans all the built documentation files for cupcake.

