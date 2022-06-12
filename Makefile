# You want latexmk to *always* run, because make does not have all the info.
# Also, include non-file targets in .PHONY so they are run regardless of any
# file of the given name existing.
.PHONY: ACC ACGA ADM CC CV GAP GTS HS MIOP PM TAN VC VD all clean

# The first rule in a Makefile is the one executed by default ("make"). It
# should always be the "all" rule, so that "make" and "make all" are identical.
all: ACC ACGA ADM CC CV GAP GTS HS MIOP PM TAN VC VD

ACC: #Notas.pdf
ACGA: ACGA.pdf
ADM: #Nada
CC: #Nada
CV: CV.pdf
GAP: #ResumenGAP.pdf
GTS: GTS.pdf
HS: HS.pdf
MIOP: MIOP.pdf
PM: PM.pdf
TAN: TAN.pdf
VC: #Nada
VD: VD.pdf

# MAIN LATEXMK RULE

# -pdf tells latexmk to generate PDF directly (instead of DVI).
# -pdflatex="" tells latexmk to call a specific backend with specific options.
# -use-make tells latexmk to call make for generating missing files.

# -interaction=nonstopmode keeps the pdflatex backend from stopping at a
# missing file reference and interactively asking you for an alternative.

LATEXMK = latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make
ACGA.pdf: ACGA/ACGA.tex
	cd ACGA && $(LATEXMK) ACGA.tex

CV.pdf: CV/CV.tex
	cd CV && $(LATEXMK) CV.tex

GTS.pdf: GTS/GTS.tex
	cd GTS && $(LATEXMK) GTS.tex

HS.pdf: HS/HS.tex
	cd HS && $(LATEXMK) HS.tex

MIOP.pdf: MIOP/MIOP.tex
	cd MIOP && $(LATEXMK) MIOP.tex

PM.pdf: PM/PM.tex
	cd PM && $(LATEXMK) PM.tex

TAN.pdf: TAN/TAN.tex
	cd TAN && $(LATEXMK) TAN.tex

VD.pdf: VD/VD.tex
	cd VD && $(LATEXMK) VD.tex
