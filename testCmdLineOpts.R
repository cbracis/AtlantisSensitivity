#!/usr/bin/Rscript

suppressMessages(library(docopt))

doc = "Usage: runProcessOutput.R [-h] [--atl ATLANTISDIR] [--out OUTDIR] (SIMID)

Options:
-a --atl ATLANTISDIR  Atlantis root directory [default: ~/simu]
-o --out OUTDIR       output directory root [default: /home1/scratch/rgirardi/output]
-h --help             show this help text"

opt = docopt(doc)

print(names(opt))
print(opt$SIMID)
print(opt$atl)
print(opt$out)