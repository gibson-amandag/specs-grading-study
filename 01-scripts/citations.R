if(!require(grateful))install.packages('grateful')
library(grateful)

cite_packages(out.dir = ".")            # save report to working directory
cite_packages(pkgs = "ggplot2", out.dir=".", out.file = "ggplot2_out", bib.file = "ggplot2_bib")

scan_packages()
