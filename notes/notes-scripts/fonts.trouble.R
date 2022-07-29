# Font problem.
## Would not install any fonts outside of the few basic ones.
### error name = No FontName. Skipping.
# solution is as follows

library(extrafont)
library(remotes)
remotes::install_version("Rttf2pt1", version = "1.3.8")
extrafont::font_import()

# to install new fonts available on the CPU
font_import("/Library/Fonts" )

# to check available fonts
fonts()

# check font paths
font.paths()

# font paths
names(pdfFonts())

