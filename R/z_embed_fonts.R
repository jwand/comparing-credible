library(grDevices)
CM <- Type1Font(family="CM",
                c(file.path("/Users","wand","Library","texmf","cm-lgc","fonts","afm","public","cm-lgc",
                            c("fcmr8a.afm","fcmb8a.afm","fcmri8a.afm","fcmbi8a.afm")),
                  file.path("/Users","wand","Library","texmf","R","cmsyase.afm")))
pdfFonts(CM = CM)
postscriptFonts(CM = CM)

embedCM <- function(name){
  embedFonts(file=name,
             fontpaths=c(file.path("/Users","wand","Library","texmf","cm-lgc","fonts","type1","public","cm-lgc"),
               file.path("/Users","wand","Library","texmf","R")))
}
