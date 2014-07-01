library(fgui)
library(rpanel)
library(gWidgets)
## Not run: 
## Create a form with tcltk routines
main <- tktoplevel()
## Create some widgets for that form
## - Create a frame, and put two widgets in it
## - Note that guiTextEntry objects will be gridded automatically
##   (which is why as an example they are put in a frame)
fr <- guiFrame( sframe=main )
te1 <- guiTextEntry( sframe=fr, text="Text entry 1", default="default" )
te2 <- guiTextEntry( sframe=fr, text="Text entry 2", default="" )
## - Put the rest of the widgets on the main frame
sl <- guiSlider( sframe=main, text="Slider", default=5, min=1, max=10 )
fl <- guiFilename( sframe=main, text="Filename", default="foo.txt" )
op <- guiOption( sframe=main, text="Option", choices=c("one","two","three") )
ed <- guiEdit( sframe=main, text="Edit", default="Edit box" )
## Now grid the widgets on the main form
tkgrid( fr )
tkgrid.configure( fr, sticky="nws" ) ## Handle alignment, as in tcl/tk package
tkgrid( sl$guiObject )
tkgrid( fl$guiObject )
tkgrid( op$guiObject )
tkgrid( ed$guiObject )
tt <- read.table(tclvalue(fl$object), header=T, sep=",") ## will print out "foo.txt", unless modified

train <- read.table('cs-training1.csv', header=T, sep=",")
## End(Not run)
