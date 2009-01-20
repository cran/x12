ImportDASText <- function() {
  filename <- tclvalue(tkgetOpenFile(filetypes=
            '{"Text Files" {".txt" ".TXT" ".dat" ".DAT" ".csv" ".CSV"}} {"All Files" {"*"}}'))
  if (filename == "") {
            return()
  }
  importDASTextHelper1(filename)
}
importDASTextHelper1 <- function(filename) {
ImportDASText <- function() {
#
# imports a csv file via GUI
#
	# get filename with tk file selector
  filename <- tclvalue(tkgetOpenFile(filetypes=
            '{"Text Files" {".txt" ".TXT" ".dat" ".DAT" ".csv" ".CSV"}} {"All Files" {"*"}}'))
	# check if file was selected
  if (filename == "") {
            return()
  }
  # open first dialog (importing options)
  importDASTextHelper1(filename)
  }
#
# Helper function for ImportDASText
# asks for dataset name and options
#
  initializeDialog(title="Read Data From Text File")
  onFocusChange <- function() {
    print("FocusOut Event")
  }
  pline <- readLines(filename,n=15)


  tkwm.geometry(top,"+0+0")
  screenheight <- as.numeric(tclvalue(tkwinfo("screenheight",top)))
  screenwidth <- as.numeric(tclvalue(tkwinfo("screenwidth",top)))

  mainFrame <- tkframe(top)
  screenwidth<-as.numeric(tclvalue(tkwinfo("screenwidth",top)))
  canvas <- tkcanvas(mainFrame,highlightthickness="0")
  mainInnerFrame <- tkframe(canvas)

  previewFrame <- tkframe(mainInnerFrame )
  tkconfigure(previewFrame,bd=2)
  tkconfigure(previewFrame,relief="raised")
  previewtext<-paste("Preview:", filename, sep=" ")
  tkgrid(tklabel(previewFrame,text=previewtext, justify="left",fg="red"),sticky="w")
  previewFrame2 <- tkframe(previewFrame)
  optionsFrame <- tkframe(top)
  fntmp <- strsplit(basename(filename),".",fixed=T)[[1]]
  dsname <- tclVar(paste(fntmp[length(fntmp)-1],sep=".",collapse="."))
  entryDsname <- tkentry(optionsFrame, width="20", textvariable=dsname)
  headerVariable <- tclVar("0")
  headerCheckBox <- tkcheckbutton(optionsFrame, variable=headerVariable)
  radioButtons(optionsFrame, "delimiter", buttons=c("whitespace", "commas", "tabs"),
      labels=c("White space", "Commas", "Tabs"), title="Field Separator",
      initialValue="commas")
  otherButton <- tkradiobutton(delimiterFrame, variable=delimiterVariable, value="other")
  otherVariable <- tclVar("")
  otherEntry <- tkentry(delimiterFrame, width="4", textvariable=otherVariable)
  radioButtons(optionsFrame, "decimal", buttons=c("period", "comma"),
      labels=c("Period [.]", "Comma [,]"), title="Decimal-Point Character")
  missingVariable <- tclVar("NA")
  missingEntry <- tkentry(optionsFrame, width="8", textvariable=missingVariable)
  onOK <- function() {
        dsnameValue <- tclvalue(dsname)
        if (dsnameValue == ""){
            errorCondition(recall=readDataSet,
                message="You must enter a name for the data set.")
                return()
        }
        if (!is.valid.name(dsnameValue)){
            errorCondition(recall=readDataSet,
                message=paste('"', dsnameValue, '" is not a valid name.', sep=""))
            return()
        }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, "Data set"))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                readDataSet()
                return()
            }
        }
        head <- tclvalue(headerVariable) == "1"
        delimiter <- tclvalue(delimiterVariable)
        del <- if (delimiter == "whitespace") ""
            else if (delimiter == "commas") ","
            else if (delimiter == "tabs") "\\t"
            else tclvalue(otherVariable)
        miss <- tclvalue(missingVariable)
        dec <- if (tclvalue(decimalVariable) == "period") "." else ","

                                tmp <- data.frame()
        if(head) {
                tmp <- read.table(filename,header=TRUE,na.strings=miss,dec=dec,sep=del,strip.white=TRUE)
          line <- readLines(filename,n=1)
                colstr <- strsplit(readLines(filename,n=1),del)[[1]]
        } else {
                tmp <- read.table(filename,header=FALSE,na.strings=miss,dec=dec,sep=del,strip.white=TRUE)
                colstr <- paste("Var",1:length(tmp),sep="")
        }
        tkgrab.release(top)
        tkdestroy(top)
        importDASTextHelper2(dsnameValue,colstr,tmp,filename,del,dec,miss,head,skip=1)
  }
  OKCancelHelp_scroll(helpSubject="read.table")
  tkgrid(tklabel(optionsFrame, text="Enter name for data set:"), entryDsname, sticky="w")
  tkgrid(tklabel(optionsFrame, text="Variable names in file:"), headerCheckBox, sticky="w")
  tkgrid(tklabel(optionsFrame, text="Missing data indicator:"), missingEntry, sticky="w")
  tkgrid(tklabel(delimiterFrame, text="Other"), otherButton,
      tklabel(delimiterFrame, text="  Specify:"), otherEntry, sticky="w")
  tkgrid(delimiterFrame, sticky="w", columnspan=2)
  tkgrid(decimalFrame, sticky="w")
  tkpack(optionsFrame,expand=TRUE,anchor="nw")

  tkpack(previewFrame)
  tkcreate(canvas, "window", 0,0,anchor="nw", window=mainInnerFrame)
  tkpack(mainFrame,expand=TRUE)

  # Scrollbar
#  vscr <- tkscrollbar(mainFrame, repeatinterval=5,
#                         command=function(...)tkyview(canvas,...))
#  hscr <- tkscrollbar(mainFrame, orient="horizontal", repeatinterval=5,
#                         command=function(...)tkxview(canvas,...))



  refreshPreview <- function() {
    s.header <- tclvalue(headerVariable) == "1"
    s.delimiter <- tclvalue(delimiterVariable)
    s.del <- if (s.delimiter == "whitespace") "\\s"
            else if (s.delimiter == "commas") ","
            else if (s.delimiter == "tabs") "\\t"
            else tclvalue(otherVariable)
    if(s.del=="") s.del <- "\\s"
    if(s.del==".") s.del <- "[.]"
    tkgrid.forget(previewFrame2)
    previewFrame2 <- tkframe(previewFrame)
    lp <- length(pline)
    for(i in 1:lp) {
      tmpline <- strsplit(pline[i],s.del)[[1]]
      if(s.header & i==1) {
        prevcommand <- 'tkgrid(tklabel(previewFrame2,text=tmpline[1], justify="left", fg="blue")'
      } else {
        prevcommand <- 'tkgrid(tklabel(previewFrame2,text=tmpline[1], justify="left")'
      }
      if(length(tmpline)>1) {
        for(j in 2:length(tmpline)) {
          if(s.header & i==1) {
            prevcommand <- paste(prevcommand, ', tklabel(previewFrame2,text=tmpline[', as.character(j) ,'], justify="left", fg="blue")', sep="")
          } else {
            prevcommand <- paste(prevcommand, ', tklabel(previewFrame2,text=tmpline[', as.character(j) ,'], justify="left")', sep="")
          }
        }
      }
      prevcommand <- paste(prevcommand, ', sticky="w")',sep="")
      eval(parse(text=prevcommand))
    }
    tkgrid(previewFrame2,row=1)


    tkpack(buttonsFrame, side="bottom",expand=TRUE)
#    tkpack(vscr,side="right",fill="y")
#    tkpack(hscr,side="bottom", fill="x")
    tkpack(canvas, side="bottom", fill="both", expand=TRUE)

 .Tcl("update idletasks")
    mainInnerFramewidth <- as.numeric(tclvalue(tkwinfo("width",mainInnerFrame)))
    mainInnerFrameheight <- as.numeric(tclvalue(tkwinfo("height",mainInnerFrame)))

    tkconfigure(canvas, width=min(mainInnerFramewidth,screenwidth-100), height=min(mainInnerFrameheight,screenheight-100))

    chscrreg <- paste("0 0",as.character(mainInnerFramewidth),as.character(mainInnerFrameheight))
#    tkconfigure(canvas, yscrollcommand=function(...)tkset(vscr,...),scrollregion=chscrreg)
#    tkconfigure(canvas, xscrollcommand=function(...)tkset(hscr,...),scrollregion=chscrreg)


#  eval(parse(text=keyscroll))


#  tkbind(top,"<Key-Down>",Down)
#  tkbind(top,"<Key-Up>",Up)
#  tkbind(top,"<Key-Left>",Left)
#  tkbind(top,"<Key-Right>",Right)
#  tkbind(top,"<Key-Prior>",Prior)
#  tkbind(top,"<Key-Next>",Next)

    .Tcl("update idletasks")
    topheight <- as.numeric(tclvalue(tkwinfo("height",top)))
    topwidth <- as.numeric(tclvalue(tkwinfo("width",top)))
    buttonsheight <- as.numeric(tclvalue(tkwinfo("height",buttonsFrame)))
    optionsFrameheight <- as.numeric(tclvalue(tkwinfo("height",optionsFrame)))

    tkwm.maxsize(top,min(screenwidth-100,topwidth),min(screenheight-100,topheight))
    tkwm.minsize(top,50,buttonsheight+optionsFrameheight+90)

  }

#  tkbind(headerCheckBox,"<ButtonRelease>",refreshPreview)
  tkbind(whitespaceButton,"<ButtonRelease>",refreshPreview)
  tkbind(commasButton,"<ButtonRelease>",refreshPreview)
  tkbind(tabsButton,"<ButtonRelease-1>",refreshPreview)
  tkbind(otherButton,"<ButtonRelease>",refreshPreview)
  tkbind(otherEntry,"<FocusOut>",refreshPreview)

  refreshPreview()
  tkwait.window(top)
  }
  
initializeDialog <- defmacro(window=top, title="", offset=10,
    expr={
        window <- tktoplevel(borderwidth=10)
        tkwm.title(window, title)
#        position <- if (is.SciViews()) -1 else commanderPosition() # +PhG
#        position <- if (any(position < 0)) "-50+50"
#            else paste("+", paste(offset + position, collapse="+"), sep="")
#        tkwm.geometry(window, position)
        }
    )

closeDialog <- defmacro(window=top, release=TRUE,
    expr={
        if (release && GrabFocus()) tkgrab.release(window)
        tkdestroy(window)
        }
    )

dialogSuffix <- defmacro(window=top, onOK=onOK, rows=1, columns=1, focus=top,
    bindReturn=TRUE, preventGrabFocus=FALSE, preventDoubleClick=FALSE,
    expr={
        for (row in 0:(rows-1)) tkgrid.rowconfigure(window, row, weight=0)
        for (col in 0:(columns-1)) tkgrid.columnconfigure(window, col, weight=0)
        .Tcl("update idletasks")
# Rudi (May 09, 2006):
#        tkwm.resizable(window, 0, 0)
        tkwm.resizable(window, 1, 1)
        if (bindReturn) tkbind(window, "<Return>", onOK)
#        if (getRcmdr("double.click") && (!preventDoubleClick)) tkbind(window, "<Double-ButtonPress-1>", onOK)
        tkwm.deiconify(window)
        # focus grabs appear to cause problems for some dialogs
        if (GrabFocus() && (!preventGrabFocus)) tkgrab.set(window)
        tkfocus(focus)
        tkwait.window(window)
        }
    )
is.SciViews <- function() {
    # SciViews defines the option "SciViews.version".
    # So, we test if we are in SciViews this way:
    res <- !is.null(getOption("SciViews.version"))
    res
    }

is.SciViews.TclTk <- function() {
    # Determine if a TclTk-communicating SciViews client is currently running
    res <- (!is.null(getOption("SciViews.TclTk")) && getOption("SciViews.TclTk") == TRUE)
    res
    }
#commanderPosition <- function (){
#   ID <- CommanderWindow()$ID
#   as.numeric(c(tclvalue(.Tcl(paste("winfo rootx", ID))),
#       tclvalue(.Tcl(paste("winfo rooty", ID)))))
#100
#   }
radioButtons <- defmacro(window=top, name, buttons, values=NULL, initialValue=..values[1], labels, title,
# Rudi (Sep 17, 2006):
  side="right",
  expr={
    ..values <- if (is.null(values)) buttons else values
    ..frame <- paste(name, "Frame", sep="")
    assign(..frame, tkframe(window))
    ..variable <- paste(name, "Variable", sep="")
    assign(..variable, tclVar(initialValue))
    tkgrid(tklabel(eval(parse(text=..frame)), text=title, fg="blue"), columnspan=2, sticky="w")
    for (i in 1:length(buttons)) {
      ..button <- paste(buttons[i], "Button", sep="")
      assign(..button,
        tkradiobutton(eval(parse(text=..frame)), variable=eval(parse(text=..variable)), value=..values[i]))
# Rudi (Sep 17, 2006):
      if ( side == "right" )
        tkgrid(tklabel(eval(parse(text=..frame)), text=labels[i], justify="left"),
          eval(parse(text=..button)), sticky="w")
      else
        tkgrid(eval(parse(text=..button)), tklabel(eval(parse(text=..frame)),
          text=labels[i], justify="left"), sticky="w")
      }
    }
  )


OKCancelHelp_onCancel <- defmacro(window=top, helpSubject=NULL, model=F,
expr={
# onCancel function removed
# June 18, 2007, Andreas Alfons
  buttonsFrame <- tkframe(window, borderwidth = 5)
  OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12",
                       command=onOK, default="active", borderwidth=3)
  cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red",
                           width="12", command=onCancel, borderwidth=3)
  if(!is.null(helpSubject)) {
    onHelp <- function() {
      if(GrabFocus() && .Platform$OS.type != "windows")
        tkgrab.release(window)
      if(as.numeric(R.Version()$major) >= 2)
        print(help(helpSubject))
      else help(helpSubject)
      }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12",
                           command=onHelp, borderwidth=3)
    }
  tkgrid(OKbutton, tklabel(buttonsFrame, text="  "), cancelButton,
         tklabel(buttonsFrame, text="            "),
         if(!is.null(helpSubject)) helpButton, sticky="w")
  })

OKCancelHelp <- defmacro(window=top, helpSubject=NULL, model=FALSE,grabset=NULL,
    expr={
# grabset added, Andreas Zainzinger January 23, 2008
        buttonsFrame <- tkframe(window, borderwidth=5)
        OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active",
            borderwidth=3)
        onCancel <- function() {
#            if (model) putRcmdr("modelNumber", getRcmdr("modelNumber") - 1)
            if (GrabFocus()) tkgrab.release(window)
            tkdestroy(window)
            if (!is.null(grabset))
              tkgrab.set(grabset) #Andreas Zainzinger January 23, 2008
            tkfocus(CommanderWindow())
            }
        cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel, borderwidth=3)
        if (!is.null(helpSubject)){
            onHelp <- function() {
                if (GrabFocus() && .Platform$OS.type != "windows") tkgrab.release(window)
#20.05.2008     if (GrabFocus() && .Platform$OS.type != "windows") tkgrab.release(window)
                if (as.numeric(R.Version()$major) >= 2) print(help(helpSubject))
                else help(helpSubject)
                }
            helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp, borderwidth=3)
            }
        tkgrid(OKbutton, tklabel(buttonsFrame, text="  "), cancelButton, tklabel(buttonsFrame, text="            "),
            if (!is.null(helpSubject)) helpButton, sticky="w")
        })

OKCancelHelp_scroll <- defmacro(topwindow=top, window=mainFrame, helpSubject=NULL, model=FALSE, oktext="OK",
# topwindow ... top window
# window ... window taking the OKCancelHelp button
#  Andreas Zainzinger   June 30, 2006
    expr={
#29.6.06#        buttonsFrame <- tkframe(window, borderwidth=5)
# Andreas: topwindow/window added:
        buttonsFrame <- tkframe(window, borderwidth=5)
        OKbutton <- tkbutton(buttonsFrame, text=oktext, fg="darkgreen", width="12", command=onOK, default="active",
            borderwidth=3)
        onCancel <- function() {
#            if (model) putRcmdr("modelNumber", getRcmdr("modelNumber") - 1)
            if (GrabFocus()) tkgrab.release(topwindow)
            tkdestroy(topwindow)
            tkfocus(CommanderWindow())
            }
        cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel, borderwidth=3)
        if (!is.null(helpSubject)){
            onHelp <- function() {
                if (GrabFocus() && .Platform$OS.type != "windows") tkgrab.release(topwindow)
                if (as.numeric(R.Version()$major) >= 2) print(help(helpSubject))
                else help(helpSubject)
                }
            helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp, borderwidth=3)
            }
        tkgrid(OKbutton, tklabel(buttonsFrame, text="  "), cancelButton, tklabel(buttonsFrame, text="            "),
            if (!is.null(helpSubject)) helpButton, sticky="w")
        })

subOKCancelHelp <- defmacro(window=subdialog, helpSubject=NULL,
    expr={
        subButtonsFrame <- tkframe(window, borderwidth=5)
        subOKbutton <- tkbutton(subButtonsFrame, text="OK", fg="darkgreen", width="12", command=onOKsub, default="active",
            borderwidth=3)
        onCancelSub <- function() {
            if (GrabFocus()) tkgrab.release(window)
            tkdestroy(window)
            tkfocus(CommanderWindow())
            }
        subCancelButton <- tkbutton(subButtonsFrame, text="Cancel", fg="red", width="12", command=onCancelSub,
            borderwidth=3)
        if (!is.null(helpSubject)){
            onHelpSub <- function(){
                if (GradFocus() && .Platform$OS.type != "windows") tkgrab.release(window)
                if (as.numeric(R.Version()$major) >= 2) print(help(helpSubject))
                else help(helpSubject)
                }
            subHelpButton <- tkbutton(subButtonsFrame, text="Help", width="12", command=onHelpSub, borderwidth=3)
            }
        tkgrid(subOKbutton, tklabel(subButtonsFrame, text="  "), subCancelButton,
            tklabel(subButtonsFrame, text="            "), if (!is.null(helpSubject)) subHelpButton, sticky="w")
        })

is.valid.name <- function(x){
    length(x) == 1 && is.character(x) && x == make.names(x)
    }

GrabFocus <- function(value){
#    if (missing(value)) getRcmdr("grab.focus")
#    else putRcmdr("grab.focus", value)
    }
    
listDataSets <- function(envir=.GlobalEnv, ...) {
	Vars <- ls(envir = envir, all.names = FALSE) # + PhG
	if (length(Vars) == 0) return(Vars) # + PhG
    names(which(sapply(Vars, function(.x) is.data.frame(eval(parse(text=.x), envir=envir))))) # + PhG
    }

importDASTextHelper2 <-
  function(dataName,coltitles,data,filename,sep,dec,nastr,head,skip) {
#
# Helper function for ImportDASText
# asks for variable type and issues commands
#

  top <- tktoplevel()
  tktitle(top) <- paste("Read Data From Text File:",filename)
  tkwm.geometry(top,"+0+0")

  screenheight <- as.numeric(tclvalue(tkwinfo("screenheight",top)))
  screenwidth <- as.numeric(tclvalue(tkwinfo("screenwidth",top)))

  mainFrame <- tkframe(top)
  canvas <- tkcanvas(mainFrame,highlightthickness="0")
  mainInnerFrame <- tkframe(canvas)

###########################################################################
###########################################################################
###########################################################################
optionsFrame <- tkframe(mainInnerFrame)

  availmodes <- c("logical","integer","double","factor","character")
  stg.modes <- rep("",length(coltitles))
  l <- length(coltitles)

#theFrame <- paste(theName, "Frame", sep="")
  theFrame <- "BoxFrame"
  assign(theFrame, tkframe(optionsFrame))

  for(i in 1:l) {
    firstmode <- 0
    if(is.logical(data[,i])) {firstmode <- 1; curmode <- "logical"}
    if(is.integer(data[,i])) {firstmode <- 2; curmode <- "integer"}
    if(is.double(data[,i])) {firstmode <- 3; curmode <- "double"}
    if(is.factor(data[,i])) {firstmode <- 4; curmode <- "factor"}
    if(is.character(data[,i])) {firstmode <- 5; curmode <- "character"}

    theName <- paste("mode",as.character(i),sep="")

    theVar <- paste(theName, "Variable", sep="")

    if(substr(coltitles[i],1,1)=="*") {
      coltitles[i] <- substr(coltitles[i],2,1000000)
      assign(theVar, tclVar("character"))
          } else {
      assign(theVar, tclVar(curmode))
    }

    radioCommand <- paste("tkgrid(tklabel(", theFrame, ",text=\"", coltitles[i], "\", fg=\"blue\")",sep="")
    for (j in 1:5) {
      if(j<firstmode) {
        labellock <- ", state=\"disabled\""
      } else {
        labellock <- ""
      }
      radioCommand <- paste(radioCommand,", tklabel(", theFrame,",text=availmodes[", as.character(j), "], justify=\"left\"", labellock, ")", sep="")
      ..button <- paste(availmodes[j], "Button", sep="")
      assign(..button, tkradiobutton(eval(parse(text=theFrame)), variable=eval(parse(text=theVar)), value=availmodes[j]))
      if(j<firstmode) {
        tkconfigure(get(..button),state="disabled")
      }
      radioCommand <- paste(radioCommand,", ", ..button, sep="")
    }
    radioCommand <- paste(radioCommand,", sticky=\"w\")",sep="")
    eval(parse(text=radioCommand))
    }

  onOK <- function() {
    colstring <- paste("c('",paste(coltitles,sep="','",collapse="','"),"')",sep="")
#    colclass <- paste("c('",tclvalue(mode1Variable),"'",sep="")
#    cat(tclvalue(curval))
#    for(i in 2:l) {
#      curval <- get(paste("mode",as.character(i),"Variable",sep=""))
#            colclass <- paste(colclass,",'",tclvalue(curval),"'",sep="")
#    }
#    colclass <- paste(colclass,")",sep="")
#    command <- paste('read.table("', filename,'", header=', head,', sep="', sep,'",
#       na.strings="', nastr, '", dec="', dec, '", col.names=', colstring , ', colClasses=',colclass ,', strip.white=TRUE)', sep="")
    command <- paste('read.table("', filename,'", header=', head,', sep="', sep,'",
       na.strings="', nastr, '", dec="', dec, '", col.names=', colstring , ', strip.white=TRUE)', sep="")

  command2 <- paste(dataName, " <<- ", command, sep="")

  eval(parse(text=command2))

#    assign(dataName, justDoIt(command), envir=.GlobalEnv)
#    activeDataSet(dataName)
    tkgrab.release(top)
    tkdestroy(top)
#    tkfocus(CommanderWindow())
    }
  OKCancelHelp_scroll(helpSubject="read.table")
  tkgrid(optionsFrame, sticky="w")
  tkgrid(BoxFrame,sticky="w")

###########################################################################
###########################################################################
###########################################################################


#Scrollbar
#  vscr <- tkscrollbar(mainFrame, repeatinterval=5,
#                       command=function(...)tkyview(canvas,...))
#  hscr <- tkscrollbar(mainFrame, repeatinterval=5,orient="horizontal",
#                       command=function(...)tkxview(canvas,...))


  tkcreate(canvas, "window", 0,0,anchor="nw", window=mainInnerFrame)
  tkpack(mainFrame,expand=TRUE)
  tkpack(buttonsFrame,side="bottom")
#  tkpack(vscr, side="right", fill="y")
#  tkpack(hscr,side="bottom", fill="x")
  tkpack(canvas, side="left", fill="both", expand=TRUE)


  .Tcl("update idletasks")
  mainInnerFrameheight <- as.numeric(tclvalue(tkwinfo("height",mainInnerFrame)))
  mainInnerFramewidth <- as.numeric(tclvalue(tkwinfo("width",mainInnerFrame)))


#size of the canvas
  tkconfigure(canvas, width=min(mainInnerFramewidth,screenwidth-100), height=min(mainInnerFrameheight,screenheight-100))


#scrollregion
  chscrreg <- paste("0 0 ",as.character(mainInnerFramewidth),as.character(mainInnerFrameheight))
#   tkconfigure(canvas, xscrollcommand=function(...)tkset(hscr,...),scrollregion=chscrreg)
#     tkconfigure(canvas, yscrollcommand=function(...)tkset(vscr,...),scrollregion=chscrreg)

#  eval(parse(text=keyscroll))

#  tkbind(top,"<Key-Down>",Down)
#  tkbind(top,"<Key-Up>",Up)
#  tkbind(top,"<Key-Left>",Left)
#  tkbind(top,"<Key-Right>",Right)
#  tkbind(top,"<Key-Prior>",Prior)
#  tkbind(top,"<Key-Next>",Next)

  .Tcl("update idletasks")
  topheight <- as.numeric(tclvalue(tkwinfo("height",top)))
  topwidth <- as.numeric(tclvalue(tkwinfo("width",top)))


  tkwm.maxsize(top, min(screenwidth-100,topwidth), min(screenheight-100,topheight))
  tkwm.minsize(top,50,50)
  tkwait.window(top)
  }
checkReplace <- function(name, type="Variable"){
    tkmessageBox(message=paste(type, " ", name, " already exists.\nOverwrite ",
        tolower(type),"?", sep=""), icon="warning", type="yesno", default="no")
    }
