##low-level functions
OpenNoteBook <- defmacro(window=tt,notebook=nb,height=400,width=400,expr={
      notebook <- tkwidget(tt, 'NoteBook', height=height,width=width)
      tkgrid(notebook)
})
InsertTab <- defmacro(notebook=nb,frame=nbFrame1,tab="tab1",text="Page 1",expr={
      ID <- tcl(notebook,"insert","end",tab,text=text)
      frame <- .Tk.newwin(ID)
})
RaiseTab <- defmacro(notebook=nb,tab="tab1",expr={
      tcl(notebook,"raise",tab)
})
# listet alle Ts objects auf
listTS <- function(envir=.GlobalEnv, ...) {
  Vars <- ls(envir = envir, all.names = TRUE) 
  if (length(Vars) == 0) return(Vars) 
  names(which(sapply(Vars, function(.x) is.ts(eval(parse(text=.x), envir=envir)))))
}
# Liest die Auswahl aus Listbox ein
getSelection <- function(object) UseMethod("getSelection")
getSelection.listbox <- function(object){
  object$varlist[as.numeric(tkcurselection(object$listbox)) + 1]
}
# Baut ListBox
variableListBox <- function (parentWindow, variableList = Variables(), bg = "white",selectmode = "single",
    export = "FALSE", initialSelection = NULL,
    listHeight = 4, title=NULL, scrollTo=TRUE){
  scroll<-function(...) tkyview(listbox,...)
  if (selectmode == "multiple")
    selectmode <- getRcmdr("multiple.select.mode")
  frame <- tkframe(parentWindow)
  listbox <- tklistbox(frame, height = min(listHeight, length(variableList)),
      selectmode = selectmode, background = bg, exportselection = export)
  scrollbar <- tkscrollbar(frame, repeatinterval = 5, command = scroll )
  tkconfigure(listbox, yscrollcommand = function(...)  tkset(scrollbar,...))
  for (var in variableList)
    tkinsert(listbox, "end", var)
  if (!is.null(initialSelection))
    for (i in initialSelection)
      tkselection.set(listbox,i)
  if ( !is.null(title) )
    tkgrid(tklabel(frame, text = title, fg = "blue"), columnspan = 2, sticky = "w")
  tkgrid(listbox, scrollbar, sticky = "nw")
  tkgrid.configure(scrollbar, sticky = "wns")
  tkgrid.configure(listbox, sticky = "ew")
  result <- list(frame = frame, listbox = listbox, scrollbar = scrollbar,
      selectmode = selectmode, varlist = variableList)
  class(result) <- "listbox"
  if ((!is.null(initialSelection))&&(scrollTo)){
    scrollRatio<-initialSelection[1]/length(variableList)
    scroll("moveto",scrollRatio)
  }
  result
}
#listet alle objects auf       
listObjects <- function(envir=.GlobalEnv, ...) {
  Vars <- ls(envir = envir) # + PhG
  Vars
}
listObjectsVecMatDat <- function(envir=.GlobalEnv, ...) {
  is.vecormatframe <- function(x){
    is.vector(x)||is.data.frame(x)||is.matrix(x)
  }
  Vars <- ls(envir = envir) # + PhG
  Vars
  if (length(Vars) == 0) return(Vars) 
  names(which(sapply(Vars, function(.x) is.vecormatframe(eval(parse(text=.x), envir=envir)))))	  
}

createTsGUI <- function(){
  onOK <- function(){
    onOK2 <- function(){
      Name <- as.character(tkget(Name,"0.0","end"))
      Start1 <- as.numeric(tkget(Start1,"0.0","end"))      
      Start2 <- as.numeric(tkget(Start2,"0.0","end"))      
      Freq <- as.numeric(tkget(Freq,"0.0","end"))
      if(exists("varname")){
        varname <- getSelection(xBox)
        objectname <- paste(objectname,"$",varname,sep="")
      }else if(exists("colnum")){
        colnum <- getSelection(xBox)
        objectname <- paste(objectname,"[,",colnum,"]",sep="")          
      }
      tt <- get("tt", envir = as.environment(x12GUIenv))
      tkdestroy(tt)
      assign("tsname", Name, envir = as.environment(x12GUIenv))
      if(length(Freq)==0){
        tkmessageBox(message="no frequency entered",icon="warning", type="ok")    
        stop("no frequency entered")
      }
      if(length(Start1)>0)
        command <- paste(Name," <<- ts(",objectname,",start=c(",Start1,"),frequency=",Freq,")")
      else if((length(Start1)>0)&&(length(Start2)>0))
        command <- paste(Name," <<- ts(",objectname,",start=c(",Start1,",",Start2,"),frequency=",Freq,")")
      else{
        tkmessageBox(message="no startdate entered",icon="warning", type="ok")    
        stop("no Startdate entered")
      }
      eval(parse(text=command))        
     # cat(command,"\n")
    }
    objectname <- getSelection(xBox)
    tt <- get("tt", envir = as.environment(x12GUIenv))
    tkdestroy(tt)
    tt <- tktoplevel()
    assign("tt",tt, envir = as.environment(x12GUIenv))
    tkwm.title(tt,"Build a ts object")      
    main.frame <- tkframe(tt)
    tkgrid(tklabel(main.frame, text="Name for the ts-object:"),sticky="w")
    Name <- tktext(main.frame, bg = "white", height=1, width=30)
    tkgrid(Name,columnspan=2)
    tkinsert(Name, "end", "TS")      
    tkgrid(tklabel(main.frame, text="           "),sticky="w",columnspan=2)
    if(is.data.frame(eval(parse(text=objectname)))){
      if(dim(eval(parse(text=objectname)))[1]==1)
        eval(parse(text=paste(objectname," <<- as.vector(t(",objectname,"))",sep="")))
      else{
        varname <- TRUE
        xBox <- variableListBox(main.frame, variableList=names(eval(parse(text=objectname))) , title="Variables in data frame (pick one)",listHeight = 6,initialSelection = 0)
        tkgrid(xBox$frame,columnspan=2)
      }
    }else if(is.matrix(eval(parse(text=objectname)))){
      colnum <- TRUE             
      xBox <- variableListBox(main.frame, variableList=as.character(1:dim(eval(parse(text=objectname)))[2]) , title="Columns in matrix (pick one)",listHeight = 6)
      
      
    }
    tkgrid(tklabel(main.frame, text="Start for the ts-object (Year,..):"),sticky="w",columnspan=2)
    Start1 <- tktext(main.frame, bg = "white", height=1, width=10)
    Start2 <- tktext(main.frame, bg = "white", height=1, width=10)
    tkgrid(Start1,Start2)
    tkgrid(tklabel(main.frame, text="Frequency for the ts-object:"),sticky="w",columnspan=2)
    Freq <- tktext(main.frame, bg = "white", height=1, width=10)
    tkgrid(Freq,columnspan=2)
    OKbutton <- tkbutton(main.frame, text="OK", fg="darkgreen", width="12", command=onOK2, default="active", borderwidth=3)
    Cancelbutton <- tkbutton(main.frame, text="Cancel", fg="red", width="12", command=function()tkdestroy(tt), default="active", borderwidth=3)
    tkgrid(OKbutton,Cancelbutton)
    tkgrid(main.frame)
    tkwait.window(tt)      
  }
  
  if(length(listObjectsVecMatDat())>0){
    tt <- tktoplevel()
    assign("tt",tt, envir = as.environment(x12GUIenv))    
    tkwm.title(tt,"Build a ts object")
    main.frame <- tkframe(tt)  
    tkgrid(tklabel(main.frame, text="           "),sticky="w")
    xBox <- variableListBox(main.frame, variableList=lsObjects <- listObjectsVecMatDat(), title="Objects in global environment (pick one)",listHeight = 6,initialSelection = 0)
    tkgrid(xBox$frame,columnspan=2)
    tkgrid(tklabel(main.frame, text="           "),sticky="w")
    OKbutton <- tkbutton(main.frame, text="OK", fg="darkgreen", width="12", command=onOK, default="active", borderwidth=3)
    Cancelbutton <- tkbutton(main.frame, text="Cancel", fg="red", width="12", command=function()tkdestroy(tt), default="active", borderwidth=3)
    tkgrid(OKbutton,Cancelbutton)
    tkgrid(tklabel(main.frame, text="           "),sticky="w")
    tkgrid(main.frame)
    tkwait.window(tt)
  }else
    tkmessageBox(message="No suitable Objects in workspace",icon="warning", type="ok")
}

selectTsGUI <- function(){
  onOKselectTS <- function(){
    tsname <- getSelection(xBox)
    assign("tsname",tsname,envir=as.environment(x12GUIenv))
    tt <- get("tt",envir = as.environment(x12GUIenv))
    tkdestroy(tt)    
  }
  tt <- tktoplevel()
  assign("tt",tt, envir = as.environment(x12GUIenv))    
  tkwm.title(tt,"Select a ts object")
  main.frame <- tkframe(tt)  
  tkgrid(tklabel(main.frame, text="           "),sticky="w")
  xBox <- variableListBox(main.frame, variableList=listTS(), title="Ts objects in global environment (pick one)",listHeight = 6,initialSelection = 0)
  tkgrid(xBox$frame,columnspan=2)
  tkgrid(tklabel(main.frame, text="           "),sticky="w")
  OKbutton <- tkbutton(main.frame, text="OK", fg="darkgreen", width="12", command=onOKselectTS, default="active", borderwidth=3)
  Cancelbutton <- tkbutton(main.frame, text="Cancel", fg="red", width="12", command=function()tkdestroy(tt), default="active", borderwidth=3)
  tkgrid(OKbutton,Cancelbutton)
  tkgrid(tklabel(main.frame, text="           "),sticky="w")
  tkgrid(main.frame)
  tkwait.window(tt)
}


x12PropertiesGUI <- function(){

	
#erstellt eine Tseries object()

	########################################
#MAIN
	onOKproperties <- function() {
    x12pars <- list()
    x12pars$transform <- tclvalue(transValueVariable) 
    x12pars$Td <- tclvalue(TdVariable)==1
    x12pars$Easter <- tclvalue(EasterVariable)==1
    x12pars$aictest <- ""
    if(x12pars$Td)
      x12pars$aictest <- "td"
    if(x12pars$Easter)
      x12pars$aictest <- paste(x12pars$aictest," easter")
    if(x12pars$aictest=="")
      x12pars$aictest <- "NULL"
    else
      x12pars$aictest <- paste("\"",x12pars$aictest,"\"")
    x12pars$FlowTd <- tclvalue(FlowTdVariable)==1
    x12pars$StockTd <- tclvalue(StockTdVariable)==1
    x12pars$StockTdDays <- c(0:31)[as.numeric(tclvalue(tcl(stockTdDaysBox,"getvalue")))+1]
    x12pars$Constant <- tclvalue(ConstantVariable)==1
    x12pars$RegEaster <- tclvalue(EasterRegVariable)==1
    x12pars$EasterDays <- c(1,8,15)[as.numeric(tclvalue(tcl(EasterDaysBox,"getvalue")))+1]
    x12pars$regvariables <- ""
    if(x12pars$FlowTd)
      x12pars$regvariables <- "td"
    else if(x12pars$StockTd)
      x12pars$regvariables <- paste("tdstock[",x12pars$StockTdDays,"]",sep="")
    if(x12pars$RegEaster)
      x12pars$regvariables <- paste(x12pars$regvariables," easter[",x12pars$EasterDays,"]",sep="")
    if(x12pars$Constant)
      x12pars$regvariables <- paste(x12pars$regvariables," const",sep="")
    if(x12pars$regvariables=="")
      x12pars$regvariables <- "NULL"
    else
      x12pars$regvariables <- paste("\"",x12pars$regvariables,"\"" )
    x12pars$reguser <- "NULL"
    x12pars$AO <- tclvalue(AOVariable)==1
    x12pars$LS <- tclvalue(LSVariable)==1
    x12pars$TC <- tclvalue(TCVariable)==1
    x12pars$outlier <- ""
    if(x12pars$AO)
      x12pars$outlier <- "AO"
    if(x12pars$LS)
      x12pars$outlier <- paste(x12pars$outlier," LS",sep="")
    if(x12pars$TC)
      x12pars$outlier <- paste(x12pars$outlier," TC",sep="")
    if(x12pars$outlier=="")
      x12pars$outlier <- "NULL"
    else
      x12pars$outlier <- paste("\"",x12pars$outlier,"\"")
    x12pars$outlierspan <- c(as.numeric(tkget(span_start,"0.0","end")),as.numeric(tkget(span_end,"0.0","end")))
    if(length(x12pars$outlierspan)==2)
      x12pars$outlierspan <- paste("c(",x12pars$outlierspan[1],",",x12pars$outlierspan[2],")",sep="")
    else
      x12pars$outlierspan <- "NULL"
    x12pars$ModelSelect <- tclvalue(ModelSelectValueVariable)
    x12pars$maxOrder <- paste("c(",1+as.numeric(tclvalue(tcl(maxOrderBox1,"getvalue"))),",",1+as.numeric(tclvalue(tcl(maxOrderBox2,"getvalue"))),")",sep="")
    x12pars$maxDiff <- paste("c(",1+as.numeric(tclvalue(tcl(maxDiffBox1,"getvalue"))),",",1+as.numeric(tclvalue(tcl(maxDiffBox2,"getvalue"))),")",sep="")
    if(x12pars$ModelSelect!="auto"){
      x12pars$arima <- paste("c(",1+as.numeric(tclvalue(tcl(arima1Box,"getvalue"))),","
        ,1+as.numeric(tclvalue(tcl(arima2Box,"getvalue"))),","
        ,1+as.numeric(tclvalue(tcl(arima3Box,"getvalue"))),")",sep="")
      x12pars$sarima <- paste("c(",1+as.numeric(tclvalue(tcl(sarima1Box,"getvalue"))),","
        ,1+as.numeric(tclvalue(tcl(sarima2Box,"getvalue"))),","
        ,1+as.numeric(tclvalue(tcl(sarima3Box,"getvalue"))),")",sep="")
    }else{
      x12pars$arima <- "NULL"
      x12pars$sarima <- "NULL"
    }
    x12pars$FctYears <- 1+as.numeric(tclvalue(tcl(FctYearsBox,"getvalue")))
    x12pars$regFile <- as.character(tkget(regText,"0.0","end"))
    if(length(x12pars$regFile)==0)
      x12pars$regFile <- "NULL"
    x12pars$regFormat <- tclvalue(tcl(FormatsBox,"getvalue"))
    x12pars$regStartDate <- as.numeric(tkget(regStartDate,"0.0","end"))
    if(length(x12pars$regStartDate)==0)
      x12pars$regStartDate <- "NULL"
    x12pars$numbReg <- 1+as.numeric(tclvalue(tcl(NumberRegBox,"getvalue")))
    x12pars$adj <- tclvalue(AdjValueVariable)
    x12pars$sigma <- c(as.numeric(tkget(sigmaLim1,"0.0","end")),as.numeric(tkget(sigmaLim2,"0.0","end")))
    if(length(x12pars$sigma!=2))
      x12pars$sigma <- paste("c(",x12pars$sigma[1],",",x12pars$sigma[2],")",sep="")
    else
      x12pars$sigma <- "NULL"
    x12pars$Seatsparameter <- as.numeric(tkget(Seatsparameter,"0.0","end"))
    x12pars$slidingSpan <- tclvalue(slidingSpan)==1
    if(exists("x12path", envir = as.environment(x12GUIenv)))
      x12path <- get("x12path", envir = as.environment(x12GUIenv))
    if(!exists("x12path")){
      x12path <- ""
      x12path <- tclvalue(tkgetOpenFile(title="Please choose the x12 binaries",filetypes = "{{x12 executable} {.exe}} {{All files} *}"))
      #x12path <- paste(tclvalue(tkchooseDirectory(title = "Please choose the directory of the x12 binaries")),"/",sep="")
      assign("x12path",x12path, envir = as.environment(x12GUIenv))
    }
    if(exists("tsname",env=as.environment(x12GUIenv)))
      tsname <- get("tsname",env=as.environment(x12GUIenv))
    else{
      tkmessageBox(message="No time series selected",icon="warning", type="ok")
      stop("No time series selected")
    }
    x12pars$Seats <- x12pars$adj=="Seats"
    x12pars$onlytd <- x12pars$adj=="td"
    outName2 <- tkget(outName,"0.0","end")
    command <- paste(outName2,"<<- x12(tso=",tsname,",transform=\"",x12pars$transform,"\",arima=",x12pars$arima,
        ",sarima=",x12pars$sarima,",automdl=",x12pars$ModelSelect=="auto",
        ",maxorder=",x12pars$maxOrder,",maxdiff=",x12pars$maxDiff,
        ",regvariables=",paste(x12pars$regvariables,collapse=" "),",reguser=",x12pars$reguser,
        ",regfile=",x12pars$regFile,",regfilestart=",x12pars$regStartDate,",regfileformat=",x12pars$regFormat,
        ", x12path=\"",x12path,"\",seats=",x12pars$Seats,",seatsparameter=",x12pars$Seatsparameter,
        ", sigmalim=",x12pars$sigma,
        ",outlier=",x12pars$outlier,",outlier_span=",x12pars$outlierspan,
        ",forecast_years=",x12pars$FctYears,",slidingspans=",x12pars$slidingspans,
        ",aictest=",x12pars$aictest,
        ",onlytd=",x12pars$onlytd,
        ")",sep="")
       #ccc<<-command
    cat(command,"\n")
    eval(parse(text=command))
    assign("x12pars",x12pars, envir = as.environment(x12GUIenv))
    eval(parse(text=paste("plot(",outName2,")",sep="")))
#		tt <- tktoplevel()
#		assign("tt",tt, envir = as.environment(x12GUIenv))
#		tkwm.title(tt,"Properties for X12")
#		main.frame <- tkframe(tt)  
#		tkgrid(tklabel(main.frame, text="           "),sticky="w")
#		tkgrid(tklabel(main.frame, text="           "),sticky="w")
#		plotTF <- tclVar(TRUE)
#		plotCheckbox <- tkcheckbutton(main.frame, variable=plotTF)
#		tkgrid(tklabel(main.frame, text="Plot: "),plotCheckbox,sticky="w")
#		tkgrid(tklabel(main.frame, text="           "),sticky="w")
#		OutName <- tktext(main.frame, bg = "white", height=1, width=20)
#		tkgrid(tklabel(main.frame, text="Name of Output-object:"),OutName,sticky="w")
#		tkinsert(OutName, "end","out")
#		OKbutton <- tkbutton(main.frame, text="OK", fg="darkgreen", width="12", command=onOKout, default="active", borderwidth=3)
#		Cancelbutton <- tkbutton(main.frame, text="Cancel", fg="red", width="12", command=function()tkdestroy(tt), default="active", borderwidth=3)
#		tkgrid(OKbutton,Cancelbutton)
#		tkgrid(tklabel(main.frame, text="           "),sticky="w")
#		tkgrid(main.frame)
	}

  tt <- tktoplevel()
	assign("tt",tt, envir = as.environment(x12GUIenv))  
	tkwm.title(tt,"Properties for X12")
  ### Notebook
  OpenNoteBook()
  InsertTab(frame=nbpradj,tab="tab1",text="Prior Adjustments")
  InsertTab(frame=nbarima,tab="tab2",text="ARIMA model")
  InsertTab(frame=nbreg,tab="tab3",text="User Regressors")
  InsertTab(frame=nbseasonal,tab="tab4",text="Seasonal Adjustment")
  RaiseTab(tab="tab1")
  ###Main Frame
  main.frame <- tkframe(tt)
  tkgrid(main.frame)
  
  if(exists("x12pars",envir=as.environment(x12GUIenv)))
    x12pars <- get("x12pars",envir=as.environment(x12GUIenv))
  
  ### First Tab
  ##Transform
  if(exists("x12pars")){
    ini <- x12pars$transform
  }else{
    ini <- "auto"
  }
  radioButtons(nbpradj, "transValue", buttons=c("log", "none", "auto"),
      labels=c("log", "none", "auto"), title="Transformation",
      initialValue=ini)
  tkgrid(transValueFrame)
  tkgrid(tklabel(nbpradj, text="        "))
  tkgrid(tklabel(nbpradj, text="Regression", fg="blue"))
  tkgrid(tklabel(nbpradj, text="AIC test (The specific TD or Easter variables to test can be \n set in the 'Regression variables' box bellow."))
  if(exists("x12pars"))
    TdVariable <- tclVar(x12pars$Td) 
  else
    TdVariable <- tclVar("0")
  checkFrame <- tkframe(nbpradj)
  TdCheckBox <- tkcheckbutton(checkFrame, variable=TdVariable)
  if(exists("x12pars"))
    EasterVariable <- tclVar(x12pars$Easter)
  else
    EasterVariable <- tclVar("0")
  EasterCheckBox <- tkcheckbutton(checkFrame, variable=EasterVariable)
  tkgrid(tklabel(checkFrame,text="TD:"),TdCheckBox,tklabel(checkFrame,text="Easter:"),EasterCheckBox,sticky="w")
  tkgrid(checkFrame)
  tkgrid(tklabel(nbpradj, text="        "))
  tkgrid(tklabel(nbpradj, text="Regression variables", fg="blue"))
  checkFrame2 <- tkframe(nbpradj)
  if(exists("x12pars"))
    FlowTdVariable <- tclVar(x12pars$FlowTd)
  else
    FlowTdVariable <- tclVar("0")
  FlowTdCheckBox <- tkcheckbutton(checkFrame2, variable=FlowTdVariable)
  if(exists("x12pars"))
    StockTdVariable <- tclVar(x12pars$StockTd)
  else
    StockTdVariable <- tclVar("0")
  StockTdCheckBox <- tkcheckbutton(checkFrame2, variable=StockTdVariable)
  stockTdDays <- as.character(0:31)
  if(exists("x12pars"))
    iniStockTdDays <- tclVar(as.character(x12pars$StockTdDays))
  else
    iniStockTdDays <- tclVar("31")
  stockTdDaysBox <- tkwidget(checkFrame2, "ComboBox", editable=FALSE,
      values=stockTdDays, width=3, textvariable=iniStockTdDays)
  tkgrid(tklabel(checkFrame2,text="Flow TD:"),FlowTdCheckBox,tklabel(checkFrame2,text="Stock TD"),StockTdCheckBox,stockTdDaysBox)
  if(exists("x12pars"))
    ConstantVariable <- tclVar(x12pars$Constant)
  else
    ConstantVariable <- tclVar("0")
  ConstantCheckBox <- tkcheckbutton(checkFrame2, variable=ConstantVariable)
  if(exists("x12pars"))
    EasterRegVariable <- tclVar(x12pars$RegEaster)
  else
    EasterRegVariable <- tclVar("0")
  EasterRegCheckBox <- tkcheckbutton(checkFrame2, variable=EasterRegVariable)
  EasterDays <- as.character(c(1,8,15))
  if(exists("x12pars"))
    iniEasterDays <- tclVar(as.character(x12pars$EasterDays))
  else
    iniEasterDays <- tclVar("1")
  EasterDaysBox <- tkwidget(checkFrame2, "ComboBox", editable=FALSE,
      values=EasterDays, width=3, textvariable=iniEasterDays)
  tkgrid(tklabel(checkFrame2,text="Constant:"),ConstantCheckBox,tklabel(checkFrame2,text="Easter:"),EasterRegCheckBox,EasterDaysBox)
  tkgrid(checkFrame2)
  tkgrid(tklabel(nbpradj, text="        "))
  tkgrid(tklabel(nbpradj, text="Outliers", fg="blue"))
  tkgrid(tklabel(nbpradj, text="Types of Outlier to Test for:"))
  checkFrame3 <- tkframe(nbpradj)
  if(exists("x12pars"))
    AOVariable <- tclVar(x12pars$AO)
  else
    AOVariable <- tclVar("0")
  AOCheckBox <- tkcheckbutton(checkFrame3, variable=AOVariable)
  if(exists("x12pars"))
    LSVariable <- tclVar(x12pars$LS)
  else
    LSVariable <- tclVar("0")
  LSCheckBox <- tkcheckbutton(checkFrame3, variable=LSVariable)
  if(exists("x12pars"))
    TCVariable <- tclVar(x12pars$TC)
  else
    TCVariable <- tclVar("0")
  TCCheckBox <- tkcheckbutton(checkFrame3, variable=TCVariable)
  tkgrid(tklabel(checkFrame3,text="AO:"),AOCheckBox,tklabel(checkFrame3,text="LS:"),LSCheckBox,tklabel(checkFrame3,text="TC:"),TCCheckBox)
  tkgrid(checkFrame3)
  span_start <- tktext(checkFrame3,height=1,width=7)
  span_end <- tktext(checkFrame3,height=1,width=7)
  if(exists("x12pars")){
    if(length(x12pars$outlier_span)==2){
      tkinsert(span_start, "end",x12pars$outlier_span[1])
      tkinsert(span_end, "end",x12pars$outlier_span[2])
    }
  }
  tkgrid(tklabel(checkFrame3,text="Span start:"),span_start,tklabel(checkFrame3,text="    "),tklabel(checkFrame3,text="    "),tklabel(checkFrame3,text="Span end:"),span_end)
  
  
  ### Second Tab
  
  if(exists("x12pars")){
  	if(x12pars$ModelSelect=="auto"){
      iniSelect <- "auto"      
    }else
      iniSelect <- "ARIMA"
  }else
    iniSelect <- "auto"
  radioButtons(nbarima, "ModelSelectValue", buttons=c("auto","ARIMA"),
      labels=c("Select model automatically","Use ARIMA model specified"), title="ARIMA model",
      initialValue=iniSelect)
  tkgrid(ModelSelectValueFrame)
  arimaFrame <- tkframe(nbarima)
  tkgrid(arimaFrame)
  maxOrder <- as.character(c(0:5))
  if(exists("x12pars"))
    inimaxOrder1 <- tclVar(as.character(as.numeric(strsplit(x12pars$maxOrder,"")[[1]][3])-1))
  else
    inimaxOrder1 <- tclVar("1")
  if(exists("x12pars"))
    inimaxOrder2 <- tclVar(as.character(as.numeric(strsplit(x12pars$maxOrder,"")[[1]][5])-1))
  else
    inimaxOrder2 <- tclVar("1")
  maxOrderBox1 <- tkwidget(arimaFrame, "ComboBox", editable=FALSE,
      values=maxOrder, width=3, textvariable=inimaxOrder1)
  maxOrderBox2 <- tkwidget(arimaFrame, "ComboBox", editable=FALSE,
      values=maxOrder, width=3, textvariable=inimaxOrder2)
  maxDiff <- as.character(c(0:2))
  if(exists("x12pars"))
    inimaxDiff1 <- tclVar(as.character(as.numeric(strsplit(x12pars$maxDiff,"")[[1]][3])-1))
  else
    inimaxDiff1 <- tclVar("0")
  if(exists("x12pars"))
    inimaxDiff2 <- tclVar(as.character(as.numeric(strsplit(x12pars$maxDiff,"")[[1]][5])-1))
  else
    inimaxDiff2 <- tclVar("0")
  maxDiffBox1 <- tkwidget(arimaFrame, "ComboBox", editable=FALSE,
      values=maxDiff, width=3, textvariable=inimaxDiff1)
  maxDiffBox2 <- tkwidget(arimaFrame, "ComboBox", editable=FALSE,
      values=maxDiff, width=3, textvariable=inimaxDiff2)
  tkgrid(tklabel(arimaFrame,text="Max. order for automdl:"),maxOrderBox1,maxOrderBox2)
  tkgrid(tklabel(arimaFrame,text="Max. diff. for automdl:"),maxDiffBox1,maxDiffBox2)
  
  arima1 <- as.character(c(0:20))
  arimaBoxFrame <- tkframe(arimaFrame)
  sarimaBoxFrame <- tkframe(arimaFrame)
  if(exists("x12pars")){
    if(x12pars$arima!="NULL")
      iniArima <- as.character(as.numeric(strsplit(paste(strsplit(x12pars$arima,"")[[1]][-c(1:2,length(strsplit(x12pars$arima,"")[[1]]))],collapse=""),",")[[1]])-1)
    else
      iniArima <-rep(1,3)
  }else
    iniArima <-rep(1,3)
  arima1Box <- tkwidget(arimaBoxFrame, "ComboBox", editable=FALSE,
      values=arima1, width=2, textvariable=tclVar(iniArima[1]))
  arima2Box <- tkwidget(arimaBoxFrame, "ComboBox", editable=FALSE,
      values=arima1, width=2, textvariable=tclVar(iniArima[2]))
  arima3Box <- tkwidget(arimaBoxFrame, "ComboBox", editable=FALSE,
      values=arima1, width=2, textvariable=tclVar(iniArima[3]))
  if(exists("x12pars")){
    if(x12pars$sarima!="NULL")
      iniSarima <- as.character(as.numeric(strsplit(paste(strsplit(x12pars$sarima,"")[[1]][-c(1:2,length(strsplit(x12pars$sarima,"")[[1]]))],collapse=""),",")[[1]])-1)
    else
      iniSarima <-as.character(rep(1,3))
  }else
    iniSarima <-as.character(rep(1,3))
  sarima1Box <- tkwidget(sarimaBoxFrame, "ComboBox", editable=FALSE,
      values=arima1, width=2, textvariable=tclVar(iniSarima[1]))
  sarima2Box <- tkwidget(sarimaBoxFrame, "ComboBox", editable=FALSE,
      values=arima1, width=2, textvariable=tclVar(iniSarima[2]))
  sarima3Box <- tkwidget(sarimaBoxFrame, "ComboBox", editable=FALSE,
      values=arima1, width=2, textvariable=tclVar(iniSarima[3]))
  tkgrid(arima1Box,arima2Box,arima3Box)
  tkgrid(sarima1Box,sarima2Box,sarima3Box)
  tkgrid(tklabel(arimaFrame, text="Arima e.g. (1,0,1) :"),arimaBoxFrame,sticky="w")
  tkgrid(tklabel(arimaFrame, text="Sarima e.g. (1,0,1) :"),sarimaBoxFrame,sticky="w")  
	tkgrid(tklabel(nbarima, text="        "))
  tkgrid(tklabel(nbarima, text="Forecasts", fg="blue"))
  fctFrame <- tkframe(nbarima)
  tkgrid(fctFrame)
  FctYears <- as.character(c(0:5))
  if(exists("x12pars"))
    iniFctYears <- tclVar(as.character(x12pars$FctYears-1))
  else
    iniFctYears <- tclVar("0")
  FctYearsBox <- tkwidget(fctFrame, "ComboBox", editable=FALSE,
      values=FctYears, width=3, textvariable=iniFctYears)
  tkgrid(tklabel(fctFrame,text="Number of years to forecast:"),FctYearsBox)
  
  ### Third Tab
  regDataFrame <- tkframe(nbreg)
  tkgrid(regDataFrame)
  choose_regfile <- function() {
    filename <- tclvalue(tkgetOpenFile(filetypes='{"All Files" {"*"}}'))
    regText <- get("regText",envir=as.environment(x12GUIenv))
    tkinsert(regText, "end",filename)
    #regfile <<- filename
  }
  regbutton <- tkbutton(regDataFrame, text="Choose File", fg="black", width="20", command=choose_regfile, default="active", borderwidth=3)  
  regText <- tktext(regDataFrame, bg = "white", height=1, width=50)
  if(exists("x12pars")){
    if(x12pars$regFile!="NULL")
      tkinsert(regText, "end",x12pars$regFile)
  }
  assign("regText",regText,envir=as.environment(x12GUIenv))
  tkgrid(tklabel(regDataFrame,text="Data file:"),regbutton)
  tkgrid(regText,columnspan=3)
  tkgrid(tklabel(regDataFrame,text="       "))
  Formats <- c("Datevalue","free","x12save")
  if(exists("x12pars"))
  iniFormats <- tclVar(Formats[as.numeric(x12pars$regFormat)+1])
  else
    iniFormats <- tclVar("free")
  FormatsBox <- tkwidget(regDataFrame, "ComboBox", editable=FALSE,
      values=Formats, width=10, textvariable=iniFormats)
  tkgrid(tklabel(regDataFrame,text="Format:"),FormatsBox)
  regStartDate <- tktext(regDataFrame, bg = "white", height=1, width=10)
  if(exists("x12pars")){
    if(x12pars$regStartDate!="NULL")
      tkinsert(regStartDate, "end",x12pars$regStartDate)
  }
  tkgrid(tklabel(regDataFrame,text="Start date:"),regStartDate)
  NumberReg <- as.character(c(1:52))
  if(exists("x12pars"))
    iniNumberReg <- tclVar(as.character(x12pars$numbReg))
  else
    iniNumberReg <- tclVar("1")
  NumberRegBox <- tkwidget(regDataFrame, "ComboBox", editable=FALSE,
      values=NumberReg, width=3, textvariable=iniNumberReg)
  tkgrid(tklabel(regDataFrame,text="How many regressors are in the file?"),NumberRegBox)
  
  
  ### Fourth Tab
  if(exists("x12pars"))
    iniAdj <- x12pars$adj
  else
    iniAdj <- "x11"
  radioButtons(nbseasonal, "AdjValue", buttons=c("x11","td","seats","none"),
      labels=c("x11","Trading day and holiday adjustment only","Seats","none"), title="Type of adjustment",
      initialValue=iniAdj)
  tkgrid(AdjValueFrame)
  seasonalFrame <- tkframe(nbseasonal)
  tkgrid(seasonalFrame)
  tkgrid(tklabel(seasonalFrame,text="       "))
  tkgrid(tklabel(nbseasonal,text="x11 filter options",fg="blue"),columnspan=3,sticky="w")
  sigmaFrame <- tkframe(nbseasonal)
  tkgrid(sigmaFrame)
  sigmaLim1 <- tktext(sigmaFrame, bg = "white", height=1, width=5)
  sigmaLim2 <- tktext(sigmaFrame, bg = "white", height=1, width=5)
  if(exists("x12pars")){
    if(x12pars$sigma!="NULL"){
      iniSigma <- strsplit(paste(strsplit(x12pars$sigma,"")[[1]][-c(1,2,length(strsplit(x12pars$sigma,"")[[1]]))],collapse=""),",")[[1]]
      tkinsert(sigmaLim1,"end",iniSigma[1])
      tkinsert(sigmaLim2,"end",iniSigma[2])
    }
  }
  tkgrid(tklabel(sigmaFrame,text="Sigma limit:  "),sigmaLim1,tklabel(sigmaFrame,text="  "),sigmaLim2)
  tkgrid(tklabel(nbseasonal,text="    "))
  tkgrid(tklabel(nbseasonal,text="Seats parameter",fg="blue"),columnspan=3,sticky="w")
  Seatsparameter <- tktext(nbseasonal, bg = "white", height=1, width=20)
  if(exists("x12pars")){
    if(length(x12pars$Seatsparameter)>0){
      tkinsert(Seatsparameter,"end",x12pars$Seatsparameter)
    }
  }
  tkgrid(Seatsparameter)
  tkgrid(tklabel(nbseasonal,text="    "))
  tkgrid(tklabel(nbseasonal,text="Stability diagnostics",fg="blue"),columnspan=3,sticky="w")
  if(exists("x12pars"))
    slidingSpan <- tclVar(x12pars$slidingSpan)
  else
    slidingSpan <- tclVar(FALSE)
  slidingSpanCheckBox <- tkcheckbutton(nbseasonal, variable=slidingSpan)
  tkgrid(tklabel(nbseasonal,text="Sliding spans:"),slidingSpanCheckBox)
   
	 
	   
  ###Bottom Frame
  outFrame <- tkframe(main.frame)
  outName <- tktext(outFrame,height=1,width=20)
  tkinsert(outName, "end","x12Out")
  tkgrid(outFrame,columnspan=2)
  tkgrid(tklabel(outFrame,text="Name for the x12 object in R"),outName)
  
  OKbutton <- tkbutton(main.frame, text="OK", fg="darkgreen", width="12", command=onOKproperties, default="active", borderwidth=3)
	Cancelbutton <- tkbutton(main.frame, text="Cancel", fg="red", width="12", command=function()tkdestroy(tt), default="active", borderwidth=3)
  Helpbutton <- tkbutton(main.frame, text="Help", fg="blue", width="12", command=function()print(help(X12GUI)), default="active", borderwidth=3)
	tkgrid(OKbutton,Cancelbutton,Helpbutton)
	tkgrid(tklabel(main.frame, text="           "),sticky="w")
	tkgrid(main.frame)
}

X12GUI <- function(){
  if(!exists("wd", envir = as.environment(x12GUIenv))){
    wsdir <- tclvalue(tkchooseDirectory(title = "Please choose a working directory, where all X12 Outputfiles will be stored"))
    setwd(wsdir)
    assign("wd",TRUE, envir = as.environment(x12GUIenv))
  }
  require(tcltk)
	createTS <- function(){
		tt <- get("tt", envir = as.environment(x12GUIenv))
		tkdestroy(tt)
    createTsGUI()
    selectTsGUI()
    x12PropertiesGUI()
	}
	readFile <- function(){
		tt <- get("tt", envir = as.environment(x12GUIenv))
		tkdestroy(tt)
    ImportDASText()
    createTsGUI()
    selectTsGUI()
    x12PropertiesGUI()
	}
	useTS <- function(){
		tt <- get("tt", envir = as.environment(x12GUIenv))
		tkdestroy(tt)
    selectTsGUI()
		x12PropertiesGUI()
	}  
	tt <- tktoplevel()
	assign("tt",tt, envir = as.environment(x12GUIenv))
	tkwm.title(tt,"Properties for X12")
	main.frame <- tkframe(tt)  
	createTSbutton <- tkbutton(main.frame, text="Create a TS object from a dataframe or vector", fg="blue", width="50", command=createTS, default="active", borderwidth=3)
	readFilebutton <- tkbutton(main.frame, text="Create a TS object from a text file", fg="blue", width="50", command=readFile, default="active", borderwidth=3)
	useTSbutton <- tkbutton(main.frame, text="Use an existing TS object", fg="blue", width="50", command=useTS, default="active", borderwidth=3)  
	tkgrid(tklabel(main.frame, text="           "),sticky="w")
	tkgrid(readFilebutton)
	tkgrid(tklabel(main.frame, text="           "),sticky="w")
	tkgrid(createTSbutton)
	tkgrid(tklabel(main.frame, text="           "),sticky="w")
	tkgrid(useTSbutton)
	tkgrid(tklabel(main.frame, text="           "),sticky="w")
	tkgrid(main.frame)
}
