listX12 <- function(envir=.GlobalEnv, ...) {
  Vars <- ls(envir = envir, all.names = TRUE) 
  if (length(Vars) == 0) return(Vars) 
  names(which(sapply(Vars, function(.x) is.x12(eval(parse(text=.x), envir=envir)))))
}
is.x12 <- function (x) 
  inherits(x, "x12") && length(x)
selectx12Out <- function(){
  onOKselectX12 <- function(){
    x12name <- getSelection(xBox)
    assign("x12name",x12name,envir=as.environment(x12GUIenv))
    ttsx12 <- get("ttsx12",envir = as.environment(x12GUIenv))
    tkdestroy(ttsx12)    
  }
  ttsx12 <- tktoplevel()
  assign("ttsx12",ttsx12, envir = as.environment(x12GUIenv))    
  tkwm.title(ttsx12,"Select a x12 object")
  main.frame <- tkframe(ttsx12)  
  tkgrid(tklabel(main.frame, text="           "),sticky="w")
  if(length(listX12())==0){
    stop("Please run X12GUI() first to create an x12 Output object!")    
  }
  xBox <- variableListBox(main.frame, variableList=listX12(), title="Ts objects in global environment (pick one)",listHeight = 6,initialSelection = 0)
  tkgrid(xBox$frame,columnspan=2)
  tkgrid(tklabel(main.frame, text="           "),sticky="w")
  OKbutton <- tkbutton(main.frame, text="OK", fg="darkgreen", width="12", command=onOKselectX12, default="active", borderwidth=3)
  Cancelbutton <- tkbutton(main.frame, text="Cancel", fg="red", width="12", command=function()tkdestroy(ttsx12), default="active", borderwidth=3)
  tkgrid(OKbutton,Cancelbutton)
  tkgrid(tklabel(main.frame, text="           "),sticky="w")
  tkgrid(main.frame)
  tkwait.window(ttsx12)
}  

PlotX12GUI <- function(){
  if(!exists("x12name",envir = as.environment(x12GUIenv))){
    selectx12Out()
    x12Out1 <- get("x12name",envir = as.environment(x12GUIenv))
  }else
    x12Out1 <- get("x12name",envir = as.environment(x12GUIenv))
  ## Main
  ttpx12 <- tktoplevel()
  assign("ttpx12",ttpx12, envir = as.environment(x12GUIenv))  
  tkwm.title(ttpx12,"Plots X12")
  ### Notebook
  OpenNoteBook(window=ttpx12)
  InsertTab(frame=nb1,tab="tab1",text="Original,Adjusted,Trend")
  InsertTab(frame=nb2,tab="tab2",text="Seasonal Factors")
  InsertTab(frame=nb3,tab="tab3",text="Spectrum")
  RaiseTab(tab="tab1")
  ###Main Frame
  main.frame <- tkframe(ttpx12)
  tkgrid(main.frame)
  
  ##First Tab
  onPlot1 <- function(){
    out <- eval(parse(text=x12Out1))
    plot_original_seasonal_trend(out,
        original=tclvalue(OriginalVariable)==1,
        trend=tclvalue(TrendVariable)==1,
        seasonaladj=tclvalue(SeasonalVariable)==1,
        col_original=tclvalue(inicol_org),
        col_seasonaladj=tclvalue(inicol_sea),
        col_trend=tclvalue(inicol_tre),
        lwd_original=tclvalue(inilwd_org),
        lwd_seasonaladj=tclvalue(inilwd_sea),
        lwd_trend=tclvalue(inilwd_tre),
        main=paste(as.character(tkget(main_1,"0.0","end")),collapse=" "),
        plot_legend=tclvalue(LegendVariable)==1,
        log=tclvalue(LogVariable)==1
    )
    gg<<-tkget(main_1,"0.0","end")
  }
  tkgrid(tklabel(nb1,text="  "))
  tkgrid(tklabel(nb1,text="Plot the Original Series:",fg="blue"),columnspan=5)
  OriginalVariable <- tclVar("1")
  OriginalCheckBox <- tkcheckbutton(nb1, variable=OriginalVariable)
  lab_col_org <- tklabel(nb1,text="    Color:   ")
  inicol_org <- tclVar("black")
  colorsp <- colors()
  col_org <- tkwidget(nb1, "ComboBox", editable=FALSE,
      values=colorsp, width=20, textvariable=inicol_org)
  lab_lwd_org <- tklabel(nb1,text=" Line Width: ")
  lwd_values <- as.character(1:10)
  inilwd_org <- tclVar("1")
  lwd_org <- tkwidget(nb1, "ComboBox", editable=FALSE,
      values=lwd_values, width=3, textvariable=inilwd_org)
  tkgrid(OriginalCheckBox, lab_col_org, col_org, lab_lwd_org, lwd_org)
  tkgrid(tklabel(nb1,text="  "))
  tkgrid(tklabel(nb1,text="Plot the seasonally adjusted Series:",fg="blue"),columnspan=5)
  SeasonalVariable <- tclVar("1")
  SeasonalCheckBox <- tkcheckbutton(nb1, variable=SeasonalVariable)
  lab_col_sea <- tklabel(nb1,text="    Color:   ")
  inicol_sea <- tclVar("blue")
  col_sea <- tkwidget(nb1, "ComboBox", editable=FALSE,
      values=colorsp, width=15, textvariable=inicol_sea)
  lab_lwd_sea <- tklabel(nb1,text=" Line Width: ")
  inilwd_sea <- tclVar("1")
  lwd_sea <- tkwidget(nb1, "ComboBox", editable=FALSE,
      values=lwd_values, width=3, textvariable=inilwd_sea)
  tkgrid(SeasonalCheckBox, lab_col_sea, col_sea, lab_lwd_sea, lwd_sea)
  tkgrid(tklabel(nb1,text="  "))
  tkgrid(tklabel(nb1,text="Plot the estimated Trend:",fg="blue"),columnspan=5)
  TrendVariable <- tclVar("1")
  TrendCheckBox <- tkcheckbutton(nb1, variable=TrendVariable)
  lab_col_tre <- tklabel(nb1,text="    Color:   ")
  inicol_tre <- tclVar("green")
  col_tre <- tkwidget(nb1, "ComboBox", editable=FALSE,
      values=colorsp, width=15, textvariable=inicol_tre)
  lab_lwd_tre <- tklabel(nb1,text=" Line Width: ")
  inilwd_tre <- tclVar("1")
  lwd_tre <- tkwidget(nb1, "ComboBox", editable=FALSE,
      values=lwd_values, width=3, textvariable=inilwd_tre)
  tkgrid(TrendCheckBox, lab_col_tre, col_tre, lab_lwd_tre, lwd_tre)
  tkgrid(tklabel(nb1,text="  "))
  tkgrid(tklabel(nb1,text="Main:"))
  main_1 <- tktext(nb1,height=1,width=50)
  tkinsert(main_1, "end","Original Series, Seasonally Adjusted Series and Trend")
  tkgrid(main_1,columnspan=5)
  tkgrid(tklabel(nb1,text="  "))
  LegendVariable <- tclVar("1")
  LegendCheckBox <- tkcheckbutton(nb1, variable=LegendVariable)
  tkgrid(tklabel(nb1,text="Legend:  "),LegendCheckBox)
  tkgrid(tklabel(nb1,text="  "))
  LogVariable <- tclVar("0")
  LogCheckBox <- tkcheckbutton(nb1, variable=LogVariable)
  tkgrid(tklabel(nb1,text="Log:  "),LogCheckBox)
  tkgrid(tklabel(nb1,text="  "))
  plot1Button <- tkbutton(nb1, text="Plot", fg="green3", width="12", command=onPlot1, default="active", borderwidth=3)
  tkgrid(plot1Button,columnspan=5)
  ##Second Tab
  onPlot2 <- function(){
    out <- eval(parse(text=x12Out1))
    plot_seasonal_factors(out,
        SI_Ratios=tclvalue(SIVariable)==1,
        col_seasonal=tclvalue(inicol_sf),
        col_mean=tclvalue(inicol_sm),
        col_siratio=tclvalue(inicol_si),
        col_replaced=tclvalue(inicol_sr),
        SI_Ratios_replaced=tclvalue(SRVariable)==1,
        plot_legend=tclvalue(Legend2Variable)==1,
        lwd_seasonal=tclvalue(inilwd_sf),
        lwd_mean=tclvalue(inilwd_sm),
        cex_siratio=as.numeric(tkget(cex_si,"0.0","end")),
        cex_replaced=as.numeric(tkget(cex_sr,"0.0","end")),
        #main=paste(as.character(tkget(main_2,"0.0","end")),collapse=" ")
    )
  }
  tkgrid(tklabel(nb2,text="  "))
  tkgrid(tklabel(nb2,text="Seasonal Factors:",fg="blue"),columnspan=5)
  lab_col_sf <- tklabel(nb2,text="    Color:   ")
  inicol_sf <- tclVar("black")
  col_sf <- tkwidget(nb2, "ComboBox", editable=FALSE,
      values=colorsp, width=15, textvariable=inicol_sf)
  lab_lwd_sf <- tklabel(nb2,text=" Line Width: ")
  inilwd_sf <- tclVar("1")
  lwd_sf <- tkwidget(nb2, "ComboBox", editable=FALSE,
      values=lwd_values, width=3, textvariable=inilwd_sf)
  tkgrid(lab_col_sf,col_sf,lab_lwd_sf, lwd_sf,tklabel(nb2,text=" "))
  tkgrid(tklabel(nb2,text="  "))
  tkgrid(tklabel(nb2,text="Mean:",fg="blue"),columnspan=5)
  lab_col_sm <- tklabel(nb2,text="    Color:   ")
  inicol_sm <- tclVar("blue")
  col_sm <- tkwidget(nb2, "ComboBox", editable=FALSE,
      values=colorsp, width=15, textvariable=inicol_sm)
  lab_lwd_sm <- tklabel(nb2,text=" Line Width: ")
  inilwd_sm <- tclVar("1")
  lwd_sm <- tkwidget(nb2, "ComboBox", editable=FALSE,
      values=lwd_values, width=3, textvariable=inilwd_sm)
  tkgrid(lab_col_sm,col_sm,lab_lwd_sm, lwd_sm,tklabel(nb2,text=" "))
  tkgrid(tklabel(nb2,text="  "))
  
  lab_cex_si <- tklabel(nb2,text="  cex:  ")
  cex_si <- tktext(nb2,height=1,width=6)
  tkinsert(cex_si, "end","0.9")  
  SIVariable <- tclVar("1")
  SICheckBox <- tkcheckbutton(nb2, variable=SIVariable)
  lab_col_si <- tklabel(nb2,text="    Color:   ")
  inicol_si <- tclVar("darkgreen")
  col_si <- tkwidget(nb2, "ComboBox", editable=FALSE,
      values=colorsp, width=15, textvariable=inicol_si)
  tkgrid(tklabel(nb2,text="with SI Ratios:",fg="blue"),columnspan=5)
  tkgrid(SICheckBox,lab_col_si,col_si,lab_cex_si,cex_si)
  tkgrid(tklabel(nb2,text="  "))
  lab_cex_sr <- tklabel(nb2,text="  cex:  ")
  cex_sr <- tktext(nb2,height=1,width=6)
  tkinsert(cex_sr, "end","0.9")  
  SRVariable <- tclVar("1")
  SRCheckBox <- tkcheckbutton(nb2, variable=SRVariable)
  lab_col_sr <- tklabel(nb2,text="    Color:   ")
  inicol_sr <- tclVar("red")
  col_sr <- tkwidget(nb2, "ComboBox", editable=FALSE,
      values=colorsp, width=15, textvariable=inicol_sr)
  tkgrid(tklabel(nb2,text="with replaced SI Ratios:",fg="blue"),columnspan=5)
  tkgrid(SRCheckBox,lab_col_sr,col_sr,lab_cex_sr,cex_sr)
  #tkgrid(tklabel(nb2,text="  "))
  #main_2 <- tktext(nb2,height=1,width=50)
  #tkinsert(main_2, "end","Seasonal Factors by period and SI Ratios")
  #tkgrid(main_2,columnspan=5)
  tkgrid(tklabel(nb2,text="  "))
  Legend2Variable <- tclVar("1")
  Legend2CheckBox <- tkcheckbutton(nb2, variable=Legend2Variable)
  tkgrid(tklabel(nb2,text="Legend:  "),Legend2CheckBox)
  tkgrid(tklabel(nb2,text="  "))
  plot2Button <- tkbutton(nb2, text="Plot", fg="green3", width="12", command=onPlot2, default="active", borderwidth=3)
  tkgrid(plot2Button,columnspan=5)
  #Third Tab
  onPlot3 <- function(){
    out <- eval(parse(text=x12Out1))
    plot_spectrum(out,
      which=tclvalue(spectrumValueVariable),
      col_bar=tclvalue(inicol_bar),
      col_seasonal=tclvalue(inicol_seasonal),
      col_td=tclvalue(inicol_td),
      lwd_bar=tclvalue(inilwd_bar),
      lwd_seasonal=tclvalue(inilwd_seasonal),
      lwd_td=tclvalue(inilwd_td),
      plot_legend=tclvalue(Legend3Variable)==1
    )
  }  
  radioButtons(nb3, "spectrumValue", buttons=c("seasonaladj","original","irregular","residuals"),
    labels=c("Spectrum of the Seasonally Adjusted Series",
        "Spectrum of the Original Series",
        "Spectrum of the Irregular",
        "Spectrum of the RegARIMA Residuals"), title="Spectrum:",
    initialValue="seasonaladj")
  tkgrid(spectrumValueFrame,columnspan=5)
  tkgrid(tklabel(nb3,text=" "))
  tkgrid(tklabel(nb3,text="Bar:",fg="blue"),columnspan=5)
  lab_col_bar <- tklabel(nb3,text="    Color:   ")
  inicol_bar <- tclVar("darkgrey")
  col_bar <- tkwidget(nb3, "ComboBox", editable=FALSE,
      values=colorsp, width=15, textvariable=inicol_bar)
  lab_lwd_bar <- tklabel(nb3,text=" Line Width: ")
  inilwd_bar <- tclVar("4")
  lwd_bar <- tkwidget(nb3, "ComboBox", editable=FALSE,
      values=lwd_values, width=3, textvariable=inilwd_bar)
  tkgrid(lab_col_bar,col_bar,lab_lwd_bar, lwd_bar,tklabel(nb3,text=" "))
  tkgrid(tklabel(nb3,text="  "))
  tkgrid(tklabel(nb3,text="Seasonal:",fg="blue"),columnspan=5)
  lab_col_seasonal <- tklabel(nb3,text="    Color:   ")
  inicol_seasonal <- tclVar("red")
  col_seasonal <- tkwidget(nb3, "ComboBox", editable=FALSE,
      values=colorsp, width=15, textvariable=inicol_seasonal)
  lab_lwd_seasonal <- tklabel(nb3,text=" Line Width: ")
  inilwd_seasonal <- tclVar("1")
  lwd_seasonal <- tkwidget(nb3, "ComboBox", editable=FALSE,
      values=lwd_values, width=3, textvariable=inilwd_seasonal)
  tkgrid(lab_col_seasonal,col_seasonal,lab_lwd_seasonal, lwd_seasonal,tklabel(nb3,text=" "))
  tkgrid(tklabel(nb3,text="  "))  
  tkgrid(tklabel(nb3,text="Trading Day:",fg="blue"),columnspan=5)
  lab_col_td <- tklabel(nb3,text="    Color:   ")
  inicol_td <- tclVar("blue")
  col_td <- tkwidget(nb3, "ComboBox", editable=FALSE,
      values=colorsp, width=15, textvariable=inicol_td)
  lab_lwd_td <- tklabel(nb3,text=" Line Width: ")
  inilwd_td <- tclVar("1")
  lwd_td <- tkwidget(nb3, "ComboBox", editable=FALSE,
      values=lwd_values, width=3, textvariable=inilwd_td)
  tkgrid(lab_col_td,col_td,lab_lwd_td, lwd_td,tklabel(nb3,text=" "))
  Legend3Variable <- tclVar("1")
  Legend3CheckBox <- tkcheckbutton(nb3, variable=Legend3Variable)
  tkgrid(tklabel(nb3,text="Legend:  "),Legend3CheckBox)
  tkgrid(tklabel(nb3,text="  "))
  plot3Button <- tkbutton(nb3, text="Plot", fg="green3", width="12", command=onPlot3, default="active", borderwidth=3)
  tkgrid(plot3Button,columnspan=5)
  ##Bottom Frame
  Cancelbutton <- tkbutton(main.frame, text="Cancel", fg="red", width="12", command=function()tkdestroy(ttpx12), default="active", borderwidth=3)
  Helpbutton <- tkbutton(main.frame, text="Help", fg="blue", width="12", command=function()print(help(PlotX12GUI)), default="active", borderwidth=3)
  tkgrid(Cancelbutton,Helpbutton)
  tkgrid(tklabel(main.frame, text="           "),sticky="w")
  tkgrid(main.frame)
  
  
}
