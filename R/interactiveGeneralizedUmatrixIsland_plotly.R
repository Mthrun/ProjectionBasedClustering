interactiveGeneralizedUmatrixIsland_plotly <- function(Umatrix, Bestmatches=NULL, Cls=NULL){
  # Imx = interactiveGeneralizedUmatrixIsland(Umatrix, Bestmatches, Cls)
  #
  # INPUT
  # Umatrix
  # Bestmatches
  # Cls
  # OUTPUT
  # island          the generated Imx
  # Author: FL, MT, QS

  #requireRpackage("shiny")
  #requireRpackage("shinyjs")
  #requireRpackage("png")
  #requireRpackage("tcltk")
  #FilePath<-getwd() #MT: sonst funktionierts nicht
  
  outplot=NULL
  ##########
  # Shiny Fenster ----
  ##########
  UmatrixUi = fluidPage(
    useShinyjs(),
    sidebarLayout(position="right",
                  mainPanel(plotly::plotlyOutput("Plot", width = "auto"),
                            width = 8, height = "100%"),
                  div(style="max-width:1150px", # die sidebar ist dann auf 33% davon beschränkt
                      sidebarPanel(
                        # busy balken
                        tags$head(tags$style(type="text/css", "
                                             #loadmessage {
                                             position: fixed;
                                             top: 0px;
                                             left: 0px;
                                             width: 100%;
                                             padding: 5px 0px 5px 0px;
                                             text-align: center;
                                             font-weight: bold;
                                             font-size: 100%;
                                             color: #000000;
                                             background-color: #CCFF66;
                                             z-index: 105;}")),
                        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                         tags$div("Loading...",id="loadmessage")),

                        fluidRow(
                        actionButton("createIsland", "Create Island"),
                        actionButton("resetIsland", "Reset Island")),
                
                        fluidRow(
                        ## DBT-ONLY
                        #actionButton("loadBM", "loadBM"),
                        #actionButton("loadCLS", "loadCLS")
                        ),
                
                        fluidRow(
                          column(12, selectInput("BmSize", "Bestmatchsize:",
                                                 c("0.5x" = 0.5,
                                                   "1x" = 1,
                                                   "2x" = 2,
                                                   "3x" = 3,
                                                   "4x" = 4,
                                                   "5x" = 5,
                                                   "6x" = 6,
                                                   "7x" = 7,
                                                   "8x" = 8),
                                                 selected = 4))
                          ),
                        
                        fluidRow(
                          column(12, selectInput("UmxSize", "Umatrixsize:",
                                                 c("0.5x" = 0.5,
                                                   "1x" = 1,
                                                   "1.5x" = 1.5,
                                                   "2x" = 2,
                                                   "2.5x" = 2.5,
                                                   "3x" = 3,
                                                   "4x" = 4),
                                                 selected = 1))
                          ),
                        checkboxInput("ShowWarnings", "Show Warnings", value = TRUE),
                
                        #htmlOutput("warnings"),
                        htmlOutput("warnings1"),
                        htmlOutput("warnings2"),
                        #htmlOutput("warnings3"),
                        br(),
                        fluidRow(
                          ## DBT-ONLY
                          #actionButton("Save", "Save"),
                          actionButton("quit", "Quit")
                          )
                      )
    )))

  #Bestmatches = CheckBestmatches(Bestmatches, Cls, shiny=T)
  #CheckUmatrix(Umatrix, shiny=T)

  outputApp=runApp(list(ui = UmatrixUi, server = function(input, output, session){

    # 1. Help functions
    # 2. Local app workflow
    
    # Help functions
    bestUmatrixTranslation <- function(Umatrix, Bestmatches=NULL){
      # rotation = bestUmatrixTranslation(Umatrix, Bestmatches)
      # Get the ideal amount of lines and columns to rotate the Umatrix by, so that the borders
      # are on positions with the greatest Heights and fewest Bestmatches
      # INPUT
      # Umatrix(1:Lines, 1:Columns)	  Umatrix
      # OPTIONAL
      # Bestmatches(1:n,1:2)		  Bestmatches
      # OUTPUT
      # list with
      # lines				  number of lines to rotate up
      # cols				  number of columns to rotate left
      # author: Florian Lerch, Michael Thrun
      
      ncols = ncol(Umatrix)
      nrows = nrow(Umatrix)
      
      # calculate Height for every line and column
      lineHeights = rowSums(Umatrix)
      colHeights = colSums(Umatrix) 
      
      if(!is.null(Bestmatches)){
        # add bm keys if not given
        if(ncol(Bestmatches) == 2) Bestmatches <- cbind(1:nrow(Bestmatches),Bestmatches)
        
        # find intruding Bestmatches on every line and column
        lineIntruders = rep(1,nrows)
        colIntruders = rep(1,ncols)
        for(i in 1:nrows) lineIntruders[i] = length(which(Bestmatches[,2] == i))
        for(i in 1:ncols) colIntruders[i] = length(which(Bestmatches[,3] == i))
        
        lineIntruders2 = rep(1,nrows)
        colIntruders2 = rep(1,ncols)
        for(i in 2:(nrows-1)) lineIntruders2[i] = lineIntruders[(i-1)]+lineIntruders[i]+lineIntruders[(i+1)]
        
        for(i in 2:(ncols-1)) colIntruders2[i] = colIntruders[(i-1)]+colIntruders[i]+colIntruders[(i+1)]
        
        lineIntruders2[1]=lineIntruders[nrows]+lineIntruders[1]+lineIntruders[2]
        lineIntruders2[nrows]=lineIntruders[nrows]+lineIntruders[1]+lineIntruders[nrows-1]
        
        colIntruders2[1]=colIntruders[ncols]+colIntruders[1]+colIntruders[2]
        colIntruders2[ncols]=colIntruders[ncols]+colIntruders[1]+colIntruders[ncols-1]
        
        colIntruders2[colIntruders2==0]=1
        lineIntruders2[lineIntruders2==0]=1
        
        # norm Heights with intruding Bestmatches
        lineHeights = lineHeights / lineIntruders2
        colHeights = colHeights / colIntruders2
      }
      
      # calculate the upper left cutting point
      cutLine = which.max(lineHeights)
      cutCol = which.max(colHeights)
      
      list(lines = cutLine, cols = cutCol)
    }
    
    affectedArea <- function() {
      selected <- plotly::event_data("plotly_selected")    # Plotly datastructure from interactive selection
      if(is.null(selected) || length(selected) == 0){
        return(NA)
      }
      IdxRows = selected[[4]]                              # Row index of plotted/rendered matrix
      IdxCols = selected[[3]]                              # Col index of plotted/rendered matrix
      IdxMatrix = cbind(IdxRows, IdxCols)
      IdxMatrix = IdxMatrix[!duplicated(IdxMatrix),]
      
      PartialIndices = c()                                 # Index of affected Umatrix points
      for(i in 1:length(IdxRows)){                         # Match Bestmatches with selection from interactive tool
        TmpVar = which(Bestmatches[,2] == IdxRows[i] & Bestmatches[,3] == IdxCols[i])
        PartialIndices = c(PartialIndices, TmpVar)
      }
      # Select the affected points of the Umatrix
      NewUmatrix            = matrix(0, nrow = dim(Umatrix)[1], ncol = dim(Umatrix)[2])
      NewUmatrix[IdxMatrix] = Umatrix[IdxMatrix]
      # Update the enlarged and modified Umatrix and Bestmatches => CutUmatrix and CutBestmatches
      CutUmatrix     <<- NewUmatrix
      Imx            <<- matrix(1,                       # 1 if part of ocean 
                                nrow = dim(Umatrix)[1],  # (will be the part cut
                                ncol = dim(Umatrix)[2])  # off see docu of Imx)
      Imx[IdxMatrix] <<- 0                               # 0 if part of island (will be used for vis. see docu of Imx)
      if(!is.null(Bestmatches)){
        CutBestmatches <<- Bestmatches[PartialIndices,]
      }
      if(!is.null(Cls)){
        CutCls         <<- Cls[PartialIndices]
      }
      return(PartialIndices)
    }

    TopographicMapTopView_hlp=function(Umatrix, BestMatchingUnits, Cls, Tiled,
                                       MarkerSize, DotLineWidth, alpha,
                                       MissingBMUs, MultipleSelectedBMUs){
      if(length(Cls[Cls==0])>0){
        TmpColors = unlist(lapply(unique(Cls)+1, function(x) ClsColors2[x]))
      }else{
        TmpColors = unlist(lapply(unique(Cls), function(x) ClsColors1[x]))
      }
      # Ensure that there are no duplicated
      if(!is.null(Bestmatches)){
        Idx = which(!duplicated(BestMatchingUnits[,2:3], fromLast = F))
        BestMatchingUnits = BestMatchingUnits[Idx, ]
        Cls = Cls[Idx]
      }
      #
      plt=helperTopographicIsland(GeneralizedUmatrix = Umatrix,
                                                              BestMatchingUnits = BestMatchingUnits,
                                                              Cls=Cls,
                                                              ClsColors = TmpColors,
                                                              Tiled=Tiled,
                                                              BmSize = MarkerSize,
                                                              DotLineWidth=DotLineWidth,
                                                              alpha=alpha,
                                                              ShinyBinding=TRUE,
                                                              #ShinyDimension=input$dimension[1],
                                                              Session=session)
      requireNamespace("plotly")
      plt$x$layout$width = UmxSize*700                        # Adjust width
      plt$x$layout$height = UmxSize*450                       # Adjust height
      plotly::highlight(p = plt, width = 10) # ???
      if(!is.null(BestMatchingUnits)){
        #idealIsland <<- bestUmatrixTranslation(OUmatrix, OBestMatches)
        idealIsland <<- bestUmatrixTranslation(Umatrix, BestMatchingUnits)
      }
      
      
      if((!is.null(idealIsland))&&(!islandExists)){
        plt = plotly::add_polygons(p = plt,
                                   x = c(idealIsland$cols,
                                         idealIsland$cols,
                                         idealIsland$cols+ncol(OUmatrix),
                                         idealIsland$cols+ncol(OUmatrix),
                                         idealIsland$cols),
                                   y = c(idealIsland$lines,
                                         idealIsland$lines+nrow(OUmatrix),
                                         idealIsland$lines+nrow(OUmatrix),
                                         idealIsland$lines,
                                         idealIsland$lines))
      }
        
      # If missing bmus given, then show them on map (show all four possible positions)
      if(!all(is.na(MissingBMUs))){
        plt = plotly::add_trace(p = plt, x = MissingBMUs[,3],
                                y = MissingBMUs[,2],
                                mode = "markers",
                                marker = list(color = 'rgb(255, 0, 0)',
                                              size = 1.5*MarkerSize,
                                              line = list(color = 'rgb(0, 0, 0)',
                                                          width = 1)),
                                showlegend = F, type = "scatter")
      }
      if(!all(is.na(MultipleSelectedBMUs))){
        plt = plotly::add_trace(p = plt, x = ~MultipleSelectedBMUs[,3],
                                y = ~MultipleSelectedBMUs[,2],
                                mode = "markers",
                                marker = list(color = 'rgb(255, 0, 0)',
                                              size = 1.5*MarkerSize,
                                              line = list(color = 'rgb(0, 0, 0)',
                                                          width = 1)),
                                showlegend = F, type = "scatter")
      }
      Rendered <- plotly::renderPlotly({plt})
      output$Plot=Rendered
      outplot <<- plt
    }#end TopographicMapTopView_hlp
    
    tileGUM=function(Umatrix, BestMatches = NULL, Cls = NULL){
      rows = nrow(Umatrix)
      cols = ncol(Umatrix)
      Umatrix <- Umatrix[c(1:rows, 1:rows), c(1:cols, 1:cols)]
      bm_keys = NULL
      if (!is.null(BestMatches)) {
        if (ncol(BestMatches) == 3) {
          bm_keys = BestMatches[, 1]
          BestMatches = BestMatches[, c(2, 3)]
        }else{
          bm_keys = 1:nrow(BestMatches)
        }
        bmRow <- nrow(BestMatches)
        BestMatches                                 <- BestMatches[rep(1:bmRow, 4), ]
        BestMatches[(bmRow + 1):(2 * bmRow), 1]     <- BestMatches[(bmRow + 1):(2 * bmRow), 1] + rows
        BestMatches[(2 * bmRow + 1):(3 * bmRow), 2] <- BestMatches[(2 * bmRow + 1):(3 * bmRow), 2] + cols
        BestMatches[(3 * bmRow + 1):(4 * bmRow), 1] <- BestMatches[(3 * bmRow + 1):(4 * bmRow), 1] + rows
        BestMatches[(3 * bmRow + 1):(4 * bmRow), 2] <- BestMatches[(3 * bmRow + 1):(4 * bmRow), 2] + cols
      }
      if (!is.null(Cls)) {
        Cls <- rep(Cls, 4)
      }
      if (!is.null(bm_keys)){
        BestMatches = cbind(rep(bm_keys, 4), BestMatches)
      }
      return(list(GeneralizedUmatrix = Umatrix, BestMatchingUnits = BestMatches, Cls = Cls))
    }
    
    show_warnings = function(){
      if(ShowWarnings){
        output$warnings1 <- renderText(warnings1)
        output$warnings2 <- renderText(warnings2)
        #output$warnings3 <- renderText(warnings3)
      }
      else{
        output$warnings1 <- renderText(" ")
        output$warnings2 <- renderText(" ")
        #output$warnings3 <- renderText(" ")
      }
    }

    # Initialize App ----
    if(!is.matrix(Umatrix))
      stop("Umatrix must be a matrix")
    if(!is.null(Bestmatches)){
      if(!is.matrix(Bestmatches))
        stop('Bestmatches must be of type matrix.')
    }
    if(!is.null(Cls)){
      if(is.list(Cls))
        stop("Cls is not a vector")
      if(!is.null(Bestmatches)){
        if(nrow(Bestmatches)!=length(Cls))
          stop('The length of the given classification does not equal the row length of the Bestmatches')
      }else{
        stop('Bestmatches not given for Cls.')
      }
    }
    
    print("Welcome to interactive GUM-island cutting in R with shiny.")
    options(warn=-1)
    
    # Original U-Matrix from ESOM, DBS or any Projectionbased Clustering
    OUmatrix     = Umatrix                        # Input UMatrix (Original UMatrix)
    # Original Bestmatches
    OBestMatches = Bestmatches                    # Input BMUs (Original BestMatches)
    # Enlarge the U-Matrix four times and arange it 2x2, in order to cut out an
    # island. Enlargement is important for clear cuts since the U-Matrix world
    # is connected at the opposite borders to form one ball shaped world like
    # the earth
    V            = tileGUM(Umatrix = OUmatrix,    # Enlarge original UMatrix to 2x2 of input Umatrix
                           BestMatches = OBestMatches,
                           Cls = Cls)
    # Save the enlarged U-Matrix as basis for the visualization of the complete map
    Umatrix      = V$GeneralizedUmatrix       # Backup the complete Umatrix (2x2 size of normal OUMatrix (original UMatrix from Input))
    Bestmatches  = V$BestMatchingUnits
    # Create a second enlarged U-Matrix which can be modified. The cuts of the
    # interactive application will be applied on this U-Matrix
    CutUmatrix   = V$GeneralizedUmatrix       # Apply modificiations (cutting island) on a copy
    CutBestmatches = V$BestMatchingUnits      # 
    # Save the cuts on a 0/1-Matrix to represent the pure shape.
    # 1 = island, 0 = ocean
    Imx          = matrix(1, nrow = dim(Umatrix)[1], ncol = dim(Umatrix)[2])
    Cls          = V$Cls
    CutCls       = V$Cls
    ShowBMU      = 1
    UmxSize      = 2
    MarkerSize   = 1
    Tiled        = F
    bmsize       = 4
    alpha        = 1
    DotLineWidth = 1.5
    ShowWarnings = 1
    MissingBMUs  = NA
    MultipleSelectedBMUs  = NA
    warnings = "<font color=\"#00FF00\"><b>No Warnings</b></font>"
    warnings1 = "<font color=\"#00FF00\"><b>No Warnings</b></font>"
    warnings2 = " "
    warnings3 = " "
    islandExists = FALSE    # Only show exemplary frame if no island was already created
    idealIsland = NULL
    
    if(!is.null(Bestmatches)){
      if(is.null(Cls)){
        Cls=rep(1,nrow(Bestmatches))
        CutCls=rep(1,nrow(Bestmatches))
      }
    }
    
    ClsColors1   = GeneralizedUmatrix::DefaultColorSequence[1:100]
    ClsColors2   = c("lightskyblue", GeneralizedUmatrix::DefaultColorSequence[1:100])
    
    # umatrix normieren
    Umatrix = Umatrix/max(Umatrix)
    
    
    if(!is.null(Bestmatches))
      idealIsland = bestUmatrixTranslation(Umatrix, Bestmatches)
    
    observeEvent(input$ShowWarnings, {
      if(ShowWarnings==0)
        ShowWarnings <<- 0
      else
        ShowWarnings <<- 1
      show_warnings()
    })
    
    # react on button: create island ----
    observeEvent(input$createIsland,{
      AffectedBestmatches = affectedArea()
      if(!is.null(AffectedBestmatches)){
        NumAllUniqueBMUs = dim(Bestmatches)[1]/4             # Get number of unique BMUs from map which has four-times the size of the original map
        AffectedClsTile1 = AffectedBestmatches %% NumAllUniqueBMUs
        AffectedUniqueTile1 = unique(AffectedClsTile1)
        if(length(which(AffectedUniqueTile1 == 0)) > 0){
          AffectedUniqueTile1[which(AffectedUniqueTile1 == 0)] = NumAllUniqueBMUs
        }
        BoolMultipleSelected = duplicated(AffectedClsTile1, fromLast = T) | duplicated(AffectedClsTile1, fromLast = F)
        IdxMultipleSelected = which(BoolMultipleSelected)
        if(length(IdxMultipleSelected) > 0){
          IdxMultipleSelectedBestmatches = AffectedBestmatches[IdxMultipleSelected]
          MultipleSelectedBMUs <<- Bestmatches[IdxMultipleSelectedBestmatches,]
        }
        NumSelectedUniqueBMUs = length(AffectedUniqueTile1)
        NumSelectedBMUs = length(AffectedBestmatches)        # 
        NumMultipleSelectedBMUs = sum(as.vector(unlist(lapply(table(AffectedClsTile1), function(x) if(x > 1) 1 else 0))))
        MissingUniqueBMUs = NumAllUniqueBMUs - NumSelectedUniqueBMUs
        NonAffectedClsTile1 = setdiff(1:(dim(Bestmatches)[1]/4), AffectedUniqueTile1)
        IdxShowMissingBMUs = c(NonAffectedClsTile1)
        for(i in 1:3){
          TmpVar = (NonAffectedClsTile1 + i*NumAllUniqueBMUs) %% (dim(Bestmatches)[1]) # unique(PartialIndices) ? TODO
          IdxShowMissingBMUs = c(IdxShowMissingBMUs, TmpVar)
        }
        MissingBMUs <<- Bestmatches[IdxShowMissingBMUs,]
        if(NumAllUniqueBMUs > NumSelectedUniqueBMUs){
          #warnings <<- paste0("<font color=\"#FF0000\"><b>",MissingUniqueBMUs," Bestmatches are missing!</b></font>")
          warnings1 <<- paste0("<font color=\"#FF0000\"><b>",MissingUniqueBMUs," Bestmatches are missing!</b></font>")
        }else{
          warnings1 <<- paste0("<font color=\"#00FF00\"><b> No Bestmatches are missing!</b></font>")
        }
        if(NumSelectedBMUs > NumSelectedUniqueBMUs){
          #warnings <<- "<font color=\"#FF0000\"><b>Some Bestmatches are duplicated! (marked with red triangles)</b></font>"
          warnings2 <<- paste0("<font color=\"#FF0000\"><b>",NumMultipleSelectedBMUs," multiple selected Bestmatches!</b></font>")
        }else{
          warnings2 <<- paste0("<font color=\"#00FF00\"><b> No multiple selected Bestmatches!</b></font>")
        }
        # warnings <<-  "<font color=\"#00FF00\"><b>No Warnings</b></font>"
        show_warnings()
        islandExists <<- TRUE
      }
      TopographicMapTopView_hlp(Umatrix = CutUmatrix,
                                BestMatchingUnits = CutBestmatches,
                                Cls = CutCls, Tiled = Tiled, MarkerSize = bmsize*MarkerSize,
                                DotLineWidth = 0.2*bmsize, alpha = alpha,
                                MissingBMUs=MissingBMUs, MultipleSelectedBMUs=MultipleSelectedBMUs)
    })

    # react on button: reset island ----
    observeEvent(input$resetIsland,{
      CutUmatrix     <<- Umatrix
      CutBestmatches <<- Bestmatches
      CutCls         <<- Cls
      islandExists   <<- FALSE
      MissingBMUs    <<- NA
      MultipleSelectedBMUs    <<- NA
      warnings1      <<- "<font color=\"#00FF00\"><b>No Warnings</b></font>"
      warnings2      <<- " "
      show_warnings()
      TopographicMapTopView_hlp(Umatrix = CutUmatrix,
                                BestMatchingUnits = CutBestmatches,
                                Cls = CutCls, Tiled = Tiled, MarkerSize = bmsize*MarkerSize,
                                DotLineWidth = 0.2*bmsize, alpha = alpha,
                                MissingBMUs=MissingBMUs, MultipleSelectedBMUs=MultipleSelectedBMUs)
      #warnings <<- "<font color=\"#00FF00\"><b>No Warnings</b></font>"
    })

    # Change size of BMU's
    observeEvent(input$BmSize, {
      bmsize <<- as.numeric(input$BmSize)
      TopographicMapTopView_hlp(Umatrix = CutUmatrix,
                                BestMatchingUnits = CutBestmatches,
                                Cls = CutCls, Tiled = Tiled, MarkerSize = bmsize*MarkerSize,
                                DotLineWidth = 0.2*bmsize, alpha = alpha,
                                MissingBMUs=MissingBMUs, MultipleSelectedBMUs=MultipleSelectedBMUs)
    })
    
    # Change size of Umatrix
    observeEvent(input$UmxSize, {
      UmxSize <<- as.numeric(input$UmxSize)
      TopographicMapTopView_hlp(Umatrix = CutUmatrix,
                                BestMatchingUnits = CutBestmatches,
                                Cls = CutCls, Tiled = Tiled, MarkerSize = bmsize*MarkerSize,
                                DotLineWidth = 0.2*bmsize, alpha = alpha,
                                MissingBMUs=MissingBMUs, MultipleSelectedBMUs=MultipleSelectedBMUs)
    })
    
    observeEvent(input$quit, {
      stopApp(Imx)
    })
    
    session$onSessionEnded(function() {
      print("Program closed.")
      stopApp(Imx)
    })
  }))

  return(outputApp)
}
