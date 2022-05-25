interactiveClustering = function(Umatrix=NULL, BestMatches=NULL, Cls=NULL) {
  outplot=NULL
  ## Schritt 2: shiny interactive tool ####
  ##########
  # Shiny Fenster
  ##########
  ui = fluidPage(
    useShinyjs(),
    sidebarLayout(position="right",
                  mainPanel(plotly::plotlyOutput("Plot", width = "auto"),
                            width = 8, height = "100%"), #width = 8, height = "100%"),
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

                       uiOutput("buttons"),
                       actionButton("addCluster", "Add Cluster"),
                       
                       uiOutput("clusterOverview"),
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
                      #fluidRow(column(12,checkboxInput("TransparentContours", "transparent contours", value=F))),
                      fluidRow(column(12,checkboxInput("showBMU", "show bestmatches", value=T))),
                      fluidRow(actionButton("quit", "Quit"))
                      ))))
  
  server <- shiny::shinyServer(function(input, output, session) {
    
    ## Helper functions:
    affectedCls <- function() {
      selected <- plotly::event_data("plotly_selected")    # Plotly datastructure from interactive selection
      if (is.null(selected) || length(selected) == 0) {
        return(NA)
      }
      IdxRows = selected[[4]]                              # Row index of plotted/rendered matrix
      IdxCols = selected[[3]]                              # Col index of plotted/rendered matrix
      PartialIndices = c()                                 # Index of affected bestmatches
      for(i in 1:length(IdxRows)){                         # Match Bestmatches with selection from interactive tool
        TmpVar = BestMatches[BestMatches[,2] == IdxRows[i] & BestMatches[,3] == IdxCols[i]]
        NumIdxEntriesTmpVar = as.integer(length(TmpVar)/3) # Multiple entries possible, index
        PartialIndices = c(PartialIndices, TmpVar[1:NumIdxEntriesTmpVar])
      }
      Translate = as.integer(length(Cls)/4)
      CompleteIndices = PartialIndices
      for(i in 1:3){
        TmpVar = (PartialIndices + i*Translate) %% length(Cls) # unique(PartialIndices) ? TODO %% (length(Cls)+1) ??? Errors?
        CompleteIndices = c(CompleteIndices, TmpVar)
      }
      if(length(which(CompleteIndices == 0)) > 0){ # QS: (newly implemented => potential error source); if it works in practices, this should be fine
        CompleteIndices[which(CompleteIndices == 0)] = length(Cls)
      }
      return(CompleteIndices)
    }

    TopographicMapTopView_hlp=function(Umatrix, BestMatchingUnits, Cls, Tiled, MarkerSize, DotLineWidth, alpha){
      if(length(Cls[Cls==0])>0){
        TmpColors = unlist(lapply(unique(Cls)+1, function(x) ClsColors2[x]))
      }else{
        TmpColors = unlist(lapply(unique(Cls), function(x) ClsColors1[x]))
      }
      
      #RGB = lapply(DefaultColorSequence[1:10], function(x) as.vector(unlist(col2rgb(x, alpha = 1))))
      #Hex = sapply(RGB, function(x) rgb(x[1], x[2], x[3], alpha = alpha*256, maxColorValue = 255))
      
      plt=GeneralizedUmatrix::TopviewTopographicMap(GeneralizedUmatrix = Umatrix,
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
      plt$x$layout$width = UmxSize*700
      plt$x$layout$height = UmxSize*450
      #plt <- plotly::layout(plt, width = 10)
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
    
    
    createClusterButtons = function(){
      #########
      # create all buttons
      #########
      output$buttons = renderUI({
        if(any(unique(Cls) == 0)){
          colors = unlist(lapply(unique(Cls)+1, function(x) ClsColors2[x]))
        }else{
          colors = unlist(lapply(unique(Cls), function(x) ClsColors1[x]))
        }
        classes = sort(unique(Cls), decreasing = F)
        colors = colors[order(unique(Cls))]
        
        lapply(classes, function(x){
          if(x == 0)    # Clusters which were created by deleting a cluster -> zero cluster
            fluidRow(id=paste0("row",x),
                     column(8, span(paste0("No Cluster (",sum(Cls == x),")"),
                                    style=paste0("color:","lightskyblue",";font-size:1.3em"))))
          else if(x %in% Cls)    # Clusters with a true cluster assignment
            fluidRow(id=paste0("row",x),
                     column(8, span(paste0('Cluster ', x, "(",sum(Cls==x),")"),
                                    style=paste0("color:",colors[which(classes==x)],";font-size:1.3em"))),
                     column(2, actionButton(paste0("addCluster", x),"+")),
                     column(2, actionButton(paste0("deleteCluster", x),"-")),
                     style="position:relative;top:50%")
          else
            hidden(fluidRow(id=paste0("row",x),
                            column(5, span(paste0('Cluster ', x),
                                           style=paste0("color:",colors[which(classes==x)],";font-size:1.3em"))),
                            column(5, actionButton(paste0("deleteCluster",x),"delete")),
                            style="position:relative;top:50%"))
        })
      })
    }
    
    updateClasses = function(){
      Classes = sort(unique(Cls), decreasing = F)
      if(any(Classes==0)){
        NewClasses = 0:(length(Classes)-1)
      }else{
        NewClasses = 1:length(Classes)
      }
      for(i in length(Classes)){
        Cls[Cls==Classes[i]] <<- NewClasses[i]
      }
    }
    
    addClasses = function(add2ClassX){
      AffectedCls = affectedCls()
      if(all(!is.na(AffectedCls))){
        #id = max(Cls) + 1
        if(add2ClassX == 0){
          #id = min(setdiff(1:unique(Cls)+1,unique(Cls)))
          #id = min(setdiff(1:100,unique(Cls)))
          id = max(unique(Cls))+1
        }else{
          id = add2ClassX
        }
        Cls[AffectedCls] <<- id
        #updateClasses()
        if(input$showBMU == T){
          TopographicMapTopView_hlp(Umatrix = Umatrix,
                                    BestMatchingUnits = BestMatches,
                                    Cls=Cls,
                                    Tiled=Tiled,
                                    MarkerSize = ShowBMU*bmsize*MarkerSize,
                                    DotLineWidth=(bmsize/2)*DotLineWidth/(log(10-bmsize))**2,
                                    alpha = alpha)
        }
      }
    }
    
    
    #updateSelectInput(session, "umsize")
    #observe({MarkerSize = as.numeric(input$umsize)})
    #shiny::updateNumericInput(session, "bmsize")
    #observe({MarkerSize = as.numeric(input$bmsize)})
    #updateSelectInput(session, "umsize", selected = UmatrixSize)
    
    # Initialize App ----
    print("Welcome to interactive clustering in R with shiny.")
    OUmatrix     = Umatrix
    OBestMatches = BestMatches
    V            = tileGUM(Umatrix = OUmatrix, BestMatches = OBestMatches, Cls = Cls)
    Umatrix      = V$GeneralizedUmatrix 
    BestMatches  = V$BestMatchingUnits
    ShowBMU      = 1
    UmxSize      = 2
    MarkerSize   = 1
    Tiled        = F
    bmsize       = 4
    alpha        = 1
    DotLineWidth = 1.5
    add2ClassOld = 0
    ClsColors1   = GeneralizedUmatrix::DefaultColorSequence[1:100]
    ClsColors2   = c("lightskyblue", GeneralizedUmatrix::DefaultColorSequence[1:100])
    if(is.null(Cls)){
      Cls = rep(1,nrow(BestMatches))
    }else{
      Cls = V$Cls
    }
    
    createClusterButtons()
    

    TopographicMapTopView_hlp(Umatrix = Umatrix, BestMatchingUnits = BestMatches,
                              Cls=Cls, Tiled=Tiled,
                              MarkerSize = ShowBMU*bmsize*MarkerSize,
                              DotLineWidth=0.2*bmsize,#(bmsize/2)*DotLineWidth/(log(10-bmsize))**2,
                              alpha = alpha)

    
    #--------------------------------------------------------------------------#
    
    # Wait for interaction via buttons and events
    # Button: Create new cluster
    observeEvent(input$addCluster, {
      #updateClasses()
      addClasses(0)
      createClusterButtons()
    })
    
    # Delete cluster
    classes <- c(unique(Cls), setdiff(1:100,unique(Cls)))
    lapply(classes, function(i){
      observeEvent(input[[paste0('deleteCluster', i)]],{
        Cls[Cls==i] <<- 0
        #updateClasses()
        createClusterButtons()
        TopographicMapTopView_hlp(Umatrix = Umatrix,
                                  BestMatchingUnits = BestMatches,
                                  Cls=Cls,
                                  Tiled=Tiled,
                                  MarkerSize = ShowBMU*bmsize*MarkerSize,
                                  DotLineWidth=0.2*bmsize,#(bmsize/2)*DotLineWidth/(log(10-bmsize))**2,
                                  alpha = alpha)
      })
    })
    
    # Add to existing cluster
    classes <- c(unique(Cls), setdiff(1:100,unique(Cls)))
    lapply(classes, function(i){
      observeEvent(input[[paste0('addCluster', i)]],{
        addClasses(i)
        createClusterButtons()
      })
    })
    
    # Show BMU's
    observeEvent(input$showBMU, {
      if(input$showBMU == T){
        ShowBMU <<- 1
      }else{
        ShowBMU <<- 0.001
      }
      TopographicMapTopView_hlp(Umatrix = Umatrix,
                                BestMatchingUnits = BestMatches,
                                Cls=Cls,
                                Tiled=Tiled,
                                MarkerSize = ShowBMU*bmsize*MarkerSize,
                                DotLineWidth=0.2*bmsize,#(bmsize/2)*DotLineWidth/(log(10-bmsize))**2,
                                alpha = alpha)
    })

    # Change size of BMU's
    observeEvent(input$BmSize, {
      bmsize <<- as.numeric(input$BmSize)
      if(input$showBMU == T){
        TopographicMapTopView_hlp(Umatrix = Umatrix,
                                  BestMatchingUnits = BestMatches,
                                  Cls=Cls,
                                  Tiled=Tiled,
                                  MarkerSize = ShowBMU*bmsize*MarkerSize,
                                  DotLineWidth=0.2*bmsize,#(bmsize/2)*DotLineWidth/(log(10-bmsize))**2,
                                  alpha = alpha)
      }
    })
    
    # Change size of Umatrix
    observeEvent(input$UmxSize, {
      UmxSize <<- as.numeric(input$UmxSize)
      TopographicMapTopView_hlp(Umatrix = Umatrix,
                                BestMatchingUnits = BestMatches,
                                Cls=Cls,
                                Tiled=Tiled,
                                MarkerSize = ShowBMU*bmsize*MarkerSize,
                                DotLineWidth=0.2*bmsize,#(bmsize/2)*DotLineWidth/(log(10-bmsize))**2,
                                alpha = alpha)
    })

    #observeEvent(input$TransparentContours, {
    #  if(input$TransparentContours == T){
    #    alpha <<- 0.25
    #  }else{
    #    alpha <<- 1
    #  }
    #  TopographicMapTopView_hlp(Umatrix = Umatrix,
    #                            BestMatchingUnits = BestMatches,
    #                            Cls=Cls,
    #                            Tiled=Tiled,
    #                            MarkerSize = ShowBMU*bmsize*MarkerSize,
    #                            alpha = alpha)
    #})
    
    
    session$onSessionEnded(function() {
      print("Program closed.")
      stopApp(list(Cls=Cls[1:dim(OBestMatches)[1]],
                   TopView_TopographicMap = outplot))
    })
    
    # Button: Exit ----
    observeEvent(input$quit, {
      #Cls <<- Cls
      stopApp(list(Cls=Cls[1:dim(OBestMatches)[1]],
                   TopView_TopographicMap = outplot))
    })
  })
  output <- runApp(list(ui = ui, server = server))
  return(output)
}

