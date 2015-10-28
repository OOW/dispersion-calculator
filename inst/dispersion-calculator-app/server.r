shinyServer( function(input, output, session) {
    
    analysisResults <- reactive({
        input$submit

        isolate({dye.path <- input$dye_input
                 dxdy.inp.path <- input$dxdy_input
                 depth.path <- input$depth_input
                 start.datetime <- input$start_datetime
                 duration <- input$duration
                 x.idx.first.cell <- input$x.idx.first.cell
                 nlayers <- input$nlayers
                })

        if (! any(sapply(list(dye.path, dxdy.inp.path, depth.path, start.datetime), is.null))) {
            start.datetime <- as.POSIXct(start.datetime, format='%m/%d/%Y %H:%M')
            performAnalysis(dye.path$datapath, dxdy.inp.path$datapath, depth.path$datapath, 
                            input$depth_file_type, start.datetime, duration, x.idx.first.cell, nlayers)
        }
    })

    # create a reactive second moment plot
    second_moment_plot <- reactive({
        
        isolate({start.datetime <- input$start_datetime
                 duration <- input$duration
              })
        
        analysisResults <- analysisResults() # grab second moment results from list
        moment2.by.time <- analysisResults[[1]]
        if (!is.null(moment2.by.time)) {
            start.datetime <- as.POSIXct(isolate(input$start_datetime), format='%m/%d/%Y %H:%M')
            end.datetime <- start.datetime + as.difftime(isolate(input$duration), units='hours')
            ggplotGrob(make.moment.plot(moment2.by.time, start.datetime, end.datetime))
        } else {
            textGrob('Upload your data, set the start time and duration, and press submit to process.')
        }
    })

    output$second_moment_timeseries <- renderPlot({
        grid.draw(second_moment_plot())
    })
    
    output$download_1 <- downloadHandler(
      filename='second_moment_plot.png',
      content=function(filename) {
        png(filename=filename, height=800, width=600)
        grid.draw(second_moment_plot())
        dev.off()
      }
    )

    # create a reactive dye mass plot
    dye_mass_plot <- reactive({
      analysisResults <- analysisResults()
      dye.mass.by.time <- analysisResults[[2]]
      if (!is.null(dye.mass.by.time)) {
        start.datetime <- as.POSIXct(isolate(input$start_datetime), format='%m/%d/%Y %H:%M')
        end.datetime <- start.datetime + as.difftime(isolate(input$duration), units='hours')
        ggplotGrob(make.dye.mass.plot(dye.mass.by.time, start.datetime, end.datetime))
      } else {
        textGrob('Upload your data, set the start time and duration, and press submit to process.')
      }
    })
    
    output$dye_mass_timeseries <- renderPlot({
      grid.draw(dye_mass_plot())
    })

    output$download_2 <- downloadHandler(
      filename='dye_mass_inventory_plot.png',
      content=function(filename) {
        png(filename=filename, height=800, width=600)
        grid.draw(dye_mass_plot())
        dev.off()
      }
    )
    
    session$onSessionEnded(function() {
      stopApp()
    })
    
})
