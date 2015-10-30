shinyServer( function(input, output, session) {
    
  # Update the start datetime with the first line of the dye input file
  observe({
    
    if(!is.null(input$dye_input)){
      r_yr <- as.character(input$r_yr)
      dye.path <- input$dye_input
      dye.path <- dye.path$datapath
      start_dtime.chr <- trimws(readLines(dye.path,n = 1))
      start_dtime.chr <- strsplit(start_dtime.chr, split = '.', fixed=TRUE)[[1]]
      tot.mins <- round(as.numeric(paste0('.', start_dtime.chr[2])) * 24 * 60)
      hours <- tot.mins %/% 60
      mins <- tot.mins %% 60
      t_date <- as.Date(paste0(r_yr, "-01-01")) + as.numeric(start_dtime.chr[1])
      start_datetime <- paste(t_date, hours, mins)
      # convert the timestep datetime strings to POSIXct datetimes
      start_datetime <- as.POSIXct(start_datetime, format='%Y-%m-%d %H %M')
      start_datetime <- format(start_datetime, format='%m/%d/%Y %H:%M')
      # update start datetime when dye file is uploaded
      updateDateInput(session, "start_datetime",
                      value = start_datetime)
    }
  })
  
  
  analysisResults <- reactive({
        input$submit

        isolate({dye.path <- input$dye_input
                 dxdy.inp.path <- input$dxdy_input
                 depth.path <- input$depth_input
                 start.datetime <- input$start_datetime
                 duration <- input$duration
                 x.idx.first.cell <- input$x.idx.first.cell
                 nlayers <- input$nlayers
                 r_yr <- as.character(input$r_yr)
                })

        if (! any(sapply(list(dye.path, dxdy.inp.path, depth.path, start.datetime), is.null))) {
            start.datetime <- as.POSIXct(start.datetime, format='%m/%d/%Y %H:%M')
            performAnalysis(dye.path$datapath, dxdy.inp.path$datapath, depth.path$datapath, 
                            input$depth_file_type, start.datetime, duration, x.idx.first.cell,
                            nlayers, r_yr)
        }
    })

    # create a reactive second moment plot
    second_moment_plot <- reactive({
        
        isolate({start.datetime <- input$start_datetime
                 duration <- input$duration
                 timestep <- input$timestep
              })
        
        analysisResults <- analysisResults() # grab second moment results from list
        moment2.by.time <- analysisResults[[1]]
        if (!is.null(moment2.by.time)) {
            start.datetime <- as.POSIXct(isolate(input$start_datetime), format='%m/%d/%Y %H:%M')
            end.datetime <- start.datetime + as.difftime(isolate(input$duration), units='hours')
            ggplotGrob(make.moment.plot(moment2.by.time, start.datetime, end.datetime, timestep))
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

    output$download_data <- downloadHandler(
      filename = function() {'analysis_results.csv'}, 
      content = function(file) {
        d <- analysisResults()
        d <- as.data.frame(d)
        names(d) <- c("weighted average m^2 x", "weighted average m^2 y", "weighted average m^2 z", "dye_mass kg")
        write.csv(d, file, quote=FALSE, row.names=FALSE)
      }
    )
    
    
        
    session$onSessionEnded(function() {
      stopApp()
    })
    
})
