shinyServer( function(input, output, session) {
    moment2_by_time <- reactive({
        input$submit

        isolate({dye.path <- input$dye_input
                dxdy.inp.path <- input$dxdy_input
                depth.path <- input$depth_input
                start.datetime <- input$start_datetime
                duration <- input$duration})

        if (! any(sapply(list(dye.path, dxdy.inp.path, depth.path, start.datetime), is.null))) {
            start.datetime <- as.POSIXct(start.datetime, format='%m/%d/%Y %H:%M')
            getSecondMoments(dye.path$datapath, dxdy.inp.path$datapath, depth.path$datapath, start.datetime, duration)
        }
    })

    second_moment_plot <- reactive({
        moment2.by.time <- moment2_by_time()
        if (!is.null(moment2.by.time)) {
            start.datetime <- as.POSIXct(isolate(input$start_datetime), format='%m/%d/%Y %H:%M')
            end.datetime <- start.datetime + as.difftime(input$duration, units='hours')
            ggplotGrob(make.plot(moment2.by.time, start.datetime, end.datetime))
        } else {
            textGrob('Upload your data and press submit to process.')
        }
    })

    output$second_moment_timeseries <- renderPlot({
        grid.draw(second_moment_plot())
    })

    output$download <- downloadHandler(
        filename='second_moment_plot.png',
        content=function(filename) {
            png(filename=filename, height=800, width=600)
            grid.draw(second_moment_plot())
            dev.off()
        }
    )

})
