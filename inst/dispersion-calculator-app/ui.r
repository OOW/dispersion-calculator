shinyUI(pageWithSidebar(
    headerPanel('DispersionCalculator v0.1'),
    sidebarPanel(wellPanel(
        fileInput('dye_input', strong('dye input (*.asc)')),
        fileInput('dxdy_input', strong('dxdy input (*.inp)')),
        fileInput('depth_input', strong('depth input (*.asc)'))
    ),
    wellPanel(
        datetimeInput('start_datetime', strong('Start time'), min='1/1/2010', max=format(Sys.Date(), '%m/%d/%Y')),
        numericInput('duration', strong('Duration (hours)'), value=2, min=2),
        actionButton('submit', strong('Submit'))
    ),
    conditionalPanel(condition="$('#second_moment_timeseries').hasClass('recalculating')",
        id='progressIndicator',
        wellPanel(
            strong("Calculating, this may take a moment ...")
        )
    ),
    wellPanel(
        downloadButton('download', 'Download plot')
    )),
    mainPanel(plotOutput('second_moment_timeseries', height='800px'))
))

# 2012-8-5 9:15
