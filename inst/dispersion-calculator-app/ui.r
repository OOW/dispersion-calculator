shinyUI(pageWithSidebar(
    headerPanel('DispersionCalculator v0.4'),
    sidebarPanel(wellPanel(
        fileInput('dye_input', strong('Dye input (*.asc)')),
        fileInput('dxdy_input', strong('Dxdy input (*.inp)')),
        fileInput('depth_input', strong('Depth input (*.asc)'))
    ),
    wellPanel(
        #datetimeInput('start_datetime', strong('Start time'), min='1/1/2010', max=format(Sys.Date(), '%m/%d/%Y')),
        datetimeInput('start_datetime', strong('Start time')),
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
        downloadButton('download_1', 'Download second-moment plot'), 
        downloadButton('download_2', 'Download dye mass inventory plot')
    )),
    mainPanel(plotOutput('second_moment_timeseries', height='800px'),
              plotOutput('dye_mass_timeseries', height='800px'))
))

# 2012-8-5 9:15
