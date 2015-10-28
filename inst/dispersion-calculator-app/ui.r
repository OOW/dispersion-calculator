shinyUI(
  pageWithSidebar(
    headerPanel('DispersionCalculator v0.4'),
    sidebarPanel(
      wellPanel(
        fileInput('dye_input', strong('Dye input (*.asc)')),
        fileInput('dxdy_input', strong('Dxdy input (*.inp)')),
        fileInput('depth_input', strong('Depth input (*.asc)'))
      ),
      wellPanel(
        radioButtons("depth_file_type", label = "Choose Depth File Type",
                     choices = list("Older (3 cloumns)" = 1, "Newer (25 columns wrapped)" = 2), 
                     selected = 1),
        datetimeInput('start_datetime', strong('Start time')),
        numericInput('duration', strong('Duration (hours)'), value=2, min=2),
        numericInput('x.idx.first.cell', strong('First idx x Cell'), value=237),
        numericInput('nlayers', strong('Number z layer'), value=5),
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
      )
    ),
    mainPanel(plotOutput('second_moment_timeseries', height='800px'),
              plotOutput('dye_mass_timeseries', height='800px'))
  )
)

# 2012-8-5 9:15
