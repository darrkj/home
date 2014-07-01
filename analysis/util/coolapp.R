require(shiny)

require(devtools) 
install_github('slidify', 'ramnathv', ref = 'dev')
install_github('rCharts', 'ramnathv') 
install_github('slidifyLibraries', 'ramnathv', ref = 'dev') # optional

runGitHub( repo = 'rCharts_notebook_512paths',
           username = "timelyportfolio")