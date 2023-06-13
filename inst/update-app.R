rsconnect::setAccountInfo(
  name = 'abjur',
  token = Sys.getenv('SHINYAPPS_TOKEN'),
  secret = Sys.getenv('SHINYAPPS_SECRET')
)

rsconnect::deployApp('inst/app', appName = "abnota")