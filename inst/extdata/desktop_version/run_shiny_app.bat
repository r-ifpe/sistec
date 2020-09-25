echo "%~dp0R-Portable\App\R-Portable\bin\R.exe"
echo "%~dp0run_shiny_app.R"
"%~dp0R-Portable\App\R-Portable\bin\R.exe" CMD BATCH --vanilla "%~dp0run_shiny_app.R" > log.txt 2>&1
