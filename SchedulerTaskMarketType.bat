@echo off
::using R.exe will produce R.out file where we can read diagnostics of the code
::"C:\Program Files\R\R-3.4.3\bin\R.exe" CMD BATCH "C:\Users\fxtrams\Documents\000_TradingRepo\R_markettype\8_ScoreData.R"
::alternative way is to use Rscript.exe program
"C:\Program Files\R\R-3.4.3\bin\Rscript.exe" "C:\Users\fxtrams\Documents\000_TradingRepo\R_markettype\8_ScoreData.R"