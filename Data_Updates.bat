@echo off

set RSCRIPT="C:\Program Files\R\R-4.2.3\bin\Rscript.exe"
set quarto="C:\Users\a1928\AppData\Local\Programs\Positron\resources\app\quarto\bin\quarto.exe"

@REM echo === Running R data scripts ===
@REM %Rscript% Scripts/Reservoir_Data.R
@REM %Rscript% Scripts/Snow_Pack_Data.R

echo === Rendering Quarto pages ===
%quarto% render index.qmd
%quarto% render "Pages/Reservoirs/Reservoir Overview.qmd"
%quarto% render "Pages/Snow Pack/Snow Pack.qmd"

echo === Committing and pushing changes ===
git add .
git diff --cached --quiet || (
  git commit -m "Scheduled Data Update: %date% %time%"
  git push origin main
  echo Changes pushed successfully
)

echo === Done ===
pause