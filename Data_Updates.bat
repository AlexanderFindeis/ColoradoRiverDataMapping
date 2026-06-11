@echo off

set RSCRIPT="C:\Program Files\R\R-4.2.3\bin\Rscript.exe"

echo === Running R data scripts ===
%Rscript% Scripts/Reservoir_Data.R
%Rscript% Scripts/Snow_Pack_Data.R

echo === Rendering Quarto pages ===
quarto.cmd render index.qmd --to html
quarto.cmd render "Pages/Reservoirs/Reservoir Overview.qmd" --to html
quarto.cmd render "Pages/Snow Pack/Snow Pack.qmd" --to html

echo === Committing and pushing changes ===
git add .
git diff --cached --quiet || (
  git commit -m "Scheduled Data Update: %date% %time%"
  git push origin main
  echo Changes pushed successfully
)

echo === Done ===
pause