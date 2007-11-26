mkdir obj
copy *.* obj\*.*
chdir obj
lhs2tex supero.tex -o final.tex
bibtex final
texify final.tex %1
cd ..
del play.dvi
copy obj\final.dvi supero.dvi
