This is a shiny app, that is meant to explore baseball data over time and evaluate whether a player was worth paying over
the terms of careers. Mega-Contracts are something worth exploring if players are just being compensated for previous work.

Data is take from http://www.seanlahman.com/baseball-archive/statistics/
CPI information http://research.stlouisfed.org/fred2/data/CPIAUCSL.txt

Files for data cleaning are included. Open file "Official Shiny.R" and run the code, everything should run accordingly.
If not make sure the files "Baseball.rdata" and "Shiny.rdata" are displayed in one folder.

Formula for Total_bases can be seen in cleaned Code File.
(1*Singles)+(2*Doubles)+(3*Triples)+(4*Homeruns)+(Stolenbases)+(Walks)+(Intentional Walks)+(Hit By Pinch)

As well as Career.DPB
(Total Career Earnings Adjusted to 2016/Total_Bases)
and DPB (Dollars per Base) =Salary.Adjusted/Total_bases
