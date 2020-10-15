This script is used to process monthly international trade data from Eurostat to add to ICO statistical database. 
The original data is from 
http://epp.eurostat.ec.europa.eu/newxtweb/setupdimselection.do
You will need to create a login(free account) to be able to get bulk files through email.
The data set is data set EU trade since 1988 by HS6 (SD-016893)
Query selections:
Reporter >add all but the EU aggregates
Partner > add all
Product > search for and add these codes(090111, 090112,090121, 090122, 210111, and 210112).
Flow > add all 
Period > any month/year selection (script doesn't deal with full year periods)
indicators > add Value_in_Euros and Quantity_in_100kg

In layout selection slice select codes and labels
Generate output in csv

If using DATASET: EU Trade Since 1988 by HS2, 4, 6 and CN8
then can get select 21011292 and 21011298 instead of just 210112. Then no need to recode it.
