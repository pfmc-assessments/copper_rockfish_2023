use gf_assessment;
select 
--year, substring(id_code, 10, 2) month, substring(id_code, 12, 2) day,
DATEFROMPARTS(year, substring(id_code, 10, 2), substring(id_code, 12, 2)) as trpdate, sp_code,
--id_code, 
--luport.cnty, luport.intsite,  site_name, 
cnty_name, 
LNGTH, 
(0.629+(1.01*LNGTH)) as Total_length
from recfin_noca_93_03
inner join luport on RecFIN_NoCA_93_03.cnty = luport.CNTY and RecFIN_NoCA_93_03.intsite = luport.intsite
where sp_code = 8826010108  and lngth is not null
order by trpdate



use CDFW_CPFV_Onboard_1987_1998;
select trpdate, --LengthID,
CDFGSP, 
--Lengths.TRIP_ID, cnty, Boat.PORT,
county, FISH_TL 
--Landing
from lengths
left join boat on boat.trip_id = lengths.trip_id
  inner join luport on luport.port = boat.port
where cdfgsp = 2308   and year(trpdate)>1996
order by trpdate, county, fish_tl
and (trpdate = '04/12/1997' and county like '%San Luis Obispo%' or
trpdate = '04/18/1997' and county like '%San Luis Obispo%' or
trpdate = '04/18/1997' and county like '%Sonoma%' or
trpdate = '04/25/1997' and county like '%Sonoma%' or
trpdate = '04/26/1997' and county like '%San Luis Obispo%' or
trpdate = '04/30/1997' and county like '%San Mateo%' or
trpdate = '05/09/1997' and county like '%San Luis Obispo%' or
trpdate = '05/10/1997' and county like '%San Luis Obispo%' or
trpdate = '05/23/1997' and county like '%San Luis Obispo%' or
trpdate = '06/16/1997' and county like '%Sonoma%' or
trpdate = '06/21/1997' and county like '%San Luis Obispo%' or
trpdate = '07/03/1997' and county like '%San Mateo%' or
trpdate = '07/08/1997' and county like '%San Mateo%' or
trpdate = '07/09/1997' and county like '%San Luis Obispo%' or
trpdate = '07/10/1997' and county like '%Sonoma%' or
trpdate = '07/11/1997' and county like '%San Mateo%' or
trpdate = '07/12/1997' and county like '%San Luis Obispo%' or
trpdate = '07/14/1997' and county like '%San Luis Obispo%' or
trpdate = '07/17/1997' and county like '%Monterey%' or
trpdate = '07/21/1997' and county like '%Monterey%' or
trpdate = '07/24/1997' and county like '%San Luis Obispo%' or
trpdate = '07/24/1997' and county like '%San Mateo%' or
trpdate = '07/25/1997' and county like '%San Mateo%' or
trpdate = '07/26/1997' and county like '%San Luis Obispo%' or
trpdate = '07/29/1997' and county like '%Monterey%' or
trpdate = '07/29/1997' and county like '%San Luis Obispo%' or
trpdate = '07/30/1997' and county like '%San Mateo%' or
trpdate = '07/31/1997' and county like '%Sonoma%' or
trpdate = '08/03/1997' and county like '%San Luis Obispo%' or
trpdate = '08/03/1997' and county like '%San Mateo%' or
trpdate = '08/05/1997' and county like '%Alameda%' or
trpdate = '08/07/1997' and county like '%Sonoma%' or
trpdate = '08/08/1997' and county like '%Monterey%' or
trpdate = '08/09/1997' and county like '%Sonoma%' or
trpdate = '08/10/1997' and county like '%San Mateo%' or
trpdate = '08/11/1997' and county like '%San Luis Obispo%' or
trpdate = '08/11/1997' and county like '%San Mateo%' or
trpdate = '08/12/1997' and county like '%Monterey%' or
trpdate = '08/13/1997' and county like '%San Luis Obispo%' or
trpdate = '08/16/1997' and county like '%San Mateo%' or
trpdate = '08/17/1997' and county like '%San Luis Obispo%' or
trpdate = '08/18/1997' and county like '%San Mateo%' or
trpdate = '08/19/1997' and county like '%San Luis Obispo%' or
trpdate = '08/21/1997' and county like '%Alameda%' or
trpdate = '08/22/1997' and county like '%San Luis Obispo%' or
trpdate = '08/22/1997' and county like '%San Mateo%' or
trpdate = '08/22/1997' and county like '%Sonoma%' or
trpdate = '09/14/1997' and county like '%Monterey%' or
trpdate = '09/24/1997' and county like '%Monterey%' or
trpdate = '10/03/1997' and county like '%Sonoma%' or
trpdate = '10/03/1997' and county like '%San Mateo%' or
trpdate = '10/08/1997' and county like '%Monterey%' or
trpdate = '10/09/1997' and county like '%San Luis Obispo%' or
trpdate = '10/11/1997' and county like '%Monterey%' or
trpdate = '10/11/1997' and county like '%San Mateo%' or
trpdate = '10/13/1997' and county like '%San Luis Obispo%' or
trpdate = '10/14/1997' and county like '%San Mateo%' or
trpdate = '10/15/1997' and county like '%San Luis Obispo%' or
trpdate = '10/16/1997' and county like '%San Luis Obispo%' or
trpdate = '10/17/1997' and county like '%Alameda%' or
trpdate = '10/17/1997' and county like '%Monterey%' or
trpdate = '10/23/1997' and county like '%San Luis Obispo%' or
trpdate = '10/24/1997' and county like '%Monterey%' or
trpdate = '10/24/1997' and county like '%San Luis Obispo%' or
trpdate = '10/27/1997' and county like '%Sonoma%' or
trpdate = '10/28/1997' and county like '%San Mateo%' or
trpdate = '10/31/1997' and county like '%Monterey%' or
trpdate = '11/03/1997' and county like '%San Luis Obispo%' or
trpdate = '11/14/1997' and county like '%Monterey%' or
trpdate = '11/18/1997' and county like '%San Luis Obispo%' or
trpdate = '11/19/1997' and county like '%San Luis Obispo%' or
trpdate = '11/21/1997' and county like '%San Luis Obispo%' or
trpdate = '11/22/1997' and county like '%San Luis Obispo%' or
trpdate = '11/22/1997' and county like '%Monterey%' or
trpdate = '11/24/1997' and county like '%San Luis Obispo%' or
trpdate = '11/29/1997' and county like '%Monterey%' or
trpdate = '12/02/1997' and county like '%San Luis Obispo%' or
trpdate = '12/12/1997' and county like '%San Mateo%' or
trpdate = '12/12/1997' and county like '%San Luis Obispo%' or
trpdate = '12/13/1997' and county like '%San Luis Obispo%' or
trpdate = '12/16/1997' and county like '%San Mateo%' or
trpdate = '12/20/1997' and county like '%San Luis Obispo%' or
trpdate = '12/23/1997' and county like '%San Mateo%' or
trpdate = '12/23/1997' and county like '%Monterey%' or
trpdate = '12/30/1997' and county like '%San Luis Obispo%' or
trpdate = '12/31/1997' and county like '%Monterey%' or
trpdate = '01/07/1998' and county like '%San Luis Obispo%' or
trpdate = '02/26/1998' and county like '%Monterey%' or
trpdate = '02/27/1998' and county like '%Monterey%' or
trpdate = '04/11/1998' and county like '%Sonoma%' or
trpdate = '04/18/1998' and county like '%San Luis Obispo%' or
trpdate = '04/23/1998' and county like '%San Luis Obispo%' or
trpdate = '04/26/1998' and county like '%Monterey%' or
trpdate = '05/30/1998' and county like '%San Luis Obispo%' or
trpdate = '06/18/1998' and county like '%San Luis Obispo%' or
trpdate = '07/07/1998' and county like '%San Mateo%' or
trpdate = '07/12/1998' and county like '%San Luis Obispo%' or
trpdate = '07/12/1998' and county like '%San Mateo%' or
trpdate = '08/25/1998' and county like '%San Mateo%' or
trpdate = '08/26/1998' and county like '%San Mateo%' or
trpdate = '08/28/1998' and county like '%Monterey%' or
trpdate = '09/11/1998' and county like '%San Mateo%' or
trpdate = '09/18/1998' and county like '%Santa Cruz%' or
trpdate = '09/30/1998' and county like '%Monterey%' or
trpdate = '10/06/1998' and county like '%Monterey%' or
trpdate = '10/11/1998' and county like '%Santa Cruz%' or
trpdate = '10/20/1998' and county like '%Monterey%' or
trpdate = '10/24/1998' and county like '%Monterey%' or
trpdate = '12/08/1998' and county like '%San Mateo%' or
trpdate = '12/27/1998' and county like '%Monterey%' or
trpdate = '12/28/1998' and county like '%Sonoma%' or
trpdate = '12/30/1998' and county like '%Monterey%')
order by trpdate, fish_tl