-- Section look 

WITH SEC_OFF AS (
SELECT COUNT(*) CC,
       CRS.COURSES_ID,
	   SEC.SEC_TERM
FROM L56_COURSE_SECTIONS SEC INNER JOIN 
	 ODS_COURSES CRS ON CRS.COURSES_ID = SEC.SEC_COURSE
WHERE
SEC.L56_SEC_CATEGORY in ('Online Education' , 'College F2F')
AND left(SEC.SEC_TERM,4) < 2020
AND SEC.SEC_CURRENT_STATUS = 'A'

GROUP BY 
CRS.COURSES_ID,
SEC.SEC_TERM
)




SELECT 
SEC.SEC_NAME, 
CRS.CRS_NAME,
SEC.TERM_REPORTING_YEAR Acad_Year,
CRS.COURSES_ID,
SEC2.SEC_NAME NamePY, 
SEC.SEC_TERM TERM, 
SEC2.SEC_TERM TERM_PY,
SEC.ACTIVE_STUDENT_COUNT, 
SEC2.SUCCESSFUL_COMP SS_PY,
SEC2.ACTIVE_STUDENT_COUNT Count_PY,
ISNULL(SEC2.WITHDRAWN_STUDENT_COUNT ,0) WD_PY ,
ISNULL(SEC2.DROPPED_STUDENT_COUNT,0) DROP_PY,
ISNULL(x.CC_PY,0) CC_PY,
So.CC as SEC_Count,
( SELECT
	AVG(NBR_OF_ENROLLMENTS)

	FROM
	(
		SELECT ENR.SCS_COURSE_SECTION AS SECTION_ID,
		COUNT(*) as NBR_OF_ENROLLMENTS

		FROM L56_STUDENT_ENROLLMENT ENR

		WHERE 
		ENR.STC_COURSE_NAME = CRS.CRS_NAME 
		AND ENR.L56_REPORTING_YEAR between SEC.TERM_REPORTING_YEAR -4 and SEC.TERM_REPORTING_YEAR -1
		AND RIGHT(ENR.STC_TERM ,2) = Right(SEC.SEC_TERM,2)
		AND (ENR.STC_CURRENT_STATUS in ('A', 'NR') OR ENR.STC_GRADE = 'W')

		GROUP BY ENR.SCS_COURSE_SECTION
	) x
) as AvgSecEnr3yr,

( SELECT
	AVG(NBR_OF_ENROLLMENTS)

	FROM
	(
		SELECT ENR.SCS_COURSE_SECTION AS SECTION_ID,
		COUNT(*) as NBR_OF_ENROLLMENTS

		FROM L56_STUDENT_ENROLLMENT ENR

		WHERE 
		ENR.STC_COURSE_NAME = CRS.CRS_NAME 
		AND ENR.L56_REPORTING_YEAR between 2012 and SEC.TERM_REPORTING_YEAR -1
		AND RIGHT(ENR.STC_TERM ,2) = Right(SEC.SEC_TERM,2)
		AND (ENR.STC_CURRENT_STATUS in ('A', 'NR') OR ENR.STC_GRADE = 'W')

		GROUP BY ENR.SCS_COURSE_SECTION
	) x
) as AvgSecEnrto



FROM L56_COURSE_SECTIONS SEC LEFT join 

     ODS_COURSES CRS ON CRS.COURSES_ID = SEC.SEC_COURSE LEFT JOIN 

	 L56_COURSE_SECTIONS SEC2 on SEC.SEC_NAME = SEC2.SEC_NAME and cast(cast(Left(SEC2.SEC_TERM,4) as int) +1 as varchar) + right(SEC2.SEC_TERM,2) =SEC.SEC_TERM   INNER JOIN
	 SEC_OFF SO on SO.COURSES_ID = CRS.COURSES_ID and SO.SEC_TERM = SEC.SEC_TERM  LEFT JOIN
	 
	 (SELECT 
			COURSES_ID,
			SEC_TERM,
			 COUNT(*) CC_PY
	  FROM L56_COURSE_SECTIONS SEC inner join 
			ODS_COURSES CRS ON CRS.COURSES_ID = SEC.SEC_COURSE 
      WHERE 
			SEC.L56_SEC_CATEGORY in ('Online Education' , 'College F2F')
			AND left(SEC.SEC_TERM,4) < 2020
			AND SEC.SEC_CURRENT_STATUS in ('C' , 'S')
	  
	  GROUP BY
	   COURSES_ID,
	   SEC_TERM ) x 
			
			on X.COURSES_ID =CRS.COURSES_ID and 
			cast(cast(Left(x.SEC_TERM,4) as int) +1 as varchar) + right(x.SEC_TERM,2) =SEC.SEC_TERM 

WHERE 
SEC.L56_SEC_CATEGORY in ('Online Education' , 'College F2F')
AND left(SEC.SEC_TERM,4) < 2020
AND SEC.SEC_CURRENT_STATUS = 'A'
AND SEC2.SEC_CURRENT_STATUS = 'A'
AND SEC2.L56_SEC_CATEGORY in ('Online Education' , 'College F2F') 
AND left(SEC2.SEC_TERM,4) <2020

ORDER BY SEC.SEC_NAME , SEC.SEC_TERM






SELECT SEC.SEC_TERM, SEC.SEC_CURRENT_STATUS 
FROM ODS_COURSES CRS INNER JOIN L56_COURSE_SECTIONS  SEC on SEC.SEC_COURSE = CRS.COURSES_ID
WHERE CRS.CRS_NAME = 'ART-148'









--SEC DATE TIME INFO
SELECT SEC_NAME,
	   SEC_TERM TERM,
	     
	   MAX(CASE WHEN MT.CSM_MONDAY IS NULL THEN 0 ELSE 1 END ) MON,
	   MAX(CASE WHEN MT.CSM_TUESDAY IS NULL THEN 0 ELSE 1 END ) TUES,
	   MAX(CASE WHEN MT.CSM_WEDNESDAY IS NULL THEN 0 ELSE 1 END ) WEDNESDAY,
	   MAX(CASE WHEN MT.CSM_THURSDAY IS NULL THEN 0 ELSE 1 END ) THURSDAY,
	   MAX(CASE WHEN MT.CSM_FRIDAY IS NULL THEN 0 ELSE 1 END ) FRIDAY,
	   MAX(CASE WHEN MT.CSM_SATURDAY IS NULL THEN 0 ELSE 1 END ) SATURDAY,
	   MAX(CASE WHEN MT.CSM_SUNDAY IS NULL THEN 0 ELSE 1 END ) SUNDAY,
	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time) < '08:00:00' Then 1 Else 0 END) B48,
	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time)  between '08:00:00' and  '08:30:00' Then 1 Else 0 END) T8830,
	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time)  between '08:30:00' and  '08:59:00' Then 1 Else 0 END) T8309,


	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time)  between '09:00:00' and  '09:29:00' Then 1 Else 0 END) T9930,
	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time)  between '09:30:00' and  '09:59:00' Then 1 Else 0 END) T93010,

	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time)  between '10:00:00' and  '10:29:00' Then 1 Else 0 END) T101030,
	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time)  between '10:30:00' and  '10:59:00' Then 1 Else 0 END) T103011,

	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time)  between '11:00:00' and  '11:30:00' Then 1 Else 0 END) T111130,
	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time)  between '11:30:00' and  '11:59:00' Then 1 Else 0 END) T113012,

	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time)  between '12:00:00' and  '12:30:00' Then 1 Else 0 END) T121230,
	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time)  between '12:30:00' and  '12:59:00' Then 1 Else 0 END) T12301,

	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time)  between '13:00:00' and  '13:30:00' Then 1 Else 0 END) T1130,
	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time)  between '13:30:00' and  '13:59:00' Then 1 Else 0 END) T1302,



	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time)  between '14:00:00' and  '14:29:00' Then 1 Else 0 END) T2230,
	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time)  between '14:30:00' and  '14:59:00' Then 1 Else 0 END) T2303,

	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time)  between '15:00:00' and  '15:29:00' Then 1 Else 0 END) T3330,
	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time)  between '15:30:00' and  '15:59:00' Then 1 Else 0 END) T3304,

	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time)  between '16:00:00' and  '16:29:00' Then 1 Else 0 END) T4430,
	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time)  between '16:30:00' and  '16:59:00' Then 1 Else 0 END) T4305,

	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time)  between '17:00:00' and  '17:29:00' Then 1 Else 0 END) T5530,
	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time)  between '17:30:00' and  '17:59:00' Then 1 Else 0 END) T5306,

	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time)  between '18:00:00' and  '18:29:00' Then 1 Else 0 END) T6630,
	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time)  between '18:30:00' and  '18:59:00' Then 1 Else 0 END) T6307,

	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time)  between '19:00:00' and  '19:29:00' Then 1 Else 0 END) T7730,
	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time)  between '19:30:00' and  '19:59:00' Then 1 Else 0 END) T7730,

	   MAX(CASE WHEN cast(MT.CSM_START_TIME  as time)  > '20:00:00' Then 1 Else 0 END) Aft8,
	   MAX(CASE WHEN SEC.L56_SEC_CATEGORY = 'Online Education' Then 1 ELSE 0 END) OnlineED,
	   MAX(CASE WHEN SEC.L56_SEC_CATEGORY = 'College F2F' Then 1 ELSE 0 END) F2F

FROM
ODS_COURSES CRS inner join 
	   L56_COURSE_SECTIONS SEC on SEC.SEC_COURSE = CRS.COURSES_ID INNER JOIN 
	    L56_COURSE_SECTION_MTG MT on MT.CSM_COURSE_SECTION = SEC.COURSE_SECTIONS_ID
WHERE 
SEC.L56_SEC_CATEGORY in ('Online Education' , 'College F2F')
AND SEC.SEC_CURRENT_STATUS = 'A'
AND left(SEC.SEC_TERM,4) < 2020

GROUP BY 
SEC_NAME,
	   SEC_TERM 