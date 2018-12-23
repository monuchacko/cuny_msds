#Question:
#Which destination in the flights database is the furthest distance away, based on information in the flights table. 
#Show the SQL query(s) that support your conclusion.
Select origin, dest, max(distance) 
from flights 
group by origin, dest
order by distance desc;

#Question:
#What are the different numbers of engines in the planes table?  
#For each number of engines, which aircraft have 
#the most number of seats?  Show the SQL statement(s) that support your result.

#What are the different numbers of engines in the planes table?  
Select engine, count(*)
from planes
group by engine;

#For each number of engines, which aircraft have 
#the most number of seats?  Show the SQL statement(s) that support your result.
Select engine, manufacturer, seats, sum(seats)
from planes
group by manufacturer, engine, seats
order by engine desc, sum(seats) desc;

#Question:
#Show the total number of flights.
Select count(flight) As TotalNoOfFlights
from flights;

#Question:
#Show the total number of flights by airline (carrier).
Select carrier, count(distinct flight) As TotalNoOfFlights
from flights
group by carrier;

#Question:
#Show all of the airlines, ordered by number of flights in descending order.
SELECT 
    airlines.carrier, sum(flights.flight) As NoOfFlights
FROM
    airlines
        INNER JOIN
    flights ON airlines.carrier = flights.carrier
group by airlines.carrier
order by sum(flights.flight) desc;

#Question:
#Show only the top 5 airlines, by number of flights, ordered by number of flights in descending order.
SELECT 
    airlines.carrier, sum(flights.flight) As NoOfFlights
FROM
    airlines
        INNER JOIN
    flights ON airlines.carrier = flights.carrier
group by airlines.carrier
order by sum(flights.flight) desc
limit 5;

#Question:
#Show only the top 5 airlines, by number of flights of distance 1,000 miles or greater, ordered by number of 
#flights in descending order
SELECT 
    airlines.carrier, sum(flights.flight) As NoOfFlights
FROM
    airlines
        INNER JOIN
    flights ON airlines.carrier = flights.carrier
where distance >= 1000
group by airlines.carrier
order by sum(flights.flight) desc
limit 5;

#Question:
#Create a question that (a) uses data from the flights database, and (b) requires aggregation to answer it, and 
#write down both the question, and the query that answers the question.

#Answer:
#Find carrier, no of flights that went more than 5 hours and 30 minutes
SELECT 
    airlines.carrier, sum(flights.flight) As NoOfFlights
FROM
    airlines
        INNER JOIN
    flights ON airlines.carrier = flights.carrier
where hour >= 5 and minute > 30
group by airlines.carrier
order by sum(flights.flight) desc;