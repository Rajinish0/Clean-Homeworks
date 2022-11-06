module HW6
import StdEnv


:: University = Elte | Corvinus | BME

:: UniRelation = Teacher | Student

:: Course = FP | OOP | DB | AI | ML


/*
Create a record Citizen with the following fields:
id::Int
uni::University
grades::[Int]
courses::[Course]
rel::UniRelation
*/


:: Citizen = {id :: Int,
			uni :: University,
			grades :: [Int],
			courses :: [Course],
			rel :: UniRelation}


/*
You are given an array of citizens , write a function that returns the list of (id,uni,rel) pair of citizens 
who will be awarded. a Citizen can be awarded if he/she has at least 3 courses , the average of his/her grades is at least 3.

*/

/*
I dont think I need to convert this to real, since all i need to do is check if it's greater than to equal to 3
and anything >3 and <4 will get floored to 3.
*/
getAvg :: [Int] -> Int
getAvg [] = 0
getAvg x = (sum x)/(length x)

AwardCitizen :: {Citizen}-> [(Int,University,UniRelation)]
AwardCitizen citizens = [(x.id, x.uni, x.rel) \\ x <-: citizens | ( (length x.courses) >= 3 && ((getAvg x.grades) >= 3) )]



//Start =  AwardCitizen {{id = 1,uni = Elte, grades = [4,2,4,1,4,6], courses = [FP,DB,AI] ,rel = Student } , {id = 2,uni = Elte, grades = [4,6,5,2,4,1,4,6], courses = [FP,ML] ,rel = Student }, {id = 3,uni = BME, grades = [4,2,5,1,8,10,4,6], courses = [FP] ,rel = Teacher }}
// [(1,Elte,Student)]

//Start =  AwardCitizen {{id = 1,uni = Elte, grades = [4,2,4,1,4,6], courses = [FP,DB,AI] ,rel = Student } , {id = 2,uni = Elte, grades = [4,6,5,2,4,1,4,6], courses = [FP,DB,AI,ML] ,rel = Student }, {id = 3,uni = BME, grades = [4,2,5,1,8,10,4,6], courses = [FP,DB,AI] ,rel = Teacher }}
// [(1,Elte,Student),(2,Elte,Student),(3,BME,Teacher)]

//Start  =  AwardCitizen {} // []


/*
Write a function that takes an array of Strings and removes consonants from each string.

Hint : a string is an array of characters
*/

remCons :: [Char] [Char] -> [Char]
remCons x vowels = filter (\x = (isMember (toLower x) vowels)) x 


RemoveCons :: {String} -> {String}
RemoveCons arr = {toString (remCons (fromString x) ['a', 'e', 'i', 'o', 'u'])  \\ x <-: arr}

//Start = RemoveCons {"Functional", "Programming", "is", "NOT", "fun"} // more realistic
//Start = RemoveCons {"hello","world","how","are","you"} // {"eo","o","o","ae","ou"}
//Start = RemoveCons {"Functional","Programming","is","fun"} // {"uioa","oai","i","u"}

