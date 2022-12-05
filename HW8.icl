module HW8
import StdEnv



:: Degree =   BSC | MSC | PHD     
:: Company = {companyName :: String, numberOfEmloeeys :: Int, employees :: {Employee}} 
:: Employee = { ename :: String,
                salary :: {Real}, //  array containing the salary of an employee for 12 months 
                age :: Int, 
                degree :: Degree   
              }

instance == Degree
	where (==) BSC BSC = True
		  (==) MSC MSC = True
		  (==) PHD PHD = True
		  (==) _ _ = False
 
e1 :: Employee
e1 = {ename = "e1" , salary = {10.0,20.0,30.0,40.0,50.0,10.0,20.0,30.0 ,40.0,50.0 ,10.0,90.0} , age = 40 , degree = BSC}
e2 :: Employee
e2 = {ename = "e2" , salary = {5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0 ,5.0,5.0 ,5.0,5.0} , age = 30 , degree = BSC}
e3 :: Employee
e3 = {ename = "e3" , salary = {50.0,20.0,20.0,20.0,20.0,20.0,20.0,20.0 ,20.0,20.0 ,20.0,20.0} , age = 60 , degree = MSC}
e4 :: Employee
e4 = {ename = "e4" , salary = {5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0 ,5.0,5.0 ,5.0,5.0} , age = 45 , degree = MSC}
e5 :: Employee
e5 = {ename = "e5" , salary = {100.0,110.0,110.0,40.0,50.0,30.0,30.0,30.0 ,140.0,50.0} , age = 80 , degree = PHD}
e6 :: Employee
e6 = {ename = "e6" , salary = {5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0 ,5.0,5.0 ,5.0,5.0} , age = 45 , degree = PHD}

cm1 :: Company
cm1 = {companyName = "cm1", numberOfEmloeeys = 3, employees = {e1,e3,e5}}
cm2 :: Company
cm2 = {companyName = "cm2", numberOfEmloeeys = 2, employees = {e1,e2}}
cm3 :: Company
cm3 = {companyName = "cm3", numberOfEmloeeys = 2, employees = {e1,e3}}
cm4 :: Company
cm4 = {companyName = "cm4", numberOfEmloeeys = 3, employees = {e1,e3,e5}}
cm5 :: Company
cm5 = {companyName = "cm5", numberOfEmloeeys = 3, employees = {e2,e3,e6}}


/*
    Write a function that takes an array of companies and a Degree and returns the company with the highest average salary for employees with the given degree.


    average salary of a company is the average of the average salaries of its employees with the given degree.
    If the list of companies is empty, the function should return error message "No companies in the list".
    If there are more than one company with the same average salary, the function should return the first company in the list.

    eg:
    highestAverageSalary [{cm1,cm2,cm3,cm4,cm5}, BSC] = cm2
*/

instance < Company
	// this should sort it in the descending order (because of >) with the first max at the first position of the array.
	where (<) c1 c2 = (getAvg c1.employees) > (getAvg c2.employees)

instance == Company
	where (==) c1 c2 = (getAvg c1.employees) == (getAvg c2.employees)

getAvgSalary :: {Real} -> Real
getAvgSalary x = (sum j) / ( toReal (length j) )
			  where j = [sal \\ sal <-: x]

getAvg :: {Employee} -> Real
getAvg x
| length y == 0 = 0.0
= 	( (sum y) / (toReal (length y) ) )
	 where y = [getAvgSalary emp.salary \\ emp <-: x]
 
modify :: Company Degree -> Company
modify cmp deg = {cmp & employees = {emp \\ emp <-: cmp.employees | emp.degree == deg} }

highestAverageSalary :: {Company}  Degree -> Company
highestAverageSalary x deg 
| mylist == [] = abort "No comapnies in the list"
= hd (sort mylist)
where mylist = [modify c deg \\ c <-: x]



//Start = highestAverageSalary {cm1,cm2,cm3,cm4,cm5} BSC // cm1


//Start = highestAverageSalary {cm1, cm2, cm3, cm4, cm5} PHD

//Start = highestAverageSalary {cm2,cm3} MSC // cm3

//Start = highestAverageSalary {} PHD // no companies in the list






/*

Define MyPair

MyPair is a type synonym for a tuple of two elements of type (x,y) where x is an element of type char and y is its number of occurrences.

write a function that takes an array of Char and returns an array of MyPair where each element of the array is paired with the number of occurrences of that element in the array.


eg:
    {'a','b','a','c','a','b','a'} = {('a',4),('b',2),('c',1)}

    {'1','2' ,'5' ,'2','5'} = {('1',1),('2',2),('5',2)}

    {} = {}


*/


:: MyPair :== (Char, Int)

updateDict :: Char [MyPair] -> [MyPair]
updateDict c [] = [(c, 1)]
updateDict c [(ch, occ):xs]
| c == ch = [(ch, occ+1)] ++ xs
= [(ch, occ)] ++ (updateDict c xs)

CountOccAux :: {Char} Int Int [MyPair] -> {MyPair}
CountOccAux arr ind l occs
| ind == l = {occ \\ occ <- occs}
= CountOccAux arr (ind+1) l (updateDict arr.[ind] occs) // array has const look up time, just wanted to try this instead of cnvting to list


CountOcc :: {Char} -> {MyPair}
CountOcc x = CountOccAux x 0 (length [y \\ y <-: x]) []


//Start = CountOcc {'1','2' ,'5' ,'2','5'} // {('1',1),('2',2),('5',2)}
//Start = CountOcc {'a','b','a','c','a','b','a'} //{('a',4),('b',2),('c',1)}