module rajinishHW5
import StdEnv

/*
Rename the file as YourNameHW5.icl
You can make as many functions as you want but the second and third task shouldnt use any function with recursion.
The first function can use recursion.
*/

/////////Task 1///////// 

/*
write a function that takes three lists , where first list contains names of students, second list contains their grades and third list contains their ages.
The function should return a list of tuples, where each tuple contains name, grade and age of a student.
If the lists are of unequal length, the data upto the length of the shortest list should be returned.
*/


ZipData :: [String]  [Int]  [Int] -> [(String, Int, Int)]
ZipData x y z = [(u, v, w) \\ u <- x & v <- y & w <- z]


//Start = ZipData ["John", "Jane", "Jack"] [90, 80, 70] [20, 21, 22] // [("John",90,20),("Jane",80,21),("Jack",70,22)]
//Start = ZipData ["John", "Jane", "Jack"] [90, 80, 70] [20, 21] // [("John",90,20),("Jane",80,21)]

/*
Write another function that takes the list of tuples returned by the above function and creates
a new list of tuples having four elements Name,grade,age and funscore
funscore is calculated as : (grade * 100) / age
****You Should Not use Recursion for this Problem****

*/


AuxData :: [(String, Int, Int)] -> [(String, Int, Int, Int)]
AuxData x = map (\(x, y, z) = (x, y, z, (y*100)/z)) x

//Start = AuxData ( ZipData ["John", "Jane", "Jack"] [90, 80, 70] [20, 21, 22])


/*
The School wants to Award Students who's funscore is more than 350.

Write the Award Function that takes the three lists and returns a list of names of students who are awarded.

****You Should Not use Recursion for this Problem****
***You Should Use the functions defined above***
*/

AwardStudents :: [String] [Int] [Int] -> [String]
AwardStudents x y z = [x \\ (x, y, z, q) <- ( AuxData (ZipData x y z) ) | q > 350] 


//Start = AwardStudents ["John", "Jane", "Jack"] [90, 80, 70] [20, 21, 22] // ["John","Jane"]
//Start = AwardStudents ["John", "Jane", "Jack","Peter"] [90, 80, 70] [20, 21] // ["John","Jane"]





