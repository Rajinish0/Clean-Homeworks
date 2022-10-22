module HW4
import StdEnv

/*
write a function that takes a matrix as list of lists and returns the transpose of the matrix
eg :
Input : [[1,2,3],
         [4,5,6],
         [7,8,9]]
Output : [[1,4,7],
          [2,5,8],
          [3,6,9]]
If there is an empty list or a list of different length, return an empty list

***Needs to use atleast one higher order function***
*/

fTranAux :: [[a]] Int [[a]] -> [[a]]
fTranAux [] _ out = out
fTranAux x len out
| length (map hd x) <> len = [] // invalid matrix
= fTranAux (filter (\z = (length z) > 0) (map tl x) ) len (out ++ [map hd x]) // filter out empty arrays, they must all run out of elements at the same time
// if they don't run out of elements at the same time then, the array was invalid 


funTran :: [[a]] -> [[a]]
funTran x = fTranAux x (length x) []
 

//Start = funTran [[1,2, 3],[4, 5, 6],[7,8,9]] // [[1,4,7],[2,5,8],[3,6,9]]
//Start = funTran [[1,2],[3,4],[5,6]] // [[1,3,5],[2,4,6]]
//Start = funTran [['a','b','c'],['d','e','f'],['g','h','i']] // [['a','d','g'],['b','e','h'],['c','f','i']]
//Start = funTran [['b','c'],['d','e','f'],['g','h','i']] // []
//Start = funTran [[1, 2], [3, 4]]
//Start = funTran [[1, 2, 3]]
//Start = funTran [[1]]
//Start= funTran [[1], [2]]


/*
Write a function that takes two matrices and returns the sum of the two matrices
If the matrices are not of the same order, return an error message
eg :
Input : [[1,2,3],     [[1,2,3],
         [4,5,6],      [4,5,6],
         [7,8,9]]      [7,8,9]]
Output : [[2,4,6],
          [8,10,12],
          [14,16,18]]
Needs to use atleast one higher order function
*/


/*


zipping it first, then mapping a function that sums a tuple of two arrays

if the arrays don't have same length an error will be thrown, (the columns are not equal)

I could check for length at the start but that's inefficient.
*/

sumArrs :: ([Int], [Int]) -> [Int]
sumArrs ([], []) = []
sumArrs (_, []) = abort "Error dims not compatible"
sumArrs ([], _) = abort "Error dims not compatible"
sumArrs ([x:xs], [y:ys]) = [x+y] ++ sumArrs (xs, ys) 

funSum :: [[Int]] [[Int]] -> [[Int]]
funSum x y | (length x) <> (length y) = abort "Error dims not compatible"
= map sumArrs (zip (x, y))

//Start = funSum [[1,2,3],[4,5,6],[7,8,9]] [[1,2,3],[4,5,6],[7,8,9]] // [[2,4,6],[8,10,12],[14,16,18]]
//Start = funSum [[1,2],[3,4],[5,6]] [[1,2],[3,4],[5,6]] // [[2,4],[6,8],[10,12]]
//Start = funSum [[1,2],[3,4],[5,6]] [[1,2],[3,4]] // Error
//Start = funSum [[],[1,3]] [[1,2],[4,5]] // Error
//Start = funSum [[1], [2]] [[2], [3]]
//Start= funSum [[1, 2, 3]] [[1, 2, 4]]







/*
JUST FOR FUN, THIS IS NOT AN OFFICIAL SUBMISSION.
this I think is more optimized and doesnt use any builtin funcs, 
but since it doesn't have a higher order function, 
I'll comment this out for now.


creates a new transpose matrix by traversing the array row down first. 
*/

/*
funTran2 :: [[a]] [[a]] [a] Int Int -> [[a]]
funTran2 [] finout cOut ind l 
| (ind == l) = (finout++[cOut]) // if the numElementsParsed (down) is not equal to the length of the row
								// that means one of the rows had less values, and got deleted when it was empty
= []

funTran2 [[z:zs]:xs] finout curout (curInd) l
// have reached the end of the row, add the parsed values to finalOutPut, 
// restart curOut buffer to [] and start reading the second column into it.
| (curInd == l) = funTran2 [[z:zs]:xs] (finout++[curout]) [] 0 l 

// [[]] ++ [array] = [array, []] which caused some errors
// so I dont add [[]] and just delete that row.
| (length zs == 0) = funTran2 (xs) finout (curout++[z]) (curInd+1) l
// send the parsed row to the bottom of the matrix, so that the
// next row can be parsed.   
= funTran2 (xs++[zs]) finout (curout++[z]) (curInd+1) l  

myFunTran :: [[a]] -> [[a]]
myFunTran x = funTran2 x [] [] 0 3

//Start = myFunTran [[1, 2, 3],[4,5,6],[7,8,9]] // [[1,4,7],[2,5,8],[3,6,9]]
//Start = myFunTran [['a','b','c'],['d','e','f'],['g','h','i']] // [['a','d','g'],['b','e','h'],['c','f','i']]
//Start = myFunTran [[2, 3],[4,5,6],[7,8,9]] // []
*/
