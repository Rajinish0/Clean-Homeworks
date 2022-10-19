module HW2
import StdEnv

// Your Neptune code goes here : OBE63L

/*
A happy number is a number which eventually reaches 1 when replaced by the sum of the square of each digit.
      e.g: 13 is a happy number
            13 -> 1^2 + 3^2 = 10
           10 -> 1^2 + 0^2 = 1
      13 eventually reaches 1, so 13 is a happy number
      4 is not a happy number because it never reaches 1

      Write a function to check if a number is a happy number or not.

        ***Please dont use the abort function.***
*/


/*
The only way I can think of doing this is that 
I don't know when the process will approach 1, the only way I can be sure that
it doesn't approach 1 is if it loops back onto one of the numbers 
that it has already hit without ever hitting 1.
(I think the complexity of the algorithm could be improved 
if the visited numbers are stored in better formats than arrays (for which isMember is
more efficient))
*/

sumSquares :: Int -> Int
sumSquares 0 = 0
sumSquares x = (x rem 10)^2 + sumSquares (x/10)

isHappy :: Int [Int] -> Bool
isHappy 1 _ = True
isHappy x a
| isMember x a = False
= isHappy (sumSquares x) [x:a]

HappyNumber :: Int -> String
HappyNumber x 
| isHappy x [] = "Happy"
= "Not Happy"

//Start = HappyNumber 68 // "Happy"
//Start = HappyNumber 4 // "Not Happy"
//Start = HappyNumber 13 // "Happy"
//Start = HappyNumber 0 // "Not Happy"
//Start = HappyNumber 1 // "Happy"
//Start = HappyNumber 69



/*
    Write a function that takes two Integer lists of the same length and returns a list containing 
    sublists having three elements each, where the first element is the sum of the elements 
    at the same index in the two lists, and the second element is the product of the elements.
    e.g: [1,2,3] [4,5,6] -> [[5,4],[7,10],[9,18]]
        from first list      from second list      result
        1                     4                     [1+4, 1*4] = [5,4]
        2                     5                     [2+5, 2*5] = [7,10]
        3                     6                     [3+6, 3*6] = [9,18]
*/


// dont really need AllPairs [] [] = [] condition.
// im not checking if the lengths are not equal, since (I think) that function
// requires me to traverse the array, which I am already doing in these recursive calls.
AllPairs :: [Int] [Int] -> [[Int]]
AllPairs _ [] = []
AllPairs [] _ = []
AllPairs [x:xs] [y:ys] = [ [x+y, x*y] : AllPairs xs ys  ]

//Start = AllPairs [1,2,3] [4,5,6] // [[5,4],[7,10],[9,18]]
//Start = AllPairs [4,6,2,7] [8,3,5,1] // [[12,32],[9,18],[7,10],[8,7]]
//Start = AllPairs [] [] // []
//Start = AllPairs [] [1,2,3] // []
//Start = AllPairs [1,2,3] [] // []



