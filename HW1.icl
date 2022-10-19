module HW1
import StdEnv

//Please write your neptun code here: OBE63L
/*
Your submission should not have any errors when running the code.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures, however, you can add as many functions as 
you wish, just make sure to name them appropriately (if function squares the number, call it 'square',
'second_power', etc. and not 'f' or 'g'). The same goes for variable names. 

Make sure that you comment all 'Start'-s before submitting the code.

    Hint : 
        1. The basis of functional programming is functions , if a problem can't be solved
        with one function divide it into multiple functions.
        2. To abort with a message you can use the 'abort' function.
        3. The logic needs to be thought by you , the implementation you can figure out 
        by going through lecture slides and practice material.
*/






//Define a function to find the minimum number of currency notes to be returned by an ATM machine for a given amount of money.
//The currency notes available in the ATM are 10, 5,2 and 1.
//The input is always a positive integer.

// the succinct way could be:
//(x/10) + (x rem 10)/5 + ((x rem 10) rem 5)/2 + ((x rem 10) rem 5) rem 2
// but I used recursion.

ATM :: Int -> Int
ATM 1 = 1
ATM 0 = 0
ATM x
| (x >= 10) = (x/10 + ATM ( x rem 10 ) )
| (x >= 5) =  (x/5 + ATM (x rem 5) )
| (x >= 2) =  (x/2 + ATM (x rem 2) )
= abort "No neg values"


//Start = ATM 100  // 10
//Start = ATM 99  // 12
//Start = ATM 28  // 5
//Start = ATM 1  // 1
//Start = ATM 9 // 1 + 2 = 3



/*
      write a function to find the greatest common divisor of two numbers.
      If the gcd is one of the input numbers, then print "y is a multiple of x"
      otherwise print "neither number is a multiple of the other".
      If one of the input numbers is 0, then print "Cannot calculate gcd if  of 0" and abort.
      e.g: input: 6 18
           output: 6
           explanation: 6 is one of the input numbers, so print "either number is a multiple of the other"
      eg: input: 21 28
          output: 7
          explanation: 7 is a not any of the input numbers, so print "neither number is a multiple of the other"

          
*/



// since the problem requires to find gcd, I have written a seperate function for that:
// From number theory
// a = bq + r, so gcd(a, b) = gcd(b, r) where r = a%b; since r is strictly decreasing
// it will approach 0.

// getGCD assumes inputs to be greater than 0.
// therefore im just checking if the second argument is 0
// which is always the remainder from the recursion
getGCD :: Int Int -> Int
getGCD x 0 = x
getGCD x y
| (x > y) = getGCD y (x rem y)
= getGCD x (y rem x)


MyFunGCD :: Int Int -> String
MyFunGCD x y
| ( (x == 0) || (y == 0) ) = abort "Cannot calculate gcd of 0"
| ((getGCD x y)  == x || ((getGCD x y) == y)) = "either num is a multiple of the other"
= "niether number is a multiple of the other"


//Start = MyFunGCD 6 18 // "either number is a multiple of the other"
//Start = MyFunGCD 21 28  // "neither number is a multiple of the other"
//Start = MyFunGCD 0 0 // "Cannot calculate gcd of 0" 