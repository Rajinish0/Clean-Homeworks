module rajinishHW3
import StdEnv

//Please Rename the file as YourNameHW3.icl

/*

    You're safe online if all your passwords are strong.
    A password is strong if it is at least 8 characters long and contains at 
    least one uppercase letter, one lowercase letter, one digit and 
    one special character. 
    The special characters are: !@#$%^&*()_+-=[]{}|;':",./<>?~`

    Write a function strongPassword that takes a list of passwords and checks if 
    all your passwords are strong.

    Eg : Input : ["Hello@World9", "HelloWorld1!", "HelloWorld1!"]
        Output : "All passwords are strong"

        Input : ["JohnDow","Hellom","yotoo@123"]
        Output : "All passwords are not strong"

        Hint : use the function fromString x to convert string x to a list of char.

        **** Please dont put your own passwords as a test case ****
*/

/*
PLEASE READ BEFORE GOING THROUGH THE CODE, I HAVE EXPLAINED THE VARIABLE NAMES

I need to keep a track of if I have seen a lowerCase, upperCase, specialCharacter, one of the digits.
I could have used a boolean array for that, but I decided to use a single integer (named y) (0000)
for that.
if I see a lowerCase I turn the first bit on (0001) by doing the bitwise OR operator with 1,
so on and so forth for other conditions like if it is upperCase I turn the second bit on by doing a 
bitor with 2.
if it satisfies all the conditions the final number will be (1111) which is 15. 
We haven't learned bitor in the class, but I was familiar with that func in other languages and 
looked up the equivalent function in clean.

I initially thought of using just one boolean to evaluate this, but couldnt think of a way to go 
forward with that. 
And then thought of using a boolean array that kept track of each of the required conditions and then in the end
getting an and for all of that array, but that would have been too much code ig.

spChr -> shorthand for special characs array
digits -> char type array of digits 
*/


// i am calculating the length as i go along, because again i dont want to call length
// since (i think) that requires me to traverse the array which I am already doing.
isStrong :: [Char] [Char] [Char] Int Int -> Bool

isStrong [] spChr digits y len = ( (y == 15) && (len >= 8) ) // no chars left to parse

isStrong [x : xs] spChr digits y len

| isLower x = isStrong xs spChr digits (y bitor 1) (len + 1)
| isUpper x = isStrong xs spChr digits (y bitor 2) (len + 1)
| isMember x spChr = isStrong xs spChr digits (y bitor 4) (len + 1)
| isMember x digits = isStrong xs spChr digits (y bitor 8) (len + 1)

= isStrong xs spChr digits y (len + 1) // could be a space


allAreStrong :: [String] [Char] [Char] -> Bool
allAreStrong [] spChr digits = True
allAreStrong [x : xs] spChr digits
| isStrong (fromString x) spChr digits 0 0 = allAreStrong xs spChr digits
= False

strongPassword :: [String] -> String
strongPassword [] = abort "No passwords are given" // needed to add this so that it doesn't give an empty array to allAreStrong
strongPassword x 
| allAreStrong x (fromString "!@#$%^&*()_+-=[]{}|;':\",./<>?~`")
				 (fromString "0123456789") = "All passwords are strong"
= "All passwords are not strong"

//Start = strongPassword ["Hello@World9", "HelloWorld1!", "Helloworld@123"] // "All passwords are strong"
//Start = strongPassword ["JohnDow","Hellom","yotoo@123"] // "All passwords are not strong"
//Start = strongPassword ["bbe32@B"] // not strong because length is 7
//Start = strongPassword ["gagaga321~`G"] // strong
//Start = strongPassword ["    69<|^aP"] // strong
//Start = strongPassword ["cQ891!vvv"] // strong
//Start = strongPassword ["cQ8911111"] // not strong
//Start = strongPassword ["cQ891111."] // strong
//Start = strongPassword ["cQ8911\"11"] // strong



/*
    Write a function that takes a list of Real numbers and returns a list of lists 
    where each sublist contains two numbers where first number is the element from the list 
    and the second number is the percentage of frequency of that element in the list.

    eg : Input : [1,2,1,3] 
        Output : [[1,50],[2,25],[3,25]]
        explanation : 1 -> occurs 2 times in the list , total elements in the list = 4 so its percentage is 50
                    2 -> occurs 1 time in the list , total elements in the list = 4 so its percentage is 25
                    3 -> occurs 1 time in the list , total elements in the list = 4 so its percentage is 25

        percentage as integer is fine
*/



//func takes the number, if it's already in the list it increments the counter by 1
// else when it's at the end of the list(meaning that it did not find the elemnet)
// it initiates the counter to one

func :: Int [[Int]] -> [[Int]]
func x [] = [[x, 1]]
func x [[x1, y1]: xs] 
| (x1 == x) = [[x1, y1 + 1] : xs]
= [[x1, y1]: func x xs]

// freq1 just iterates through the numbers and passes it to func
// freq1 basically gets the count of each number in the list, 
// func checks if it has already seen that number before, if yes it increases the counter by 1
// if not it sets the counter to 1
freq1 :: [Int] [[Int]] -> [[Int]]
freq1 [] y = y
freq1 [x: xs] y = freq1 xs (func x y)

//used a lambda function, to just divide the count of each number by the length of the array.
frequency :: [Int] -> [[Int]]
frequency x = map (\[z, y] = [z, (y*100)/(length x)] ) (freq1 x [])


/*
problem statement asked to write a function that takes an array of type Real, but
since all these test cases had type Int and the frequencies were int, I wrote a function for [Int].
*/
//Start = frequency [1,2,1,3] // [[1,50],[2,25],[3,25]]
//Start = frequency [1,1,1,3,2,2] // [[1,50],[2,33],[3,16]]
//Start = frequency [1,2,5,5,1,1,0,0] // [[1,37],[2,12],[5,25],[0,25]]
//Start = frequency []
//Start = frequency [2, 4, 4, 4, 19, 0, 2, 2, 4, 4, 9, 8]
//Start = frequency [-1, -2, -1, -1, -1]