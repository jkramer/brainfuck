
import System.Environment
import System.IO
import Data.Char


data RunTime = RunTime Int [Int] deriving (Show)


-- RunTime constructor.
newRunTime = RunTime 0 $ replicate 512 0


-- Main function.
main = getArgs >>= mapM_ (\ p -> readFile p >>= execute newRunTime)


-- Execute the brainfuck code from a string.
execute runTime "" = return runTime

execute runTime (command:rest) = do
	case command of
		'+' -> execute (increase runTime) rest
		'-' -> execute (decrease runTime) rest
		'>' -> execute (up       runTime) rest
		'<' -> execute (down     runTime) rest

		',' -> input runTime >>= flip execute rest
		'.' -> output runTime >> execute runTime rest
		'#' -> print runTime >> execute runTime rest

		'[' -> runLoop loop runTime >>= flip execute loopRest

		_   -> execute runTime rest

	where
		loop = init (loopCode rest 1)
		loopRest = drop ((length loop) + 1) rest


-- Increase the value under the pointer in memory.
increase = changeMemory (+ 1)


-- Decrease the value under the pointer.
decrease = changeMemory (+ (- 1))


-- Move the pointer to the next register.
up (RunTime offset memory) = RunTime (offset + 1) memory


-- Move the pointer to the previous register.
down (RunTime offset memory) = RunTime (offset - 1) memory


-- Read a character into the register at the current position.
input runTime = safeGetChar >>= return . flip changeMemory runTime . const . ord


-- Read a character and return it (or \0 if EOF is reached).
safeGetChar = hIsEOF stdin >>= \ eof -> if eof then return '\0' else getChar


-- Print the character in the current register.
output = putChar . chr . currentValue


-- Take a callback, apply it on the value of the current register.
changeMemory callback (RunTime offset memory) =
	let (left, right) = splitAt offset memory
	in RunTime offset (left ++ (callback $ head right) : (tail right))


-- Return the value of the current register.
currentValue (RunTime offset memory) = memory !! offset


-- Run a piece of code until the value of the current register is zero.
runLoop code runTime =
	if (currentValue runTime) == 0
		then return runTime
		else execute runTime code >>= runLoop code


-- Extract code until the end of the current loop.
loopCode _ 0                  = []
loopCode "" _                 = error "no closing bracket"
loopCode (command:rest) level =
    command : (loopCode rest level')
    where
        level' = case command of
            '[' -> level + 1
            ']' -> level - 1
            _   -> level

