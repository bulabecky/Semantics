import Data.List
import System.IO

-- Autumn 2016 paper
tr::[[a]]->[[a]] 
tr ([]:_) = []
tr (a) = (map head a) : (map tail a)

-- January 2016 Paper
revCount :: [Char] -> [Int] -> [String]
revCount chars ints = reverse (repeater chars ints)
repeater chars ints =
	if chars == []
		then []
	else
	replicate (head ints) (head chars) : repeater (tail chars) (tail ints)