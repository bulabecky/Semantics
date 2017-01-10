import Data.List
import System.IO

-- Autumn 2016 paper
tr::[[a]]->[[a]]
tr ([]:) = []
tr x = (map head a) : (map tail a)