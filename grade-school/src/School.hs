module School (School, add, empty, grade, sorted) where

import Data.Maybe (fromMaybe)
import Data.List (sort, sortBy)

type Grade = Int
type Student = String
type School = [(Grade, [Student])]

add :: Int -> String -> School -> School
add grade student school = case lookup grade school of
  Just students -> map insert school
    where
      insert t@(x, students) =
        if x == grade
        then (x, student:students)
        else t
  Nothing -> (grade, [student]) : school

empty :: School
empty = []

grade :: Int -> School -> [String]
grade n = sort . (fromMaybe []) . lookup n

sorted :: School -> [(Int, [String])]
sorted = sortByGrade . sortStudentsPerGrade
  where
    sortByGrade = sortBy $ \(a, _) (b, _) -> compare a b
    sortStudentsPerGrade = map (\(x, students) -> (x, sort students))
