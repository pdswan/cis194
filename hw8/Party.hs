module Party where

import Employee
import Data.Monoid
import Data.Tree
import Control.Applicative
import Data.List (sort)

glCons :: Employee -> GuestList -> GuestList
glCons employee (GL employees fun) =
  GL (employee:employees) (fun + empFun employee)

instance Monoid GuestList where
  mempty              = GL [] 0
  mappend (GL e f) gl = foldr glCons gl e

guestListFun :: GuestList -> Fun
guestListFun (GL _ fun) = fun

guestListEmployees :: GuestList -> [Employee]
guestListEmployees (GL employees _) = employees

moreFun :: GuestList -> GuestList -> GuestList
moreFun gla glb = max gla glb

treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold fn accum (Node a subTrees) =
  foldl (treeFold fn) (fn accum a) subTrees

treeFold' :: (a -> b -> b) -> b -> Tree a -> b
treeFold' fn accum (Node a subTrees) =
  fn a (foldl (treeFold' fn) accum subTrees)

treeFold'' :: (a -> [b] -> b) -> Tree a -> b
treeFold'' fn (Node a subTrees) =
  fn a (map (treeFold'' fn) subTrees)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss [] = (glCons boss mempty, mempty)
nextLevel boss subTrees =
  (maximum withBoss, maximum canNotContainBoss)
  where
    canNotContainBoss = map fst subTrees
    canContainBoss = map snd subTrees
    withBoss = map (glCons boss) canContainBoss

maxFun :: Tree Employee -> GuestList
maxFun = (uncurry max) . (treeFold'' nextLevel)

main :: IO ()
main = do
  tree <- readTree <$> readFile "company.txt"
  let guestList = maxFun tree
  putStrLn $ "Total fun: " ++ (show $ guestListFun guestList)
  putStr $ (unlines . sort) (employeeNames (guestListEmployees guestList))
  return ()
  where
    employeeNames :: [Employee] -> [String]
    employeeNames = map empName

readTree :: String -> Tree Employee
readTree = read
