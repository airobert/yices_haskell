-- vim:sw=2:ts=2:expandtab:autoindent

module Main where 

-- example of generating three test cases of 6 integer variables
-- l1, u1, i, l2, u2, j sush that l1 < i <= u1 and l2 < j <= u2.

import Math.SMT.Yices.Parser
import Math.SMT.Yices.Syntax
import Math.SMT.Yices.Pipe
import Data.List
import Control.Monad
-- import Random

yicesPath = "/Users/robertwhite/Projects/yices-1.0.40/bin/yices" -- your yices path

main =
  do yp@(Just hin, Just hout, Nothing, p) <- createYicesPipe yicesPath []
     runCmdsY yp (defs ++ ctrs)

     -- gr <- getStdGen
     -- let (rn,gr') = next gr

     Sat ss <- checkY yp

     print (head ss)

     runCmdsY yp [ASSERT_P (NOT $ ss!!0) Nothing]
     Sat ss <- checkY yp
     print ss

     runCmdsY yp [ASSERT_P (NOT $ ss!!0) Nothing]
     Sat ss <- checkY yp
     print ss

     exitY yp
     --return ss


defs = map (\x -> DEFINE (x,int) Nothing) ["l1","u1","i","l2","u2","j"]

ctrs = map ASSERT [ l1:<u1, l1:<=i, i:<=u1, l2:<u2, l2:<=j, j:<=u2 ]
     -- ++ map (\e -> ASSERT_P e Nothing) [ i:<j, j:<i ]
     where
       l1 = VarE "l1"
       u1 = VarE "u1"
       i = VarE "i"
       l2 = VarE "l2"
       u2 = VarE "u2"
       j = VarE "j"

int = VarT "int"
nat = VarT "nat"
bool = VarT "bool"
real = VarT "real"

true = LitB True
false = LitB False


test =
  do yp@(Just hin, Just hout, Nothing, p) <- createYicesPipe yicesPath []
     runCmdsY yp (defs' ++ ctrs')

     -- gr <- getStdGen
     -- let (rn,gr') = next gr

     Sat ss <- checkY yp

     return (obtain_bool_value vlist' ss)


defs' = map (\x -> DEFINE (x,bool) Nothing) ["l1","l2","l3","l4","l5","l6"]

vlist' = map (\x -> VarE x) ["l1","l2","l3"]
l1 = VarE "l1"
l2 = VarE "l2"
l3 = VarE "l3"

ctrs' = map ASSERT [ OR vlist']
      ++ map (\e -> ASSERT_P e Nothing) [l1:=false]
      ++ [ASSERT (FORALL [("l1",bool), ("l2", bool)] (OR[AND[l1, l2], l3]))]

obtain_bvalue x [] = Nothing
obtain_bvalue x l = 
  let h = head l in 
  let ((VarE vname) := (LitB b)) = h in 
  let VarE xname = x in 
  if xname == vname then 
    Just b 
    else 
      (obtain_bvalue x (tail l))

obtain_bool_value vars valuelist =
  map (\x -> obtain_bvalue  x valuelist) vars








 
