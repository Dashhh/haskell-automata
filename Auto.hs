module Auto (Auto, accepts, emptyA, epsA, symA, leftA, sumA, thenA, fromLists, toLists) where
import Data.List
data Auto a q = A { states      :: [q]
                 , initStates  :: [q]
                 , isAccepting :: q -> Bool 
                 , transition  :: q -> a -> [q] }

accepts :: Eq q => Auto a q -> [a] -> Bool
emptyA :: Auto a ()
epsA :: Auto a ()
symA :: Eq a => a -> Auto a Bool
leftA :: Auto a q -> Auto a (Either q r)
sumA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
thenA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
fromLists :: (Eq q, Eq a) => [q] -> [q] -> [q] -> [(q,a,[q])] -> Auto a q
toLists :: (Enum a,Bounded a) => Auto a q -> ([q],[q],[q],[(q,a,[q])])

instance (Show a, Enum a, Bounded a, Show q) => Show (Auto a q) where
    show = show . toLists

step :: Eq q => (q -> a -> [q]) -> [q] -> [a] -> [q]
step trans [] as = []
step trans qs [] = qs
step trans qs (a:as) = step trans new_states as where
    new_states = nub . concat $ raw_states
    raw_states = fmap (flip trans a) qs

accepts (A st ist isA trans) as = any isA result_states where
    result_states = step trans ist as

emptyA = A [] [] false ftrans where 
    false q = False
    ftrans q a = []

epsA = A [()] [()] true ttrans where
    true q = True
    ttrans q a = []

symA x = A st ist isA trans where
    st = [False, True]
    ist = [False]
    isA = id
    trans q a
        |q == True = []
        |a /= x = []
        |otherwise = [True]

aleft :: [q] -> [Either q b]
aleft qs = fmap Left qs

aright :: [q] -> [Either a q]
aright qs = fmap Right qs

leftA (A st ist isA trans) = A lst list lisA ltrans where
    lst = aleft st
    list = aleft ist
    lisA q = either isA (\x -> False) q
    ltrans q a = aleft (either (flip trans a) (\x -> []) q)

sumA (A st1 ist1 isA1 trans1) (A st2 ist2 isA2 trans2) = (A st ist isA trans) where
    st = aleft st1 ++ aright st2
    ist = aleft ist1 ++ aright ist2
    isA = either isA1 isA2
    trans q a = either  (getStates Left trans1 a) (getStates Right trans2 a) q
    getStates f t a q = fmap f (t q a)

thenA (A st1 ist1 isA1 trans1) (A st2 ist2 isA2 trans2) = (A st ist isA trans) where
    st = aleft st1
    ist = aleft ist1 ++ check isA1 ist1 ist2
    check f qs iqs
        |any f qs == True = aright iqs
        |otherwise = []
    isA = either (\x -> False) isA2
    trans q a = either (lstates trans1 a) (rstates trans2 a) q
    rstates f a q = aright (f q a)
    lstates f a q = aleft newStates ++ check isA1 newStates ist2 where
        newStates = f q a

fromLists st ist ast qtrs = (A st ist isA trans) where
    isA q = any (== q) ast 
    trans q a = maybe [] id genStates where 
        genStates = lookup (q,a) transformedList
    transformedList = zip (zip q a) qs where
        (q,a,qs) = unzip3 qtrs


toLists (A st ist isA trans) = (st, ist, ast, qtrans) where
    ast = filter isA st
    qtrans = genStates st (enumFrom minBound)
    genStates [] as = []
    genStates (q:qs) as = expandState q as ++ genStates qs as
    expandState q [] = []
    expandState q (a:as)
        |length nst == 0 = expandState q as
        |otherwise = [(q,a,nst)] ++ expandState q as where
            nst = trans q a
