
-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array
import Text.Read (readMaybe)
import Data.Maybe (fromJust)

import Common

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
type ColumnName = String

-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}

string_to_int :: String -> Int
string_to_int s = read s :: Int


string_to_float :: String -> Float
string_to_float s = read s :: Float


-- Task 1

title_avg_steps = ["Name", "Average Number of Steps"]

compute_average_steps :: Table -> Table
compute_average_steps m = title_avg_steps : map average_steps_row (tail m)


average_steps_row :: Row -> Row
average_steps_row r = [head r, gen_average_steps (tail r) 8]


gen_average_steps :: Row -> Int -> String
gen_average_steps r n =
    printf "%.2f" (sum (map string_to_float r) / fromIntegral n)


-- Task 2

-- Number of people who have achieved their goal:
get_passed_people_num :: Table -> Int
get_passed_people_num m =
    foldr (\x acc -> if x >= 1000 then acc + 1 else acc) 0 (get_daily_steps m)


-- Percentage of people who have achieved their goal:
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m =
    fromIntegral (get_passed_people_num m) / fromIntegral (length m - 1)


-- Average number of daily steps
get_steps_avg :: Table -> Float
get_steps_avg m =
    fromIntegral (sum (get_daily_steps m)) / fromIntegral (length m - 1)


get_daily_steps :: Table -> [Int]
get_daily_steps m = map gen_daily_steps (tail m)


gen_daily_steps :: Row -> Int
gen_daily_steps r = sum (map string_to_int (tail r))


-- Task 3

title_hours = ["H10", "H11", "H12", "H13", "H14", "H15", "H16", "H17"]

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h m =
    title_hours : [map average_steps_hour_row (tail (transpose m))]


average_steps_hour_row :: Row -> String
average_steps_hour_row r = gen_average_steps (tail r) (length r - 1)


-- Task 4

title_ranges = ["column", "range1", "range2", "range3"]

get_activ_summary :: Table -> Table
get_activ_summary m = [title_ranges, gen_row1 m, gen_row2 m, gen_row3 m]


is_in_range :: Int -> Int -> Int -> Bool
is_in_range x a b
    | x >= a && x < b = True
    | otherwise = False


count_in_range :: Row -> Int -> Int -> Int
count_in_range r a b = foldr op 0 (tail r)
    where op = \x acc -> if is_in_range (string_to_int x) a b then acc + 1 else acc


count_for_range :: Table -> Int -> Int -> Int -> String
count_for_range m i a b = printf "%d" (count_in_range (m !! i) a b)


gen_row :: Table -> String -> Int -> Row
gen_row m s i =
    s : [count_for_range m i 0 50, count_for_range m i 50 100, count_for_range m i 100 500]


gen_row1 :: Table -> Row
gen_row1 m = gen_row (transpose m) "VeryActiveMinutes" 3


gen_row2 :: Table -> Row
gen_row2 m = gen_row (transpose m) "FairlyActiveMinutes" 4


gen_row3 :: Table -> Row
gen_row3 m = gen_row (transpose m) "LightlyActiveMinutes" 5


-- Task 5

title_ranking = ["Name", "Total Steps"]

get_ranking :: Table -> Table
get_ranking m =
    title_ranking : sortBy compare_func (transpose (take 2 (transpose (tail m))))


compare_func :: Row -> Row -> Ordering
compare_func r1 r2
    | string_to_float (last r1) /= string_to_float (last r2) =
        compare (string_to_float (last r1)) (string_to_float (last r2))
    | otherwise = compare (head r1) (head r2)


-- Task 6

title_diff = ["Name", "Average first 4h", "Average last 4h", "Difference"]

get_steps_diff_table :: Table -> Table
get_steps_diff_table m =
    title_diff : sortBy compare_func (gen_diff (map average4h (tail m)))


average4h :: Row -> Row
average4h r =
    [head r, gen_average_steps (take 4 (tail r)) 4, gen_average_steps (drop 4 (tail r)) 4]


gen_diff :: Table -> Table
gen_diff = map (\r -> r ++ [diff_string (r !! 2) (r !! 1)])


diff_string :: String -> String -> String
diff_string s1 s2 = printf "%.2f" (abs (string_to_float s1 - string_to_float s2))


-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f m = map (map f) m


-- Task 8

-- Applies the given function to all the entries
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = s : map f (tail m)


get_sleep_total :: Row -> Row
get_sleep_total r = [head r, printf "%.2f" (sum (map string_to_float (tail r)))]


{-
    TASK SET 2
-}

-- Task 1

tsort :: ColumnName -> Table -> Table
tsort column table = head table : sortBy compareByColumn (tail table)
    where
        compareByColumn r1 r2
            | elem1 /= elem2 = compareElem elem1 elem2
            | otherwise = compareElem (head r1) (head r2)
            where
                index = getColIndex column table
                elem1 = r1 !! index
                elem2 = r2 !! index

getColIndex :: ColumnName -> Table -> Int
getColIndex col table = op (head table) 0
    where
        op row i
            | i >= length row = -1 -- coloana inexistenta
            | (row !! i) == col = i
            | otherwise = op row (i + 1)

compareElem :: String -> String -> Ordering
compareElem e1 e2
    | isNum e1 && isNum e2 = compare (string_to_float e1) (string_to_float e2)
    | otherwise = compare e1 e2

isNum :: String -> Bool
isNum x
    | toNum == Nothing = False
    | otherwise = True
    where
        toNum = readMaybe x :: Maybe Float


-- Task 2

vunion :: Table -> Table -> Table
vunion t1 t2
    | head t1 == head t2 = t1 ++ tail t2
    | otherwise = t1


-- Task 3

hunion :: Table -> Table -> Table
hunion t1 t2 = op t1 t2
    where
        op (t1:ts1) (t2:ts2) = (t1 ++ t2) : op ts1 ts2
        op [] [] = [] -- s-au terminat de parcurs ambele tabele
        op (t1:ts1) [] = (t1 ++ emptyStrings (nrCol t2)) : op ts1 []
        op [] (t2:ts2) = (emptyStrings (nrCol t1) ++ t2) : op [] ts2

nrCol :: Table -> Int
nrCol table = length (transpose table)

emptyStrings :: Int -> Row
emptyStrings x = op x []
    where
        op 0 acc = acc
        op n acc = op (n - 1) ("" : acc)


-- Task 4

tjoin :: ColumnName -> Table -> Table -> Table
tjoin key_column t1 t2 = mergeHeaders (head t1) (head t2) : op (tail t1) []
    where
        i1 = getColIndex key_column t1
        i2 = getColIndex key_column t2
        op [] acc = acc
        op (r:t) acc
            | isInTable (r !! i1) i2 t2
                = op t (acc ++ [mergeCol [head t1, r] [head t2, getFromTable (r !! i1) i2 t2]])
            | otherwise = op t acc

mergeHeaders :: Row -> Row -> Row
mergeHeaders t1 t2 = t1 ++ getUnique t2
    where
        getUnique t = foldr (\x acc -> if x `elem` t1 then acc else x:acc) [] t

mergeCol :: Table -> Table -> Row
mergeCol t1 t2 = op col 0 []
    where
        col = mergeHeaders (head t1) (head t2)
        op c i acc
            | i >= length c = acc -- s-a terminat parcurgerea coloanelor
            | getColIndex (c !! i) t2 /= -1 && val_t2 /= "" = op c (i + 1) (acc ++ [val_t2])
            | otherwise = op c (i + 1) (acc ++ [val_t1])
            where
                val_t1 = getFromRow (c !! i) t1
                val_t2 = getFromRow (c !! i) t2

isInTable :: Value -> Int -> Table -> Bool
isInTable val i t
    | getFromTable val i t /= [] = True
    | otherwise = False

getFromTable :: Value -> Int -> Table -> Row
getFromTable val i [] = []
getFromTable val i (t:tt)
    | t !! i == val = t
    | otherwise = getFromTable val i tt

getFromRow :: ColumnName -> Table -> Value
getFromRow col t = last t !! getColIndex col t


-- Task 5

cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table
cartesian new_row_function new_column_names t1 t2 =
    new_column_names : op (tail t1) []
        where
            op [] acc = acc
            op (t:tt) acc = op tt (acc ++ productRow new_row_function t (tail t2))

productRow :: (Row -> Row -> Row) -> Row -> Table -> Table
productRow f row table = op table []
    where
        op [] acc = acc
        op (t:tt) acc = op tt (acc ++ [f row t])


-- Task 6

projection :: [ColumnName] -> Table -> Table
projection columns_to_extract t = transpose (op columns_to_extract [])
    where
        op [] acc = acc
        op (c:cc) acc = op cc (acc ++ [getCol c t])

getCol :: ColumnName -> Table -> Row
getCol col t = transpose t !! getColIndex col t


-- Task 7

filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table
filterTable condition key_column t = head t : op (tail t) []
    where
        i = getColIndex key_column t
        op [] acc = acc
        op (r:tt) acc
            | condition (r !! i) = op tt (acc ++ [r])
            | otherwise = op tt acc


{-
    TASK SET 3
-}


-- 3.1

data Query =
    FromTable Table
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query
    | Graph EdgeOp Query

instance Show QResult where
    show (List l) = show l
    show (Table t) = show t

toTable :: QResult -> Table
toTable (Table t) = t
toTable (List l) = transpose [l]

class Eval a where
    eval :: a -> QResult

instance Eval Query where
    eval (FromTable table) = Table table
    eval (AsList colname query) = List (tail (getCol colname (toTable (eval query))))
    eval (Sort colname query) = Table (tsort colname (toTable (eval query)))
    eval (ValueMap op query) = Table (vmap op (toTable (eval query)))
    eval (RowMap op colnames query) = Table (rmap op colnames (toTable (eval query)))
    eval (VUnion query1 query2) = Table (vunion (toTable (eval query1)) (toTable (eval query2)))
    eval (HUnion query1 query2) = Table (hunion (toTable (eval query1)) (toTable (eval query2)))
    eval (TableJoin colname query1 query2) =
        Table (tjoin colname (toTable (eval query1)) (toTable (eval query2)))
    eval (Cartesian op colnames query1 query2) =
        Table (cartesian op colnames (toTable (eval query1)) (toTable (eval query2)))
    eval (Projection colnames query) = Table (projection colnames (toTable (eval query)))
    eval (Filter cond query) = Table (head table : filter (feval (head table) cond) (tail table))
        where table = toTable (eval query)
    eval (Graph edgeop query) = Table (["From", "To", "Value"] : genGraph edgeop (tail table))
        where table = toTable (eval query)


-- 3.2 & 3.3

type FilterOp = Row -> Bool

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp

instance FEval Float where
    feval colnames (Eq colname ref) =
        \row -> string_to_float (row !! getColIndex colname [colnames, row]) == ref
    feval colnames (Lt colname ref) =
        \row -> string_to_float (row !! getColIndex colname [colnames, row]) < ref
    feval colnames (Gt colname ref) =
        \row -> string_to_float (row !! getColIndex colname [colnames, row]) > ref
    feval colnames (In colname list) =
        \row -> string_to_float (row !! getColIndex colname [colnames, row]) `elem` list
    feval colnames (FNot cond) =
        \row -> not (feval colnames cond row)
    feval colnames (FieldEq colname1 colname2) =
        \row -> string_to_float (row !! getColIndex colname1 [colnames, row]) ==
                string_to_float (row !! getColIndex colname2 [colnames, row])

instance FEval String where
    feval colnames (Eq colname ref) =
        \row -> (row !! getColIndex colname [colnames, row]) == ref
    feval colnames (Lt colname ref) =
        \row -> (row !! getColIndex colname [colnames, row]) < ref
    feval colnames (Gt colname ref) =
        \row -> (row !! getColIndex colname [colnames, row]) > ref
    feval colnames (In colname list) =
        \row -> (row !! getColIndex colname [colnames, row]) `elem` list
    feval colnames (FNot cond) =
        \row -> not (feval colnames cond row)
    feval colnames (FieldEq colname1 colname2) =
        \row -> (row !! getColIndex colname1 [colnames, row]) ==
                (row !! getColIndex colname2 [colnames, row])


-- 3.4

-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

genGraph :: EdgeOp -> Table -> Table
genGraph edgeop table = for_i [] 0
    where
        for_i acc_i i
            | i == length table = acc_i -- s-a terminat parcurgerea dupa index-ul i
            | otherwise = for_i (acc_i ++ (for_j [] (i + 1))) (i + 1)
            where
                for_j acc_j j
                    | j == length table = acc_j -- s-a terminat parcurgerea dupa index-ul j
                    -- nu exista muchie intre nodul i si nodul j
                    | edgeop (table !! i) (table !! j) == Nothing = for_j acc_j (j + 1)
                    -- exista deja => nu se mai adauga
                    | line `elem` (acc_i ++ acc_j) = for_j acc_j (j + 1)
                    | otherwise = for_j (acc_j ++ [line]) (j + 1)
                        where
                            nodes = sort [head (table !! i), head (table !! j)]
                            edge = [fromJust (edgeop (table !! i) (table !! j))]
                            line = nodes ++ edge


-- 3.5

similarities_query :: Query
similarities_query = Sort "Value" (Filter (FNot (Lt "Value" (string_to_float "5"))) graph)

-- elimina intrarile in care "Name" lipseste (este "") si creeaza graf
graph :: Query
graph = Graph edge (Filter (FNot (Eq "Name" "")) (FromTable eight_hours))

edge :: EdgeOp
edge r1 r2 = Just (printf "%d" (distance r1 r2))

distance :: Row -> Row -> Int
distance r1 r2 = genDist 1 0
    where
        genDist i acc
            | i == length r1 = acc
            | val1 == val2 = genDist (i + 1) (acc + 1)
            | otherwise = genDist (i + 1) acc
                where
                    val1 = string_to_int (r1 !! i)
                    val2 = string_to_int (r2 !! i)


-- 3.6 (Typos)

correct_table :: String -> Table -> Table -> Table
correct_table col csv1 csv2 = head csv1 : corrected (tail csv1)
    where
        typos = getCol col csv1 -- coloana typo-uri
        ref = getCol col csv2 -- coloana de referinta
        typosTable = -- elemente ce contin typo-uri
            toTable (eval (Filter (FNot (In col ref)) (FromTable (transpose [typos]))))
        refTable = -- valorile corecte ale elementelor cu typo-uri
            toTable (eval (Filter (FNot (In col typos)) (FromTable (transpose [ref]))))
        -- transforma din tabel in lista, eliminand coloana titlu
        typos' = head (transpose (tail typosTable))
        ref' = head (transpose (tail refTable))
        index = getColIndex col csv1 -- index-ul coloanei cu typo-uri
        corrected table = aux table [] 
            where 
                aux [] acc = acc 
                aux (t:tt) acc 
                    -- daca valoarea este un typo => se modifica linia
                    | (t !! index) `elem` typos' = aux tt (acc ++ [replace index ref' t])
                    -- altfel => se pastreaza linia originala
                    | otherwise = aux tt (acc ++ [t])

replace :: Int -> [String] -> Row -> Row 
replace i correct row = new_row 0 []
    where
        new_row index acc
            | index == length row = acc -- s-a terminat de parcurs linia
            | index == i = new_row (index + 1) (acc ++ [bestMatch (row !! i) correct])
            | otherwise = new_row (index + 1) (acc ++ [row !! index])

bestMatch :: String -> [String] -> String
bestMatch typo strs = find (head strs) (tail strs)
    where
        find best [] = best
        find best (s:ss) = find new_best ss
            where
                new_best = if distanceTypos typo s < distanceTypos typo best then s else best

distanceTypos :: String -> String -> Int
distanceTypos s1 s2 = dp ! (length s1, length s2)
    where
        bounds = ((0, 0), (length s1, length s2))
        dp = listArray bounds [gen_dp_table i j | (i, j) <- range bounds]
        gen_dp_table i j
            | i == 0 = j 
            | j == 0 = i 
            | s1 !! (i - 1) == s2 !! (j - 1) = dp ! (i - 1, j - 1)
            | otherwise = 1 + minimum [dp ! (i, j - 1), dp ! (i - 1, j), dp ! (i - 1, j - 1)]
