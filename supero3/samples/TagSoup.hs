{-# OPTIONS_GHC -DMAIN -cpp #-}

module Main where

import Prelude hiding (id,seq,null,not,head,tail,fst,snd,flip,concatMap,elem,maybe,($),(.),(&&),(||),(++),filter,reverse,drop,(==),concat,map)
import qualified Prelude
import Data.Char
import Numeric


#if MAIN

data Position = Position !Int !Int deriving Show

data Tag str =
     TagOpen str [(str,str)]  -- ^ An open tag with 'Attribute's in their original order
   | TagClose str                 -- ^ A closing tag
   | TagText str                  -- ^ A text node, guaranteed not to be the empty string
   | TagComment str               -- ^ A comment
   | TagWarning str               -- ^ Meta: A syntax error in the input file
   | TagPosition !Int !Int        -- ^ Meta: The position of a parsed element
     deriving Show

data Out
    = Char Char
    | Tag          -- <
    | TagShut      -- </
    | AttName
    | AttVal
    | TagEnd       -- >
    | TagEndClose  -- />
    | Comment      -- <!--
    | CommentEnd   -- -->
    | Entity       -- &
    | EntityNum    -- &#
    | EntityHex    -- &#x
    | EntityEnd    -- ;
    | EntityEndAtt -- missing the ; and in an attribute
    | Warn String
    | Pos Position
      deriving Show

data S = S
    {tl :: S
    ,hd :: Char
    ,eof :: Bool
    ,next :: String -> Maybe S
    ,pos :: [Out] -> [Out]
    }

data ParseOptions str = ParseOptions
    {optTagPosition :: Bool -- ^ Should 'TagPosition' values be given before some items (default=False,fast=False)
    ,optTagWarning :: Bool  -- ^ Should 'TagWarning' values be given (default=False,fast=False)
    ,optEntityData :: str -> [Tag str] -- ^ How to lookup an entity
    ,optEntityAttrib :: (str,Bool) -> (str,[Tag str]) -- ^ How to lookup an entity in an attribute (Bool = has ending @';'@?)
    ,optTagTextMerge :: Bool -- ^ Require no adjacent 'TagText' values (default=True,fast=False)
    }

eqChar'2 = (Prelude.==) :: Char -> Char -> Bool
eqInt'2 = (Prelude.==) :: Int -> Int -> Bool

#endif

#if MAIN_SUPERO
#endif

#if SUPERO
isDigit = isDigit'1
isAlpha = isAlpha'1
isAlphaNum = isAlphaNum'1
isHexDigit = isHexDigit'1
show = show'1
(+) = addInt'2
(-) = subInt'2
error = error'1
chr = chr'1
read = read'1
readHex = readHex'1
mod = modInt'2

#endif

(==) = eqChar'2

concat x = case x of
    [] -> []
    x:xs -> x ++ concat xs

map f x = case x of
    [] -> []
    y:ys -> f y : map f ys

reverse x = rev [] x
rev acc x = case x of
    [] -> acc
    y:ys -> rev (y:acc) ys

filter f x = case x of
    [] -> []
    y:ys -> case f y of
        True -> y : filter f ys
        False -> filter f ys

drop n x = case eqInt'2 n 0 of
    True -> x
    False -> case x of
        [] -> []
        z:zs -> drop (n-1) zs

fst x = case x of (a,b) -> a
snd x = case x of (a,b) -> b

($) f x = f x
(.) f g x = f (g x)

maybe nil op x = case x of
    Nothing -> nil
    Just y -> op y

concatMap f x = concat (map f x)

elem :: Char -> [Char] -> Bool
elem x ys = case ys of
    [] -> False
    y:ys -> (x == y) || elem x ys

flip f x y = f y x

entData :: String -> [Tag String]
entData x = [TagText x]

entAttrib :: (String,Bool) -> (String,[Tag String])
entAttrib y = case y of (x,z) -> (x,[])

strConcat = concat
fromString = id
empty = []
strNull = null
cons = (:)
append = (++)
nullPosition = Position 1 1
fromChar x = [x]

hexChar y x = case y of
    False -> isDigit x
    True -> isHexDigit x

positionChar :: Position -> Char -> Position
positionChar y x = case y of
    Position r c -> case x == '\n' of
        True -> Position (r+1) 1
        False -> case x == '\t' of
            True -> Position r (c + 8 - mod (c-1) 8)
            False    -> Position r (c+1)



isTagWarning x = case x of
    TagWarning x -> True
    TagOpen y x -> False
    TagClose x -> False
    TagText x -> False
    TagComment x -> False
    TagPosition x y -> False



patternMatchFail = error "pattern match failure"

tagPosition :: Position -> Tag str
tagPosition x = case x of
    Position r c -> TagPosition r c
assert y x = x
second f x = (fst x, f $ snd x)

root
  = \ x ->
      ((output
          (((((ParseOptions False) False) entData) entAttrib) False))
         (parse (toString x)))
id x = x
seq x y = y
null x = case x of
    [] -> True
    x:y -> False
not x = case x of
    True -> False
    False -> True

head x = case x of
    [] -> error "head"
    x:xs -> x
tail x = case x of
    [] -> error "tail"
    x:xs -> xs

(||) x y = case x of
    True -> True
    False -> y
(&&) x y = case x of
    True -> y
    False -> False

(++) xs ys = case xs of
    [] -> ys
    x:xs -> x : (xs ++ ys)
    

opStarStarStar = \ v_2 v_3 v_4 -> (((,) (v_2 (fst v_4))) (v_3 (snd v_4)))

toString x = x

white = \ v_2 -> ((elem v_2) "\t\n\f ")
parse = (((.) dat) state) 
dat = \ v_2 ->
      (case v_2 of
           S v_4 v_5 v_6 v_7 v_8 -> ((($) v_8)
                                       (let f_12
                                              = (let f_11
                                                       = (let f_10 = ((ampChar v_5) (dat v_4)) in
                                                            (case v_6 of
                                                                 True -> []
                                                                 False -> f_10))
                                                   in
                                                   (case v_5 == '<' of
                                                        True -> (tagOpen v_4)
                                                        False -> f_11))
                                          in
                                          (case v_5 == '&' of
                                               True -> (charReference v_4)
                                               False -> f_12))))
charReference = \ v_2 -> ((((charRef dat) False) Nothing) v_2)
tagOpen = \ v_2 ->
      (case v_2 of
           S v_4 v_5 v_6 v_7 v_8 -> (let f_14
                                           = (let f_13
                                                    = (let f_12
                                                             = (let f_11
                                                                      = (let f_10
                                                                               = ((ampOut
                                                                                     (errSeen "<"))
                                                                                    ((ampChar '<')
                                                                                       (dat v_2)))
                                                                           in
                                                                           (case v_5 == '?' of
                                                                                True -> (neilXmlTagOpen
                                                                                          v_4)
                                                                                False -> f_10))
                                                                  in
                                                                  (case v_5 == '>' of
                                                                       True -> ((ampOut
                                                                                  (errSeen "<>"))
                                                                                 ((ampChar '<')
                                                                                    ((ampChar '>')
                                                                                       (dat v_4))))
                                                                       False -> f_11))
                                                         in
                                                         (case (isAlpha v_5) of
                                                              True -> ((ampOut Tag)
                                                                         ((ampChar v_5)
                                                                            ((tagName False) v_4)))
                                                              False -> f_12))
                                                in
                                                (case v_5 == '/' of
                                                     True -> (closeTagOpen v_4)
                                                     False -> f_13))
                                       in
                                       (case v_5 == '!' of
                                            True -> (markupDeclOpen v_4)
                                            False -> f_14)))
neilXmlTagOpen = \ v_2 ->
      (case v_2 of
           S v_4 v_5 v_6 v_7 v_8 -> ((($) v_8)
                                       (let f_10
                                              = ((ampOut (errSeen "<?"))
                                                   ((ampChar '<') ((ampChar '?') (dat v_2))))
                                          in
                                          (case (isAlpha v_5) of
                                               True -> ((ampOut Tag)
                                                          ((ampChar '?')
                                                             ((ampChar v_5) ((tagName True) v_4))))
                                               False -> f_10))))
neilXmlTagClose = \ v_2 ->
      (case v_2 of
           S v_4 v_5 v_6 v_7 v_8 -> ((($) v_8)
                                       (let f_10
                                              = ((ampOut (errSeen "?")) ((beforeAttName True) v_2))
                                          in
                                          (case v_5 == '>' of
                                               True -> ((ampOut TagEnd) (dat v_4))
                                               False -> f_10))))
neilTagEnd = \ v_2 v_3 ->
      (case v_3 of
           S v_5 v_6 v_7 v_8 v_9 -> (let f_10
                                           = ((($) v_9) ((ampOut TagEnd) (dat v_3)))
                                       in
                                       (case v_2 of
                                            True -> ((($) v_9)
                                                       ((ampOut (errWant "?>"))
                                                          ((ampOut TagEnd) (dat v_3))))
                                            False -> f_10)))
closeTagOpen = \ v_2 ->
      (case v_2 of
           S v_4 v_5 v_6 v_7 v_8 -> (let f_12
                                           = (let f_11
                                                    = (let f_10
                                                             = ((ampOut (errWant "tag name"))
                                                                  (bogusComment v_2))
                                                         in
                                                         (case v_6 of
                                                              True -> ((ampChar '<')
                                                                         ((ampChar '/') (dat v_2)))
                                                              False -> f_10))
                                                in
                                                (case v_5 == '>' of
                                                     True -> ((ampOut (errSeen "</>"))
                                                               ((ampChar '<')
                                                                  ((ampChar '/')
                                                                     ((ampChar '>') (dat v_4)))))
                                                     False -> f_11))
                                       in
                                       (case (((||) (isAlpha v_5)) ((elem v_5) "?!")) of
                                            True -> ((ampOut TagShut)
                                                       ((ampChar v_5) ((tagName False) v_4)))
                                            False -> f_12)))
tagName = \ v_2 v_3 ->
      (case v_3 of
           S v_5 v_6 v_7 v_8 v_9 -> ((($) v_9)
                                       (let f_17
                                              = (let f_16
                                                       = (let f_15
                                                                = (let f_14
                                                                         = (let f_13
                                                                                  = (let f_11
                                                                                           = ((ampChar
                                                                                                 v_6)
                                                                                                ((tagName
                                                                                                    v_2)
                                                                                                   v_5))
                                                                                       in
                                                                                       (case v_7 of
                                                                                            True -> ((ampOut
                                                                                                        (errWant
                                                                                                           (let f_12
                                                                                                                  = ">"
                                                                                                              in
                                                                                                              (case
                                                                                                                 v_2
                                                                                                                 of
                                                                                                                   True -> "?>"
                                                                                                                   False -> f_12))))
                                                                                                       (dat
                                                                                                          v_3))
                                                                                            False -> f_11))
                                                                              in
                                                                              (case (isAlpha v_6) of
                                                                                   True -> ((ampChar
                                                                                               v_6)
                                                                                              ((tagName
                                                                                                  v_2)
                                                                                                 v_5))
                                                                                   False -> f_13))
                                                                     in
                                                                     (case v_6 == '?' of
                                                                          True -> (case v_2 of
                                                                                      True -> (neilXmlTagClose
                                                                                                 v_5)
                                                                                      False -> f_14)
                                                                          False -> f_14))
                                                            in
                                                            (case v_6 == '>' of
                                                                 True -> ((neilTagEnd v_2) v_5)
                                                                 False -> f_15))
                                                   in
                                                   (case v_6 == '/' of
                                                        True -> ((selfClosingStartTag v_2) v_5)
                                                        False -> f_16))
                                          in
                                          (case (white v_6) of
                                               True -> ((beforeAttName v_2) v_5)
                                               False -> f_17))))
beforeAttName = beforeAttName_root
beforeAttName_root
  = \ v_2 v_3 ->
      (case v_3 of
           S v_5 v_6 v_7 v_8 v_9 -> ((($) v_9)
                                       (let f_18
                                              = (let f_17
                                                       = (let f_16
                                                                = (let f_15
                                                                         = (let f_14
                                                                                  = (let f_13
                                                                                           = (let f_11
                                                                                                    = ((ampOut
                                                                                                          AttName)
                                                                                                         ((ampChar
                                                                                                             v_6)
                                                                                                            ((attName
                                                                                                                v_2)
                                                                                                               v_5)))
                                                                                                in
                                                                                                (case
                                                                                                   v_7
                                                                                                   of
                                                                                                     True -> ((ampOut
                                                                                                                 (errWant
                                                                                                                    (let f_12
                                                                                                                           = ">"
                                                                                                                       in
                                                                                                                       (case
                                                                                                                          v_2
                                                                                                                          of
                                                                                                                            True -> "?>"
                                                                                                                            False -> f_12))))
                                                                                                                (dat
                                                                                                                   v_3))
                                                                                                     False -> f_11))
                                                                                       in
                                                                                       (case
                                                                                          ((elem
                                                                                              v_6)
                                                                                             "\"'<=")
                                                                                          of
                                                                                            True -> ((ampOut
                                                                                                        (errSeen
                                                                                                           (((:)
                                                                                                               v_6)
                                                                                                              [])))
                                                                                                       ((ampOut
                                                                                                           AttName)
                                                                                                          ((ampChar
                                                                                                              v_6)
                                                                                                             ((attName
                                                                                                                 v_2)
                                                                                                                v_5))))
                                                                                            False -> f_13))
                                                                              in
                                                                              (case
                                                                                 ((elem v_6) "'\"")
                                                                                 of
                                                                                   True -> ((beforeAttValue
                                                                                               v_2)
                                                                                              v_3)
                                                                                   False -> f_14))
                                                                     in
                                                                     (case v_6 == '?' of
                                                                          True -> (case v_2 of
                                                                                      True -> (neilXmlTagClose
                                                                                                 v_5)
                                                                                      _ -> f_15)
                                                                          False -> f_15))
                                                            in
                                                            (case v_6 == '>' of
                                                                 True -> ((neilTagEnd v_2) v_5)
                                                                 False -> f_16))
                                                   in
                                                   (case v_6 == '/' of
                                                        True -> ((selfClosingStartTag v_2) v_5)
                                                        False -> f_17))
                                          in
                                          (case (white v_6) of
                                               True -> ((beforeAttName v_2) v_5)
                                               False -> f_18))))
attName = attName_root
attName_root
  = \ v_2 v_3 ->
      (case v_3 of
           S v_5 v_6 v_7 v_8 v_9 -> (let def
                                           = ((ampChar v_6) ((attName v_2) v_5))
                                       in
                                       ((($) v_9)
                                          (let f_18
                                                 = (let f_17
                                                          = (let f_16
                                                                   = (let f_15
                                                                            = (let f_14
                                                                                     = (let f_13
                                                                                              = (case
                                                                                                   v_7
                                                                                                   of
                                                                                                     True -> ((ampOut
                                                                                                                 (errWant
                                                                                                                    (let f_12
                                                                                                                           = ">"
                                                                                                                       in
                                                                                                                       (case
                                                                                                                          v_2
                                                                                                                          of
                                                                                                                            True -> "?>"
                                                                                                                            False -> f_12))))
                                                                                                                (dat
                                                                                                                   v_3))
                                                                                                     False -> def)
                                                                                          in
                                                                                          (case
                                                                                             ((elem
                                                                                                 v_6)
                                                                                                "\"'<")
                                                                                             of
                                                                                               True -> ((ampOut
                                                                                                           (errSeen
                                                                                                              (((:)
                                                                                                                  v_6)
                                                                                                                 [])))
                                                                                                          def)
                                                                                               False -> f_13))
                                                                                 in
                                                                                 (case v_6 == '?' of
                                                                                      True -> (case
                                                                                                v_2
                                                                                                of
                                                                                                  True -> (neilXmlTagClose
                                                                                                             v_5)
                                                                                                  False -> f_14)
                                                                                      False -> f_14))
                                                                        in
                                                                        (case v_6 == '>' of
                                                                             True -> ((neilTagEnd
                                                                                        v_2)
                                                                                       v_5)
                                                                             False -> f_15))
                                                               in
                                                               (case v_6 == '=' of
                                                                    True -> ((beforeAttValue v_2)
                                                                              v_5)
                                                                    False -> f_16))
                                                      in
                                                      (case v_6 == '/' of
                                                           True -> ((selfClosingStartTag v_2) v_5)
                                                           False -> f_17))
                                             in
                                             (case (white v_6) of
                                                  True -> ((afterAttName v_2) v_5)
                                                  False -> f_18)))))
afterAttName = afterAttName_root
afterAttName_root
  = \ v_2 v_3 ->
      (case v_3 of
           S v_5 v_6 v_7 v_8 v_9 -> (let def
                                           = ((ampOut AttName) ((ampChar v_6) ((attName v_2) v_5)))
                                       in
                                       ((($) v_9)
                                          (let f_19
                                                 = (let f_18
                                                          = (let f_17
                                                                   = (let f_16
                                                                            = (let f_15
                                                                                     = (let f_14
                                                                                              = (let f_13
                                                                                                       = (case
                                                                                                            v_7
                                                                                                            of
                                                                                                              True -> ((ampOut
                                                                                                                          (errWant
                                                                                                                             (let f_12
                                                                                                                                    = ">"
                                                                                                                                in
                                                                                                                                (case
                                                                                                                                   v_2
                                                                                                                                   of
                                                                                                                                     True -> "?>"
                                                                                                                                     False -> f_12))))
                                                                                                                         (dat
                                                                                                                            v_3))
                                                                                                              False -> def)
                                                                                                   in
                                                                                                   (case
                                                                                                      ((elem
                                                                                                          v_6)
                                                                                                         "\"'<")
                                                                                                      of
                                                                                                        True -> ((ampOut
                                                                                                                    (errSeen
                                                                                                                       (((:)
                                                                                                                           v_6)
                                                                                                                          [])))
                                                                                                                   def)
                                                                                                        False -> f_13))
                                                                                          in
                                                                                          (case
                                                                                             ((elem
                                                                                                 v_6)
                                                                                                "\"'")
                                                                                             of
                                                                                               True -> ((ampOut
                                                                                                           AttVal)
                                                                                                          ((beforeAttValue
                                                                                                              v_2)
                                                                                                             v_3))
                                                                                               False -> f_14))
                                                                                 in
                                                                                 (case v_6 == '?' of
                                                                                      True -> (case
                                                                                                v_2
                                                                                                of
                                                                                                  True -> (neilXmlTagClose
                                                                                                             v_5)
                                                                                                  False -> f_15)
                                                                                      False -> f_15))
                                                                        in
                                                                        (case v_6 == '>' of
                                                                             True -> ((neilTagEnd
                                                                                        v_2)
                                                                                       v_5)
                                                                             False -> f_16))
                                                               in
                                                               (case v_6 == '=' of
                                                                    True -> ((beforeAttValue v_2)
                                                                              v_5)
                                                                    False -> f_17))
                                                      in
                                                      (case v_6 == '/' of
                                                           True -> ((selfClosingStartTag v_2) v_5)
                                                           False -> f_18))
                                             in
                                             (case (white v_6) of
                                                  True -> ((afterAttName v_2) v_5)
                                                  False -> f_19)))))
beforeAttValue = beforeAttValue_root
beforeAttValue_root
  = \ v_2 v_3 ->
      (case v_3 of
           S v_5 v_6 v_7 v_8 v_9 -> (let def
                                           = ((ampOut AttVal)
                                                ((ampChar v_6) ((attValueUnquoted v_2) v_5)))
                                       in
                                       ((($) v_9)
                                          (let f_19
                                                 = (let f_18
                                                          = (let f_17
                                                                   = (let f_16
                                                                            = (let f_15
                                                                                     = (let f_14
                                                                                              = (let f_13
                                                                                                       = (case
                                                                                                            v_7
                                                                                                            of
                                                                                                              True -> ((ampOut
                                                                                                                          (errWant
                                                                                                                             (let f_12
                                                                                                                                    = ">"
                                                                                                                                in
                                                                                                                                (case
                                                                                                                                   v_2
                                                                                                                                   of
                                                                                                                                     True -> "?>"
                                                                                                                                     False -> f_12))))
                                                                                                                         (dat
                                                                                                                            v_3))
                                                                                                              False -> def)
                                                                                                   in
                                                                                                   (case
                                                                                                      ((elem
                                                                                                          v_6)
                                                                                                         "<=")
                                                                                                      of
                                                                                                        True -> ((ampOut
                                                                                                                    (errSeen
                                                                                                                       (((:)
                                                                                                                           v_6)
                                                                                                                          [])))
                                                                                                                   def)
                                                                                                        False -> f_13))
                                                                                          in
                                                                                          (case v_6 == '?'
                                                                                             of
                                                                                               True -> (case
                                                                                                         v_2
                                                                                                         of
                                                                                                           True -> (neilXmlTagClose
                                                                                                                      v_5)
                                                                                                           _ -> f_14)
                                                                                               False -> f_14))
                                                                                 in
                                                                                 (case v_6 == '>' of
                                                                                      True -> ((ampOut
                                                                                                 (errSeen
                                                                                                    "="))
                                                                                                ((neilTagEnd
                                                                                                    v_2)
                                                                                                   v_5))
                                                                                      False -> f_15))
                                                                        in
                                                                        (case v_6 == '\'' of
                                                                             True -> ((ampOut
                                                                                         AttVal)
                                                                                        ((attValueSQuoted
                                                                                            v_2)
                                                                                           v_5))
                                                                             False -> f_16))
                                                               in
                                                               (case v_6 == '&' of
                                                                    True -> ((ampOut AttVal)
                                                                              ((attValueUnquoted
                                                                                  v_2)
                                                                                 v_3))
                                                                    False -> f_17))
                                                      in
                                                      (case v_6 == '"' of
                                                           True -> ((ampOut AttVal)
                                                                     ((attValueDQuoted v_2) v_5))
                                                           False -> f_18))
                                             in
                                             (case (white v_6) of
                                                  True -> ((beforeAttValue v_2) v_5)
                                                  False -> f_19)))))
attValueDQuoted = attValueDQuoted_root
attValueDQuoted_root
  = \ v_2 v_3 ->
      (case v_3 of
           S v_5 v_6 v_7 v_8 v_9 -> ((($) v_9)
                                       (let f_13
                                              = (let f_12
                                                       = (let f_11
                                                                = ((ampChar v_6)
                                                                     ((attValueDQuoted v_2) v_5))
                                                            in
                                                            (case v_7 of
                                                                 True -> ((ampOut (errWant "\""))
                                                                            (dat v_3))
                                                                 False -> f_11))
                                                   in
                                                   (case v_6 == '&' of
                                                        True -> (((charRefAttValue
                                                                    (attValueDQuoted v_2))
                                                                   (Just '"'))
                                                                  v_5)
                                                        False -> f_12))
                                          in
                                          (case v_6 == '"' of
                                               True -> ((afterAttValueQuoted v_2) v_5)
                                               False -> f_13))))
attValueSQuoted = attValueSQuoted_root
attValueSQuoted_root
  = \ v_2 v_3 ->
      (case v_3 of
           S v_5 v_6 v_7 v_8 v_9 -> ((($) v_9)
                                       (let f_13
                                              = (let f_12
                                                       = (let f_11
                                                                = ((ampChar v_6)
                                                                     ((attValueSQuoted v_2) v_5))
                                                            in
                                                            (case v_7 of
                                                                 True -> ((ampOut (errWant "'"))
                                                                            (dat v_3))
                                                                 False -> f_11))
                                                   in
                                                   (case v_6 == '&' of
                                                        True -> (((charRefAttValue
                                                                    (attValueSQuoted v_2))
                                                                   (Just '\''))
                                                                  v_5)
                                                        False -> f_12))
                                          in
                                          (case v_6 == '\'' of
                                               True -> ((afterAttValueQuoted v_2) v_5)
                                               False -> f_13))))
attValueUnquoted = attValueUnquoted_root
attValueUnquoted_root
  = \ v_2 v_3 ->
      (case v_3 of
           S v_5 v_6 v_7 v_8 v_9 -> (let def
                                           = ((ampChar v_6) ((attValueUnquoted v_2) v_5))
                                       in
                                       ((($) v_9)
                                          (let f_17
                                                 = (let f_16
                                                          = (let f_15
                                                                   = (let f_14
                                                                            = (let f_13
                                                                                     = (case v_7 of
                                                                                            True -> ((ampOut
                                                                                                        (errWant
                                                                                                           (let f_12
                                                                                                                  = ">"
                                                                                                              in
                                                                                                              (case
                                                                                                                 v_2
                                                                                                                 of
                                                                                                                   True -> "?>"
                                                                                                                   False -> f_12))))
                                                                                                       (dat
                                                                                                          v_3))
                                                                                            False -> def)
                                                                                 in
                                                                                 (case
                                                                                    ((elem v_6)
                                                                                       "\"'<=")
                                                                                    of
                                                                                      True -> ((ampOut
                                                                                                  (errSeen
                                                                                                     (((:)
                                                                                                         v_6)
                                                                                                        [])))
                                                                                                 def)
                                                                                      False -> f_13))
                                                                        in
                                                                        (case v_6 == '?' of
                                                                             True -> (case v_2 of
                                                                                         True -> (neilXmlTagClose
                                                                                                    v_5)
                                                                                         _ -> f_14)
                                                                             False -> f_14))
                                                               in
                                                               (case v_6 == '>' of
                                                                    True -> ((neilTagEnd v_2) v_5)
                                                                    False -> f_15))
                                                      in
                                                      (case v_6 == '&' of
                                                           True -> (((charRefAttValue
                                                                       (attValueUnquoted v_2))
                                                                      Nothing)
                                                                     v_5)
                                                           False -> f_16))
                                             in
                                             (case (white v_6) of
                                                  True -> ((beforeAttName v_2) v_5)
                                                  False -> f_17)))))
charRefAttValue = charRefAttValue_root
charRefAttValue_root
  = \ v_2 v_3 v_4 -> ((((charRef v_2) True) v_3) v_4)
afterAttValueQuoted = afterAttValueQuoted_root
afterAttValueQuoted_root
  = \ v_2 v_3 ->
      (case v_3 of
           S v_5 v_6 v_7 v_8 v_9 -> ((($) v_9)
                                       (let f_15
                                              = (let f_14
                                                       = (let f_13
                                                                = (let f_12
                                                                         = (let f_11
                                                                                  = ((ampOut
                                                                                        (errSeen
                                                                                           (((:)
                                                                                               v_6)
                                                                                              [])))
                                                                                       ((beforeAttName
                                                                                           v_2)
                                                                                          v_3))
                                                                              in
                                                                              (case v_7 of
                                                                                   True -> (dat v_3)
                                                                                   False -> f_11))
                                                                     in
                                                                     (case v_6 == '?' of
                                                                          True -> (case v_2 of
                                                                                      True -> (neilXmlTagClose
                                                                                                 v_5)
                                                                                      _ -> f_12)
                                                                          False -> f_12))
                                                            in
                                                            (case v_6 == '>' of
                                                                 True -> ((neilTagEnd v_2) v_5)
                                                                 False -> f_13))
                                                   in
                                                   (case v_6 == '/' of
                                                        True -> ((selfClosingStartTag v_2) v_5)
                                                        False -> f_14))
                                          in
                                          (case (white v_6) of
                                               True -> ((beforeAttName v_2) v_5)
                                               False -> f_15))))
selfClosingStartTag = selfClosingStartTag_root
selfClosingStartTag_root
  = \ v_2 v_3 ->
      (case v_3 of
           S v_5 v_6 v_7 v_8 v_9 -> ((($) v_9)
                                       (let f_13
                                              = (let f_12
                                                       = (let f_11
                                                                = ((ampOut (errSeen "/"))
                                                                     ((beforeAttName v_2) v_3))
                                                            in
                                                            (case v_7 of
                                                                 True -> ((ampOut (errWant ">"))
                                                                            (dat v_3))
                                                                 False -> f_11))
                                                   in
                                                   (case v_6 == '>' of
                                                        True -> ((ampOut TagEndClose) (dat v_5))
                                                        False -> f_12))
                                          in
                                          (case v_2 of
                                               True -> ((ampOut (errSeen "/"))
                                                          ((beforeAttName v_2) v_3))
                                               False -> f_13))))
bogusComment = bogusComment_root
bogusComment_root
  = \ v_2 ->
      (case v_2 of
           S v_4 v_5 v_6 v_7 v_8 -> ((ampOut Comment) (bogusComment1 v_2)))
bogusComment1 = bogusComment1_root
bogusComment1_root
  = \ v_2 ->
      (case v_2 of
           S v_4 v_5 v_6 v_7 v_8 -> ((($) v_8)
                                       (let f_11
                                              = (let f_10 = ((ampChar v_5) (bogusComment1 v_4)) in
                                                   (case v_6 of
                                                        True -> ((ampOut CommentEnd) (dat v_2))
                                                        False -> f_10))
                                          in
                                          (case v_5 == '>' of
                                               True -> ((ampOut CommentEnd) (dat v_4))
                                               False -> f_11))))
markupDeclOpen = markupDeclOpen_root
markupDeclOpen_root
  = \ v_2 ->
      (case v_2 of
           S v_4 v_5 v_6 v_7 v_8 -> ((($) v_8)
                                       (let f_14
                                              = (let f_13
                                                       = (let f_10
                                                                = ((ampOut (errWant "tag name"))
                                                                     (bogusComment v_2))
                                                            in
                                                            (let v_11 = (v_7 "[CDATA[") in
                                                               (case v_11 of
                                                                    Just v_12 -> (cdataSection v_12)
                                                                    Nothing -> f_10)))
                                                   in
                                                   (case (isAlpha v_5) of
                                                        True -> ((ampOut Tag)
                                                                   ((ampChar '!')
                                                                      ((ampChar v_5)
                                                                         ((tagName False) v_4))))
                                                        False -> f_13))
                                          in
                                          (let v_15 = (v_7 "--") in
                                             (case v_15 of
                                                  Just v_16 -> ((ampOut Comment)
                                                                  (commentStart v_16))
                                                  Nothing -> f_14)))))
commentStart = commentStart_root
commentStart_root
  = \ v_2 ->
      (case v_2 of
           S v_4 v_5 v_6 v_7 v_8 -> ((($) v_8)
                                       (let f_12
                                              = (let f_11
                                                       = (let f_10 = ((ampChar v_5) (comment v_4))
                                                            in
                                                            (case v_6 of
                                                                 True -> ((ampOut (errWant "-->"))
                                                                            ((ampOut CommentEnd)
                                                                               (dat v_2)))
                                                                 False -> f_10))
                                                   in
                                                   (case v_5 == '>' of
                                                        True -> ((ampOut (errSeen "<!-->"))
                                                                  ((ampOut CommentEnd) (dat v_4)))
                                                        False -> f_11))
                                          in
                                          (case v_5 == '-' of
                                               True -> (commentStartDash v_4)
                                               False -> f_12))))
commentStartDash = commentStartDash_root
commentStartDash_root
  = \ v_2 ->
      (case v_2 of
           S v_4 v_5 v_6 v_7 v_8 -> ((($) v_8)
                                       (let f_12
                                              = (let f_11
                                                       = (let f_10
                                                                = ((ampChar '-')
                                                                     ((ampChar v_5) (comment v_4)))
                                                            in
                                                            (case v_6 of
                                                                 True -> ((ampOut (errWant "-->"))
                                                                            ((ampOut CommentEnd)
                                                                               (dat v_2)))
                                                                 False -> f_10))
                                                   in
                                                   (case v_5 == '>' of
                                                        True -> ((ampOut (errSeen "<!--->"))
                                                                  ((ampOut CommentEnd) (dat v_4)))
                                                        False -> f_11))
                                          in
                                          (case v_5 == '-' of
                                               True -> (commentEnd v_4)
                                               False -> f_12))))
comment = comment_root
comment_root
  = \ v_2 ->
      (case v_2 of
           S v_4 v_5 v_6 v_7 v_8 -> ((($) v_8)
                                       (let f_11
                                              = (let f_10 = ((ampChar v_5) (comment v_4)) in
                                                   (case v_6 of
                                                        True -> ((ampOut (errWant "-->"))
                                                                   ((ampOut CommentEnd) (dat v_2)))
                                                        False -> f_10))
                                          in
                                          (case v_5 == '-' of
                                               True -> (commentEndDash v_4)
                                               False -> f_11))))
commentEndDash = commentEndDash_root
commentEndDash_root
  = \ v_2 ->
      (case v_2 of
           S v_4 v_5 v_6 v_7 v_8 -> ((($) v_8)
                                       (let f_11
                                              = (let f_10
                                                       = ((ampChar '-')
                                                            ((ampChar v_5) (comment v_4)))
                                                   in
                                                   (case v_6 of
                                                        True -> ((ampOut (errWant "-->"))
                                                                   ((ampOut CommentEnd) (dat v_2)))
                                                        False -> f_10))
                                          in
                                          (case v_5 == '-' of
                                               True -> (commentEnd v_4)
                                               False -> f_11))))
commentEnd = commentEnd_root
commentEnd_root
  = \ v_2 ->
      (case v_2 of
           S v_4 v_5 v_6 v_7 v_8 -> ((($) v_8)
                                       (let f_14
                                              = (let f_13
                                                       = (let f_12
                                                                = (let f_11
                                                                         = (let f_10
                                                                                  = ((ampOut
                                                                                        (errSeen
                                                                                           "--"))
                                                                                       ((ampChar
                                                                                           '-')
                                                                                          ((ampChar
                                                                                              '-')
                                                                                             ((ampChar
                                                                                                 v_5)
                                                                                                (comment
                                                                                                   v_4)))))
                                                                              in
                                                                              (case v_6 of
                                                                                   True -> ((ampOut
                                                                                               (errWant
                                                                                                  "-->"))
                                                                                              ((ampOut
                                                                                                  CommentEnd)
                                                                                                 (dat
                                                                                                    v_2)))
                                                                                   False -> f_10))
                                                                     in
                                                                     (case v_5 == '!' of
                                                                          True -> ((ampOut
                                                                                     (errSeen "!"))
                                                                                    (commentEndBang
                                                                                       v_4))
                                                                          False -> f_11))
                                                            in
                                                            (case (white v_5) of
                                                                 True -> ((ampOut (errSeen "--"))
                                                                            ((ampChar '-')
                                                                               ((ampChar '-')
                                                                                  ((ampChar v_5)
                                                                                     (commentEndSpace
                                                                                        v_4)))))
                                                                 False -> f_12))
                                                   in
                                                   (case v_5 == '-' of
                                                        True -> ((ampOut (errWant "-->"))
                                                                  ((ampChar '-') (commentEnd v_4)))
                                                        False -> f_13))
                                          in
                                          (case v_5 == '>' of
                                               True -> ((ampOut CommentEnd) (dat v_4))
                                               False -> f_14))))
commentEndBang = commentEndBang_root
commentEndBang_root
  = \ v_2 ->
      (case v_2 of
           S v_4 v_5 v_6 v_7 v_8 -> ((($) v_8)
                                       (let f_12
                                              = (let f_11
                                                       = (let f_10
                                                                = ((ampChar '-')
                                                                     ((ampChar '-')
                                                                        ((ampChar '!')
                                                                           ((ampChar v_5)
                                                                              (comment v_4)))))
                                                            in
                                                            (case v_6 of
                                                                 True -> ((ampOut (errWant "-->"))
                                                                            ((ampOut CommentEnd)
                                                                               (dat v_2)))
                                                                 False -> f_10))
                                                   in
                                                   (case v_5 == '-' of
                                                        True -> ((ampChar '-')
                                                                  ((ampChar '-')
                                                                     ((ampChar '!')
                                                                        (commentEndDash v_4))))
                                                        False -> f_11))
                                          in
                                          (case v_5 == '>' of
                                               True -> ((ampOut CommentEnd) (dat v_4))
                                               False -> f_12))))
commentEndSpace = commentEndSpace_root
commentEndSpace_root
  = \ v_2 ->
      (case v_2 of
           S v_4 v_5 v_6 v_7 v_8 -> ((($) v_8)
                                       (let f_13
                                              = (let f_12
                                                       = (let f_11
                                                                = (let f_10
                                                                         = ((ampChar v_5)
                                                                              (comment v_4))
                                                                     in
                                                                     (case v_6 of
                                                                          True -> ((ampOut
                                                                                      (errWant
                                                                                         "-->"))
                                                                                     ((ampOut
                                                                                         CommentEnd)
                                                                                        (dat v_2)))
                                                                          False -> f_10))
                                                            in
                                                            (case (white v_5) of
                                                                 True -> ((ampChar v_5)
                                                                            (commentEndSpace v_4))
                                                                 False -> f_11))
                                                   in
                                                   (case v_5 == '-' of
                                                        True -> (commentEndDash v_4)
                                                        False  -> f_12))
                                          in
                                          (case v_5 == '>' of
                                               True -> ((ampOut CommentEnd) (dat v_4))
                                               False -> f_13))))
cdataSection = cdataSection_root
cdataSection_root
  = \ v_2 ->
      (case v_2 of
           S v_4 v_5 v_6 v_7 v_8 -> ((($) v_8)
                                       (let f_11
                                              = (let f_10 = ((ampChar v_5) (cdataSection v_4)) in
                                                   (case v_6 of
                                                        True -> (dat v_2)
                                                        False -> f_10))
                                          in
                                          (let v_12 = (v_7 "]]>") in
                                             (case v_12 of
                                                  Just v_13 -> (dat v_13)
                                                  Nothing -> f_11)))))
charRef = charRef_root
charRef_root
  = \ v_2 v_3 v_4 v_5 ->
      (case v_5 of
           S v_7 v_8 v_9 v_10 v_11 -> ((($) v_11)
                                         (let f_14
                                                = (let f_13 = (((charRefAlpha v_2) v_3) v_5) in
                                                     (case v_8 == '#' of
                                                          True -> (((charRefNum v_2) v_5) v_7)
                                                          False -> f_13))
                                            in
                                            (case
                                               (((||) v_9)
                                                  (((||) ((elem v_8) "\t\n\f <&"))
                                                     (((maybe False) ((flip (==)) v_8)) v_4)))
                                               of
                                                 True -> ((ampChar '&') (v_2 v_5))
                                                 False -> f_14))))
charRefNum = charRefNum_root
charRefNum_root
  = \ v_2 v_3 v_4 ->
      (case v_4 of
           S v_6 v_7 v_8 v_9 v_10 -> ((($) v_10)
                                        (let f_12 = ((((charRefNum2 v_2) v_3) False) v_4) in
                                           (case ((elem v_7) "xX") of
                                                True -> ((((charRefNum2 v_2) v_3) True) v_6)
                                                False -> f_12))))
charRefNum2 = charRefNum2_root
charRefNum2_root
  = \ v_2 v_3 v_4 v_5 ->
      (case v_5 of
           S v_7 v_8 v_9 v_10 v_11 -> ((($) v_11)
                                         (let f_13
                                                = ((ampOut (errSeen "&")) ((ampChar '&') (v_2 v_3)))
                                            in
                                            (case ((hexChar v_4) v_8) of
                                                 True -> ((ampOut
                                                             (let f_14 = EntityNum in
                                                                (case v_4 of
                                                                     True -> EntityHex
                                                                     False -> f_14)))
                                                            ((ampChar v_8)
                                                               (((charRefNum3 v_2) v_4) v_7)))
                                                 False -> f_13))))
charRefNum3 = charRefNum3_root
charRefNum3_root
  = \ v_2 v_3 v_4 ->
      (case v_4 of
           S v_6 v_7 v_8 v_9 v_10 -> ((($) v_10)
                                        (let f_13
                                               = (let f_12
                                                        = ((ampOut (errWant ";"))
                                                             ((ampOut EntityEnd) (v_2 v_4)))
                                                    in
                                                    (case v_7 == ';' of
                                                         True -> ((ampOut EntityEnd) (v_2 v_6))
                                                         False -> f_12))
                                           in
                                           (case ((hexChar v_3) v_7) of
                                                True -> ((ampChar v_7)
                                                           (((charRefNum3 v_2) v_3) v_6))
                                                False -> f_13))))
charRefAlpha = charRefAlpha_root
charRefAlpha_root
  = \ v_2 v_3 v_4 ->
      (case v_4 of
           S v_6 v_7 v_8 v_9 v_10 -> ((($) v_10)
                                        (let f_12
                                               = ((ampOut (errSeen "&")) ((ampChar '&') (v_2 v_4)))
                                           in
                                           (case (isAlpha v_7) of
                                                True -> ((ampOut Entity)
                                                           ((ampChar v_7)
                                                              (((charRefAlpha2 v_2) v_3) v_6)))
                                                False -> f_12))))
charRefAlpha2 = charRefAlpha2_root
charRefAlpha2_root
  = \ v_2 v_3 v_4 ->
      (case v_4 of
           S v_6 v_7 v_8 v_9 v_10 -> ((($) v_10)
                                        (let f_14
                                               = (let f_13
                                                        = (let f_12
                                                                 = ((ampOut (errWant ";"))
                                                                      ((ampOut EntityEnd)
                                                                         (v_2 v_4)))
                                                             in
                                                             (case v_3 of
                                                                  True -> ((ampOut EntityEndAtt)
                                                                             (v_2 v_4))
                                                                  _ -> f_12))
                                                    in
                                                    (case v_7 == ';' of
                                                         True -> ((ampOut EntityEnd) (v_2 v_6))
                                                         False -> f_13))
                                           in
                                           (case (alphaChar v_7) of
                                                True -> ((ampChar v_7)
                                                           (((charRefAlpha2 v_2) v_3) v_6))
                                                False -> f_14))))
alphaChar = alphaChar_root
alphaChar_root
  = \ v_2 -> (((||) (isAlphaNum v_2)) ((elem v_2) ":-_"))
errSeen = errSeen_root
errSeen_root
  = \ v_2 -> ((($) Warn) (((++) "Unexpected ") (show v_2)))
errWant = errWant_root
errWant_root
  = \ v_2 -> ((($) Warn) (((++) "Expected ") (show v_2)))
expand = expand_root
expand_root
  = \ v_2 v_3 ->
      (let res
             = (((((S ((expand ((positionChar v_2) (head v_3))) (tail v_3)))
                     (let f_5 = (head v_3) in
                        (case (null v_3) of
                             True -> '\NUL'
                             _ -> f_5)))
                    (null v_3))
                   ((((expand_next v_2) v_3) v_2) v_3))
                  ((:) (Pos v_2)))
         in res)
expand_next
  = \ v_2 v_3 v_7 v_8 v_9 ->
      (let f_12
             = (let f_11 = Nothing in
                  (case v_9 of
                       [] -> ((($) Just) ((expand v_7) v_8))
                       x:y -> f_11))
         in
         (case v_8 of
              (:) v_13 v_14 -> (case v_9 of
                                    (:) v_15 v_16 -> (case (((==) v_13) v_15) of
                                                          True -> (((((expand_next v_2) v_3)
                                                                       ((positionChar v_7) v_13))
                                                                      v_14)
                                                                     v_16)
                                                          False -> f_12)
                                    [] -> f_12)
              [] -> f_12))
ampChar = ampChar_root
ampChar_root = \ v_2 v_3 -> (((:) (Char v_2)) v_3)
ampOut = ampOut_root
ampOut_root = \ v_2 v_3 -> (((:) v_2) v_3)
state = state_root
state_root = \ v_2 -> ((expand nullPosition) v_2)
output = \ v_2 v_3 ->
      case v_2 of
           ParseOptions v_5 v_6 v_7 v_8 v_9 -> ((($)
                                                   (case v_9 of
                                                        True -> tagTextMerge
                                                        False -> id))
                                                  ((((((((output_go v_2) v_3) v_5) v_6) v_7) v_8)
                                                      v_9)
                                                     (((,) (((,) nullPosition) [])) v_3)))
output_go
  = \ v_2 v_3 v_5 v_6 v_7 v_8 v_9 v_11 ->
      (let f_41
             = (let f_35
                      = (let f_27
                               = (let f_26
                                        = (let f_24
                                                 = (let f_21
                                                          = (let f_20
                                                                   = (let f_18
                                                                            = (let f_17
                                                                                     = (let f_13
                                                                                              = (case
                                                                                                   (isEof
                                                                                                      v_11)
                                                                                                   of
                                                                                                     True -> []
                                                                                                     False -> patternMatchFail)
                                                                                          in
                                                                                          (let v_14
                                                                                                 = (fromWarn
                                                                                                      v_11)
                                                                                             in
                                                                                             (case
                                                                                                v_14
                                                                                                of
                                                                                                  Just
                                                                                                    v_15 -> (let f_16
                                                                                                                   = ((((((((output_go
                                                                                                                               v_2)
                                                                                                                              v_3)
                                                                                                                             v_5)
                                                                                                                            v_6)
                                                                                                                           v_7)
                                                                                                                          v_8)
                                                                                                                         v_9)
                                                                                                                        ((((((((output_next
                                                                                                                                  v_2)
                                                                                                                                 v_3)
                                                                                                                                v_5)
                                                                                                                               v_6)
                                                                                                                              v_7)
                                                                                                                             v_8)
                                                                                                                            v_9)
                                                                                                                           v_11))
                                                                                                               in
                                                                                                               (case
                                                                                                                  v_6
                                                                                                                  of
                                                                                                                    True -> ((($)
                                                                                                                                ((((((((output_pos
                                                                                                                                          v_2)
                                                                                                                                         v_3)
                                                                                                                                        v_5)
                                                                                                                                       v_6)
                                                                                                                                      v_7)
                                                                                                                                     v_8)
                                                                                                                                    v_9)
                                                                                                                                   v_11))
                                                                                                                               (((:)
                                                                                                                                   (TagWarning
                                                                                                                                      (fromString
                                                                                                                                         v_15)))
                                                                                                                                  ((((((((output_go
                                                                                                                                            v_2)
                                                                                                                                           v_3)
                                                                                                                                          v_5)
                                                                                                                                         v_6)
                                                                                                                                        v_7)
                                                                                                                                       v_8)
                                                                                                                                      v_9)
                                                                                                                                     ((((((((output_next
                                                                                                                                               v_2)
                                                                                                                                              v_3)
                                                                                                                                             v_5)
                                                                                                                                            v_6)
                                                                                                                                           v_7)
                                                                                                                                          v_8)
                                                                                                                                         v_9)
                                                                                                                                        v_11))))
                                                                                                                    False -> f_16))
                                                                                                  Nothing -> f_13)))
                                                                                 in
                                                                                 (let y_a
                                                                                        = ((($)
                                                                                              (((((((output_chars
                                                                                                       v_2)
                                                                                                      v_3)
                                                                                                     v_5)
                                                                                                    v_6)
                                                                                                   v_7)
                                                                                                  v_8)
                                                                                                 v_9))
                                                                                             ((((((((output_next
                                                                                                       v_2)
                                                                                                      v_3)
                                                                                                     v_5)
                                                                                                    v_6)
                                                                                                   v_7)
                                                                                                  v_8)
                                                                                                 v_9)
                                                                                                v_11))
                                                                                    in
                                                                                    (let y
                                                                                           = (fst
                                                                                                y_a)
                                                                                         a
                                                                                           = (snd
                                                                                                y_a)
                                                                                       in
                                                                                       (case
                                                                                          (isEntityChr
                                                                                             v_11)
                                                                                          of
                                                                                            True -> ((($)
                                                                                                        ((((((((output_pos
                                                                                                                  v_2)
                                                                                                                 v_3)
                                                                                                                v_5)
                                                                                                               v_6)
                                                                                                              v_7)
                                                                                                             v_8)
                                                                                                            v_9)
                                                                                                           v_11))
                                                                                                       (((:)
                                                                                                           (TagText
                                                                                                              ((($)
                                                                                                                  fromChar)
                                                                                                                 ((entityChr
                                                                                                                     v_11)
                                                                                                                    a))))
                                                                                                          ((((((((output_go
                                                                                                                    v_2)
                                                                                                                   v_3)
                                                                                                                  v_5)
                                                                                                                 v_6)
                                                                                                                v_7)
                                                                                                               v_8)
                                                                                                              v_9)
                                                                                                             (((((((((output_skip
                                                                                                                        v_2)
                                                                                                                       v_3)
                                                                                                                      v_5)
                                                                                                                     v_6)
                                                                                                                    v_7)
                                                                                                                   v_8)
                                                                                                                  v_9)
                                                                                                                 isEntityEnd)
                                                                                                                y))))
                                                                                            False -> f_17))))
                                                                        in
                                                                        (let y_a
                                                                               = ((($)
                                                                                     (((((((output_charsStr
                                                                                              v_2)
                                                                                             v_3)
                                                                                            v_5)
                                                                                           v_6)
                                                                                          v_7)
                                                                                         v_8)
                                                                                        v_9))
                                                                                    ((((((((output_next
                                                                                              v_2)
                                                                                             v_3)
                                                                                            v_5)
                                                                                           v_6)
                                                                                          v_7)
                                                                                         v_8)
                                                                                        v_9)
                                                                                       v_11))
                                                                           in
                                                                           (let y = (fst y_a)
                                                                                a = (snd y_a)
                                                                              in
                                                                              (case (isEntity v_11)
                                                                                 of
                                                                                   True -> (((++)
                                                                                               (((((((((output_poss
                                                                                                          v_2)
                                                                                                         v_3)
                                                                                                        v_5)
                                                                                                       v_6)
                                                                                                      v_7)
                                                                                                     v_8)
                                                                                                    v_9)
                                                                                                   v_11)
                                                                                                  ((($)
                                                                                                      (let f_19
                                                                                                             = (filter
                                                                                                                  (((.)
                                                                                                                      not)
                                                                                                                     isTagWarning))
                                                                                                         in
                                                                                                         (case
                                                                                                            v_6
                                                                                                            of
                                                                                                              True -> id
                                                                                                              False -> f_19)))
                                                                                                     (v_7
                                                                                                        a))))
                                                                                              ((((((((output_go
                                                                                                        v_2)
                                                                                                       v_3)
                                                                                                      v_5)
                                                                                                     v_6)
                                                                                                    v_7)
                                                                                                   v_8)
                                                                                                  v_9)
                                                                                                 (((((((((output_skip
                                                                                                            v_2)
                                                                                                           v_3)
                                                                                                          v_5)
                                                                                                         v_6)
                                                                                                        v_7)
                                                                                                       v_8)
                                                                                                      v_9)
                                                                                                     isEntityEnd)
                                                                                                    y)))
                                                                                   False -> f_18))))
                                                               in
                                                               (let y_a
                                                                      = ((($)
                                                                            (((((((output_charsStr
                                                                                     v_2)
                                                                                    v_3)
                                                                                   v_5)
                                                                                  v_6)
                                                                                 v_7)
                                                                                v_8)
                                                                               v_9))
                                                                           ((((((((output_next v_2)
                                                                                    v_3)
                                                                                   v_5)
                                                                                  v_6)
                                                                                 v_7)
                                                                                v_8)
                                                                               v_9)
                                                                              v_11))
                                                                  in
                                                                  (let y = (fst y_a)
                                                                       a = (snd y_a)
                                                                     in
                                                                     (case (isComment v_11) of
                                                                          True -> ((($)
                                                                                      ((((((((output_pos
                                                                                                v_2)
                                                                                               v_3)
                                                                                              v_5)
                                                                                             v_6)
                                                                                            v_7)
                                                                                           v_8)
                                                                                          v_9)
                                                                                         v_11))
                                                                                     (((:)
                                                                                         (TagComment
                                                                                            a))
                                                                                        ((((((((output_go
                                                                                                  v_2)
                                                                                                 v_3)
                                                                                                v_5)
                                                                                               v_6)
                                                                                              v_7)
                                                                                             v_8)
                                                                                            v_9)
                                                                                           (((((((((output_skip
                                                                                                      v_2)
                                                                                                     v_3)
                                                                                                    v_5)
                                                                                                   v_6)
                                                                                                  v_7)
                                                                                                 v_8)
                                                                                                v_9)
                                                                                               isCommentEnd)
                                                                                              y))))
                                                                          False -> f_20))))
                                                      in
                                                      (let y_a
                                                             = ((($)
                                                                   (((((((output_charsStr v_2) v_3)
                                                                          v_5)
                                                                         v_6)
                                                                        v_7)
                                                                       v_8)
                                                                      v_9))
                                                                  ((((((((output_next v_2) v_3) v_5)
                                                                         v_6)
                                                                        v_7)
                                                                       v_8)
                                                                      v_9)
                                                                     v_11))
                                                         in
                                                         (let y = (fst y_a)
                                                              a = (snd y_a)
                                                            in
                                                            (let z_b
                                                                   = ((((((((output_atts v_2) v_3)
                                                                             v_5)
                                                                            v_6)
                                                                           v_7)
                                                                          v_8)
                                                                         v_9)
                                                                        y)
                                                               in
                                                               (let z = (fst z_b)
                                                                    b = (snd z_b)
                                                                  in
                                                                  (case (isTagShut v_11) of
                                                                       True -> ((($)
                                                                                   ((((((((output_pos
                                                                                             v_2)
                                                                                            v_3)
                                                                                           v_5)
                                                                                          v_6)
                                                                                         v_7)
                                                                                        v_8)
                                                                                       v_9)
                                                                                      v_11))
                                                                                  ((($)
                                                                                      ((:)
                                                                                         (TagClose
                                                                                            a)))
                                                                                     ((($)
                                                                                         (case
                                                                                            (not
                                                                                               (null
                                                                                                  b))
                                                                                            of
                                                                                              True -> (((((((((output_warn
                                                                                                                 v_2)
                                                                                                                v_3)
                                                                                                               v_5)
                                                                                                              v_6)
                                                                                                             v_7)
                                                                                                            v_8)
                                                                                                           v_9)
                                                                                                          v_11)
                                                                                                         "Unexpected attributes in close tag")
                                                                                              _ -> id))
                                                                                        (let f_23
                                                                                               = ((((((((output_go
                                                                                                           v_2)
                                                                                                          v_3)
                                                                                                         v_5)
                                                                                                        v_6)
                                                                                                       v_7)
                                                                                                      v_8)
                                                                                                     v_9)
                                                                                                    (((((((((output_skip
                                                                                                               v_2)
                                                                                                              v_3)
                                                                                                             v_5)
                                                                                                            v_6)
                                                                                                           v_7)
                                                                                                          v_8)
                                                                                                         v_9)
                                                                                                        isTagEnd)
                                                                                                       z))
                                                                                           in
                                                                                           (case
                                                                                              (isTagEndClose
                                                                                                 z)
                                                                                              of
                                                                                                True -> ((($)
                                                                                                            (((((((((output_warn
                                                                                                                       v_2)
                                                                                                                      v_3)
                                                                                                                     v_5)
                                                                                                                    v_6)
                                                                                                                   v_7)
                                                                                                                  v_8)
                                                                                                                 v_9)
                                                                                                                v_11)
                                                                                                               "Unexpected self-closing in close tag"))
                                                                                                           ((((((((output_go
                                                                                                                     v_2)
                                                                                                                    v_3)
                                                                                                                   v_5)
                                                                                                                  v_6)
                                                                                                                 v_7)
                                                                                                                v_8)
                                                                                                               v_9)
                                                                                                              ((((((((output_next
                                                                                                                        v_2)
                                                                                                                       v_3)
                                                                                                                      v_5)
                                                                                                                     v_6)
                                                                                                                    v_7)
                                                                                                                   v_8)
                                                                                                                  v_9)
                                                                                                                 z)))
                                                                                                False -> f_23)))))
                                                                       False -> f_21))))))
                                             in
                                             (let y_a
                                                    = ((($)
                                                          (((((((output_charsStr v_2) v_3) v_5) v_6)
                                                               v_7)
                                                              v_8)
                                                             v_9))
                                                         ((((((((output_next v_2) v_3) v_5) v_6)
                                                               v_7)
                                                              v_8)
                                                             v_9)
                                                            v_11))
                                                in
                                                (let y = (fst y_a)
                                                     a = (snd y_a)
                                                   in
                                                   (let z_b
                                                          = ((((((((output_atts v_2) v_3) v_5) v_6)
                                                                  v_7)
                                                                 v_8)
                                                                v_9)
                                                               y)
                                                      in
                                                      (let z = (fst z_b)
                                                           b = (snd z_b)
                                                         in
                                                         (case (isTag v_11) of
                                                              True -> ((($)
                                                                          ((((((((output_pos v_2)
                                                                                   v_3)
                                                                                  v_5)
                                                                                 v_6)
                                                                                v_7)
                                                                               v_8)
                                                                              v_9)
                                                                             v_11))
                                                                         (((:) ((TagOpen a) b))
                                                                            (let f_25
                                                                                   = ((((((((output_go
                                                                                               v_2)
                                                                                              v_3)
                                                                                             v_5)
                                                                                            v_6)
                                                                                           v_7)
                                                                                          v_8)
                                                                                         v_9)
                                                                                        (((((((((output_skip
                                                                                                   v_2)
                                                                                                  v_3)
                                                                                                 v_5)
                                                                                                v_6)
                                                                                               v_7)
                                                                                              v_8)
                                                                                             v_9)
                                                                                            isTagEnd)
                                                                                           z))
                                                                               in
                                                                               (case
                                                                                  (isTagEndClose z)
                                                                                  of
                                                                                    True -> ((($)
                                                                                                ((((((((output_pos
                                                                                                          v_2)
                                                                                                         v_3)
                                                                                                        v_5)
                                                                                                       v_6)
                                                                                                      v_7)
                                                                                                     v_8)
                                                                                                    v_9)
                                                                                                   v_11))
                                                                                               (((:)
                                                                                                   (TagClose
                                                                                                      a))
                                                                                                  ((((((((output_go
                                                                                                            v_2)
                                                                                                           v_3)
                                                                                                          v_5)
                                                                                                         v_6)
                                                                                                        v_7)
                                                                                                       v_8)
                                                                                                      v_9)
                                                                                                     ((((((((output_next
                                                                                                               v_2)
                                                                                                              v_3)
                                                                                                             v_5)
                                                                                                            v_6)
                                                                                                           v_7)
                                                                                                          v_8)
                                                                                                         v_9)
                                                                                                        z))))
                                                                                    False -> f_25))))
                                                              False -> f_24))))))
                                    in
                                    (let y_a
                                           = ((((((((output_charsStr v_2) v_3) v_5) v_6) v_7) v_8)
                                                 v_9)
                                                v_11)
                                       in
                                       (let y = (fst y_a)
                                            a = (snd y_a)
                                          in
                                          (case (isChar v_11) of
                                               True -> ((($)
                                                           ((((((((output_pos v_2) v_3) v_5) v_6)
                                                                 v_7)
                                                                v_8)
                                                               v_9)
                                                              v_11))
                                                          (((:) (TagText a))
                                                             ((((((((output_go v_2) v_3) v_5) v_6)
                                                                   v_7)
                                                                  v_8)
                                                                 v_9)
                                                                y)))
                                               False -> f_26))))
                           in
                           (case v_11 of
                                (,) v_28 v_29 -> (case v_28 of
                                                      (,) v_30 v_31 -> (case v_29 of
                                                                            (:) v_32 v_33 -> (case
                                                                                                v_32
                                                                                                of
                                                                                                  Pos
                                                                                                    v_34 -> ((((((((output_go
                                                                                                                      v_2)
                                                                                                                     v_3)
                                                                                                                    v_5)
                                                                                                                   v_6)
                                                                                                                  v_7)
                                                                                                                 v_8)
                                                                                                                v_9)
                                                                                                               (((,)
                                                                                                                   (((,)
                                                                                                                       v_34)
                                                                                                                      v_31))
                                                                                                                  v_33))
                                                                                                  _ -> f_27)
                                                                            [] -> f_27))))
                  in
                  (case v_11 of
                       (,) v_36 v_37 -> (case v_36 of
                                             (,) v_38 v_39 -> (case ((($) not) (null v_39)) of
                                                                   True -> ((($)
                                                                               (case v_6 of
                                                                                    True -> ((++)
                                                                                               (reverse
                                                                                                  v_39))
                                                                                    _ -> id))
                                                                              ((((((((output_go v_2)
                                                                                       v_3)
                                                                                      v_5)
                                                                                     v_6)
                                                                                    v_7)
                                                                                   v_8)
                                                                                  v_9)
                                                                                 (((,)
                                                                                     (((,) v_38)
                                                                                        []))
                                                                                    v_37)))
                                                                   False -> f_35))))
         in
         (case v_11 of
              (,) v_42 v_43 -> (case v_42 of
                                    (,) v_44 v_45 -> (case ((seq v_44) False) of
                                                          True -> []
                                                          False -> f_41))))
output_atts
  = \ v_2 v_3 v_5 v_6 v_7 v_8 v_9 v_47 ->
      (let f_50
             = (let f_49 = (((,) v_47) []) in
                  (let y_a
                         = ((((((((output_charsEntsStr v_2) v_3) v_5) v_6) v_7) v_8) v_9)
                              ((((((((output_next v_2) v_3) v_5) v_6) v_7) v_8) v_9) v_47))
                     in
                     (let y = (fst y_a)
                          a = (snd y_a)
                        in
                        (case (isAttVal v_47) of
                             True -> ((($) (second ((:) (((,) empty) a))))
                                        ((((((((output_atts v_2) v_3) v_5) v_6) v_7) v_8) v_9) y))
                             _ -> f_49))))
         in
         (let y_a
                = ((((((((output_charsStr v_2) v_3) v_5) v_6) v_7) v_8) v_9)
                     ((((((((output_next v_2) v_3) v_5) v_6) v_7) v_8) v_9) v_47))
            in
            (let y = (fst y_a)
                 a = (snd y_a)
               in
               (let z_b
                      = (let f_51 = (((,) y) empty) in
                           (case (isAttVal y) of
                                True -> ((((((((output_charsEntsStr v_2) v_3) v_5) v_6) v_7) v_8)
                                            v_9)
                                           ((((((((output_next v_2) v_3) v_5) v_6) v_7) v_8) v_9)
                                              y))
                                _ -> f_51))
                  in
                  (let z = (fst z_b)
                       b = (snd z_b)
                     in
                     (case (isAttName v_47) of
                          True -> ((($) (second ((:) (((,) a) b))))
                                     ((((((((output_atts v_2) v_3) v_5) v_6) v_7) v_8) v_9) z))
                          _ -> f_50))))))
output_chars
  = \ v_2 v_3 v_5 v_6 v_7 v_8 v_9 v_53 ->
      (((((((((output_charss v_2) v_3) v_5) v_6) v_7) v_8) v_9) False)
         v_53)
output_charsStr
  = \ v_2 v_3 v_5 v_6 v_7 v_8 v_9 v_56 ->
      ((($) ((opStarStarStar id) fromString))
         ((((((((output_chars v_2) v_3) v_5) v_6) v_7) v_8) v_9) v_56))
output_charsEntsStr
  = \ v_2 v_3 v_5 v_6 v_7 v_8 v_9 v_59 ->
      ((($) ((opStarStarStar id) fromString))
         (((((((((output_charss v_2) v_3) v_5) v_6) v_7) v_8) v_9) True)
            v_59))
output_charss
  = \ v_2 v_3 v_5 v_6 v_7 v_8 v_9 v_62 v_63 ->
      (let f_81
             = (let f_79
                      = (let f_77
                               = (let f_69
                                        = (let f_65 = (((,) v_63) []) in
                                             (let v_66 = (fromWarn v_63) in
                                                (case v_66 of
                                                     Just v_67 -> ((($)
                                                                      ((((((((output_charss v_2)
                                                                               v_3)
                                                                              v_5)
                                                                             v_6)
                                                                            v_7)
                                                                           v_8)
                                                                          v_9)
                                                                         v_62))
                                                                     ((($)
                                                                         (case v_6 of
                                                                              True -> ((((((((output_addWarns
                                                                                                v_2)
                                                                                               v_3)
                                                                                              v_5)
                                                                                             v_6)
                                                                                            v_7)
                                                                                           v_8)
                                                                                          v_9)
                                                                                         (((:)
                                                                                             ((($)
                                                                                                 TagWarning)
                                                                                                (fromString
                                                                                                   v_67)))
                                                                                            []))
                                                                              _ -> id))
                                                                        ((((((((output_next v_2)
                                                                                 v_3)
                                                                                v_5)
                                                                               v_6)
                                                                              v_7)
                                                                             v_8)
                                                                            v_9)
                                                                           v_63)))
                                                     Nothing -> f_65)))
                                    in
                                    (case v_63 of
                                         (,) v_70 v_71 -> (case v_70 of
                                                               (,) v_72 v_73 -> (case v_71 of
                                                                                     (:) v_74
                                                                                       v_75 -> (case
                                                                                                  v_74
                                                                                                  of
                                                                                                    Pos
                                                                                                      v_76 -> (((((((((output_charss
                                                                                                                         v_2)
                                                                                                                        v_3)
                                                                                                                       v_5)
                                                                                                                      v_6)
                                                                                                                     v_7)
                                                                                                                    v_8)
                                                                                                                   v_9)
                                                                                                                  v_62)
                                                                                                                 (((,)
                                                                                                                     (((,)
                                                                                                                         v_76)
                                                                                                                        v_73))
                                                                                                                    v_75))
                                                                                                    _ -> f_69)
                                                                                     [] -> f_69))))
                           in
                           (let y_a
                                  = ((($) (((((((output_chars v_2) v_3) v_5) v_6) v_7) v_8) v_9))
                                       ((((((((output_next v_2) v_3) v_5) v_6) v_7) v_8) v_9) v_63))
                              in
                              (let y = (fst y_a)
                                   a = (snd y_a)
                                 in
                                 (let z
                                        = ((($) fst)
                                             ((($)
                                                 ((((((((output_charss v_2) v_3) v_5) v_6) v_7) v_8)
                                                     v_9)
                                                    v_62))
                                                (let f_78
                                                       = (((((((((output_skip v_2) v_3) v_5) v_6)
                                                                v_7)
                                                               v_8)
                                                              v_9)
                                                             isEntityEndAtt)
                                                            y)
                                                   in
                                                   (case (isEntityEnd y) of
                                                        True -> ((((((((output_next v_2) v_3) v_5)
                                                                       v_6)
                                                                      v_7)
                                                                     v_8)
                                                                    v_9)
                                                                   y)
                                                        _ -> f_78))))
                                    in
                                    (case v_62 of
                                         True -> (case (isEntityChr v_63) of
                                                      True -> ((($)
                                                                  (second
                                                                     ((:) ((entityChr v_63) a))))
                                                                 (((((((((output_charss v_2) v_3)
                                                                          v_5)
                                                                         v_6)
                                                                        v_7)
                                                                       v_8)
                                                                      v_9)
                                                                     v_62)
                                                                    z))
                                                      _ -> f_77)
                                         _ -> f_77)))))
                  in
                  (let y_a
                         = ((($) (((((((output_charsStr v_2) v_3) v_5) v_6) v_7) v_8) v_9))
                              ((((((((output_next v_2) v_3) v_5) v_6) v_7) v_8) v_9) v_63))
                     in
                     (let y = (fst y_a)
                          a = (snd y_a)
                        in
                        (let b = ((($) not) (isEntityEndAtt y)) in
                           (let z
                                  = (let f_80
                                           = ((((((((output_next v_2) v_3) v_5) v_6) v_7) v_8) v_9)
                                                y)
                                       in
                                       (case b of
                                            True -> (((((((((output_skip v_2) v_3) v_5) v_6) v_7)
                                                          v_8)
                                                         v_9)
                                                        isEntityEnd)
                                                       y)
                                            _ -> f_80))
                                n_m = (v_8 (((,) a) b))
                              in
                              (let n = (fst n_m)
                                   m = (snd n_m)
                                 in
                                 (case v_62 of
                                      True -> (case (isEntity v_63) of
                                                   True -> ((($) (second ((++) (toString n))))
                                                              ((($)
                                                                  ((((((((output_charss v_2) v_3)
                                                                          v_5)
                                                                         v_6)
                                                                        v_7)
                                                                       v_8)
                                                                      v_9)
                                                                     v_62))
                                                                 (((((((((output_addWarns v_2) v_3)
                                                                          v_5)
                                                                         v_6)
                                                                        v_7)
                                                                       v_8)
                                                                      v_9)
                                                                     m)
                                                                    z)))
                                                   _ -> f_79)
                                      _ -> f_79)))))))
         in
         (let y_b
                = (((((((((output_charss v_2) v_3) v_5) v_6) v_7) v_8) v_9) v_62)
                     ((((((((output_next v_2) v_3) v_5) v_6) v_7) v_8) v_9) v_63))
            in
            (let y = (fst y_b)
                 b = (snd y_b)
               in
               (let v_82 = (fromChr v_63) in
                  (case v_82 of
                       Just v_83 -> (((,) y) (((:) v_83) b))
                       _ -> f_81)))))
output_next
  = \ v_2 v_3 v_5 v_6 v_7 v_8 v_9 v_85 -> ((second (drop 1)) v_85)
output_skip
  = \ v_2 v_3 v_5 v_6 v_7 v_8 v_9 v_88 v_89 ->
      ((assert (((||) (isEof v_89)) (v_88 v_89)))
         ((((((((output_next v_2) v_3) v_5) v_6) v_7) v_8) v_9) v_89))
output_addWarns
  = \ v_2 v_3 v_5 v_6 v_7 v_8 v_9 v_92 v_93 ->
      (case v_93 of
           (,) v_95 v_96 -> (case v_95 of
                                 (,) v_97 v_98 -> (((,)
                                                      (((,) v_97)
                                                         (((++)
                                                             (reverse
                                                                (((((((((output_poss v_2) v_3) v_5)
                                                                        v_6)
                                                                       v_7)
                                                                      v_8)
                                                                     v_9)
                                                                    v_93)
                                                                   v_92)))
                                                            v_98)))
                                                     v_96)))
output_pos
  = \ v_2 v_3 v_5 v_6 v_7 v_8 v_9 v_100 v_101 ->
      (case v_100 of
           (,) v_103 v_104 -> (case v_103 of
                                   (,) v_105 v_106 -> (case v_5 of
                                                           True -> (((:) (tagPosition v_105)) v_101)
                                                           _ -> v_101)))
output_warn
  = \ v_2 v_3 v_5 v_6 v_7 v_8 v_9 v_109 v_110 v_111 ->
      (case v_6 of
           True -> ((($)
                       ((((((((output_pos v_2) v_3) v_5) v_6) v_7) v_8) v_9) v_109))
                      (((:) (TagWarning (fromString v_110))) v_111))
           _ -> v_111)
output_poss
  = \ v_2 v_3 v_5 v_6 v_7 v_8 v_9 v_115 ->
      (concatMap
         ((((((((output_v_119 v_2) v_3) v_5) v_6) v_7) v_8) v_9) v_115))
output_v_119
  = \ v_2 v_3 v_5 v_6 v_7 v_8 v_9 v_115 v_118 ->
      (((((((((output_pos v_2) v_3) v_5) v_6) v_7) v_8) v_9) v_115)
         (((:) v_118) []))
entityChr = entityChr_root
entityChr_root
  = \ v_2 v_3 ->
      (let f_5
             = (case (isEntityHex v_2) of
                    True -> ((($) chr) ((($) fst) ((($) head) (readHex v_3))))
                    _ -> patternMatchFail)
         in
         (case (isEntityNum v_2) of
              True -> ((($) chr) (read v_3))
              _ -> f_5))
isEof = isEof_root
isEof_root
  = \ v_2 ->
      (let f_4 = False in
         (case v_2 of
              (,) v_5 v_6 -> (case v_6 of
                                  [] -> True
                                  _ -> f_4)))
isChar = isChar_root
isChar_root
  = \ v_2 ->
      (let f_4 = False in
         (case v_2 of
              (,) v_5 v_6 -> (case v_6 of
                                  (:) v_7 v_8 -> (case v_7 of
                                                      Char v_9 -> True
                                                      _ -> f_4)
                                  _ -> f_4)))
isTag = isTag_root
isTag_root
  = \ v_2 ->
      (let f_4 = False in
         (case v_2 of
              (,) v_5 v_6 -> (case v_6 of
                                  (:) v_7 v_8 -> (case v_7 of
                                                      Tag -> True
                                                      _ -> f_4)
                                  _ -> f_4)))
isTagShut = isTagShut_root
isTagShut_root
  = \ v_2 ->
      (let f_4 = False in
         (case v_2 of
              (,) v_5 v_6 -> (case v_6 of
                                  (:) v_7 v_8 -> (case v_7 of
                                                      TagShut -> True
                                                      _ -> f_4)
                                  _ -> f_4)))
isAttName = isAttName_root
isAttName_root
  = \ v_2 ->
      (let f_4 = False in
         (case v_2 of
              (,) v_5 v_6 -> (case v_6 of
                                  (:) v_7 v_8 -> (case v_7 of
                                                      AttName -> True
                                                      _ -> f_4)
                                  _ -> f_4)))
isAttVal = isAttVal_root
isAttVal_root
  = \ v_2 ->
      (let f_4 = False in
         (case v_2 of
              (,) v_5 v_6 -> (case v_6 of
                                  (:) v_7 v_8 -> (case v_7 of
                                                      AttVal -> True
                                                      _ -> f_4)
                                  _ -> f_4)))
isTagEnd = isTagEnd_root
isTagEnd_root
  = \ v_2 ->
      (let f_4 = False in
         (case v_2 of
              (,) v_5 v_6 -> (case v_6 of
                                  (:) v_7 v_8 -> (case v_7 of
                                                      TagEnd -> True
                                                      _ -> f_4)
                                  _ -> f_4)))
isTagEndClose = isTagEndClose_root
isTagEndClose_root
  = \ v_2 ->
      (let f_4 = False in
         (case v_2 of
              (,) v_5 v_6 -> (case v_6 of
                                  (:) v_7 v_8 -> (case v_7 of
                                                      TagEndClose -> True
                                                      _ -> f_4)
                                  _ -> f_4)))
isComment = isComment_root
isComment_root
  = \ v_2 ->
      (let f_4 = False in
         (case v_2 of
              (,) v_5 v_6 -> (case v_6 of
                                  (:) v_7 v_8 -> (case v_7 of
                                                      Comment -> True
                                                      _ -> f_4)
                                  _ -> f_4)))
isCommentEnd = isCommentEnd_root
isCommentEnd_root
  = \ v_2 ->
      (let f_4 = False in
         (case v_2 of
              (,) v_5 v_6 -> (case v_6 of
                                  (:) v_7 v_8 -> (case v_7 of
                                                      CommentEnd -> True
                                                      _ -> f_4)
                                  _ -> f_4)))
isEntity = isEntity_root
isEntity_root
  = \ v_2 ->
      (let f_4 = False in
         (case v_2 of
              (,) v_5 v_6 -> (case v_6 of
                                  (:) v_7 v_8 -> (case v_7 of
                                                      Entity -> True
                                                      _ -> f_4)
                                  _ -> f_4)))
isEntityChr = isEntityChr_root
isEntityChr_root
  = \ v_2 ->
      (let f_9
             = (let f_4 = False in
                  (case v_2 of
                       (,) v_5 v_6 -> (case v_6 of
                                           (:) v_7 v_8 -> (case v_7 of
                                                               EntityHex -> True
                                                               _ -> f_4)
                                           _ -> f_4)))
         in
         (case v_2 of
              (,) v_10 v_11 -> (case v_11 of
                                    (:) v_12 v_13 -> (case v_12 of
                                                          EntityNum -> True
                                                          _ -> f_9)
                                    _ -> f_9)))
isEntityNum = isEntityNum_root
isEntityNum_root
  = \ v_2 ->
      (let f_4 = False in
         (case v_2 of
              (,) v_5 v_6 -> (case v_6 of
                                  (:) v_7 v_8 -> (case v_7 of
                                                      EntityNum -> True
                                                      _ -> f_4)
                                  _ -> f_4)))
isEntityHex = isEntityHex_root
isEntityHex_root
  = \ v_2 ->
      (let f_4 = False in
         (case v_2 of
              (,) v_5 v_6 -> (case v_6 of
                                  (:) v_7 v_8 -> (case v_7 of
                                                      EntityHex -> True
                                                      _ -> f_4)
                                  _ -> f_4)))
isEntityEnd = isEntityEnd_root
isEntityEnd_root
  = \ v_2 ->
      (let f_4 = False in
         (case v_2 of
              (,) v_5 v_6 -> (case v_6 of
                                  (:) v_7 v_8 -> (case v_7 of
                                                      EntityEnd -> True
                                                      _ -> f_4)
                                  _ -> f_4)))
isEntityEndAtt = isEntityEndAtt_root
isEntityEndAtt_root
  = \ v_2 ->
      (let f_4 = False in
         (case v_2 of
              (,) v_5 v_6 -> (case v_6 of
                                  (:) v_7 v_8 -> (case v_7 of
                                                      EntityEndAtt -> True
                                                      _ -> f_4)
                                  _ -> f_4)))
isWarn = isWarn_root
isWarn_root
  = \ v_2 ->
      (let f_4 = False in
         (case v_2 of
              (,) v_5 v_6 -> (case v_6 of
                                  (:) v_7 v_8 -> (case v_7 of
                                                      Warn v_9 -> True
                                                      _ -> f_4)
                                  _ -> f_4)))
fromChr = fromChr_root
fromChr_root
  = \ v_2 ->
      (let f_4 = Nothing in
         (case v_2 of
              (,) v_5 v_6 -> (case v_6 of
                                  (:) v_7 v_8 -> (case v_7 of
                                                      Char v_9 -> (Just v_9)
                                                      _ -> f_4)
                                  _ -> f_4)))
fromWarn = \ v_2 ->
      (let f_4 = Nothing in
         (case v_2 of
              (,) v_5 v_6 -> (case v_6 of
                                  (:) v_7 v_8 -> (case v_7 of
                                                      Warn v_9 -> (Just v_9)
                                                      _ -> f_4)
                                  _ -> f_4)))
tagTextMerge = \ v_2 ->
      (let f_7
             = (let f_4
                      = (case v_2 of
                             [] -> []
                             _ -> patternMatchFail)
                  in
                  (case v_2 of
                       (:) v_5 v_6 -> (((:) v_5) (tagTextMerge v_6))
                       _ -> f_4))
         in
         (case v_2 of
              (:) v_8 v_9 -> (case v_8 of
                                  TagText v_10 -> (let a_b
                                                         = ((((((tagTextMerge_f v_2) f_7) v_8) v_9)
                                                               v_10)
                                                              v_9)
                                                     in
                                                     (let a = (fst a_b)
                                                          b = (snd a_b)
                                                        in
                                                        (((:) (TagText (strConcat (((:) v_10) a))))
                                                           (tagTextMerge b))))
                                  _ -> f_7)
              _ -> f_7))
tagTextMerge_f
  = \ v_2 f_7 v_8 v_9 v_10 v_12 ->
      (let f_22
             = (let f_14
                      = ((((((((tagTextMerge_g v_2) f_7) v_8) v_9) v_10) v_12) id) v_12)
                  in
                  (case v_12 of
                       (:) v_15 v_16 -> (case v_15 of
                                             TagPosition v_17 v_18 -> (case v_16 of
                                                                           (:) v_19 v_20 -> (case
                                                                                               v_19
                                                                                               of
                                                                                                 TagText
                                                                                                   v_21 -> ((($)
                                                                                                               (((((tagTextMerge_f
                                                                                                                      v_2)
                                                                                                                     f_7)
                                                                                                                    v_8)
                                                                                                                   v_9)
                                                                                                                  v_10))
                                                                                                              (((:)
                                                                                                                  v_19)
                                                                                                                 v_20))
                                                                                                 _ -> f_14)
                                                                           _ -> f_14)
                                             _ -> f_14)
                       _ -> f_14))
         in
         (case v_12 of
              (:) v_23 v_24 -> (case v_23 of
                                    TagText v_25 -> (let a_b
                                                           = ((((((tagTextMerge_f v_2) f_7) v_8)
                                                                  v_9)
                                                                 v_10)
                                                                v_24)
                                                       in
                                                       (let a = (fst a_b)
                                                            b = (snd a_b)
                                                          in (((,) (((:) v_25) a)) b)))
                                    _ -> f_22)
              _ -> f_22))
tagTextMerge_g
  = \ v_2 f_7 v_8 v_9 v_10 v_27 v_28 v_29 ->
      (let f_47
             = (let f_43
                      = (let f_35
                               = (let f_31 = (((,) []) v_27) in
                                    (case v_29 of
                                         (:) v_32 v_33 -> (case v_32 of
                                                               TagText v_34 -> ((($)
                                                                                   (((((tagTextMerge_f
                                                                                          v_2)
                                                                                         f_7)
                                                                                        v_8)
                                                                                       v_9)
                                                                                      v_10))
                                                                                  (((:) v_32)
                                                                                     (v_28 v_33)))
                                                               _ -> f_31)
                                         _ -> f_31))
                           in
                           (case v_29 of
                                (:) v_36 v_37 -> (case v_36 of
                                                      TagPosition v_38 v_39 -> (case v_37 of
                                                                                    (:) v_40
                                                                                      v_41 -> (case
                                                                                                 v_40
                                                                                                 of
                                                                                                   TagText
                                                                                                     v_42 -> ((($)
                                                                                                                 (((((tagTextMerge_f
                                                                                                                        v_2)
                                                                                                                       f_7)
                                                                                                                      v_8)
                                                                                                                     v_9)
                                                                                                                    v_10))
                                                                                                                (((:)
                                                                                                                    v_36)
                                                                                                                   (((:)
                                                                                                                       v_40)
                                                                                                                      (v_28
                                                                                                                         v_41))))
                                                                                                   _ -> f_35)
                                                                                    _ -> f_35)
                                                      _ -> f_35)
                                _ -> f_35))
                  in
                  (case v_29 of
                       (:) v_44 v_45 -> (case v_44 of
                                             TagWarning v_46 -> ((((((((tagTextMerge_g v_2) f_7)
                                                                        v_8)
                                                                       v_9)
                                                                      v_10)
                                                                     v_27)
                                                                    (((.) v_28) ((:) v_44)))
                                                                   v_45)
                                             _ -> f_43)
                       _ -> f_43))
         in
         (case v_29 of
              (:) v_48 v_49 -> (case v_48 of
                                    TagPosition v_50 v_51 -> (case v_49 of
                                                                  (:) v_52 v_53 -> (case v_52 of
                                                                                        TagWarning
                                                                                          v_54 -> ((((((((tagTextMerge_g
                                                                                                            v_2)
                                                                                                           f_7)
                                                                                                          v_8)
                                                                                                         v_9)
                                                                                                        v_10)
                                                                                                       v_27)
                                                                                                      (((.)
                                                                                                          v_28)
                                                                                                         (((.)
                                                                                                             ((:)
                                                                                                                v_48))
                                                                                                            ((:)
                                                                                                               v_52))))
                                                                                                     v_53)
                                                                                        _ -> f_47)
                                                                  _ -> f_47)
                                    _ -> f_47)
              _ -> f_47))
