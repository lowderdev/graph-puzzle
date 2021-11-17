module Grid
  ( Grid,
    blankGrid,
    printGrid,
    rotateCell,
    randomGrid,
    -- getCell,
    -- detectWin,
    -- detectTie,
  )
where

import           Cell

data Grid a = Grid
  { one   :: a,
    two   :: a,
    three :: a,
    four  :: a
    -- five :: a,
    -- six :: a,
    -- seven :: a,
    -- eight :: a,
    -- nine :: a
  }
  deriving (Show)

blankGrid :: Grid Cell
blankGrid = Grid Empty Empty Empty Empty -- Empty Empty Empty Empty Empty


randomGrid :: Grid Cell
randomGrid = Grid NE WN E W

instance Functor Grid where
  fmap f grid =
    Grid
      (f $ one grid)
      (f $ two grid)
      (f $ three grid)
      (f $ four grid)
      -- (f $ five grid)
      -- (f $ six grid)
      -- (f $ seven grid)
      -- (f $ eight grid)
      -- (f $ nine grid)

toRows :: Grid a -> [(a, a)]
toRows (Grid a b c d) = [row1, row2, col1, col2, adDiagonal, bcDiagonal]
  where
    row1 = (a, b)
    row2 = (c, d)
    col1 = (a, c)
    col2 = (b, d)
    adDiagonal = (a, d)
    bcDiagonal = (b, c)

printGrid :: Grid Cell -> IO ()
printGrid grid =
  putStrLn ""
    >> putStrLn (concat [a, b])
    >> putStrLn (concat [c, d])
    >> putStrLn ""
  where
    Grid a b c d = printCell <$> grid

-- getCell :: String -> Grid Cell -> Cell
-- getCell cellNum (Grid a b c d) =
--   case cellNum of
--     "1" -> a
--     "2" -> b
--     "3" -> c
--     "4" -> d

rotateCell :: String -> Grid Cell -> Grid Cell
rotateCell cellNum (Grid a b c d) =
  case cellNum of
    "1" -> Grid (rotate a) b c d
    "2" -> Grid a (rotate b) c d
    "3" -> Grid a b (rotate c) d
    "4" -> Grid a b c (rotate d)

-- detectWin :: Grid Cell -> Bool
-- detectWin = any checkThreeInARow . toRows

-- detectTie :: Grid Cell -> Bool
-- detectTie (Grid a b c d e f g h i) =
--   Empty `notElem` [a, b, c, d, e, f, g, h, i]

-- -- private

-- checkThreeInARow :: (Cell, Cell, Cell) -> Bool
-- checkThreeInARow (a, b, c) = a /= Empty && a == b && a == c
