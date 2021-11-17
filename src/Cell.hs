module Cell where

-- ╹ ╺ ╻ ╸
-- ┃ ━
-- ┗ ┏ ┓ ┛
-- ┣ ┳ ┫ ┻
-- ╋
data Cell = N | E | S | W | NS | EW | NE | ES | SW | WN | NES | ESW | SWN | WNE | NESW | Empty
  deriving (Eq, Show)

printCell :: Cell -> String
printCell N     = "╹"
printCell E     = "╺"
printCell S     = "╻"
printCell W     = "╸"
printCell NS    = "┃"
printCell EW    = "━"
printCell NE    = "┗"
printCell ES    = "┏"
printCell SW    = "┓"
printCell WN    = "┛"
printCell NES   = "┣"
printCell ESW   = "┳"
printCell SWN   = "┫"
printCell WNE   = "┻"
printCell NESW  = "╋"
printCell Empty = " "

rotate :: Cell -> Cell
-- ╻
rotate N     = E
rotate E     = S
rotate S     = W
rotate W     = N
-- ┃
rotate NS    = EW
rotate EW    = NS
-- ┗
rotate NE    = ES
rotate ES    = SW
rotate SW    = WN
rotate WN    = NE
-- ┣
rotate NES   = ESW
rotate ESW   = SWN
rotate SWN   = WNE
rotate WNE   = NES
-- ╋
rotate NESW  = NESW
rotate Empty = Empty
