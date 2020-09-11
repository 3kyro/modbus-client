module Palette exposing
    ( darkGrey
    , grey
    , greyWhite
    , purpleDark
    , steelBlue
    , blueSapphire
    , white
    , maximumBluePurple
    , darkBluePurple
    , smallFont
    , maximumBluePurpleLight
    , lightGreen
    , lightGrey
    , black
    , gray14
    , scampi
    , slateGrey
    , baliHai
    , shuttleGrey
    , cloudBurst
    , fireBrick
    )

import Element exposing (Color, Attribute, rgb255)
import Element.Font as Font
import Types exposing (Msg)

darkGrey : Color
darkGrey = rgb255 50 50 50

darkBluePurple : Color
darkBluePurple = rgb255 114 112 143

grey : Color
grey = rgb255 90 90 90

lightGrey : Color
lightGrey = rgb255 122 122 122

greyWhite : Color
greyWhite = rgb255 220 220 220

purpleDark : Color
purpleDark = rgb255 145 145 233

steelBlue : Color
steelBlue = rgb255 69 126 172

blueSapphire : Color
blueSapphire = rgb255 45 93 123

white : Color
white = rgb255 245 245 245

maximumBluePurple : Color
maximumBluePurple = rgb255 145 145 233

maximumBluePurpleLight : Color
maximumBluePurpleLight = rgb255 194 175 240

lightGreen : Color
lightGreen = rgb255 82 172 162

smallFont : Attribute Msg
smallFont = Font.size 14

black : Color
black = rgb255 0 0 0

gray14 : Color
gray14 = rgb255 36 36 36

scampi : Color
scampi = rgb255 113 109 142

slateGrey : Color
slateGrey = rgb255 109 122 142

baliHai : Color
baliHai = rgb255 138 149 165

shuttleGrey : Color
shuttleGrey = rgb255 88 95 106

cloudBurst : Color
cloudBurst = rgb255 56 62 73

fireBrick : Color
fireBrick = rgb255 179 45 17
