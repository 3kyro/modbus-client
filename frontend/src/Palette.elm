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
    , maximumBluePurpleLight, lightGreen, lightGrey, black, gray14
    )

import Element exposing (Color, Attribute, rgb255)
import Element.Font as Font
import Types exposing (Msg)


-- /* CSV */
-- 686868,c2aff0,9191e9,457eac,2d5d7b

-- /* Array */
-- ["686868","c2aff0","9191e9","457eac","2d5d7b"]

-- /* Object */
-- {"Dim Gray":"686868","Maximum Blue Purple":"c2aff0","Maximum Blue Purple 2":"9191e9","Steel Blue":"457eac","Blue Sapphire":"2d5d7b"}

-- /* Extended Array */
-- [{"name":"Dim Gray","hex":"686868","rgb":[104,104,104],"cmyk":[0,0,0,59],"hsb":[0,0,41],"hsl":[0,0,41],"lab":[44,0,0]}
-- ,{"name":"Maximum Blue Purple","hex":"c2aff0","rgb":[194,175,240],"cmyk":[19,27,0,6],"hsb":[258,27,94],"hsl":[258,68,81],"lab":[75,20,-30]}
-- ,{"name":"Maximum Blue Purple","hex":"9191e9","rgb":[145,145,233],"cmyk":[38,38,0,9],"hsb":[240,38,91],"hsl":[240,67,74],"lab":[63,21,-44]}
-- ,{"name":"Steel Blue","hex":"457eac","rgb":[69,126,172],"cmyk":[60,27,0,33],"hsb":[207,60,67],"hsl":[207,43,47],"lab":[51,-5,-30]}
-- ,{"name":"Blue Sapphire","hex":"2d5d7b","rgb":[45,93,123],"cmyk":[63,24,0,52],"hsb":[203,63,48],"hsl":[203,46,33],"lab":[38,-6,-22]}]

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

