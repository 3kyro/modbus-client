module Dropdown exposing (..)

import Element
    exposing
        ( Attribute
        , Decoration
        , Element
        , below
        , column
        , el
        , fill
        , height
        , mouseOver
        , none
        , padding
        , pointer
        , px
        , spacing
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input exposing (Label)
import Html.Attributes exposing (selected)
import Palette
    exposing
        ( dClrHover
        , dClrNotExpanded
        )


type alias Option value msg =
    { value : value
    , element : Element msg
    }


type alias Dropdown value msg =
    { onClick : Option value msg -> msg
    , options : List (Option value msg)
    , selected : Option value msg
    , expanded : Bool
    , label : String
    }


dropdown :
    List (Attribute msg)
    -> Dropdown value msg
    -> Element msg
dropdown attributes drop =
    if drop.expanded then
        renderExpandedDropdown attributes drop

    else
        renderOption attributes drop drop.selected


expand : Dropdown value msg -> Dropdown value msg
expand dd =
    { dd | expanded = True }


retract : Dropdown value msg -> Dropdown value msg
retract dd =
    { dd | expanded = False }


getOption : value -> Element msg -> Option value msg
getOption value element =
    Option value element


renderExpandedDropdown :
    List (Attribute msg)
    -> Dropdown value msg
    -> Element msg
renderExpandedDropdown attributes drop =
    el
        [ below <|
            column
                (attributes ++ [ padding 0, height <| px 38, width fill, spacing 10 ])
            <|
                renderRemainingOptions attributes drop
        , width fill
        , spacing 10
        ]
    <|
        renderOption attributes drop drop.selected


renderOption :
    List (Attribute msg)
    -> Dropdown value msg
    -> Option value msg
    -> Element msg
renderOption attributes drop value =
    el
        ([ Background.color dClrNotExpanded
         , Border.width 1
         , mouseOver [ setHoverColor drop.selected value ]
         , Events.onClick (drop.onClick value)
         , pointer
         ]
            ++ attributes
        )
        value.element


renderRemainingOptions : List (Attribute msg) -> Dropdown value msg -> List (Element msg)
renderRemainingOptions attributes drop =
    List.map
        (\opt ->
            if opt == drop.selected then
                none

            else
                renderOption attributes drop opt
        )
        drop.options


setHoverColor : Option value msg -> Option value msg -> Decoration
setHoverColor selectedValue value =
    if value == selectedValue then
        Background.color dClrNotExpanded

    else
        Background.color dClrHover


setDropdownExpanded : Dropdown value msg -> Bool -> Dropdown value msg
setDropdownExpanded drop flag =
    { drop | expanded = flag }


setDropdown : Dropdown value msg -> Option value msg -> Dropdown value msg
setDropdown drop value =
    { drop
        | selected = value
        , expanded = not drop.expanded
    }
