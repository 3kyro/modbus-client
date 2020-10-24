module Dropdown exposing (..)

import Element
    exposing
        ( Attribute
        , Decoration
        , Element
        , below
        , column
        , el
        , mouseOver
        , none
        , pointer
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
    let
        whatToRender =
            if drop.expanded then
                renderExpandedDropdown drop

            else
                renderOption drop drop.selected
    in
    el
        []
        whatToRender


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
    Dropdown value msg
    -> Element msg
renderExpandedDropdown drop =
    el
        [ below <|
            column
                []
            <|
                renderRemainingOptions drop
        ]
    <|
        renderOption drop drop.selected


renderOption : Dropdown value msg -> Option value msg -> Element msg
renderOption drop value =
    el
        [ Border.width 1
        , Background.color dClrNotExpanded
        , mouseOver [ setHoverColor drop.selected value ]
        , Events.onClick (drop.onClick value)
        , pointer
        ]
        value.element


renderRemainingOptions : Dropdown value msg -> List (Element msg)
renderRemainingOptions drop =
    List.map
        (\opt ->
            if opt == drop.selected then
                none

            else
                renderOption drop opt
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
