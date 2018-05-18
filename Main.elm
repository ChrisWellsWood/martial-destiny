module Main exposing (..)

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    emptyModel ! []


type alias Model =
    { newCombatant : Combatant
    , combatants : Combatants
    , tick : Int
    , turn : Int
    }


emptyModel : Model
emptyModel =
    Model
        (Combatant "" 1)
        []
        1
        1


type alias Combatant =
    { name : String
    , initiative : Int
    }


type alias Combatants =
    List Combatant



-- Update


type Msg
    = UpdateNewName String
    | UpdateNewInit String
    | AddCombatant
    | UpdateCombatant Int CombatantMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateNewName name ->
            let
                newCombatant =
                    model.newCombatant

                updatedCombatant =
                    { newCombatant | name = name }
            in
                { model | newCombatant = updatedCombatant } ! []

        UpdateNewInit initiative ->
            let
                newCombatant =
                    model.newCombatant

                updatedCombatant =
                    case String.toInt initiative of
                        Ok baseInit ->
                            { newCombatant | initiative = baseInit }

                        Err _ ->
                            newCombatant
            in
                { model | newCombatant = updatedCombatant } ! []

        AddCombatant ->
            { model
                | newCombatant = Combatant "" 0
                , combatants =
                    model.newCombatant
                        :: model.combatants
            }
                ! []

        UpdateCombatant index combatantMsg ->
            let
                ( updatedCombatants, cmds ) =
                    updateOneOf (updateCombatant combatantMsg) index model.combatants
            in
                { model | combatants = updatedCombatants }
                    ! [ Cmd.map (UpdateCombatant index) cmds ]


updateOneOf : (a -> ( a, Cmd msg )) -> Int -> List a -> ( List a, Cmd msg )
updateOneOf fn refIdx list =
    let
        ( updatedList, cmds ) =
            List.indexedMap (,) list
                |> List.map
                    (\( idx, item ) ->
                        if refIdx == idx then
                            fn item
                        else
                            ( item, Cmd.none )
                    )
                |> List.unzip
    in
        updatedList ! cmds


type CombatantMsg
    = ModifyInitiative Int


updateCombatant : CombatantMsg -> Combatant -> ( Combatant, Cmd CombatantMsg )
updateCombatant msg combatant =
    case msg of
        ModifyInitiative modifyBy ->
            { combatant | initiative = combatant.initiative + modifyBy } ! []



-- Subscriptions


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none



-- Styles


type alias Colour =
    Color


type alias ColourPallette =
    { c1 : Colour
    , c2 : Colour
    , c3 : Colour
    , c4 : Colour
    , c5 : Colour
    }



-- https://coolors.co/413c58-a3c4bc-bfd7b5-e7efc5-f2dda4


colourPallette : ColourPallette
colourPallette =
    { c1 = hex "413c58"
    , c2 = hex "a3c4bc"
    , c3 = hex "bfd7b5"
    , c4 = hex "e7efc5"
    , c5 = hex "f2dda4"
    }



-- Views


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Martial Destiny" ]
        , h3 [] [ text "A combat manager for Exalted 3rd" ]
        , managePanel model.newCombatant
        , tracker model.combatants
        ]


managePanel : Combatant -> Html Msg
managePanel { name, initiative } =
    div []
        [ input [ onInput UpdateNewName, value name ] []
        , input [ onInput UpdateNewInit, value <| toString initiative ] []
        , button [ onClick AddCombatant ] [ text "Add Combatant" ]
        ]


tracker : Combatants -> Html Msg
tracker combatants =
    div [ css [ trackerStyling ] ]
        (List.indexedMap
            combatantCard
            combatants
        )


trackerStyling : Style
trackerStyling =
    Css.batch
        [ padding (px 5)
        , displayFlex
        , flexWrap Css.wrap
        ]


combatantCard : Int -> Combatant -> Html Msg
combatantCard index { name, initiative } =
    let
        indexModBtn =
            modifyInitiativeBtn index
    in
        div [ css [ combatantCardStyle ] ]
            [ div
                [ css [ initiativeFont ] ]
                [ (toString initiative)
                    ++ "i"
                    |> text
                ]
            , indexModBtn -5
            , indexModBtn -1
            , indexModBtn 1
            , indexModBtn 5
            , div [] [ text name ]
            ]


combatantCardStyle : Style
combatantCardStyle =
    Css.batch
        [ padding (px 5)
        , margin (px 5)
        , backgroundColor colourPallette.c3
        , Css.width (px 150)
        , Css.height (px 150)
        , overflow Css.hidden
        , overflowWrap normal
        ]


initiativeFont : Style
initiativeFont =
    Css.batch
        [ fontSize (px 30)
        , fontWeight bold
        ]


modifyInitiativeBtn : Int -> Int -> Html Msg
modifyInitiativeBtn index modifyBy =
    button
        [ onClick <|
            UpdateCombatant index <|
                ModifyInitiative modifyBy
        ]
        [ text <| toString modifyBy ]
