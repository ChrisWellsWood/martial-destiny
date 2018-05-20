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
    , turn : Int
    , newInitiative : Int
    , popUp : Maybe PopUp
    }


emptyModel : Model
emptyModel =
    Model
        defaultCombatant
        []
        1
        1
        Nothing


type alias Combatant =
    { name : String
    , initiative : Int
    }


defaultCombatant : Combatant
defaultCombatant =
    Combatant "" 1


type alias Combatants =
    List Combatant


type PopUp
    = EditInitiative Int



-- Update


type Msg
    = UpdateNewName String
    | UpdateNewInit String
    | AddCombatant
    | UpdateCombatant Int CombatantMsg
    | OpenPopUp PopUp
    | ClosePopUp
    | ModifyNewInitiative Int
    | SetNewInitiative String
    | ApplyNewInitiative Int


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
                | newCombatant = defaultCombatant
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

        OpenPopUp popUp ->
            { model | popUp = Just popUp } ! []

        ClosePopUp ->
            { model | popUp = Nothing } ! []

        ModifyNewInitiative initiative ->
            { model | newInitiative = model.newInitiative + initiative } ! []

        SetNewInitiative initiativeString ->
            case String.toInt initiativeString of
                Ok initiative ->
                    { model | newInitiative = initiative } ! []

                Err _ ->
                    model ! []

        ApplyNewInitiative index ->
            let
                ( updatedModel, cmds ) =
                    update
                        (SetInitiative model.newInitiative
                            |> UpdateCombatant index
                        )
                        model
            in
                { updatedModel | popUp = Nothing } ! [ cmds ]


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
    = SetInitiative Int


updateCombatant : CombatantMsg -> Combatant -> ( Combatant, Cmd CombatantMsg )
updateCombatant msg combatant =
    case msg of
        SetInitiative val ->
            { combatant | initiative = val } ! []



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
        ([ h1 [] [ text "Martial Destiny" ]
         , h3 [] [ text "A combat manager for Exalted 3rd" ]
         , managePanel model.newCombatant
         , tracker model.combatants
         ]
            ++ case model.popUp of
                Just (EditInitiative index) ->
                    [ editPopUp index model.newInitiative ]

                Nothing ->
                    []
        )


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
        (List.sortBy .initiative combatants
            |> List.reverse
            |> List.indexedMap
                combatantCard
        )


trackerStyling : Style
trackerStyling =
    Css.batch
        [ padding (px 5)
        , displayFlex
        , flexWrap Css.wrap
        ]


combatantCard : Int -> Combatant -> Html Msg
combatantCard index combatant =
    let
        { name, initiative } =
            combatant
    in
        div [ css [ combatantCardStyle ] ]
            [ div
                [ css [ initiativeFont ] ]
                [ (toString initiative)
                    ++ "i"
                    |> text
                ]
            , button
                [ onClick <| OpenPopUp <| EditInitiative index ]
                [ text "Edit" ]
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


editPopUp : Int -> Int -> Html Msg
editPopUp index initiative =
    let
        modifySetIndex =
            modifyInitiativeBtn index
    in
        div []
            [ disablingDiv
            , div [ css [ popUpStyle ] ]
                [ modifySetIndex -5
                , modifySetIndex -1
                , input
                    [ onInput SetNewInitiative
                    , value <| toString initiative
                    ]
                    [ text <| toString initiative ]
                , modifySetIndex 1
                , modifySetIndex 5
                , button [ onClick <| ApplyNewInitiative index ] [ text "Ok" ]
                , button [ onClick ClosePopUp ] [ text "Cancel" ]
                ]
            ]


disablingDiv : Html msg
disablingDiv =
    div [ css [ disablingStyle ] ] []


disablingStyle : Style
disablingStyle =
    Css.batch
        [ zIndex (int 1000)
        , position absolute
        , top (pct 0)
        , left (pct 0)
        , Css.width (pct 100)
        , Css.height (pct 100)
        , backgroundColor <| hex "dddddd"
        , opacity (num 0.5)
        ]


popUpStyle : Style
popUpStyle =
    Css.batch
        [ zIndex (int 1001)
        , backgroundColor colourPallette.c3
        , padding (px 5)
        , position absolute
        , top (pct 50)
        , left (pct 50)
        , Css.width (px 200)
        , Css.height (px 200)
        ]


modifyInitiativeBtn : Int -> Int -> Html Msg
modifyInitiativeBtn index modifyBy =
    button
        [ onClick <|
            ModifyNewInitiative modifyBy
        ]
        [ text <| toString modifyBy ]
