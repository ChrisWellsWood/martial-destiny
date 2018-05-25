module Main exposing (..)

import Css exposing (..)
import Dict
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
    , popUp : PopUp
    }


emptyModel : Model
emptyModel =
    Model
        defaultCombatant
        Dict.empty
        1
        1
        Closed


type alias Combatant =
    { name : String
    , initiative : Int
    , crash : Maybe Crash
    , onslaught : Int
    }


type alias Crash =
    { crasher : String
    , turnsUntilReset : Int
    }


defaultCombatant : Combatant
defaultCombatant =
    Combatant "" 1 Nothing 0


type alias Combatants =
    Dict.Dict String Combatant



-- Remove maybe for attacker


type PopUp
    = EditInitiative Combatant String
    | WitheringAttack (Maybe Combatant) (Maybe Combatant) (Maybe String) (Maybe Shift)
    | Closed


type Shift
    = Shifted String
    | NoShift



-- Update


type Msg
    = UpdateNewName String
    | UpdateNewInit String
    | AddCombatant
    | OpenPopUp PopUp
    | ClosePopUp
    | ModifyNewInitiative Int
    | SetNewInitiative String
    | ApplyNewInitiative
    | SetWitheringTarget Combatant
    | SetWitheringDamage String
    | ResolveWitheringDamage
    | SetShiftJoinCombat String
    | ResolveInitiativeShift


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
                        -- Add 3 to this
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
                    Dict.insert
                        model.newCombatant.name
                        model.newCombatant
                        model.combatants
            }
                ! []

        OpenPopUp popUp ->
            { model | popUp = popUp } ! []

        ClosePopUp ->
            { model | popUp = Closed } ! []

        ModifyNewInitiative modifyBy ->
            case model.popUp of
                EditInitiative combatant initiativeString ->
                    { model
                        | popUp =
                            EditInitiative
                                combatant
                                (String.toInt initiativeString
                                    |> Result.withDefault 0
                                    |> (+) modifyBy
                                    |> toString
                                )
                    }
                        ! []

                _ ->
                    { model | popUp = Closed } ! []

        SetNewInitiative initiativeString ->
            case model.popUp of
                EditInitiative combatant _ ->
                    { model
                        | popUp =
                            EditInitiative
                                combatant
                                initiativeString
                    }
                        ! []

                _ ->
                    { model | popUp = Closed } ! []

        ApplyNewInitiative ->
            case model.popUp of
                EditInitiative combatant newInitiativeStr ->
                    case String.toInt newInitiativeStr of
                        Ok newInitiative ->
                            { model
                                | popUp = Closed
                                , combatants =
                                    Dict.insert
                                        combatant.name
                                        { combatant
                                            | initiative = newInitiative
                                        }
                                        model.combatants
                            }
                                ! []

                        Err _ ->
                            { model | popUp = Closed } ! []

                _ ->
                    { model | popUp = Closed } ! []

        SetWitheringTarget defender ->
            case model.popUp of
                WitheringAttack attacker _ _ _ ->
                    { model
                        | popUp =
                            WitheringAttack
                                attacker
                                (Just defender)
                                (Just "0")
                                Nothing
                    }
                        ! []

                _ ->
                    { model | popUp = Closed } ! []

        SetWitheringDamage damage ->
            case model.popUp of
                WitheringAttack (Just attacker) (Just defender) (Just _) _ ->
                    { model
                        | popUp =
                            WitheringAttack
                                (Just attacker)
                                (Just defender)
                                (Just damage)
                                Nothing
                    }
                        ! []

                _ ->
                    { model | popUp = Closed } ! []

        ResolveWitheringDamage ->
            case model.popUp of
                WitheringAttack (Just attacker) (Just defender) (Just damage) Nothing ->
                    let
                        ( uAttacker, uDefender, shift ) =
                            resolveWithering attacker defender damage

                        updatedCombatants =
                            Dict.insert attacker.name uAttacker model.combatants
                                |> Dict.insert defender.name uDefender
                    in
                        case shift of
                            Shifted _ ->
                                { model
                                    | popUp =
                                        WitheringAttack
                                            (Just uAttacker)
                                            (Just uDefender)
                                            (Just damage)
                                            (Just shift)
                                }
                                    ! []

                            NoShift ->
                                { model
                                    | popUp = Closed
                                    , combatants = updatedCombatants
                                }
                                    ! []

                _ ->
                    { model | popUp = Closed } ! []

        SetShiftJoinCombat shiftJoinCombat ->
            case model.popUp of
                WitheringAttack (Just a) (Just d) (Just dam) (Just (Shifted _)) ->
                    { model
                        | popUp =
                            WitheringAttack
                                (Just a)
                                (Just d)
                                (Just dam)
                                (Just (Shifted shiftJoinCombat))
                    }
                        ! []

                _ ->
                    { model | popUp = Closed } ! []

        ResolveInitiativeShift ->
            case model.popUp of
                WitheringAttack (Just a) (Just d) (Just dam) (Just (Shifted jc)) ->
                    let
                        joinCombat =
                            String.toInt jc
                                |> Result.withDefault 0

                        shiftInitiative =
                            3 + joinCombat

                        attacker =
                            { a
                                | initiative =
                                    if shiftInitiative > a.initiative then
                                        shiftInitiative
                                    else
                                        a.initiative
                            }

                        updatedCombatants =
                            Dict.insert attacker.name attacker model.combatants
                    in
                        { model
                            | popUp = Closed
                            , combatants = updatedCombatants
                        }
                            ! []

                _ ->
                    { model | popUp = Closed } ! []


resolveWithering :
    Combatant
    -> Combatant
    -> String
    -> ( Combatant, Combatant, Shift )
resolveWithering attacker defender damageStr =
    let
        damage =
            String.toInt damageStr
                |> Result.withDefault 0

        defInitiative =
            defender.initiative - damage

        hasCrashed =
            if (defender.initiative > 0) && (defInitiative <= 0) then
                True
            else
                False

        updatedDefender =
            { defender
                | initiative = defInitiative
                , crash =
                    if hasCrashed then
                        Just (Crash attacker.name 3)
                    else
                        Nothing
                , onslaught = defender.onslaught + 1
            }

        attInitiative =
            attacker.initiative
                + damage
                + 1
                + (if hasCrashed then
                    5
                   else
                    0
                  )

        shift =
            case attacker.crash of
                Just crash ->
                    if hasCrashed && (crash.crasher == defender.name) then
                        Shifted "0"
                    else
                        NoShift

                Nothing ->
                    NoShift

        updatedAttacker =
            { attacker
                | initiative = attInitiative
                , crash =
                    if attInitiative > 0 then
                        Nothing
                    else
                        attacker.crash
            }
    in
        ( updatedAttacker, updatedDefender, shift )



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
    div [ css [ defaultStyle ] ]
        ([ h1 [] [ text "Threads of Martial Destiny" ]
         , h3 [] [ text "A combat manager for Exalted 3rd" ]
         , managePanel model.newCombatant
         , tracker model.combatants
         ]
            ++ case model.popUp of
                (EditInitiative _ _) as editInitiative ->
                    [ editPopUp editInitiative ]

                (WitheringAttack _ _ _ _) as witheringAttack ->
                    [ witheringPopUp
                        model.combatants
                        witheringAttack
                    ]

                Closed ->
                    []
        )


defaultStyle : Style
defaultStyle =
    Css.batch
        [ fontFamilies [ "Tahoma", "Geneva", "sans-serif" ]
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
        (Dict.toList combatants
            |> List.map combatantCard
        )


trackerStyling : Style
trackerStyling =
    Css.batch
        [ padding (px 5)
        , displayFlex
        , flexWrap Css.wrap
        ]


combatantCard : ( String, Combatant ) -> Html Msg
combatantCard ( name, combatant ) =
    let
        { name, initiative } =
            combatant
    in
        div [ css [ combatantCardStyle ] ]
            [ div [] [ text name ]
            , div
                [ css [ initiativeFont ] ]
                [ (toString initiative)
                    ++ "i"
                    |> text
                ]
            , text ("Onslaught: " ++ (toString combatant.onslaught))
            , br [] []
            , button
                [ onClick <| OpenPopUp <| EditInitiative combatant "0" ]
                [ text "Edit" ]
            , button
                [ onClick <|
                    OpenPopUp <|
                        WitheringAttack (Just combatant) Nothing Nothing Nothing
                ]
                [ text "Withering" ]
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


editPopUp : PopUp -> Html Msg
editPopUp editInitiative =
    let
        modifyInitiativeBtn modifyBy =
            button
                [ onClick <|
                    ModifyNewInitiative modifyBy
                ]
                [ text <| toString modifyBy ]
    in
        div []
            [ disablingDiv
            , div [ css [ popUpStyle ] ]
                ((case editInitiative of
                    EditInitiative combatant newInitiative ->
                        [ modifyInitiativeBtn -5
                        , modifyInitiativeBtn -1
                        , input
                            [ onInput SetNewInitiative
                            , value newInitiative
                            ]
                            []
                        , modifyInitiativeBtn 1
                        , modifyInitiativeBtn 5
                        , button [ onClick <| ApplyNewInitiative ] [ text "Ok" ]
                        ]

                    _ ->
                        []
                 )
                    ++ [ button [ onClick ClosePopUp ] [ text "Cancel" ]
                       ]
                )
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
        ]


witheringPopUp : Combatants -> PopUp -> Html Msg
witheringPopUp combatants popUp =
    let
        selectTarget combatant =
            div [ onClick <| SetWitheringTarget combatant ]
                [ text combatant.name ]
    in
        div []
            [ disablingDiv
            , div [ css [ popUpStyle ] ]
                ((case popUp of
                    WitheringAttack (Just attacker) Nothing Nothing _ ->
                        [ b [] [ text "Select Target" ]
                        ]
                            ++ (Dict.toList combatants
                                    |> List.filter (\( n, c ) -> n /= attacker.name)
                                    |> List.map Tuple.second
                                    |> List.map selectTarget
                               )

                    WitheringAttack (Just attacker) (Just defender) (Just damageStr) Nothing ->
                        let
                            resolveDisabled =
                                case String.toInt damageStr of
                                    Ok damage ->
                                        False

                                    Err _ ->
                                        True
                        in
                            [ b [] [ text "Set Post-Soak Damage" ]
                            , br [] []
                            , attacker.name
                                ++ " vs "
                                ++ defender.name
                                |> text
                            , br [] []
                            , input
                                [ onInput SetWitheringDamage
                                , value <| damageStr
                                , size 3
                                ]
                                []
                            , br [] []
                            , button
                                [ onClick ResolveWitheringDamage
                                , Html.Styled.Attributes.disabled
                                    resolveDisabled
                                ]
                                [ text "Resolve" ]
                            ]

                    WitheringAttack _ _ _ (Just (Shifted joinCombatStr)) ->
                        let
                            resolveDisabled =
                                case String.toInt joinCombatStr of
                                    Ok joinCombat ->
                                        False

                                    Err _ ->
                                        True
                        in
                            [ b [] [ text "Initiative Shift!" ]
                            , br [] []
                            , text "Join Combat Result"
                            , br [] []
                            , input
                                [ onInput SetShiftJoinCombat ]
                                []
                            , br [] []
                            , button
                                [ onClick ResolveInitiativeShift
                                , Html.Styled.Attributes.disabled
                                    resolveDisabled
                                ]
                                [ text "Resolve" ]
                            ]

                    _ ->
                        []
                 )
                    ++ [ button [ onClick ClosePopUp ] [ text "Cancel" ] ]
                )
            ]
