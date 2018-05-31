module Main exposing (..)

import Css exposing (..)
import Dict
import Dom
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Task


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
    { combatants : Combatants
    , round : Int
    , popUp : PopUp
    }


emptyModel : Model
emptyModel =
    Model
        Dict.empty
        1
        Closed


type alias Combatant =
    { name : String
    , initiative : Int
    , crash : Maybe Crash
    , onslaught : Int
    , turnFinished : Bool
    }


type alias Crash =
    { crasher : String
    , turnsUntilReset : Int
    }


type alias Combatants =
    Dict.Dict String Combatant


type PopUp
    = NewCombatant String String
    | EditInitiative Combatant String
    | WitheringAttack Combatant (Maybe Combatant) (Maybe String) (Maybe Shift)
    | DecisiveAttack Combatant
    | Confirm String Msg
    | Closed


type Shift
    = Shifted String
    | NoShift


type AttackOutcome
    = Hit
    | Miss



-- Update


type Msg
    = OpenPopUp PopUp
    | FocusResult (Result Dom.Error ())
    | ClosePopUp
    | SetCombatantName String
    | SetJoinCombat String
    | AddNewCombatant
    | StartNewRound
    | ModifyInitiative Int
    | SetInitiative String
    | ApplyNewInitiative
    | SetWitheringTarget Combatant
    | SetWitheringDamage String
    | ResolveWitheringDamage
    | SetShiftJoinCombat String
    | ResolveInitiativeShift
    | ResolveDecisive AttackOutcome
    | ResetOnslaught Combatant
    | EndTurn Combatant
    | ResolveDelete Combatant


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenPopUp popUp ->
            { model | popUp = popUp }
                ! [ Dom.focus "pop-up-focus"
                        |> Task.attempt FocusResult
                  ]

        FocusResult result ->
            case result of
                Err (Dom.NotFound id) ->
                    let
                        error =
                            "ID \"" ++ id ++ "\"not found."
                    in
                        model ! []

                Ok () ->
                    model ! []

        ClosePopUp ->
            { model | popUp = Closed } ! []

        SetCombatantName name ->
            case model.popUp of
                NewCombatant _ joinCombat ->
                    { model
                        | popUp =
                            NewCombatant
                                name
                                joinCombat
                    }
                        ! []

                _ ->
                    { model | popUp = Closed } ! []

        SetJoinCombat joinCombat ->
            case model.popUp of
                NewCombatant name _ ->
                    { model
                        | popUp =
                            NewCombatant
                                name
                                joinCombat
                    }
                        ! []

                _ ->
                    { model | popUp = Closed } ! []

        AddNewCombatant ->
            case model.popUp of
                NewCombatant name joinCombatStr ->
                    let
                        joinCombat =
                            String.toInt joinCombatStr
                                |> Result.withDefault 0
                                |> (+) 3

                        newCombatant =
                            Combatant
                                name
                                joinCombat
                                Nothing
                                0
                                False

                        updatedCombatants =
                            Dict.insert name newCombatant model.combatants
                    in
                        { model
                            | popUp = Closed
                            , combatants = updatedCombatants
                        }
                            ! []

                _ ->
                    { model | popUp = Closed } ! []

        StartNewRound ->
            let
                updatedCombatants =
                    Dict.values model.combatants
                        |> List.map decrementCrash
                        |> List.map (\c -> { c | turnFinished = False })
                        |> List.map (\c -> ( c.name, c ))
                        |> Dict.fromList
            in
                { model
                    | combatants = updatedCombatants
                    , round = model.round + 1
                    , popUp = Closed
                }
                    ! []

        ModifyInitiative modifyBy ->
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

        SetInitiative initiativeString ->
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
                WitheringAttack attacker (Just defender) (Just _) _ ->
                    { model
                        | popUp =
                            WitheringAttack
                                attacker
                                (Just defender)
                                (Just damage)
                                Nothing
                    }
                        ! []

                _ ->
                    { model | popUp = Closed } ! []

        ResolveWitheringDamage ->
            case model.popUp of
                WitheringAttack attacker (Just defender) (Just damage) Nothing ->
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
                                            uAttacker
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
                WitheringAttack a (Just d) (Just dam) (Just (Shifted _)) ->
                    { model
                        | popUp =
                            WitheringAttack
                                a
                                (Just d)
                                (Just dam)
                                (Just (Shifted shiftJoinCombat))
                    }
                        ! []

                _ ->
                    { model | popUp = Closed } ! []

        ResolveInitiativeShift ->
            case model.popUp of
                WitheringAttack att (Just def) (Just dam) (Just (Shifted jc)) ->
                    let
                        joinCombat =
                            String.toInt jc
                                |> Result.withDefault 0

                        shiftInitiative =
                            3 + joinCombat

                        attacker =
                            { att
                                | initiative =
                                    if shiftInitiative > att.initiative then
                                        shiftInitiative
                                    else
                                        att.initiative
                            }

                        updatedCombatants =
                            Dict.insert attacker.name attacker model.combatants
                                |> Dict.insert def.name def
                    in
                        { model
                            | popUp = Closed
                            , combatants = updatedCombatants
                        }
                            ! []

                _ ->
                    { model | popUp = Closed } ! []

        ResolveDecisive decisiveOutcome ->
            case model.popUp of
                DecisiveAttack combatant ->
                    let
                        attacker =
                            resolveDecisive decisiveOutcome combatant

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

        ResetOnslaught combatant ->
            let
                updatedCombatant =
                    { combatant | onslaught = 0 }

                updatedCombatants =
                    Dict.insert
                        updatedCombatant.name
                        updatedCombatant
                        model.combatants
            in
                { model
                    | combatants = updatedCombatants
                    , popUp = Closed
                }
                    ! []

        EndTurn combatant ->
            let
                updatedCombatant =
                    { combatant | turnFinished = True }

                updatedCombatants =
                    Dict.insert
                        updatedCombatant.name
                        updatedCombatant
                        model.combatants
            in
                { model
                    | combatants = updatedCombatants
                    , popUp = Closed
                }
                    ! []

        ResolveDelete combatant ->
            let
                updatedCombatants =
                    Dict.remove
                        combatant.name
                        model.combatants
            in
                { model
                    | combatants = updatedCombatants
                    , popUp = Closed
                }
                    ! []


sortByInitiative : Combatants -> Combatants
sortByInitiative combatants =
    Dict.toList combatants
        |> List.sortBy (\( n, c ) -> c.initiative)
        |> List.reverse
        |> Dict.fromList


decrementCrash : Combatant -> Combatant
decrementCrash combatant =
    case combatant.crash of
        Just crash ->
            let
                updatedCrash =
                    { crash | turnsUntilReset = crash.turnsUntilReset - 1 }
            in
                if updatedCrash.turnsUntilReset < 1 then
                    { combatant
                        | crash = Nothing
                        , initiative = 3
                    }
                else
                    { combatant | crash = Just updatedCrash }

        Nothing ->
            combatant


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
                , onslaught = 0
                , crash =
                    if attInitiative > 0 then
                        Nothing
                    else
                        attacker.crash
                , turnFinished = True
            }
    in
        ( updatedAttacker, updatedDefender, shift )


resolveDecisive : AttackOutcome -> Combatant -> Combatant
resolveDecisive outcome combatant =
    case outcome of
        Hit ->
            { combatant | initiative = 3 }

        Miss ->
            { combatant
                | initiative =
                    if combatant.initiative < 11 then
                        combatant.initiative - 2
                    else
                        combatant.initiative - 3
                , onslaught = 0
                , turnFinished = True
            }



-- Subscriptions


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none



-- Views


view : Model -> Html Msg
view model =
    div [ css [ defaultStyle ] ]
        [ header
            [ css [ headerStyle, rowFlexStyle ] ]
            [ div []
                [ h1 [ css [ h1Style ] ] [ text "Threads of Martial Destiny" ]
                , b [] [ text "A combat manager for Exalted 3rd Edition" ]
                ]
            , div []
                [ img
                    [ css [ iconStyle True ]
                    , src "imgs/add.svg"
                    , NewCombatant
                        ""
                        "0"
                        |> OpenPopUp
                        |> onClick
                    , title "Add Combatant"
                    ]
                    [ text "Add Combatant" ]
                ]
            ]
        , div [ css [ bodyStyle ] ]
            ([ tracker model.round model.combatants ]
                ++ (case model.popUp of
                        (NewCombatant _ _) as newCombatant ->
                            [ newCombatantPopUp newCombatant ]

                        (EditInitiative _ _) as editInitiative ->
                            [ editPopUp editInitiative ]

                        (WitheringAttack _ _ _ _) as witheringAttack ->
                            [ witheringPopUp
                                model.combatants
                                witheringAttack
                            ]

                        (DecisiveAttack _) as decisiveAttack ->
                            [ decisivePopUp decisiveAttack
                            ]

                        (Confirm _ _) as confirm ->
                            [ confirmPopUp confirm
                            ]

                        Closed ->
                            []
                   )
            )
        , footer [ css [ footerStyle ] ]
            [ text "© Chris Wells Wood, 2018. Exalted is © White Wolf AB and Onyx Path."
            , br [] []
            , text "Icons made by "
            , a
                [ href "https://www.flaticon.com/authors/appzgear"
                , title "Appzgear"
                ]
                [ text "Appzgear" ]
            , text ", "
            , a
                [ href "https://www.flaticon.com/authors/dave-gandy"
                , title "Dave Gandy"
                ]
                [ text "Dave Gandy" ]
            , text ", "
            , a
                [ href "https://www.flaticon.com/authors/eleonor-wang"
                , title "Eleonor Wang"
                ]
                [ text "Eleonor Wang" ]
            , text ", "
            , a
                [ href "http://www.freepik.com"
                , title "Freepik"
                ]
                [ text "Freepik" ]
            , text " and "
            , a
                [ href "https://www.flaticon.com/authors/pixel-perfect"
                , title "Pixel perfect"
                ]
                [ text "Pixel perfect" ]
            , text " from "
            , a
                [ href "https://www.flaticon.com/"
                , title "Flaticon"
                ]
                [ text "www.flaticon.com" ]
            , text " is licensed by "
            , a
                [ href "http://creativecommons.org/licenses/by/3.0/"
                , title "Creative Commons BY 3.0"
                , Html.Styled.Attributes.target "_blank"
                ]
                [ text "CC 3.0 BY." ]
            ]
        ]


tracker : Int -> Combatants -> Html Msg
tracker round combatants =
    let
        readyCombatants =
            Dict.values combatants
                |> List.filter (not << .turnFinished)
                |> List.sortBy .initiative
                |> List.reverse

        turnFinishedCombatants =
            Dict.values combatants
                |> List.filter .turnFinished
                |> List.sortBy .initiative
                |> List.reverse
    in
        div []
            (if Dict.size combatants > 0 then
                [ div [ css [ turnStyle, rowFlexStyle ] ]
                    [ h2 [ css [ h2Style ] ]
                        [ text <| "Round " ++ (toString round) ]
                    , img
                        [ css [ iconStyle True ]
                        , onClick <|
                            OpenPopUp <|
                                Confirm "End Round" <|
                                    StartNewRound
                        , src "imgs/end-round.svg"
                        , title "Next Round"
                        ]
                        [ text "Next Round" ]
                    ]
                , h2 [ css [ h2Style ] ] [ text "Ready" ]
                , div [ css [ trackerStyling ] ]
                    (List.map (combatantCard <| Dict.size combatants)
                        readyCombatants
                    )
                , styledHR [] []
                , h2 [ css [ h2Style ] ] [ text "Turn Finished" ]
                , div [ css [ trackerStyling ] ]
                    (List.map (combatantCard <| Dict.size combatants)
                        turnFinishedCombatants
                    )
                ]
             else
                []
            )


combatantCard : Int -> Combatant -> Html Msg
combatantCard numCombatants combatant =
    let
        { name, initiative, turnFinished } =
            combatant

        attacksActive =
            if numCombatants < 2 then
                False
            else
                True

        colour =
            if initiative < 1 then
                colourPallette.crash
            else if initiative < 11 then
                colourPallette.lowInitiative
            else
                colourPallette.highInitiative
    in
        div [ css [ combatantCardStyle colour ] ]
            [ b [] [ text name ]
            , styledHR [] []
            , div
                [ css
                    [ rowFlexStyle
                    , initiativeStyle
                    ]
                ]
                [ (toString initiative)
                    ++ "i"
                    |> text
                , img
                    [ src "imgs/edit.svg"
                    , css
                        [ iconStyle True
                        ]
                    , onClick <|
                        OpenPopUp <|
                            EditInitiative combatant (toString initiative)
                    , title "Edit"
                    ]
                    []
                ]
            , styledHR [] []
            , div []
                ([ text ("Onslaught: " ++ (toString combatant.onslaught))
                 ]
                    ++ case combatant.crash of
                        Just crash ->
                            [ br [] []
                            , text
                                ("Crash:"
                                    ++ (toString crash.turnsUntilReset)
                                )
                            ]

                        Nothing ->
                            []
                )
            , styledHR [] []
            , div [ css [ rowFlexStyle ] ]
                [ img
                    ([ css [ iconStyle attacksActive ]
                     , src "imgs/withered-flower.svg"
                     , title "Withering attack"
                     ]
                        ++ if attacksActive then
                            [ onClick <|
                                OpenPopUp <|
                                    WitheringAttack combatant Nothing Nothing Nothing
                            ]
                           else
                            []
                    )
                    [ text "Withering" ]
                , img
                    [ css [ iconStyle attacksActive ]
                    , onClick <|
                        OpenPopUp <|
                            DecisiveAttack combatant
                    , src "imgs/sword.svg"
                    , title "Decisive Attack"
                    ]
                    [ text "Decisive" ]
                , img
                    [ css [ iconStyle True ]
                    , onClick <|
                        OpenPopUp <|
                            Confirm "Reset Onslaught" <|
                                ResetOnslaught combatant
                    , src "imgs/reset.svg"
                    , title "Reset Onslaught"
                    ]
                    [ text "Reset Onslaught" ]
                , img
                    [ css [ iconStyle True ]
                    , onClick <|
                        OpenPopUp <|
                            Confirm "End Turn" <|
                                EndTurn combatant
                    , src "imgs/end-turn.svg"
                    , title "End Turn"
                    ]
                    [ text "End Turn" ]
                , img
                    [ css [ iconStyle True ]
                    , onClick <|
                        OpenPopUp <|
                            Confirm "Delete Combatant" <|
                                ResolveDelete combatant
                    , src "imgs/delete.svg"
                    , title "Delete Combatant"
                    ]
                    [ text "Delete Combatant" ]
                ]
            ]


newCombatantPopUp : PopUp -> Html Msg
newCombatantPopUp newCombatant =
    div []
        [ disablingDiv
        , div [ css [ popUpStyle ] ]
            ((case newCombatant of
                NewCombatant name joinCombatStr ->
                    let
                        addDisabled =
                            case String.toInt joinCombatStr of
                                Ok joinCombat ->
                                    case name of
                                        "" ->
                                            True

                                        _ ->
                                            False

                                Err _ ->
                                    True
                    in
                        [ b [] [ text "Add New Combatant" ]
                        , br [] []
                        , text "Name"
                        , br [] []
                        , styledInput [ id "pop-up-focus", onInput SetCombatantName ] []
                        , br [] []
                        , text "Join Combat Successes"
                        , br [] []
                        , styledInput
                            [ onInput SetJoinCombat
                            , size 3
                            , value joinCombatStr
                            ]
                            []
                        , br [] []
                        , styledButton
                            [ onClick AddNewCombatant
                            , Html.Styled.Attributes.disabled addDisabled
                            ]
                            [ text "Add" ]
                        ]

                _ ->
                    []
             )
                ++ [ styledButton [ onClick ClosePopUp ] [ text "Cancel" ]
                   ]
            )
        ]


editPopUp : PopUp -> Html Msg
editPopUp editInitiative =
    let
        modifyInitiativeBtn modifyBy =
            styledButton
                [ onClick <|
                    ModifyInitiative modifyBy
                ]
                [ text <| toString modifyBy ]
    in
        div []
            [ disablingDiv
            , div [ css [ popUpStyle ] ]
                ((case editInitiative of
                    EditInitiative combatant newInitiative ->
                        let
                            resolveDisabled =
                                case String.toInt newInitiative of
                                    Ok _ ->
                                        False

                                    Err _ ->
                                        True
                        in
                            [ b [] [ text "Edit Initiative" ]
                            , br [] []
                            , modifyInitiativeBtn -5
                            , modifyInitiativeBtn -1
                            , styledInput
                                [ id "pop-up-focus"
                                , onInput SetInitiative
                                , value newInitiative
                                , size 3
                                ]
                                []
                            , modifyInitiativeBtn 1
                            , modifyInitiativeBtn 5
                            , br [] []
                            , styledButton
                                [ onClick <| ApplyNewInitiative
                                , Html.Styled.Attributes.disabled resolveDisabled
                                , title "Edit"
                                ]
                                [ text "Ok" ]
                            ]

                    _ ->
                        []
                 )
                    ++ [ styledButton [ onClick ClosePopUp ] [ text "Cancel" ]
                       ]
                )
            ]


disablingDiv : Html msg
disablingDiv =
    div [ css [ disablingStyle ] ] []


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
                    WitheringAttack attacker Nothing Nothing _ ->
                        [ b [] [ text "Select Target" ]
                        ]
                            ++ (Dict.toList combatants
                                    |> List.filter (\( n, c ) -> n /= attacker.name)
                                    |> List.map Tuple.second
                                    |> List.map selectTarget
                               )

                    WitheringAttack attacker (Just defender) (Just damageStr) Nothing ->
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
                            , styledInput
                                [ onInput SetWitheringDamage
                                , value <| damageStr
                                , size 3
                                ]
                                []
                            , br [] []
                            , styledButton
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
                            , styledInput
                                [ onInput SetShiftJoinCombat ]
                                []
                            , br [] []
                            , styledButton
                                [ onClick ResolveInitiativeShift
                                , Html.Styled.Attributes.disabled
                                    resolveDisabled
                                ]
                                [ text "Resolve" ]
                            ]

                    _ ->
                        []
                 )
                    ++ [ styledButton [ onClick ClosePopUp ] [ text "Cancel" ] ]
                )
            ]


decisivePopUp : PopUp -> Html Msg
decisivePopUp popUp =
    div []
        [ disablingDiv
        , div [ css [ popUpStyle ] ]
            ((case popUp of
                DecisiveAttack combatant ->
                    [ b [] [ text "Decisive Attack" ]
                    , br [] []
                    , styledButton [ onClick <| ResolveDecisive Hit ] [ text "Hit" ]
                    , styledButton [ onClick <| ResolveDecisive Miss ] [ text "Miss" ]
                    , br [] []
                    ]

                _ ->
                    []
             )
                ++ [ styledButton [ onClick ClosePopUp ] [ text "Cancel" ] ]
            )
        ]


confirmPopUp : PopUp -> Html Msg
confirmPopUp popUp =
    div []
        [ disablingDiv
        , div [ css [ popUpStyle ] ]
            ((case popUp of
                Confirm description msg ->
                    [ b [] [ text description ]
                    , br [] []
                    , text "Are you sure?"
                    , br [] []
                    , styledButton [ onClick <| msg ]
                        [ text "Ok" ]
                    ]

                _ ->
                    []
             )
                ++ [ styledButton [ onClick ClosePopUp ] [ text "Cancel" ] ]
            )
        ]



-- Styles


type alias Colour =
    Color


type alias ColourPallette =
    { lowInitiative : Colour
    , highInitiative : Colour
    , crash : Colour
    , turnFinished : Colour
    , clicked : Colour
    , backgroundColor : Colour
    }



-- https://coolors.co/91d696-b4d174-edc855-e49f64-d96969


colourPallette : ColourPallette
colourPallette =
    { highInitiative = hex "91d696"
    , lowInitiative = hex "edc855"
    , crash = hex "d96969"
    , turnFinished = hex "999999"
    , clicked = hex "777777"
    , backgroundColor = hex "eeeeee"
    }


defaultStyle : Style
defaultStyle =
    Css.batch
        [ fontFamilies [ "Tahoma", "Geneva", "sans-serif" ]
        , fontSize (px 18)
        , Css.height (pct 100)
        , Css.width (pct 100)
        , position absolute
        , top (px 0)
        , left (px 0)
        ]


styledHR : List (Attribute msg) -> List (Html msg) -> Html msg
styledHR =
    styled hr
        [ borderWidth (px 1)
        , borderStyle solid
        , borderColor <| hex "000000"
        , marginLeft (px 0)
        , marginRight (px 0)
        ]


styledInput : List (Attribute msg) -> List (Html msg) -> Html msg
styledInput =
    styled input
        [ borderWidth (px 2)
        , borderStyle solid
        , borderColor <| hex "000000"
        , fontSize (px 18)
        , margin (px 3)
        , padding (px 3)
        ]


styledButton : List (Attribute msg) -> List (Html msg) -> Html msg
styledButton =
    styled button
        [ backgroundColor colourPallette.turnFinished
        , Css.active
            [ backgroundColor colourPallette.clicked ]
        , Css.disabled
            [ backgroundColor colourPallette.backgroundColor ]
        , borderWidth (px 2)
        , borderStyle solid
        , borderColor <| hex "000000"
        , color <| hex "000000"
        , fontSize (px 18)
        , margin (px 3)
        , outline none
        , padding (px 3)
        ]


headerStyle : Style
headerStyle =
    Css.batch
        [ backgroundColor colourPallette.crash
        , padding (px 8)
        , Css.width auto
        , borderWidth4 (px 0) (px 0) (px 2) (px 0)
        , borderStyle solid
        , borderColor <| hex "000000"
        ]


turnStyle : Style
turnStyle =
    Css.batch
        [ backgroundColor colourPallette.highInitiative
        , padding (px 8)
        , Css.width auto
        , borderWidth4 (px 0) (px 0) (px 2) (px 0)
        , borderStyle solid
        , borderColor <| hex "000000"
        , margin4 (px 0) (px 0) (px 4) (px 0)
        ]


rowFlexStyle : Style
rowFlexStyle =
    Css.batch
        [ displayFlex
        , position relative
        , justifyContent spaceBetween
        , alignItems center
        ]


h1Style : Style
h1Style =
    Css.batch
        [ padding (px 0)
        , margin (px 0)
        ]


h2Style : Style
h2Style =
    Css.batch
        [ padding (px 0)
        , margin (px 0)
        , textAlign center
        ]


bodyStyle : Style
bodyStyle =
    Css.batch
        [ backgroundColor colourPallette.backgroundColor
        , Css.width (pct 100)
        , Css.height (pct 100)
        ]


footerStyle : Style
footerStyle =
    Css.batch
        [ borderWidth4 (px 2) (px 0) (px 0) (px 0)
        , borderStyle solid
        , borderColor <| hex "000000"
        , position absolute
        , bottom (px 0)
        , Css.width (pct 100)
        , fontSize (px 12)
        , color <| hex "aaaaaa"
        , padding (px 5)
        ]


iconStyle : Bool -> Style
iconStyle active =
    Css.batch
        ([ Css.height (px 24)
         , padding (px 3)
         , Css.width (px 24)
         , borderStyle solid
         , borderWidth (px 2)
         ]
            ++ if active then
                [ backgroundColor colourPallette.turnFinished
                , Css.active
                    [ backgroundColor colourPallette.clicked ]
                ]
               else
                [ backgroundColor colourPallette.backgroundColor ]
        )


trackerStyling : Style
trackerStyling =
    Css.batch
        [ displayFlex
        , flexWrap Css.wrap
        , justifyContent center
        ]


combatantCardStyle : Colour -> Style
combatantCardStyle bgColour =
    Css.batch
        [ backgroundColor bgColour
        , borderStyle solid
        , borderWidth (px 2)
        , Css.width (px 200)
        , overflow Css.hidden
        , overflowWrap normal
        , padding (px 8)
        , margin4 (px 2) (px 0) (px 2) (px 2)
        , displayFlex
        , flexDirection column
        , justifyContent spaceBetween
        ]


initiativeStyle : Style
initiativeStyle =
    Css.batch
        [ fontSize (px 36)
        , position relative
        ]


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
        [ backgroundColor colourPallette.lowInitiative
        , borderStyle solid
        , borderWidth (px 2)
        , left (pct 50)
        , padding (px 5)
        , position absolute
        , top (pct 50)
        , transform (translate2 (pct -50) (pct -50))
        , Css.width (px 300)
        , zIndex (int 1001)
        ]
