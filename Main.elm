port module Main exposing (..)

import Css exposing (..)
import Dict
import Dom
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Task


main : Program (Maybe ExportedModel) Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


init : Maybe ExportedModel -> ( Model, Cmd Msg )
init savedSession =
    case savedSession of
        Just exportedModel ->
            (importModel exportedModel) ! []

        Nothing ->
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


type alias ExportedModel =
    { combatants : List ( String, Combatant )
    , round : Int
    }


exportModel : Model -> ExportedModel
exportModel model =
    ExportedModel
        (Dict.toList model.combatants)
        model.round


importModel : ExportedModel -> Model
importModel exportedModel =
    Model
        (Dict.fromList exportedModel.combatants)
        exportedModel.round
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
    | EditOnslaught Combatant String
    | Confirm String Msg
    | Help
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
    | Save
    | NewCombat
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
    | ModifyOnslaught Int
    | SetOnslaught String
    | ApplyNewOnslaught
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
                        update Save
                            { model
                                | popUp = Closed
                                , combatants = updatedCombatants
                            }

                _ ->
                    { model | popUp = Closed } ! []

        Save ->
            model ! [ exportModel model |> saveState ]

        NewCombat ->
            update Save emptyModel

        StartNewRound ->
            let
                updatedCombatants =
                    Dict.values model.combatants
                        |> List.map decrementCrash
                        |> List.map (\c -> { c | turnFinished = False })
                        |> List.map (\c -> ( c.name, c ))
                        |> Dict.fromList
            in
                update Save
                    { model
                        | combatants = updatedCombatants
                        , round = model.round + 1
                        , popUp = Closed
                    }

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
                            update Save
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
                                update Save
                                    { model
                                        | popUp = Closed
                                        , combatants = updatedCombatants
                                    }

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
                        update Save
                            { model
                                | popUp = Closed
                                , combatants = updatedCombatants
                            }

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
                        update Save
                            { model
                                | popUp = Closed
                                , combatants = updatedCombatants
                            }

                _ ->
                    { model | popUp = Closed } ! []

        ModifyOnslaught modifyBy ->
            case model.popUp of
                EditOnslaught combatant onslaughtString ->
                    { model
                        | popUp =
                            EditOnslaught
                                combatant
                                (String.toInt onslaughtString
                                    |> Result.withDefault 0
                                    |> (+) modifyBy
                                    |> toString
                                )
                    }
                        ! []

                _ ->
                    { model | popUp = Closed } ! []

        SetOnslaught onslaughtString ->
            case model.popUp of
                EditOnslaught combatant _ ->
                    { model
                        | popUp =
                            EditOnslaught
                                combatant
                                onslaughtString
                    }
                        ! []

                _ ->
                    { model | popUp = Closed } ! []

        ApplyNewOnslaught ->
            case model.popUp of
                EditOnslaught combatant newOnslaughtStr ->
                    case String.toInt newOnslaughtStr of
                        Ok newOnslaught ->
                            update Save
                                { model
                                    | popUp = Closed
                                    , combatants =
                                        Dict.insert
                                            combatant.name
                                            { combatant
                                                | onslaught = newOnslaught
                                            }
                                            model.combatants
                                }

                        Err _ ->
                            { model | popUp = Closed } ! []

                _ ->
                    { model | popUp = Closed } ! []

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
                update Save
                    { model
                        | combatants = updatedCombatants
                        , popUp = Closed
                    }

        ResolveDelete combatant ->
            let
                updatedCombatants =
                    Dict.remove
                        combatant.name
                        model.combatants
            in
                update Save
                    { model
                        | combatants = updatedCombatants
                        , popUp = Closed
                    }


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



-- Ports


port saveState : ExportedModel -> Cmd msg



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
                , b [] [ text "A combat tracker for Exalted 3rd Edition" ]
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
                , img
                    [ css [ iconStyle True ]
                    , src "imgs/help.svg"
                    , onClick <|
                        OpenPopUp <|
                            Help
                    , title "Help"
                    ]
                    [ text "Help" ]
                , img
                    [ css [ iconStyle True ]
                    , src "imgs/new-combat.svg"
                    , onClick <|
                        OpenPopUp <|
                            Confirm "New Combat" <|
                                NewCombat
                    , title "New Combat"
                    ]
                    [ text "New Combat" ]
                ]
            ]
        , div [ css [ bodyStyle ] ]
            ([ tracker model.round model.combatants ]
                ++ (case model.popUp of
                        (NewCombatant _ _) as newCombatant ->
                            [ newCombatantPopUp newCombatant ]

                        (EditInitiative _ _) as editInitiative ->
                            [ editInitiativePopUp editInitiative ]

                        (WitheringAttack _ _ _ _) as witheringAttack ->
                            [ witheringPopUp
                                model.combatants
                                witheringAttack
                            ]

                        (DecisiveAttack _) as decisiveAttack ->
                            [ decisivePopUp decisiveAttack
                            ]

                        (EditOnslaught _ _) as editOnslaught ->
                            [ editOnslaughtPopUp editOnslaught ]

                        (Confirm _ _) as confirm ->
                            [ confirmPopUp confirm
                            ]

                        Help ->
                            [ helpPopUp ]

                        Closed ->
                            []
                   )
            )
        , footer [ css [ footerStyle ] ]
            [ text
                ("© Chris Wells Wood, 2018. Version 1.1.0. "
                    ++ "Exalted is © White Wolf AB and Onyx Path. "
                )
            , a [ href "https://github.com/ChrisWellsWood/martial-destiny" ]
                [ text "Source code." ]
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
            , text " and are licensed by "
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
        { name, initiative, onslaught, turnFinished } =
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
                    , title "Edit Initiative"
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
                                ("Crash: "
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
                     , title "Withering Attack"
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
                    [ src "imgs/reset.svg"
                    , css
                        [ iconStyle True
                        ]
                    , onClick <|
                        OpenPopUp <|
                            EditOnslaught combatant (toString onslaught)
                    , title "Edit Onslaught"
                    ]
                    []
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
                        , styledHR [] []
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


editInitiativePopUp : PopUp -> Html Msg
editInitiativePopUp editInitiative =
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
                            , styledHR [] []
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
            div
                [ css [ selectStyle ]
                , onClick <| SetWitheringTarget combatant
                ]
                [ text combatant.name
                ]
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
                            ++ [ styledHR [] [] ]

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
                            , styledHR [] []
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
                            , styledHR [] []
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
                    , styledHR [] []
                    ]

                _ ->
                    []
             )
                ++ [ styledButton [ onClick ClosePopUp ] [ text "Cancel" ] ]
            )
        ]


editOnslaughtPopUp : PopUp -> Html Msg
editOnslaughtPopUp editOnslaught =
    let
        modifyOnslaughtBtn modifyBy =
            styledButton
                [ onClick <|
                    ModifyOnslaught modifyBy
                ]
                [ text <| toString modifyBy ]
    in
        div []
            [ disablingDiv
            , div [ css [ popUpStyle ] ]
                ((case editOnslaught of
                    EditOnslaught combatant newOnslaught ->
                        let
                            resolveDisabled =
                                case String.toInt newOnslaught of
                                    Ok _ ->
                                        False

                                    Err _ ->
                                        True
                        in
                            [ b [] [ text "Edit Onslaught" ]
                            , br [] []
                            , modifyOnslaughtBtn -5
                            , modifyOnslaughtBtn -1
                            , styledInput
                                [ id "pop-up-focus"
                                , onInput SetOnslaught
                                , value newOnslaught
                                , size 3
                                ]
                                []
                            , modifyOnslaughtBtn 1
                            , modifyOnslaughtBtn 5
                            , styledHR [] []
                            , styledButton
                                [ onClick <| ApplyNewOnslaught
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
                    , styledHR [] []
                    , styledButton [ onClick <| msg ]
                        [ text "Ok" ]
                    ]

                _ ->
                    []
             )
                ++ [ styledButton [ onClick ClosePopUp ] [ text "Cancel" ] ]
            )
        ]


helpPopUp : Html Msg
helpPopUp =
    div []
        [ disablingDiv
        , div
            [ css
                [ popUpStyle
                , Css.maxHeight (pct 80)
                , Css.width (pct 80)
                , overflow auto
                ]
            ]
            [ text "Welcome to Threads of Martial Destiny, a combat tracker for "
            , a [ href "http://theonyxpath.com/category/worlds/exalted/" ]
                [ text "Exalted 3rd Edition" ]
            , ". The app saves its state regularly so don't worry if you "
                ++ "close or refresh the tab, you can pick up where you left "
                ++ "off. Here's a list of the buttons and what a description of "
                ++ "what they do:"
                |> text
            , Html.Styled.table []
                [ iconDescription
                    "imgs/add.svg"
                    "Adds a new combatant to the combat."
                , iconDescription
                    "imgs/new-combat.svg"
                    "Starts a new combat deleting the current session."
                , iconDescription
                    "imgs/end-round.svg"
                    ("Ends the current round of combat, indicating that all "
                        ++ "combatants have made taken actions."
                    )
                , iconDescription
                    "imgs/edit.svg"
                    "Edits the combatants initiative value."
                , iconDescription
                    "imgs/withered-flower.svg"
                    "Initiates a withering attack (the icon is a withered flower)."
                , iconDescription
                    "imgs/sword.svg"
                    "Initiates a decisive attack."
                , iconDescription
                    "imgs/reset.svg"
                    "Edits the combatants onslaught value."
                , iconDescription
                    "imgs/end-turn.svg"
                    ("Ends the combatants turn, indicating that they have taken "
                        ++ "all their actions."
                    )
                , iconDescription
                    "imgs/delete.svg"
                    "Deletes the combatant."
                ]
            , "If you find any bugs or have any feature requests please add "
                ++ "an issue on "
                |> text
            , a
                [ href
                    "https://github.com/ChrisWellsWood/martial-destiny/issues"
                ]
                [ text "GitHub" ]
            , text " or, even better, send me a pull request!"
            , styledHR [] []
            , styledButton [ onClick ClosePopUp ] [ text "Close" ]
            ]
        ]


iconDescription : String -> String -> Html msg
iconDescription iconPath description =
    tr []
        [ td []
            [ img
                [ css [ iconStyle True ]
                , src iconPath
                ]
                []
            ]
        , td [] [ text description ]
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
        [ displayFlex
        , flexDirection column
        , fontFamilies [ "Tahoma", "Geneva", "sans-serif" ]
        , fontSize (px 18)
        , Css.height (pct 100)
        , justifyContent spaceBetween
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
        , overflow auto
        ]


footerStyle : Style
footerStyle =
    Css.batch
        [ borderWidth4 (px 2) (px 0) (px 0) (px 0)
        , borderStyle solid
        , borderColor <| hex "000000"
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
         , margin (px 3)
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


selectStyle : Style
selectStyle =
    Css.batch
        [ margin (px 3)
        , textAlign center
        , Css.hover
            [ backgroundColor colourPallette.turnFinished
            ]
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
