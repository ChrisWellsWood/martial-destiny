module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
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



-- Subscriptions


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none



-- Styles
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
    div []
        (List.map
            combatantCard
            combatants
        )


combatantCard : Combatant -> Html Msg
combatantCard { name, initiative } =
    div []
        [ b [] [ text name ]
        , text <| toString initiative
        ]
