import Html exposing (..)
import Html.App exposing (beginnerProgram)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import String exposing (..)


main =
  beginnerProgram { model = init, view = view, update = update }

init : Model
init  =
  { reg1 = ""
  , reg2 = ""
  , int1 = 0
  , int2 = 0
  , operator = NoOp
  , resultI = ""
  , result1 = 0
  , syntaxError1 = "" -- this should be a maybe type
  , syntaxError2 = ""
  , lut = [(1000, "M"), (900, "CM"), (500, "D"), (400, "CD"), (100, "C"), (90, "XC"), (50, "L"), (40, "XL"), (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")]
  }


type alias LookUpTab =
  List ( Int, String )

type Operator
  = NoOp
  | Add
  | Subtract
  | Multiply
  | Divide

type alias Model =
  { reg1 : String
  , reg2 : String
  , int1 : Int
  , int2 : Int
  , operator : Operator
  , resultI : String
  , result1 : Int
  , syntaxError1 : String
  , syntaxError2 : String
  , lut  : LookUpTab
  }


view : Model -> Html Msg
view model =
  div []
    [ div [] [ text model.syntaxError1 ]
    , div [] [ text model.syntaxError2 ]
    , input [ type' "text", placeholder "Reg1", onInput Input1 ] []
    , input [ type' "text", placeholder "Reg2", onInput Input2 ] []
    , button [ onClick (Operator Add) ] [ text "+" ]
    , button [ onClick (Operator Subtract) ] [ text "-" ]
    , button [ onClick (Operator Multiply) ] [ text "*" ]
    , button [ onClick (Operator Divide) ] [ text "/" ]
    , button [ onClick Compute ] [ text "=" ]
    , div [] [ text ("Operand 1: " ++ (parse1 model.lut model.int1) ++ " " ++ toString model.int1) ]
    , div [] [ text ("Operand 2: " ++ (parse1 model.lut model.int2) ++ " " ++ toString model.int2) ]
    , div [] [ text ("Operator: " ++ toString model.operator) ]
    , div [] [ text ("Result: " ++ model.resultI ++ " " ++ toString model.result1)]
    ]


type Msg
  = Input1 String
  | Input2 String
  | Operator Operator
  | Compute


update : Msg -> Model -> Model
update msg model =
  case msg of
    Input1 reg1 ->
      let upperReg1 = toUpper reg1 in
        let (int1, error) = parseI upperReg1 model.lut in
              { model | reg1 = upperReg1, int1 = int1, syntaxError1 = error }
    Input2 reg2 ->
      let upperReg2 = toUpper reg2 in
        let (int2, error) = parseI upperReg2 model.lut in
              { model | reg2 = upperReg2, int2 = int2, syntaxError2 = error }
    Operator op ->
      { model | operator = op }
    Compute ->
      let result = compute model.int1 model.int2 model.operator in
      { model | result1 = result, resultI = parse1 model.lut result}

compute : Int -> Int -> Operator -> Int
compute reg1 reg2 op =
  case op of
    NoOp -> 0
    Add -> reg1 + reg2
    Subtract -> reg1 - reg2
    Multiply -> reg1 * reg2
    Divide -> reg1 // reg2

parseI : String -> LookUpTab -> (Int, String)
parseI roman lut =
  case lut of
    ((k, v) :: t) ->
      if startsWith v roman then
        let (number, error) = parseI (dropLeft (length v) roman) lut in
          (k + number, error)
      else parseI roman t
    [] ->
      case roman of
        "" -> (0, "")
        _ -> (0, "Invalid Roman numeral")


parse1 : LookUpTab -> Int -> String
parse1 lut dec =
  case lut of
    ((k, v) :: t) ->
      if k <= dec then
        v ++ (parse1 lut (dec - k))
      else parse1 t dec
    [] -> ""
