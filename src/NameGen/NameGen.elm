module NameGen.NameGen exposing (..)

import Dict
import List
import NameGen.NameGenSymbols exposing (symbolMap)


type Wrappers
    = Capitalizer
    | ReverserWrappers


type GroupType
    = Symbol
    | ReverserGroup


type alias GeneratorData =
    { generators : List Generator }


type Generator
    = LiteralGenerator GeneratorData { pattern : String }
    | SequenceGenerator GeneratorData {}
    | RandomGenerator GeneratorData {}
    | CapitalizerGenerator GeneratorData {}
    | ReverserGenerator GeneratorData {}


type alias Group =
    { wrappers : List Wrappers
    , set : List Generator
    , groupType : GroupType
    }


initGroup : GroupType -> Group
initGroup groupType =
    { wrappers = []
    , set = []
    , groupType = groupType
    }


groupProduce : Group -> Generator
groupProduce group =
    case group.set of
        [] ->
            initLiteralGenerator ""

        [ gen ] ->
            gen

        _ ->
            Debug.todo "pick a random generator from the group.sets"


groupSplit : Group -> Group
groupSplit group =
    let
        newSet =
            List.head group.set
                |> Maybe.withDefault initSequence
                |> (\head -> [ head ] ++ group.set ++ [ initSequence ])
    in
    { group | set = newSet }


groupWrap : Group -> Wrappers -> Group
groupWrap group newWrappers =
    { group | wrappers = group.wrappers ++ [ newWrappers ] }


groupAddPattern : Group -> String -> Group
groupAddPattern group pattern =
    initRandomGenerator [ LiteralGenerator { pattern = pattern } ]


groupAddGenerator : Group -> Generator -> Group
groupAddGenerator group generator =
    let
        newSet =
            List.foldr
                (\wrapper g ->
                    case wrapper of
                        CapitalizerWrappers ->
                            CapitalizerGenerator { generators = [ g ] }

                        ReverserWrappers ->
                            ReverserGenerator { generators = [ g ] }
                )
                generator
                group.wrappers

        newerSet =
            let
                newSet_ =
                    List.head group.set
                        |> Maybe.withDefault initSequence
                        |> (\head -> [ head ] ++ group.set ++ newSet)
                        --- TODO TODO TODO ended here https://github.com/skeeto/fantasyname/blob/e5a475287902457dac09284fb681c13dfdd96715/ts/namegen.ts#L226
            in
            newSet_
    in
    { group | set = newerSet, wrappers = [] }


initRandomGenerator : List Generator -> Generator
initRandomGenerator generators =
    RandomGenerator generators


initSequence : Sequence
initSequence =
    {}


initLiteralGenerator : String -> LiteralGenerator
initLiteralGenerator value =
    Debug.todo "construct literal generator"


initNameGenerator : String -> Bool -> Generator
initNameGenerator pattern collapseTriples =
    -- TODO: handle pattern being a Generator? instead
    Debug.todo "construct base generator"


qwe =
    123
