module Main exposing (..)

import Browser
import Html
import Html.Attributes as Attr
import Html.Events as Event
import Interface exposing (receivedSelection, requestSelection)
import Json.Decode as D
import List.Extra
import Maybe.Extra
import Set exposing (Set)
import Tuple exposing (first, second)
import Unicode exposing (isAlpha, isSeparator)
import Dict exposing (Dict)


type Msg
    = NewText String
    | NewTextName String
    | GenerateTokens
    | ReceivedSelection String
    | TokenFocused
    | TokenStringChanged Int String
    | RemoveToken Int
    | SwapTokenRight Int
    | SwapTokenLeft Int
    | Split PartT Int
    | Merge PartT Int
    | InsertToken Int
    | ChangeEditPane PartT
    | Edit MetaT
    | TempChanged String
    | Save MetaT
    | CancelEditing
    | Remove MetaT


type alias Model =
    { text : String
    , text_name : String
    , tokens : Dict Int Token
    , order : List Int
    , options : Flags
    , counter : Int
    , selection : Maybe ( Int, Int )
    , editing : Maybe Mode
    , blocks : Dict ( Int, Int ) Meta
    , lines : Dict ( Int, Int ) Meta
    , meta : Meta
    , editPane : PartT
    , editingMeta : MetaT
    , temp : String
    }


type alias Meta =
    { tags : Set String
    , properties : Dict String String
    }


type Mode
    = Single Int
    | Range ( Int, Int )


type alias Token =
    { string : String
    , meta : Meta
    , identifier : Int
    }


type PartT
    = TextT
    | BlockT
    | LineT
    | TokenT
    | NoteT


type alias Index = ( Int, Int )

    
type MetaT
    = TagsT PartT Index
    | PropValueT PartT Index String
    | PropKeyT PartT Index String
    | NoneT


type alias Flags =
    { text : String
    , file : String
    }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Flags -> ( Model, Cmd Msg )
init options =
    let
        model =
            { text = ""
            , text_name = ""
            , tokens = Dict.empty
            , order = []
            , options = options
            , counter = 0
            , selection = Nothing
            , editing = Nothing
            , blocks = Dict.empty
            , lines = Dict.empty
            , meta = Meta Set.empty Dict.empty
            , editPane = TextT
            , editingMeta = NoneT
            , temp = ""
            }
    in
    ( model, Cmd.none )

    -- else
    --     ( model, getDoc options )


-- getDoc : Options -> Cmd Msg
-- getDoc _ =
--     Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewText s ->
            ( { model | text = s }, Cmd.none )

        NewTextName s ->
            ( { model | text_name = s }, Cmd.none )

        GenerateTokens ->
            let
                new_tokens =
                    processText model.counter model.text

                new_counter =
                    Dict.size new_tokens - 1
            in
            ( { model
                | tokens = new_tokens
                , order = List.range 0 new_counter
                , counter = new_counter
                , blocks =
                    Dict.fromList
                        [ ( ( 0, new_counter )
                          , { tags = Set.empty
                            , properties = Dict.empty
                            }
                          )
                        ]
                , lines =
                    Dict.fromList
                        [ ( ( 0, new_counter )
                          , { tags = Set.empty
                            , properties = Dict.empty
                            }
                          )
                        ]
              }
            , Cmd.none
            )

        ReceivedSelection s ->
            case D.decodeString selectionD s of
                Ok (Just ( s1, s2 )) ->
                    let
                        single =
                            Just (Single s1)

                        range =
                            Just (Range ( s1, s2 ))

                        newModel =
                            if s1 == s2 then
                                { model | editing = single }

                            else
                                { model | editing = range }
                    in
                    ( { newModel | selection = Just ( s1, s2 ) }
                    , Cmd.none
                    )

                _ ->
                    ( { model | selection = Nothing }
                    , Cmd.none
                    )

        TokenFocused ->
            ( model, requestSelection () )

        TokenStringChanged identifier s ->
            let
                updateS token =
                    Maybe.map (\x -> { x | string = s }) token

                tokens =
                    Dict.update identifier updateS model.tokens
            in
            ( { model | tokens = tokens }, Cmd.none )

        RemoveToken identifier ->
            let
                index =
                    getIndex identifier model

                singleton ( k1, k2 ) =
                    k1 == k2 && k1 == index

                keyDown ( k1, k2 ) =
                    if k1 > index then
                        ( k1 - 1, k2 - 1 )

                    else if k2 >= index then
                        ( k1, k2 - 1 )

                    else
                        ( k1, k2 )

                updateKey k v d =
                    if singleton k then
                        d

                    else
                        Dict.insert (keyDown k) v d

                order =
                    List.Extra.removeAt index model.order

                blocks =
                    Dict.foldl updateKey Dict.empty model.blocks

                lines =
                    Dict.foldl updateKey Dict.empty model.lines

                tokens =
                    Dict.remove identifier model.tokens
            in
            ( { model
                | tokens = tokens
                , order = order
                , blocks = blocks
                , lines = lines
              }
            , Cmd.none
            )

        SwapTokenLeft identifier ->
            let
                index =
                    getIndex identifier model

                swap =
                    List.Extra.swapAt (index - 1) index
            in
            ( { model | order = swap model.order }
            , Cmd.none
            )

        SwapTokenRight identifier ->
            let
                index =
                    getIndex identifier model

                swap =
                    List.Extra.swapAt index (index + 1)
            in
            ( { model | order = swap model.order }
            , Cmd.none
            )

        Split BlockT identifier ->
            let
                blocks =
                    splitGrouping model .blocks identifier

                lines =
                    splitGrouping model .lines identifier
            in
            ( { model | blocks = blocks, lines = lines }
            , Cmd.none
            )

        Split LineT identifier ->
            let
                lines =
                    splitGrouping model .lines identifier
            in
            ( { model | lines = lines }, Cmd.none )

        Split _ _ ->
            ( model, Cmd.none )

        Merge BlockT identifier ->
            let
                index =
                    getIndex identifier model

                prevBlock =
                    Dict.toList model.blocks
                        |> List.filter
                            (key2 >> (==) (index - 1))
                        |> List.head

                currBlock =
                    Dict.toList model.blocks
                        |> List.filter (key1 >> (==) index)
                        |> List.head

                merge ( ( pKey1, pKey2 ), pv ) ( ( cKey1, cKey2 ), cv ) =
                    model.blocks
                        |> Dict.remove ( pKey1, pKey2 )
                        |> Dict.remove ( cKey1, cKey2 )
                        |> Dict.insert ( pKey1, cKey2 )
                            { tags =
                                Set.union pv.tags cv.tags
                            , properties =
                                Dict.union
                                    pv.properties
                                    cv.properties
                            }
            in
            case ( prevBlock, currBlock ) of
                ( Just prev, Just curr ) ->
                    ( { model | blocks = merge prev curr }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Merge LineT identifier ->
            let
                index =
                    getIndex identifier model

                block =
                    Dict.toList model.blocks
                        |> List.filter (key1 >> (>=) index)
                        |> List.filter (key2 >> (<=) index)
                        |> List.head

                lines b =
                    Dict.toList model.lines
                        |> List.filter (key2 >> (<=) (key1 b))
                        |> List.filter (key2 >> (>=) (key2 b))

                currLine =
                    Maybe.map lines block
                        |> Maybe.map
                            (List.filter (key1 >> (==) index))
                        |> Maybe.andThen List.head

                prevLine =
                    Maybe.map lines block
                        |> Maybe.map
                            (List.filter
                                (key2 >> (==) (index - 1))
                            )
                        |> Maybe.andThen List.head

                merge ( ( pKey1, pKey2 ), pv ) ( ( cKey1, cKey2 ), cv ) =
                    model.lines
                        |> Dict.remove ( pKey1, pKey2 )
                        |> Dict.remove ( cKey1, cKey2 )
                        |> Dict.insert ( pKey1, cKey2 )
                            { tags =
                                Set.union pv.tags cv.tags
                            , properties =
                                Dict.union
                                    pv.properties
                                    cv.properties
                            }
            in
            case ( prevLine, currLine ) of
                ( Just prev, Just curr ) ->
                    ( { model | lines = merge prev curr }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Merge _ _ ->
            ( model, Cmd.none )

        InsertToken identifier ->
            let
                index =
                    getIndex identifier model

                counter =
                    model.counter + 1

                token =
                    { string = "?"
                    , meta = Meta Set.empty Dict.empty
                    , identifier = counter
                    }

                tokens =
                    Dict.insert counter token model.tokens

                order =
                    List.Extra.splitAt (index + 1) model.order
                        |> (\( x, y ) -> x ++ (counter :: y))

                editing =
                    Just <| Single counter

                updateKey ( k1, k2 ) v d =
                    if k1 > index then
                        Dict.insert ( k1 + 1, k2 + 1 ) v d

                    else if k2 >= index then
                        Dict.insert ( k1, k2 + 1 ) v d

                    else
                        Dict.insert ( k1, k2 ) v d

                blocks =
                    Dict.foldl updateKey Dict.empty model.blocks

                lines =
                    Dict.foldl updateKey Dict.empty model.lines
            in
            ( { model
                | counter = counter
                , tokens = tokens
                , order = order
                , editing = editing
                , blocks = blocks
                , lines = lines
              }
            , Cmd.none
            )

        ChangeEditPane part ->
            ( { model | editPane = part }, Cmd.none )
                
        Edit (TagsT part index) ->
            let
                tagSet =
                    case part of
                        TextT ->
                            Just model.meta.tags

                        LineT ->
                            Dict.get index model.lines
                                |> Maybe.map .tags

                        BlockT ->
                            Dict.get index model.blocks
                                |> Maybe.map .tags

                        _ ->
                            Nothing

                tags =
                    tagSet
                        |> Maybe.withDefault Set.empty
                        |> Set.toList
                        |> String.join "\n"
            in
            ( { model
                | editingMeta = TagsT part index
                , temp = tags
              }
            , Cmd.none
            )

        Edit (PropKeyT part index s) ->
            ( { model
                  | editingMeta = PropKeyT part index s
                  , temp = ""
              }
            , Cmd.none
            )

        Edit (PropValueT part index s) ->
            let
                value =
                    case part of
                        TextT ->
                            Dict.get s model.meta.properties
                                |> Maybe.withDefault ""

                        BlockT ->
                            Dict.get index model.blocks
                                |> Maybe.map .properties
                                |> Maybe.andThen (Dict.get s)
                                |> Maybe.withDefault ""

                        LineT ->
                            Dict.get index model.blocks
                                |> Maybe.map .properties
                                |> Maybe.andThen (Dict.get s)
                                |> Maybe.withDefault ""

                        _ ->
                            ""
            in
            ( { model
                  | editingMeta = PropValueT part index s
                  , temp = value
              }
            , Cmd.none
            )
            
        Edit _ ->
            ( model, Cmd.none )

        TempChanged s ->
            ( { model | temp = s }, Cmd.none )

        Remove (PropKeyT part index s) ->
            case part of
                TextT ->
                    let
                        meta =
                            model.meta
                    in
                    ( { model | meta =
                            { meta | properties =
                                  Dict.remove s meta.properties
                            }
                      }
                    , Cmd.none
                    )
                    
                LineT ->
                    let
                        line =
                            Dict.get index model.lines

                        lines =
                            case line of
                                Nothing ->
                                    model.lines

                                Just l ->
                                    Dict.insert index
                                        { l | properties =
                                              Dict.remove s
                                              l.properties
                                        } model.lines
                    in
                    ( { model | lines = lines }, Cmd.none )

                BlockT ->
                    let
                        block =
                            Dict.get index model.blocks

                        blocks =
                            case block of
                                Nothing ->
                                    model.blocks

                                Just b ->
                                    Dict.insert index
                                        { b | properties =
                                              Dict.remove s
                                              b.properties
                                        } model.blocks
                    in
                    ( { model | blocks = blocks }, Cmd.none )

                _ ->
                    ( model, Cmd.none )
                
        Remove _ ->
            ( model, Cmd.none )
                
        Save (PropKeyT part index _) ->
            let
                nmodel =
                    { model
                        | editingMeta = NoneT
                        , temp = ""
                    }
            in
            case part of
                TextT ->
                    let
                        meta =
                            model.meta
                    in
                    ( { nmodel | meta =
                            { meta | properties =
                                  Dict.insert model.temp ""
                                  meta.properties
                            }
                      }
                    , Cmd.none
                    )

                LineT ->
                    let
                        line =
                            Dict.get index model.lines

                        lines =
                            case line of
                                Nothing ->
                                    model.lines

                                Just l ->
                                    Dict.insert index
                                        { l | properties =
                                              Dict.insert
                                              model.temp ""
                                              l.properties
                                        } model.lines
                    in
                    ( { nmodel | lines = lines }, Cmd.none )

                BlockT ->
                    let
                        block =
                            Dict.get index model.blocks

                        blocks =
                            case block of
                                Nothing ->
                                    model.blocks

                                Just b ->
                                    Dict.insert index
                                        { b | properties =
                                              Dict.insert
                                              model.temp ""
                                              b.properties
                                        } model.blocks
                    in
                    ( { nmodel | blocks = blocks }, Cmd.none )

                _ ->
                    ( nmodel, Cmd.none )
                
        Save (PropValueT part index s) ->
            let
                nmodel =
                    { model
                        | editingMeta = NoneT
                        , temp = ""
                    }
            in
            case part of
                TextT ->
                    let
                        meta =
                            model.meta
                    in
                    ( { nmodel | meta =
                            { meta | properties =
                                  Dict.insert s model.temp
                                  meta.properties
                            }
                      }
                    , Cmd.none
                    )

                LineT ->
                    let
                        line =
                            Dict.get index model.lines

                        lines =
                            case line of
                                Nothing ->
                                    model.lines

                                Just l ->
                                    Dict.insert index
                                        { l | properties =
                                              Dict.insert
                                              s model.temp
                                              l.properties
                                        } model.lines
                    in
                    ( { nmodel | lines = lines }, Cmd.none )

                BlockT ->
                    let
                        block =
                            Dict.get index model.blocks

                        blocks =
                            case block of
                                Nothing ->
                                    model.blocks

                                Just b ->
                                    Dict.insert index
                                        { b | properties =
                                              Dict.insert
                                              s model.temp
                                              b.properties
                                        } model.blocks
                    in
                    ( { nmodel | blocks = blocks }, Cmd.none )

                _ ->
                    ( nmodel, Cmd.none )
                                
        Save (TagsT part index) ->
            let
                tags =
                    model.temp
                        |> String.split "\n"
                        |> List.map String.trim
                        |> List.filter (String.isEmpty >> not)
                        |> Set.fromList

                nmodel =
                    { model
                        | editingMeta = NoneT
                        , temp = ""
                    }
            in
            case part of
                TextT ->
                    let
                        meta =
                            model.meta
                    in
                    ( { nmodel | meta = { meta | tags = tags } }
                    , Cmd.none
                    )

                LineT ->
                    let
                        line =
                            Dict.get index model.lines

                        lines =
                            case line of
                                Nothing ->
                                    model.lines

                                Just l ->
                                    Dict.insert index
                                        { l | tags = tags }
                                        model.lines
                    in
                    ( { nmodel | lines = lines }, Cmd.none )

                BlockT ->
                    let
                        block =
                            Dict.get index model.blocks

                        blocks =
                            case block of
                                Nothing ->
                                    model.blocks

                                Just b ->
                                    Dict.insert index
                                        { b | tags = tags }
                                        model.blocks
                    in
                    ( { nmodel | blocks = blocks }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Save _ ->
            ( model, Cmd.none )

        CancelEditing ->
            ( { model
                | editingMeta = NoneT
                , temp = ""
              }
            , Cmd.none
            )


splitGrouping :
    Model
    -> (Model -> Dict ( Int, Int ) Meta)
    -> Int
    -> Dict ( Int, Int ) Meta
splitGrouping model group identifier =
    let
        index =
            getIndex identifier model

        currGroup =
            Dict.filter
                (\( k1, k2 ) v -> index > k1 && index <= k2)
                (group model)
                |> Dict.toList
                |> List.head

        prevIndex =
            if (index - 1) >= 0 then
                Just (index - 1)

            else
                Nothing

        split ( ( k1, k2 ), v ) p =
            group model
                |> Dict.remove ( k1, k2 )
                |> Dict.insert ( k1, p ) v
                |> Dict.insert ( index, k2 ) v
    in
    case ( currGroup, prevIndex ) of
        ( Just curr, Just prev ) ->
            split curr prev

        _ ->
            group model


subscriptions : Model -> Sub Msg
subscriptions model =
    receivedSelection ReceivedSelection


view : Model -> Html.Html Msg
view model =
    Html.main_ [ Attr.class "container-fluid" ] <|
        if Dict.isEmpty model.tokens then
            [ Html.input
              [ Attr.value model.text_name
              , Attr.type_ "text"
              , Event.onInput NewTextName
              ] []
            , Html.textarea
                [ Attr.value model.text
                , Event.onInput NewText
                ] []
            , Html.button
                [ Event.onClick GenerateTokens ]
                [ Html.text "Generate Tokens" ]
            ]

        else
            [ Html.h1 [] [ Html.text model.text_name ]
            , viewEditorPanel model
            , Html.div [ Attr.class "grid-with-side" ]
                [ Html.div [ Attr.class "side" ] []
                , viewEditPane model
                , Html.div []
                    (List.map
                        (viewBlock model)
                        (Dict.keys model.blocks)
                    )
                ]
            ]


viewEditPane : Model -> Html.Html Msg
viewEditPane model =
    Html.div []
        [ viewEditPaneHead model
        , viewEditPaneBody model
        ]


viewEditPaneBody : Model -> Html.Html Msg
viewEditPaneBody model =
    case model.editPane of
        TextT ->
            viewEditText model

        BlockT ->
            viewEditGrouping BlockT .blocks model

        LineT ->
            viewEditGrouping LineT .lines model

        _ ->
            Html.text "Cannot Edit This"


viewEditPaneHead : Model -> Html.Html Msg
viewEditPaneHead model =
    Html.div [ Attr.attribute "role" "group" ]
        [ Html.button
            [ Attr.classList
                [ ( "outline", True )
                , ( "secondary", model.editPane /= TextT )
                ]
            , Event.onClick <| ChangeEditPane TextT
            ]
            [ Html.text "Text" ]
        , Html.button
            [ Attr.classList
                [ ( "outline", True )
                , ( "secondary", model.editPane /= BlockT )
                ]
            , Event.onClick <| ChangeEditPane BlockT
            ]
            [ Html.text "Blocks" ]
        , Html.button
            [ Attr.classList
                [ ( "outline", True )
                , ( "secondary", model.editPane /= LineT )
                ]
            , Event.onClick <| ChangeEditPane LineT
            ]
            [ Html.text "Lines" ]
        ]


viewTagEditor : Model -> Html.Html Msg
viewTagEditor model =
    Html.div []
        [ Html.textarea
            [ Attr.value model.temp
            , Event.onInput TempChanged
            ]
            []
        , Html.button
            [ Attr.class "secondary"
            , Event.onClick <| Save model.editingMeta
            ]
            [ Html.text "Done" ]
        , Html.button
            [ Attr.class "secondary"
            , Event.onClick <| CancelEditing
            ]
            [ Html.text "Cancel" ]
        ]


viewTagViewer : PartT -> ( Int, Int ) -> Model -> Html.Html Msg
viewTagViewer part index model =
    let
        tagSet =
            case part of
                TextT ->
                    Just model.meta.tags

                LineT ->
                    Dict.get index model.lines
                        |> Maybe.map .tags

                BlockT ->
                    Dict.get index model.blocks
                        |> Maybe.map .tags

                _ ->
                    Nothing

        tags =
            Maybe.withDefault Set.empty tagSet
                |> Set.toList
    in
    Html.section []
        [ Html.p []
            (List.map
                (\t ->
                    Html.span [ Attr.class "tag" ]
                        [ Html.text " "
                        , Html.text t
                        , Html.text " "
                        ]
                )
                tags
            )
        , Html.button
            [ Event.onClick <| Edit (TagsT part index)
            , Attr.class "secondary"
            ]
            [ Html.text "Edit" ]
        ]


viewPropertyRow : PartT -> ( Int, Int ) -> Model
                -> ( String, String ) -> Html.Html Msg
viewPropertyRow part index model (k, v) =
    let
        default =
            [ Html.td [] [ Html.text k ]
            , Html.td [] [ Html.text v ]
            , Html.td []
                [ Html.button
                      [ Event.onClick <|
                            Edit (PropValueT part index k) ]
                      [ Html.text "Edit" ]
                ]
            , Html.td []
                [ Html.button
                      [ Event.onClick <|
                            Remove (PropKeyT part index k) ]
                      [ Html.text "Delete" ]
                ]
            ]
    in
    Html.tr []
        ( case model.editingMeta of
              PropValueT p i k_ ->
                  if p == part && i == index && k_ == k then
                      [ Html.td []
                            [ Html.text k ]
                      , Html.td []
                          [ Html.input
                                [ Attr.value model.temp
                                , Event.onInput TempChanged
                                , Attr.type_ "text"
                                ] []
                          ]
                      , Html.td []
                          [ Html.button
                                [ Event.onClick <|
                                      Save model.editingMeta ]
                                [ Html.text "Done" ]
                          ]
                      , Html.td []
                          [ Html.button
                                [ Event.onClick CancelEditing ]
                                [ Html.text "Cancel" ]
                          ]
                      ]
                  else
                      default
              _ ->
                  default
        )

        
viewProperties : PartT -> ( Int, Int ) -> Model -> Html.Html Msg
viewProperties part index model =
    let
        propSet =
            case part of
                TextT ->
                    Just model.meta.properties

                LineT ->
                    Dict.get index model.lines
                        |> Maybe.map .properties

                BlockT ->
                    Dict.get index model.blocks
                        |> Maybe.map .properties

                _ ->
                    Nothing

        properties =
            Maybe.withDefault Dict.empty propSet
                |> Dict.toList

        default =
            Html.button
                [ Attr.class "quiet"
                , Event.onClick <| Edit ( PropKeyT part index "" )
                ]
            [ Html.text "Add" ]
    in
    Html.section []
        [ Html.table []
              [ Html.thead []
                    [ Html.tr []
                          [ Html.th [] [ Html.text "Property" ]
                          , Html.th [] [ Html.text "Value" ]
                          , Html.td [] []
                          , Html.td [] []
                          ]
                    ]
              , Html.tbody []
                  (List.map
                       (viewPropertyRow part index model)
                       properties
                  )
              ]
        , case model.editingMeta of
              PropKeyT p i _ ->
                  if p == part && i == index then
                      Html.div []
                          [ Html.input
                                [ Attr.type_ "text"
                                , Attr.value model.temp
                                , Event.onInput TempChanged
                                ] []
                          , Html.button
                              [ Event.onClick <|
                                    Save model.editingMeta ]
                              [ Html.text "Done" ]
                          , Html.button
                              [ Event.onClick CancelEditing ]
                              [ Html.text "Cancel" ]
                          ]

                  else
                      default

              _ ->
                  default
        ]


viewEditText : Model -> Html.Html Msg
viewEditText model =
    Html.div []
        [ Html.details []
            [ Html.summary []
                [ Html.text "Tags" ]
            , case model.editingMeta of
                TagsT TextT _ ->
                    viewTagEditor model

                _ ->
                    viewTagViewer TextT ( -1, -1 ) model
            ]
        , Html.hr [] []
        , Html.details []
            [ Html.summary []
                [ Html.text "Properties" ]
            , viewProperties TextT ( -1, -1 ) model
            ]
        , Html.hr [] []
        , Html.details [ Attr.attribute "open" "" ]
            [ Html.summary []
                [ Html.text "Original Text" ]
            , Html.div [ Attr.class "whitespace" ]
                [ Html.text model.text ]
            ]
        ]


viewEditGrouping : PartT -> (Model -> Dict (Int, Int) Meta)
             -> Model -> Html.Html Msg
viewEditGrouping part group model =
    let
        tags ( k, v ) =
            Html.li []
                [ case model.editingMeta of
                    TagsT p i ->
                        if i == k && part == p then
                            viewTagEditor model

                        else
                            viewTagViewer part k model

                    _ ->
                        viewTagViewer part k model
                ]

        properties ( k, v ) =
            Html.li [] [ viewProperties part k model ]
    in
    Html.div []
        [ Html.details []
            [ Html.summary []
                [ Html.text "Tags" ]
            , (group model)
                |> Dict.toList
                |> List.map tags
                |> Html.ol []
            ]
        , Html.hr [] []
        , Html.details []
            [ Html.summary []
                [ Html.text "Properties" ]
            , (group model)
                |> Dict.toList
                |> List.map properties
                |> Html.ol []
            ]
        , Html.hr [] []
        ]


viewLine : Model -> ( Int, Int ) -> Html.Html Msg
viewLine model line =
    model.order
        |> List.Extra.splitAt (first line)
        |> second
        |> List.Extra.splitAt (second line - first line + 1)
        |> first
        |> List.map (\i -> Dict.get i model.tokens)
        |> Maybe.Extra.values
        |> List.map (viewToken model)
        |> Html.div [ Attr.class "token-line" ]


viewBlock : Model -> ( Int, Int ) -> Html.Html Msg
viewBlock model block =
    Dict.keys model.lines
        |> List.filter (second >> (>=) (second block))
        |> List.filter (first >> (<=) (first block))
        |> List.map (viewLine model)
        |> Html.article [ Attr.class "token-block" ]


viewSingleEditor : Token -> List (Html.Html Msg)
viewSingleEditor token =
    [ Html.input
        [ Attr.value token.string
        , Attr.type_ "text"
        , Event.onInput <| TokenStringChanged token.identifier
        ]
        []
    , Html.details [ Attr.class "dropdown" ]
        [ Html.summary
            [ Attr.attribute "role" "button"
            , Attr.class "secondary"
            ]
            [ Html.text "Edit Token" ]
        , Html.ul []
            [ Html.li
                [ Event.onClick <|
                    RemoveToken token.identifier
                ]
                [ Html.text "Remove" ]
            , Html.li
                [ Event.onClick <|
                    InsertToken token.identifier
                ]
                [ Html.text "Insert" ]
            , Html.li
                [ Event.onClick <|
                    SwapTokenLeft token.identifier
                ]
                [ Html.text "Swap Left" ]
            , Html.li
                [ Event.onClick <|
                    SwapTokenRight token.identifier
                ]
                [ Html.text "Swap Right" ]
            ]
        ]
    , Html.details [ Attr.class "dropdown" ]
        [ Html.summary
            [ Attr.attribute "role" "button"
            , Attr.class "secondary"
            ]
            [ Html.text "Edit Grouping" ]
        , Html.ul []
            [ Html.li
                [ Event.onClick <|
                    Split BlockT token.identifier
                ]
                [ Html.text "Split Block" ]
            , Html.li
                [ Event.onClick <|
                    Split LineT token.identifier
                ]
                [ Html.text "Split Line" ]
            , Html.li
                [ Event.onClick <|
                    Merge BlockT token.identifier
                ]
                [ Html.text "Merge Block" ]
            , Html.li
                [ Event.onClick <|
                    Merge LineT token.identifier
                ]
                [ Html.text "Merge Line" ]
            ]
        ]
    ]


viewRangeEditor : ( Int, Int ) -> List (Html.Html Msg)
viewRangeEditor sel =
    []


viewEditorPanel : Model -> Html.Html Msg
viewEditorPanel model =
    case model.editing of
        Just (Single identifier) ->
            model.tokens
                |> Dict.get identifier
                |> Maybe.map viewSingleEditor
                |> Maybe.withDefault []
                |> Html.div [ Attr.class "token-edit-bar" ]

        Just (Range sel) ->
            Html.div [ Attr.class "grid" ] <|
                viewRangeEditor sel

        Nothing ->
            Html.text ""


viewTokenCell :
    { editing : Bool
    , inRange : Bool
    , token : Token
    }
    -> Html.Html Msg
viewTokenCell { editing, inRange, token } =
    Html.td
        [ Attr.classList
            [ ( "token-cell", True )
            , ( "editing-token", editing )
            , ( "editing-range", inRange )
            ]
        , Attr.id (String.fromInt token.identifier)
        , Event.onMouseUp TokenFocused
        ]
        [ Html.text token.string ]


viewOrDefault :
    Html.Html Msg
    -> Bool
    -> Html.Html Msg
    -> Html.Html Msg
viewOrDefault default condition viewHtml =
    if condition then
        viewHtml

    else
        default


viewToken : Model -> Token -> Html.Html Msg
viewToken model token =
    let
        default =
            { editing = False
            , inRange = False
            , token = token
            }

        range sel =
            ( getIndex (first sel) model
            , getIndex (second sel) model
            )

        index =
            getIndex token.identifier model

        orDefault =
            viewOrDefault (viewTokenCell default)

        none =
            Html.text ""
    in
    Html.table
        [ Attr.style "display" "inline" ]
        [ Html.tr []
            [ case model.editing of
                Nothing ->
                    orDefault False none

                Just (Single identifier) ->
                    viewTokenCell { default | editing = True }
                        |> orDefault
                            (identifier == token.identifier)

                Just (Range sel) ->
                    let
                        r =
                            range sel
                    in
                    viewTokenCell { default | inRange = True }
                        |> orDefault
                            (first r <= index && second r >= index)
            ]
        ]


processText : Int -> String -> Dict Int Token
processText counter input =
    let
        wordToToken index word =
            ( index + counter
            , { string = word
              , meta = Meta Set.empty Dict.empty
              , identifier = index + counter
              }
            )
    in
    input
        |> String.toLower
        |> String.filter (\x -> isAlpha x || isSeparator x)
        |> String.words
        |> List.indexedMap wordToToken
        |> Dict.fromList


selectionD : D.Decoder (Maybe ( Int, Int ))
selectionD =
    D.nullable <|
        D.map2 Tuple.pair
            (D.field "left" D.int)
            (D.field "right" D.int)


getIndex : Int -> Model -> Int
getIndex identifier model =
    List.Extra.elemIndex identifier model.order
        |> Maybe.withDefault (List.length model.order)


exampleText : String
exampleText =
    "Dodani memeide mono memeide inanite boyo epene ime epene ompadike dao doa go onowede go ne goñe doobe koña koña anike doobe bao bao me booko tei. Boto kodomai ante onowede godani koña koña ante me bo kote bao bao ameñonga oonge weno ta ñemiñenani wei tei wenamai toma onoga ñawoke boyo epodo onoga oñonani onoga mei onoga mei onoga mei mono tomemo aya epede oñoñomo penai gawadani ate bo kote weginpa ante.\n Boyo inte dodani apededani manomai baga timpa ante boyo koga timpa dodani ante. Ñowo poni ñowo moni mini kewemoni gawamoni boto imo ino mani ñomo bo ko weta aye ino bo ko weta manino bo koka penke penke meñate wodo wenta. Dodani monomai boyo dodani kogatima ante apededani. Maninke aye adoke."

      
key1 : ( ( a, b ), v ) -> a
key1 =
    first >> first


key2 : ( ( a, b ), v ) -> b
key2 =
    first >> second
