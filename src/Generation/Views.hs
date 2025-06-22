module Generation.Views where

import Control.Monad (foldM)

import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import qualified Parsing.Xml as Xm
import qualified Generation.Elm as E
import qualified Generation.Svg as Sv


-- TODO: add return of errors to the caller.
genTree :: T.Text -> [Xm.TreeElement] -> E.FunctionDef
genTree vName tElements =
  let
    columns = foldr (\anElement accum -> case anElement of
        Xm.FieldTE attribs nFixes ->
          case procTreeFields attribs of
            Left err ->
              E.td [E.scope "col", E.class_ "px-4 py-3"] [ E.text (T.pack err) ] : accum
            Right (mbName, mbWidget, mbString) ->
              E.td [E.scope "col", E.class_ "px-4 py-3"] [ E.text (fromMaybe (fromMaybe "<no-name>" mbName) mbString) ] : accum
        Xm.ButtonTE attribs ->
          case procTreeButton attribs of
            Left err ->
              E.td [E.scope "col", E.class_ "px-4 py-3"] [ E.text (T.pack err) ] : accum
            Right (mbName, mbHelp, mbString) ->
              E.td [E.scope "col", E.class_ "px-4 py-3"] [ E.button [E.class_ "btn btn-primary" ] [ E.text (fromMaybe (fromMaybe "<no-name>" mbHelp) mbString) ] ] : accum
      ) [] tElements
  in
  E.FunctionDef {
    nameFD = vName
  , typeDef = E.StringTD
  , argsFD = []
  , bodyFD = tableBuilder (vName <> "_fetch") columns
  , events = [vName <> "_fetch"]
  }


procTreeFields :: [Xm.Attribute] -> Either String (Maybe T.Text, Maybe T.Text, Maybe T.Text)
procTreeFields =
  foldM (\(mbName, mbWidget, mbString) anAttrib -> case anAttrib.nameA of
      "name" -> Right (Just anAttrib.valueA, mbWidget, mbString)
      "widget" -> Right (mbName, Just anAttrib.valueA, mbString)
      "string" -> Right (mbName, mbWidget, Just anAttrib.valueA)
      "expand" -> Right (mbName, mbWidget, mbString)
      "tree_invisible" -> Right (mbName, mbWidget, mbString)
      "readonly" -> Right (mbName, mbWidget, mbString)
      "icon" -> Right (mbName, mbWidget, mbString)
      _ -> Left ("Unknown attribute: " <> T.unpack anAttrib.nameA)
    ) (Nothing, Nothing, Nothing)


procTreeButton :: [Xm.Attribute] -> Either String (Maybe T.Text, Maybe T.Text, Maybe T.Text)
procTreeButton =
  foldM (\(mbName, mbHelp, mbString) anAttrib -> case anAttrib.nameA of
      "name" -> Right (Just anAttrib.valueA, mbHelp, mbString)
      "help" -> Right (mbName, Just anAttrib.valueA, mbString)
      "string" -> Right (mbName, mbHelp, Just anAttrib.valueA)
      _ -> Left "Unknown attribute"
    ) (Nothing, Nothing, Nothing)


genForm :: T.Text -> Xm.FormElement -> E.FunctionDef
genForm vName aForm =
  E.FunctionDef {
    nameFD = vName
  , typeDef = E.StringTD
  , argsFD = []
  , bodyFD = E.form [E.class_ "form" ] []
  , events = [vName <> "_insert"]
  }


tableBuilder :: T.Text -> [E.ElmExpr] -> E.ElmExpr
tableBuilder tbodyMid columns =
  E.div
    [ E.class_ "mx-auto max-w-screen-xl px-4 lg:px-12"
    ]
    [ 
      E.div
        [ E.class_ "bg-white dark:bg-gray-800 relative shadow-md sm:rounded-lg overflow-hidden"
        ]
        [ 
          -- Table structure overall header:
          E.div
            [ E.class_ "flex flex-col md:flex-row items-center justify-between space-y-3 md:space-y-0 md:space-x-4 p-4"
            ]
            [ E.div
                [ E.class_ "w-full md:w-1/2"
                ]
                [ E.form
                    [ E.class_ "flex items-center"
                    ]
                    [ E.label
                        [ E.for_ "simple-search"
                        , E.class_ "sr-only"
                        ]
                        [ E.text "Search" ]
                    , E.div
                        [ E.class_ "relative w-full"
                        ]
                        [ E.div
                            [ E.class_ "absolute inset-y-0 left-0 flex items-center pl-3 pointer-events-none"
                            ]
                            [ E.svg
                                [ Sv.attribute "aria-hidden" "true"
                                , Sv.class_ "w-5 h-5 text-gray-500 dark:text-gray-400"
                                , Sv.fill "currentColor"
                                , Sv.viewBox "0 0 20 20"
                                ]
                                [ Sv.path
                                    [ Sv.fillRule "evenodd"
                                    , Sv.d "M8 4a4 4 0 100 8 4 4 0 000-8zM2 8a6 6 0 1110.89 3.476l4.817 4.817a1 1 0 01-1.414 1.414l-4.816-4.816A6 6 0 012 8z"
                                    , Sv.clipRule "evenodd"
                                    ]
                                    []
                                ]
                            ]
                        , E.input
                            [ E.type_ "text"
                            , E.id "simple-search"
                            , E.class_ "bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-primary-500 focus:border-primary-500 block w-full pl-10 p-2 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-primary-500 dark:focus:border-primary-500"
                            , E.placeholder "Search"
                            , E.required True
                            ]
                            []
                        ]
                    ]
                ]
            , E.div
                [ E.class_ "w-full md:w-auto flex flex-col md:flex-row space-y-2 md:space-y-0 items-stretch md:items-center justify-end md:space-x-3 flex-shrink-0"
                ]
                [ E.button
                    [ E.type_ "button"
                    , E.class_ "flex items-center justify-center text-white bg-primary-700 hover:bg-primary-800 focus:ring-4 focus:ring-primary-300 font-medium rounded-lg text-sm px-4 py-2 dark:bg-primary-600 dark:hover:bg-primary-700 focus:outline-none dark:focus:ring-primary-800"
                    ]
                    [ E.svg
                        [ Sv.class_ "h-3.5 w-3.5 mr-2"
                        , Sv.fill "currentColor"
                        , Sv.viewBox "0 0 20 20"
                        , Sv.attribute "aria-hidden" "true"
                        ]
                        [ Sv.path
                            [ Sv.clipRule "evenodd"
                            , Sv.fillRule "evenodd"
                            , Sv.d "M10 3a1 1 0 011 1v5h5a1 1 0 110 2h-5v5a1 1 0 11-2 0v-5H4a1 1 0 110-2h5V4a1 1 0 011-1z"
                            ]
                            []
                        ]
                    , E.text " Add Item " ]
                , E.div
                    [ E.class_ "flex items-center space-x-3 w-full md:w-auto"
                    ]
                    [ E.button
                        [ E.id "actionsDropdownButton"
                        , E.attribute "data-dropdown-toggle" "actionsDropdown"
                        , E.class_ "w-full md:w-auto flex items-center justify-center py-2 px-4 text-sm font-medium text-gray-900 focus:outline-none bg-white rounded-lg border border-gray-200 hover:bg-gray-100 hover:text-primary-700 focus:z-10 focus:ring-4 focus:ring-gray-200 dark:focus:ring-gray-700 dark:bg-gray-800 dark:text-gray-400 dark:border-gray-600 dark:hover:text-white dark:hover:bg-gray-700"
                        , E.type_ "button"
                        ]
                        [ E.svg
                            [ Sv.class_ "-ml-1 mr-1.5 w-5 h-5"
                            , Sv.fill "currentColor"
                            , Sv.viewBox "0 0 20 20"
                            , Sv.attribute "aria-hidden" "true"
                            ]
                            [ Sv.path
                                [ Sv.clipRule "evenodd"
                                , Sv.fillRule "evenodd"
                                , Sv.d "M5.293 7.293a1 1 0 011.414 0L10 10.586l3.293-3.293a1 1 0 111.414 1.414l-4 4a1 1 0 01-1.414 0l-4-4a1 1 0 010-1.414z"
                                ]
                                []
                            ]
                        , E.text " Actions " ]
                    , E.div
                        [ E.id "actionsDropdown"
                        , E.class_ "hidden z-10 w-44 bg-white rounded divide-y divide-gray-100 shadow dark:bg-gray-700 dark:divide-gray-600"
                        ]
                        [ E.ul
                            [ E.class_ "py-1 text-sm text-gray-700 dark:text-gray-200"
                            , E.attribute "aria-labelledby" "actionsDropdownButton"
                            ]
                            [ E.li []
                                [ E.a
                                    [ E.href "#"
                                    , E.class_ "block py-2 px-4 hover:bg-gray-100 dark:hover:bg-gray-600 dark:hover:text-white"
                                    ]
                                    [ E.text "Mass Edit" ]
                                ]
                            ]
                        , E.div
                            [ E.class_ "py-1"
                            ]
                            [ E.a
                                [ E.href "#"
                                , E.class_ "block py-2 px-4 text-sm text-gray-700 hover:bg-gray-100 dark:hover:bg-gray-600 dark:text-gray-200 dark:hover:text-white"
                                ]
                                [ E.text "Delete all" ]
                            ]
                        ]
                    , E.button
                        [ E.id "filterDropdownButton"
                        , E.attribute "data-dropdown-toggle" "filterDropdown"
                        , E.class_ "w-full md:w-auto flex items-center justify-center py-2 px-4 text-sm font-medium text-gray-900 focus:outline-none bg-white rounded-lg border border-gray-200 hover:bg-gray-100 hover:text-primary-700 focus:z-10 focus:ring-4 focus:ring-gray-200 dark:focus:ring-gray-700 dark:bg-gray-800 dark:text-gray-400 dark:border-gray-600 dark:hover:text-white dark:hover:bg-gray-700"
                        , E.type_ "button"
                        ]
                        [ E.svg
                            [ Sv.attribute "aria-hidden" "true"
                            , Sv.class_ "h-4 w-4 mr-2 text-gray-400"
                            , Sv.viewBox "0 0 20 20"
                            , Sv.fill "currentColor"
                            ]
                            [ Sv.path
                                [ Sv.fillRule "evenodd"
                                , Sv.d "M3 3a1 1 0 011-1h12a1 1 0 011 1v3a1 1 0 01-.293.707L12 11.414V15a1 1 0 01-.293.707l-2 2A1 1 0 018 17v-5.586L3.293 6.707A1 1 0 013 6V3z"
                                , Sv.clipRule "evenodd"
                                ]
                                []
                            ]
                        , E.text " Filter ", E.svg
                            [ Sv.class_ "-mr-1 ml-1.5 w-5 h-5"
                            , Sv.fill "currentColor"
                            , Sv.viewBox "0 0 20 20"
                            , Sv.attribute "aria-hidden" "true"
                            ]
                            [ Sv.path
                                [ Sv.clipRule "evenodd"
                                , Sv.fillRule "evenodd"
                                , Sv.d "M5.293 7.293a1 1 0 011.414 0L10 10.586l3.293-3.293a1 1 0 111.414 1.414l-4 4a1 1 0 01-1.414 0l-4-4a1 1 0 010-1.414z"
                                ]
                                []
                            ]
                        ]
                    , E.div
                        [ E.id "filterDropdown"
                        , E.class_ "z-10 hidden w-48 p-3 bg-white rounded-lg shadow dark:bg-gray-700"
                        ]
                        [ E.h6
                            [ E.class_ "mb-3 text-sm font-medium text-gray-900 dark:text-white"
                            ]
                            [ E.text "Choose brand" ]
                        , E.ul
                            [ E.class_ "space-y-2 text-sm"
                            , E.attribute "aria-labelledby" "filterDropdownButton"
                            ]
                            [ E.li
                                [ E.class_ "flex items-center"
                                ]
                                [ E.input
                                    [ E.id "apple"
                                    , E.type_ "checkbox"
                                    , E.value ""
                                    , E.class_ "w-4 h-4 bg-gray-100 border-gray-300 rounded text-primary-600 focus:ring-primary-500 dark:focus:ring-primary-600 dark:ring-offset-gray-700 focus:ring-2 dark:bg-gray-600 dark:border-gray-500"
                                    ]
                                    []
                                , E.label
                                    [ E.for_ "apple"
                                    , E.class_ "ml-2 text-sm font-medium text-gray-900 dark:text-gray-100"
                                    ]
                                    [ E.text "First Value" ]
                                ]
                            , E.li
                                [ E.class_ "flex items-center"
                                ]
                                [ E.input
                                    [ E.id "fitbit"
                                    , E.type_ "checkbox"
                                    , E.value ""
                                    , E.class_ "w-4 h-4 bg-gray-100 border-gray-300 rounded text-primary-600 focus:ring-primary-500 dark:focus:ring-primary-600 dark:ring-offset-gray-700 focus:ring-2 dark:bg-gray-600 dark:border-gray-500"
                                    ]
                                    []
                                , E.label
                                    [ E.for_ "fitbit"
                                    , E.class_ "ml-2 text-sm font-medium text-gray-900 dark:text-gray-100"
                                    ]
                                    [ E.text "Second Value" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
          , E.div
              [ E.class_ "overflow-x-auto"
              ]
              [ E.table
                  [ E.class_ "w-full text-sm text-left text-gray-500 dark:text-gray-400"
                  ]
                  [ E.thead
                      [ E.class_ "text-xs text-gray-700 uppercase bg-gray-50 dark:bg-gray-700 dark:text-gray-400"
                      ]
                      [
                        E.tr [] columns
                      , E.tbody [ E.id "tbody"] [

                        ]
                      ]
                  ]
              ]
          , E.nav
              [ E.class_ "flex flex-col md:flex-row justify-between items-start md:items-center space-y-3 md:space-y-0 p-4"
              , E.attribute "aria-label" "Table navigation"
              ]
              [
                E.htInvoke E.AHE "tbody" tbodyMid [("offset", E.intL 0)] [
                      E.title ""
                    , E.class_ "flex items-center mt-4 gap-1 font-medium text-primary-700 hover:text-primary-600 hover:underline dark:text-primary-500 dark:hover:text-primary-400"
                  ] [ E.text " Load Table " ]
              , E.span
                  [ E.class_ "text-sm font-normal text-gray-500 dark:text-gray-400"
                  ]
                  [ E.text " Showing ", E.span
                      [ E.class_ "font-semibold text-gray-900 dark:text-white"
                      ]
                      [ E.text "1-10" ]
                  , E.text " of ", E.span
                      [ E.class_ "font-semibold text-gray-900 dark:text-white"
                      ]
                      [ E.text "1000" ]
                  ]
              , E.ul
                  [ E.class_ "inline-flex items-stretch -space-x-px"
                  ]
                  [ E.li []
                      [ E.a
                          [ E.href "#"
                          , E.class_ "flex items-center justify-center h-full py-1.5 px-3 ml-0 text-gray-500 bg-white rounded-l-lg border border-gray-300 hover:bg-gray-100 hover:text-gray-700 dark:bg-gray-800 dark:border-gray-700 dark:text-gray-400 dark:hover:bg-gray-700 dark:hover:text-white"
                          ]
                          [ E.span
                              [ E.class_ "sr-only"
                              ]
                              [ E.text "Previous" ]
                          , E.svg
                              [ Sv.class_ "w-5 h-5"
                              , Sv.attribute "aria-hidden" "true"
                              , Sv.fill "currentColor"
                              , Sv.viewBox "0 0 20 20"
                              ]
                              [ Sv.path
                                  [ Sv.fillRule "evenodd"
                                  , Sv.d "M12.707 5.293a1 1 0 010 1.414L9.414 10l3.293 3.293a1 1 0 01-1.414 1.414l-4-4a1 1 0 010-1.414l4-4a1 1 0 011.414 0z"
                                  , Sv.clipRule "evenodd"
                                  ]
                                  []
                              ]
                          ]
                      ]
                  , E.li []
                      [ E.a
                          [ E.href "#"
                          , E.class_ "flex items-center justify-center text-sm py-2 px-3 leading-tight text-gray-500 bg-white border border-gray-300 hover:bg-gray-100 hover:text-gray-700 dark:bg-gray-800 dark:border-gray-700 dark:text-gray-400 dark:hover:bg-gray-700 dark:hover:text-white"
                          ]
                          [ E.text "1" ]
                      ]
                  , E.li []
                      [ E.a
                          [ E.href "#"
                          , E.class_ "flex items-center justify-center text-sm py-2 px-3 leading-tight text-gray-500 bg-white border border-gray-300 hover:bg-gray-100 hover:text-gray-700 dark:bg-gray-800 dark:border-gray-700 dark:text-gray-400 dark:hover:bg-gray-700 dark:hover:text-white"
                          ]
                          [ E.text "2" ]
                      ]
                  , E.li []
                      [ E.a
                          [ E.href "#"
                          , E.attribute "aria-current" "page"
                          , E.class_ "flex items-center justify-center text-sm z-10 py-2 px-3 leading-tight text-primary-600 bg-primary-50 border border-primary-300 hover:bg-primary-100 hover:text-primary-700 dark:border-gray-700 dark:bg-gray-700 dark:text-white"
                          ]
                          [ E.text "3" ]
                      ]
                  , E.li []
                      [ E.a
                          [ E.href "#"
                          , E.class_ "flex items-center justify-center text-sm py-2 px-3 leading-tight text-gray-500 bg-white border border-gray-300 hover:bg-gray-100 hover:text-gray-700 dark:bg-gray-800 dark:border-gray-700 dark:text-gray-400 dark:hover:bg-gray-700 dark:hover:text-white"
                          ]
                          [ E.text "..." ]
                      ]
                  , E.li []
                      [ E.a
                          [ E.href "#"
                          , E.class_ "flex items-center justify-center text-sm py-2 px-3 leading-tight text-gray-500 bg-white border border-gray-300 hover:bg-gray-100 hover:text-gray-700 dark:bg-gray-800 dark:border-gray-700 dark:text-gray-400 dark:hover:bg-gray-700 dark:hover:text-white"
                          ]
                          [ E.text "100" ]
                      ]
                  , E.li []
                      [ E.a
                          [ E.href "#"
                          , E.class_ "flex items-center justify-center h-full py-1.5 px-3 leading-tight text-gray-500 bg-white rounded-r-lg border border-gray-300 hover:bg-gray-100 hover:text-gray-700 dark:bg-gray-800 dark:border-gray-700 dark:text-gray-400 dark:hover:bg-gray-700 dark:hover:text-white"
                          ]
                          [ E.span
                              [ E.class_ "sr-only"
                              ]
                              [ E.text "Next" ]
                          , E.svg
                              [ Sv.class_ "w-5 h-5"
                              , Sv.attribute "aria-hidden" "true"
                              , Sv.fill "currentColor"
                              , Sv.viewBox "0 0 20 20"
                              ]
                              [ Sv.path
                                  [ Sv.fillRule "evenodd"
                                  , Sv.d "M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z"
                                  , Sv.clipRule "evenodd"
                                  ]
                                  []
                              ]
                          ]
                      ]
                  ]
              ]
    
        ]
    ]

