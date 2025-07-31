{-# LANGUAGE BangPatterns #-}

module Tryton.Process where

import qualified Data.Map as Mp
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.FilePath ((</>))

import Tryton.Types


processDefinitions :: [(T.Text, [Definition])] -> (Mp.Map T.Text [ClassInstance], ViewDefs)
processDefinitions allDefs =
  let
    (modelDefs, trees, forms, lists, graphs, boards, calendars, errors) =
      foldl (\(modelA, treeA, formA, listA, graphA, boardA, calendarA, errorA) (fileName, someDefs) ->
        foldl (\(modelB, treeB, formB, listB, graphB, boardB, calendarB, errorB) aDef -> case aDef of
          ModelDF modelEle -> (Mp.insertWith (<>) modelEle.modelDF [modelEle] modelB, treeB, formB, listB, graphB, boardB, calendarB, errorB)
          TreeDF attrs elements -> (modelB, Mp.insert fileName (attrs, elements) treeB, formB, listB, graphB, boardB, calendarB, errorB)
          FormDF attrs elements -> (modelB, treeB, Mp.insert fileName (attrs, elements) formB, listB, graphB, boardB, calendarB, errorB)
          ListFormDF attrs elements -> (modelB, treeB, formB, Mp.insert fileName (attrs, elements) listB, graphB, boardB, calendarB, errorB)
          GraphDF attrs elements -> (modelB, treeB, formB, listB, Mp.insertWith (\_ (aL, eL) -> (aL, eL <> elements)) fileName ([],[]) graphB, boardB, calendarB, errorB)
          BoardDF attrs elements -> (modelB, treeB, formB, listB, graphB, Mp.insertWith (\_ (aL, eL) -> (aL, eL <> elements)) fileName ([],[]) boardB, calendarB, errorB)
          CalendarDF attrs elements -> (modelB, treeB, formB, listB, graphB, boardB, Mp.insertWith (\_ (aL, eL) -> (aL, eL <> elements)) fileName ([],[]) calendarB, errorB)
          ParseErrorDF errs -> (modelB, treeB, formB, listB, graphB, boardB, calendarB, Mp.insertWith (<>) fileName [errs] errorB)
        ) (modelA, treeA, formA, listA, graphA, boardA, calendarA, errorA) someDefs
      ) (Mp.empty, Mp.empty, Mp.empty, Mp.empty, Mp.empty, Mp.empty, Mp.empty, Mp.empty) allDefs
  in
  (modelDefs, ViewDefs trees forms lists graphs boards calendars errors)


processDefinition :: (T.Text, [Definition]) -> ViewDefs
processDefinition (fileName, someDefs) =
  let
    (trees, forms, lists, graphs, boards, calendars, errors) =
      foldl (\(treeB, formB, listB, graphB, boardB, calendarB, errorB) aDef -> case aDef of
        TreeDF attrs elements -> (Mp.insert fileName (attrs, elements) treeB, formB, listB, graphB, boardB, calendarB, errorB)
        FormDF attrs elements -> (treeB, Mp.insert fileName (attrs, elements) formB, listB, graphB, boardB, calendarB, errorB)
        ListFormDF attrs elements -> (treeB, formB, Mp.insert fileName (attrs, elements) listB, graphB, boardB, calendarB, errorB)
        GraphDF attrs elements -> (treeB, formB, listB, Mp.insertWith (\_ (aL, eL) -> (aL, eL <> elements)) fileName ([],[]) graphB, boardB, calendarB, errorB)
        BoardDF attrs elements -> (treeB, formB, listB, graphB, Mp.insertWith (\_ (aL, eL) -> (aL, eL <> elements)) fileName ([],[]) boardB, calendarB, errorB)
        CalendarDF attrs elements -> (treeB, formB, listB, graphB, boardB, Mp.insertWith (\_ (aL, eL) -> (aL, eL <> elements)) fileName ([],[]) calendarB, errorB)
        ParseErrorDF errs -> (treeB, formB, listB, graphB, boardB, calendarB, Mp.insertWith (<>) fileName [errs] errorB)
      ) (Mp.empty, Mp.empty, Mp.empty, Mp.empty, Mp.empty, Mp.empty, Mp.empty) someDefs
  in
  (ViewDefs trees forms lists graphs boards calendars errors)


buildMenuTree :: [MenuItem] -> [MenuItem]
buildMenuTree !items =
  let
    !allIds = Set.fromList (map idMI items)
    !childrenMap = Mp.fromListWith (++) [ (p, [i]) | i <- items, p <- maybeToList i.parentMI ]
    attachChildren !i =
        let
          !kids = Mp.findWithDefault [] (idMI i) childrenMap
        in
        i { childrenMI = map attachChildren kids }
    isRoot :: MenuItem -> Bool
    isRoot !i = case i.parentMI of
        Just p  -> not (Set.member p allIds)
        Nothing -> True
    roots = [ attachChildren i | i <- items, isRoot i ]
    in roots


printMenuTree :: [MenuItem] -> Int -> FilePath -> IO ()
printMenuTree !items !depth !destPath =
  TIO.writeFile (destPath </> "tree.txt") . T.intercalate "\n" $
    printMenuTree' items depth
  where
  printMenuTree' :: [MenuItem] -> Int -> [T.Text]
  printMenuTree' !items !depth =
    let
      !offset = T.replicate (depth * 2) " "
    in
    map (\i ->
      let
        !label = maybe ("id=" <> i.idMI) ("menu: " <>) i.nameMI
        !parent = ("p=" <>) <$> i.parentMI
        !icon = ("i=" <>) <$> i.iconMI
        !piPart = T.intercalate ", " (maybeToList parent <> maybeToList icon)
        !details = " [id=" <> i.idMI <> if piPart == "" then "" else (", " <> piPart) <> "]"
        prefix = case depth of
            0 -> label <> details
            _ -> offset <> label <> maybe "" (\i -> " [" <> i <> "]") icon
      in
      prefix <> "\n" <> T.intercalate "\n" (printMenuTree' i.childrenMI (depth + 1))
    ) items


printClassInstances :: Mp.Map T.Text [ClassInstance] -> FilePath -> IO ()
printClassInstances defMap destPath =
  TIO.writeFile (destPath </> "classInstances.txt") . T.intercalate "\n" $
    map (\(pathName, defs) ->
        "-- class: " <> pathName <> " --\n"
        <> T.intercalate "\n" (
          map (\modelEle ->
                  modelEle.idDF <> ":\n"
                  <> T.intercalate "\n" (map (\(k,v) ->
                        "  - " <> k <> ": " <> showField v
                      ) (Mp.toList modelEle.fieldsDF))
          ) defs
        )
      ) (Mp.toList defMap)

printViewDefs :: ViewDefs -> FilePath -> IO ()
printViewDefs viewDefs destPath =
  let
    treeOut = map (\(fileName, (attrs, elements)) -> "-- tree: " <> fileName <> " --\nattrs: "
                  <> T.intercalate "\n  " (map (T.pack . show) attrs)
                  <> "\nelements: " <> T.intercalate "\n  " (map (T.pack . show) elements)
      ) (Mp.toList viewDefs.trees)
    formOut = map (\(fileName, (attrs, elements)) -> "-- form: " <> fileName <> " --\nattrs: "
                  <> T.intercalate "\n  " (map (T.pack . show) attrs)
                  <> "\nelements: " <> T.intercalate "\n  " (map (T.pack . show) elements)
              ) (Mp.toList viewDefs.forms)
    listOut = map (\(fileName, (attrs, elements)) -> "-- list: " <> fileName <> " --\nattrs: "
                  <> T.intercalate "\n  " (map (T.pack . show) attrs)
                  <> "\nelements: " <> T.intercalate "\n  " (map (T.pack . show) elements)
              ) (Mp.toList viewDefs.lists)
    graphOut = map (\(fileName, (attrs, elements)) -> "-- graph: " <> fileName <> " --\nattrs: "
                  <> T.intercalate "\n  " (map (T.pack . show) attrs)
                  <> "\nelements: " <> T.intercalate "\n  " (map (T.pack . show) elements)
              ) (Mp.toList viewDefs.graphs)
    boardOut = map (\(fileName, (attrs, elements)) -> "-- board: " <> fileName <> " --\nattrs: "
                  <> T.intercalate "\n  " (map (T.pack . show) attrs)
                  <> "\nelements: " <> T.intercalate "\n  " (map (T.pack . show) elements)
              ) (Mp.toList viewDefs.boards)
    calendarOut = map (\(fileName, (attrs, elements)) -> "-- calendar: " <> fileName <> " --\nattrs: "
                  <> T.intercalate "\n  " (map (T.pack . show) attrs)
                  <> "\nelements: " <> T.intercalate "\n  " (map (T.pack . show) elements)
              ) (Mp.toList viewDefs.calendars)
    errorOut = map (\(fileName, errors) -> "-- error: " <> fileName <> " --\n"
                  <> T.intercalate "\n" (map T.pack errors)
              ) (Mp.toList viewDefs.errors)
  in
  TIO.writeFile (destPath </> "viewDefs.txt") . T.intercalate "\n" $
    treeOut <> formOut <> listOut <> graphOut <> boardOut <> calendarOut <> errorOut

printViewErrs :: ViewDefs -> FilePath -> IO ()
printViewErrs viewDefs destPath =
  let
    errorOut = map (\(fileName, errors) -> "-- error: " <> fileName <> " --\n"
                  <> T.intercalate "\n" (map T.pack errors)
              ) (Mp.toList viewDefs.errors)
  in
  TIO.writeFile (destPath </> "viewDefs.txt") $ T.intercalate "\n" errorOut


showField :: Field -> T.Text
showField !f = case f.kindF of
  LabelFK -> f.valueF
  EvalFK -> "#<" <> f.valueF <> ">"
  ReferenceFK -> "@<" <> f.valueF <> ">"
