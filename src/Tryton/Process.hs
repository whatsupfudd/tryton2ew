{-# LANGUAGE BangPatterns #-}

module Tryton.Process where

import qualified Data.Ord as Do
import qualified Data.List as L
import qualified Data.Map as Mp
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.FilePath ((</>))

import Tryton.Types


consolidateDefinitions :: [(T.Text, [Definition])] -> (Mp.Map T.Text [ModelInstance], ViewDefs)
consolidateDefinitions allDefs =
  let
    (modelDefs, trees, forms, lists, graphs, boards, calendars, errors) =
      foldl (\(modelA, treeA, formA, listA, graphA, boardA, calendarA, errorA) (fileName, someDefs) ->
        foldl (\(modelB, treeB, formB, listB, graphB, boardB, calendarB, errorB) aDef -> case aDef of
          ModelDF modelEle -> (Mp.insertWith (<>) modelEle.modelDF [modelEle] modelB, treeB, formB, listB, graphB, boardB, calendarB, errorB)
          TreeDF attrs elements -> (modelB, Mp.insert fileName (attrs, elements) treeB, formB, listB, graphB, boardB, calendarB, errorB)
          FormDF attrs elements -> (modelB, treeB, Mp.insert fileName (attrs, elements) formB, listB, graphB, boardB, calendarB, errorB)
          ListFormDF content -> (modelB, treeB, formB, Mp.insert fileName content listB, graphB, boardB, calendarB, errorB)
          GraphDF attrs elements -> (modelB, treeB, formB, listB, Mp.insertWith (\_ (aL, eL) -> (aL, eL <> elements)) fileName ([],[]) graphB, boardB, calendarB, errorB)
          BoardDF attrs elements -> (modelB, treeB, formB, listB, graphB, Mp.insertWith (\_ (aL, eL) -> (aL, eL <> elements)) fileName ([],[]) boardB, calendarB, errorB)
          CalendarDF attrs elements -> (modelB, treeB, formB, listB, graphB, boardB, Mp.insertWith (\_ (aL, eL) -> (aL, eL <> elements)) fileName ([],[]) calendarB, errorB)
          ParseErrorDF errs -> (modelB, treeB, formB, listB, graphB, boardB, calendarB, Mp.insertWith (<>) fileName [errs] errorB)
        ) (modelA, treeA, formA, listA, graphA, boardA, calendarA, errorA) someDefs
      ) (Mp.empty, Mp.empty, Mp.empty, Mp.empty, Mp.empty, Mp.empty, Mp.empty, Mp.empty) allDefs
  in
  (modelDefs, ViewDefs trees forms lists graphs boards calendars errors)

{-
processDefinition :: (T.Text, [Definition]) -> ViewDefs
processDefinition (fileName, someDefs) =
  let
    (trees, forms, lists, graphs, boards, calendars, errors) =
      foldl (\(treeB, formB, listB, graphB, boardB, calendarB, errorB) aDef -> case aDef of
        TreeDF attrs elements -> (Mp.insert fileName (attrs, elements) treeB, formB, listB, graphB, boardB, calendarB, errorB)
        FormDF attrs elements -> (treeB, Mp.insert fileName (attrs, elements) formB, listB, graphB, boardB, calendarB, errorB)
        ListFormDF content -> (treeB, formB, Mp.insert fileName content listB, graphB, boardB, calendarB, errorB)
        GraphDF attrs elements -> (treeB, formB, listB, Mp.insertWith (\_ (aL, eL) -> (aL, eL <> elements)) fileName ([],[]) graphB, boardB, calendarB, errorB)
        BoardDF attrs elements -> (treeB, formB, listB, graphB, Mp.insertWith (\_ (aL, eL) -> (aL, eL <> elements)) fileName ([],[]) boardB, calendarB, errorB)
        CalendarDF attrs elements -> (treeB, formB, listB, graphB, boardB, Mp.insertWith (\_ (aL, eL) -> (aL, eL <> elements)) fileName ([],[]) calendarB, errorB)
        ParseErrorDF errs -> (treeB, formB, listB, graphB, boardB, calendarB, Mp.insertWith (<>) fileName [errs] errorB)
      ) (Mp.empty, Mp.empty, Mp.empty, Mp.empty, Mp.empty, Mp.empty, Mp.empty) someDefs
  in
  (ViewDefs trees forms lists graphs boards calendars errors)
-}


buildMenuTree :: [MenuItem] -> [MenuItem]
buildMenuTree !items =
  let
    orderedItems = L.sortOn (Do.Down . seqOrdMI) items
    !allIds = Set.fromList (map idMI orderedItems)
    !childrenMap = Mp.fromListWith (++) [ (p, L.sortOn (Do.Down . seqOrdMI) [i]) | i <- orderedItems, p <- maybeToList i.parentMI ]
    attachChildren !i =
        let
          !kids = Mp.findWithDefault [] (idMI i) childrenMap
        in
        i { childrenMI = map attachChildren kids }
    isRoot :: MenuItem -> Bool
    isRoot !i = case i.parentMI of
        Just p  -> not (Set.member p allIds)
        Nothing -> True
    roots = [ attachChildren i | i <- orderedItems, isRoot i ]
    in roots


printMenuTree :: [MenuItem] -> Int -> FilePath -> IO ()
printMenuTree !items !depth !destFilePath =
  TIO.writeFile destFilePath . T.intercalate "\n" $
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
        !namespace = ("n=" <>) <$> i.namespaceMI
        !icon = ("i=" <>) <$> i.iconMI
        !sequence = "s=" <> show i.seqOrdMI
        !piPart = T.intercalate ", " (maybeToList parent <> maybeToList namespace <> maybeToList icon <> [T.pack sequence])
        !details = " [id=" <> i.idMI <> if piPart == "" then "" else (", " <> piPart) <> "]"
        prefix = case depth of
            0 -> label <> details
            _ -> offset <> label <> maybe "" (\i -> " [" <> i <> "]") icon <> "; " <> details
      in
      prefix <> "\n" <> T.intercalate "\n" (printMenuTree' i.childrenMI (depth + 1))
    ) items


printModelInstances :: Mp.Map T.Text [ModelInstance] -> FilePath -> IO ()
printModelInstances defMap destPath =
  TIO.writeFile (destPath </> "ModelInstances.txt") . T.intercalate "\n" $
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
    listOut = map (\(fileName, content) -> "-- list: " <> fileName <> " --\nattrs: "
                  <> T.intercalate "\n  " (map (T.pack . show) content.attributes)
                  <> "\nelements: " <> T.intercalate "\n  " (map (T.pack . show) content.elements)
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
