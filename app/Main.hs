{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Main where

import Control.Monad (when, forM_)
import Control.Exception (try)
import System.IO (stdin, hSetEcho, hSetBuffering, hReady, BufferMode (NoBuffering) )
import Control.Monad.State.Strict (StateT, get, modify, runStateT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map as Map
import ViewUtils (clearScreen, showInRectangle, clearRectangle, showInGrid, drawGrid, highlightCell, printFromBottom)
import FreerProgram (Program(..), foldFreer)
import Prelude hiding (log)

data RowData = Row { smth :: String } deriving Eq

initialRows = [
  Row "something a"
  , Row "something b"
  , Row "something c"
  , Row "something d"
  , Row "something e"
  ]

data AppStateData m = AppState
  { rows :: [RowData]
  , activeCellY :: Maybe Int
  , debugMessages :: [String]
  , listeners :: AppStateListenersData m
  }

data AppStateListenersData m = AppStateListeners
  { rowsListeners :: [[RowData] -> m ()]
  , activeCellYListeners :: [Maybe Int -> m ()]
  , debugMessagesListeners :: [[String] -> m ()]
  }

addRowsListener :: Monad m => ([RowData] -> m ()) -> AppStateListenersData m -> AppStateListenersData m
addRowsListener listener (AppStateListeners rowsListeners _activeCellYListeners _debugMessagesListeners) =
  AppStateListeners (listener:rowsListeners) _activeCellYListeners _debugMessagesListeners

addActiveCellYListener :: Monad m => (Maybe Int -> m ()) -> AppStateListenersData m -> AppStateListenersData m
addActiveCellYListener listener (AppStateListeners _rowsListeners activeCellYListeners _debugMessagesListeners) =
  AppStateListeners _rowsListeners (listener:activeCellYListeners) _debugMessagesListeners

addDebugMessagesListener :: Monad m => ([String] -> m ()) -> AppStateListenersData m -> AppStateListenersData m
addDebugMessagesListener listener (AppStateListeners _rowsListeners _activeCellYListeners debugMessagesListeners) =
  AppStateListeners _rowsListeners _activeCellYListeners (listener:debugMessagesListeners)


data EditableListAppI a where
  GetList :: EditableListAppI [RowData]
  GetActiveCellY :: EditableListAppI (Maybe Int)
  GetLogs :: EditableListAppI [String]

  UpdateList :: [RowData] -> EditableListAppI ()
  UpdateActiveCellY :: (Maybe Int) -> EditableListAppI ()
  Log :: String -> EditableListAppI ()

  LiftIO :: IO b -> EditableListAppI b

getList :: Program EditableListAppI [RowData]
getList = Instr $ GetList

getActiveCellY :: Program EditableListAppI (Maybe Int)
getActiveCellY = Instr $ GetActiveCellY

getLogs :: Program EditableListAppI [String]
getLogs = Instr $ GetLogs

updateList :: [RowData] -> Program EditableListAppI ()
updateList x = Instr . UpdateList $ x

updateActiveCellY :: (Maybe Int) -> Program EditableListAppI ()
updateActiveCellY y = Instr . UpdateActiveCellY $ y

log :: String -> Program EditableListAppI ()
log msg = Instr . Log $ msg

instance MonadIO (Program EditableListAppI) where
  liftIO = Instr . LiftIO

newtype DictStateHolder a = Dict (StateT (AppStateData DictStateHolder) IO a) deriving (Functor, Applicative, Monad, MonadIO)

interpret' :: EditableListAppI a -> DictStateHolder a
interpret' GetList = Dict $ rows <$> get
interpret' GetActiveCellY = Dict $ activeCellY <$> get
interpret' GetLogs = Dict $ debugMessages <$> get

interpret' (UpdateList l) = do
  Dict $ modify $ \s -> s { rows = l }
  reacts <- Dict $ (rowsListeners . listeners) <$> get
  forM_ reacts ($ l) -- same as forM_ reacts $ \react -> react l
interpret' (UpdateActiveCellY y) = do
  Dict $ modify $ \s -> s { activeCellY = y }
  s <- Dict $ get
  let reacts = activeCellYListeners (listeners s)
  forM_ reacts ($ y) -- same as forM_ reacts $ \react -> react y
interpret' (Log msg) = do
  Dict $ modify $ \s -> s { debugMessages = take debugLinesCount (msg:(debugMessages s)) }
  logs <- Dict $ debugMessages <$> get
  reacts <- Dict $ (debugMessagesListeners . listeners) <$> get
  forM_ reacts ($ logs) -- same as forM_ reacts $ \react -> react logs
interpret' (LiftIO a) = do
  v <- liftIO a
  return v

interpret :: Program EditableListAppI a -> DictStateHolder a
interpret = foldFreer interpret'

dictStateAction :: AppStateData DictStateHolder -> DictStateHolder a -> IO ()
dictStateAction state (Dict action) = do
  runStateT action state
  return ()

debugLinesCount = 20

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  clearScreen
  --dictStateAction initialState (interpret (do ...))
  dictStateAction initialState $ interpret $ do
    initRows
    loop
  where
    xUpperLeft = 0
    yUpperLeft = 0
    columnCount = 1
    columnWidth = 14
    rowCount = length initialRows

    initialState :: AppStateData DictStateHolder
    initialState = AppState [] Nothing [] initListeners

    initRows :: Program EditableListAppI ()
    initRows = updateList initialRows

    initListeners :: AppStateListenersData DictStateHolder
    -- initListeners =
    --     addRowsListener (interpret . mainRowsListener)
    --     (addActiveCellYListener (interpret . activeCellYListener)
    --     (addDebugMessagesListener (interpret . debugMessagesListener)
    --     (empty)))
    initListeners =
        addRowsListener (interpret . mainRowsListener)
        $ addActiveCellYListener (interpret . activeCellYListener)
        $ addDebugMessagesListener (interpret . debugMessagesListener)
        $ empty
      where
        empty = AppStateListeners [] [] []

    mainRowsListener :: [RowData] -> Program EditableListAppI ()
    mainRowsListener rows = do
      activeCellCoords <- fmap (\y -> (0, y)) <$> getActiveCellY
      liftIO $ showInGrid
                 xUpperLeft
                 yUpperLeft
                 columnCount
                 columnWidth
                 activeCellCoords
                 (map (\row -> [smth row]) rows)
      log "updated rows"

    activeCellYListener :: Maybe Int -> Program EditableListAppI ()
    activeCellYListener activeCellY = do
      let activeCellCoords = fmap (\y -> (0, y)) activeCellY
      liftIO $ drawGrid xUpperLeft yUpperLeft columnWidth columnCount rowCount
      case activeCellCoords of
        Nothing -> return ()
        Just coordsPair -> do
          liftIO $ highlightCell xUpperLeft yUpperLeft columnWidth columnCount rowCount coordsPair
          log "highlighted cell"

    debugMessagesListener :: [String] -> Program EditableListAppI ()
    debugMessagesListener debugMessages = do
      liftIO $ printFromBottom
                 xUpperLeft
                 (yUpperLeft+12+debugLinesCount)
                 debugMessages

    loop :: Program EditableListAppI ()
    loop = do
      key <- liftIO $ getKey
      when (key /= "\ESC") $ do
        case key of
          "\ESC[A" -> do -- up
              activeCellY <- getActiveCellY
              let
                newActiveCellY =
                  case activeCellY of
                    Just y -> Just $ max 0 (y-1)
                    Nothing -> Just 0
              updateActiveCellY newActiveCellY
              log $ "up, " ++ show(newActiveCellY)
              loop
          "\ESC[B" -> do -- down
              activeCellY <- getActiveCellY
              let
                newActiveCellY =
                  case activeCellY of
                    Just y -> Just $ min (rowCount-1) (y+1)
                    Nothing -> Just 0
              updateActiveCellY newActiveCellY
              log $ "down, " ++ show(newActiveCellY)
              loop
          "\n" -> do -- enter
              activeCellY <- getActiveCellY
              rows <- getList
                
              let 
                  eitherValue :: Either String String
                  eitherValue =
                    case activeCellY of
                      Nothing -> Left "there's no selected cell"
                      Just cellIndex ->
                        if cellIndex < 0 || cellIndex >= (length rows)
                          then Left $ "index out of bounds: " ++ (show cellIndex)
                          else Right $ smth $ rows !! cellIndex

                  showEditField :: String -> Program EditableListAppI ()
                  showEditField value = do
                    let
                      txt = "edit cell value:"
                      lentxt = length txt
                      yPos = 0
                      xPos = (columnCount * (columnWidth + 1)) + 3
                      replaceNth lst idx val = if idx < 1 then val:(tail lst) else (head lst) : (replaceNth (tail lst) (idx - 1) val)
                    liftIO $ showInRectangle xPos yPos lentxt [txt, value]
                    key <- liftIO $ getKey
                    case key of
                      "\n" -> do
                        case activeCellY of
                          Nothing -> return ()
                          Just cellIndex -> do
                            liftIO $ clearRectangle xPos yPos lentxt 2
                            rows <- getList
                            updateList $ replaceNth rows cellIndex (Row value)
                            loop
                      "\DEL" -> showEditField (if (length value) == 0 then value else init value)
                      c -> showEditField (value ++ c)
              case eitherValue of
                Left e -> do
                  log $ "error: " ++ (show e)
                  loop
                Right v -> do
                  showEditField v
          "q" -> return ()
          _ -> return ()

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char:chars)
