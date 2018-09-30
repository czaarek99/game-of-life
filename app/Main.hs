{-# LANGUAGE RecordWildCards  #-}
module Main where
{-|
Author: Tomas Möre
-}
import Program.Loop
import qualified Graphics.Cube as Cube
import qualified Graphics.Init as Init
import qualified Graphics.Shaders as Shaders

import qualified Graphics.UI.GLFW as GLFW
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Control.Monad

import System.Directory

import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Input.Events

vertexShader = "shaders/cube.vert"
fragmentShader =  "shaders/cube.frag"

type Cell = (Float, Float)
type Camera = (Float, Float, Float)
type CellSet = Set Cell

getCameraX :: Camera -> Float
getCameraX (x, _, _) = x

getCameraY :: Camera -> Float
getCameraY (_, y, _) = y

getCameraZ :: Camera -> Float
getCameraZ (_, _, z) = z

-- | Example state of the program. A list may not be the best choce.
-- You can roll with it or chose other datastructures.
data State = State { 
    cells :: CellSet , 
    camera :: Camera,
    running :: Bool
}

instance HasDrawData State where
  cubes = cells
  cameraPosition = camera

defaultState :: State
defaultState = State Set.empty (0, 0, 4) False

update :: Event -> State -> State
update LeftArrow state = 
    moveCamera (-1, 0, 0) state

update RightArrow state = 
    moveCamera (1, 0, 0) state

update UpArrow state = 
    moveCamera (0, 1, 0) state

update DownArrow state = 
    moveCamera (0, -1, 0) state

update (CharPress char) state  
    | char == 'j' = moveCamera (0, 0, 1) state
    | char == 'k' = moveCamera (0, 0, -1) state
    | otherwise = state

update Space state = 
    State (cells state) (camera state) (not $ running state)

update Enter state = 
    if running state then
        state
    else
        if hasCell cellToChange oldCells then
            State newCellsKilled (camera state) (running state)
        else
            State newCellsBorn (camera state) (running state)
    where
        cam = camera state
        oldCells = cells state
        cellToChange = (getCameraX cam, getCameraY cam)
        newCellsBorn = makeCell cellToChange oldCells
        newCellsKilled = killCell cellToChange oldCells 


update Generation state = 
    if isRunning then
        State newCellMap (camera state) (running state)
    else
        state
    where
        cellMap = cells state
        newCellMap = Set.filter (\cell -> survives cell cellMap) (getAllActive cellMap)
        isRunning = running state

hasCell :: Cell -> CellSet -> Bool
hasCell cell cells = 
    Set.member cell cells

killCell :: Cell -> CellSet -> CellSet
killCell cell cells =
    Set.delete cell cells

makeCell :: Cell -> CellSet -> CellSet
makeCell cell cells = 
    Set.insert cell cells

offset :: Cell -> (Float, Float) -> Cell
offset (x, y) (dx, dy) =
    (x + dx, y + dy)

getNeighbors :: Cell -> CellSet
getNeighbors cell = 
    Set.fromList [
        offset cell (dx, dy)
        | dx <- offsets, dy <- offsets,
        not (dx == 0 && dy == 0)
    ]
    where
        offsets = [-1, 0, 1]

getLivingNeighbors :: Cell -> CellSet -> CellSet
getLivingNeighbors cellToCheck cellGrid =
    Set.filter (\cell -> hasCell cell cellGrid) (getNeighbors cellToCheck)

getAllActive :: CellSet -> CellSet
getAllActive cells = 
    Set.union cells $ Set.unions [getNeighbors cell | cell <- Set.toList cells]

survives :: Cell -> CellSet -> Bool
survives cell cells =
    if hasCell cell cells then
        livingNeighborSize == 3 || livingNeighborSize == 2
    else
        livingNeighborSize == 3
    where
        livingNeighbors = getLivingNeighbors cell cells
        livingNeighborSize = Set.size livingNeighbors

moveCamera :: Camera -> State -> State
moveCamera (x, y, z) prevState =
    State (cells prevState) (getCameraX cam + x,
       getCameraY cam + y, getCameraZ cam + z) (running prevState)
    where
        cam = camera prevState

{-| The main simply initiates the rendering and starts the gameloop with
the an update function for your specific games logic.
Remember that this basicly just can do a 2D grid of cubes.
-}
main :: IO ()
main = do
  renderDataEither <- loadProgram
  case renderDataEither  of
    Right (window, renderData) -> do
      runProgram window
                 renderData
                 defaultState
                 update
    Left e -> putStrLn e

{-| Some bootstap codeupdateGameOfLife
-}
loadProgram :: IO (Either String (GLFW.Window, Cube.RenderData))
loadProgram = runExceptT $ do
  window <- ExceptT $ Init.makeBasicWindow "Hello world"
  liftIO $ GLFW.makeContextCurrent (Just window)
  vertexExists <- liftIO $ doesFileExist vertexShader
  unless vertexExists $ throwE ("Vertex shader file doesn't exist: " ++ vertexShader)
  fragmentExists <- liftIO $ doesFileExist fragmentShader
  unless fragmentExists $  throwE ("fragment shader file doesn't exist: " ++ fragmentShader)
  baseProgram <- ExceptT $ Shaders.loadProgram vertexShader fragmentShader
  renderData <- liftIO $ Cube.initRenderData baseProgram
  pure (window, renderData)
