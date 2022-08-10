{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Dialog (main, Choice (..)) where

import qualified Brick as B
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Util (bg, on)
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (padAll, str)
import qualified Brick.Widgets.Dialog as D
import qualified Graphics.Vty as V
import qualified Options.Applicative as O
import System.Exit (exitFailure, exitSuccess)
import System.Process (callCommand)

data Opts = Opts {dry :: Bool, command :: Maybe Choice}

opts :: O.Parser Opts
opts =
  Opts
    <$> O.switch
      ( O.long "dry-run"
          <> O.help "Dry run"
      )
    <*> O.optional
      ( O.option
          O.auto
          ( O.long "command"
              <> O.short 'c'
              <> O.metavar "COMMAND"
              <> O.help "Command to run"
          )
      )

fullOpts :: O.ParserInfo Opts
fullOpts = O.info (O.helper <*> opts) (O.fullDesc <> O.header "powermenu")

data Choice = Cancel | Shutdown | Reboot | Lock | Suspend | Logout deriving (Show, Eq, Read)

index :: Choice -> Int
index Shutdown = 0
index Reboot = 1
index Lock = 2
index Suspend = 3
index Logout = 4
index Cancel = 5

choices :: [(String, Choice)]
choices =
  -- [ ("Cancel", Cancel),
  [ ("Shutdown", Shutdown),
    ("Reboot", Reboot),
    ("Lock", Lock),
    ("Suspend", Suspend),
    ("Logout", Logout)
  ]

drawUI :: D.Dialog Choice -> [T.Widget ()]
drawUI d = [ui]
  where
    ui = D.renderDialog d $ C.hCenter $ padAll 1 $ B.str "Please select."

appEvent :: D.Dialog Choice -> T.BrickEvent () e -> T.EventM () (T.Next (D.Dialog Choice))
appEvent d (T.VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt cancelState
    V.EvKey V.KEnter [] -> M.halt d
    _ -> M.continue =<< D.handleDialogEvent ev d
appEvent d _ = M.continue d

initialState :: Maybe Choice -> D.Dialog Choice
initialState choice = D.dialog (Just "Powermenu") (Just (maybe 0 index choice, choices)) 100

cancelState :: D.Dialog Choice
cancelState = D.dialog (Just "Powermenu") (Just (0, [("Cancel", Cancel)])) 100

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (D.dialogAttr, V.white `on` V.black),
      (D.buttonAttr, V.black `on` V.white),
      (D.buttonSelectedAttr, bg V.green)
    ]

theApp :: M.App (D.Dialog Choice) e ()
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return,
      M.appAttrMap = const theMap
    }

executeCommand :: Maybe Choice -> IO ()
executeCommand choice =
  case choice of
    Just value -> case value of
      Cancel -> exitSuccess
      Shutdown -> callCommand "systemctl poweroff"
      Reboot -> callCommand "systemctl reboot"
      Lock -> callCommand "i3lock -c 000000"
      Suspend -> callCommand "systemctl suspend"
      Logout -> callCommand "loginctl"
    Nothing -> exitFailure

main :: IO ()
main = do
  (Opts dry command) <- O.execParser fullOpts
  d <- M.defaultMain theApp $ initialState command
  if dry
    then print $ D.dialogSelection d
    else executeCommand $ D.dialogSelection d
