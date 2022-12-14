{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brick
import Brick.Focus
  ( focusGetCurrent,
    focusRingCursor,
  )
import Brick.Forms
  ( Form,
    allFieldsValid,
    checkboxField,
    editPasswordField,
    editShowableField,
    editTextField,
    focusedFormInputAttr,
    formFocus,
    formState,
    handleFormEvent,
    invalidFields,
    invalidFormInputAttr,
    newForm,
    radioField,
    renderForm,
    setFieldValid,
    (@@=),
  )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import Control.Monad.State (gets, modify)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Lens.Micro.TH

data Name
  = NameField
  | AgeField
  | BikeField
  | HandedField
  | PasswordField
  | LeftHandField
  | RightHandField
  | AmbiField
  | AddressField
  deriving (Eq, Ord, Show)

data Handedness = LeftHanded | RightHanded | Ambidextrous
  deriving (Show, Eq)

data UserInfo = UserInfo
  { _name :: T.Text,
    _age :: Int,
    _address :: T.Text,
    _ridesBike :: Bool,
    _handed :: Handedness,
    _password :: T.Text
  }
  deriving (Show)

makeLenses ''UserInfo

mkForm :: UserInfo -> Form UserInfo e Name
mkForm =
  let label s w =
        padBottom (Pad 1) $
          vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w
   in newForm
        [ label "Name"
            @@= editTextField name NameField (Just 1),
          label "Address"
            @@= B.borderWithLabel (str "Mailing")
            @@= editTextField address AddressField (Just 3),
          label "Age"
            @@= editShowableField age AgeField,
          label "Password"
            @@= editPasswordField password PasswordField,
          label "Dominant hand"
            @@= radioField
              handed
              [ (LeftHanded, LeftHandField, "Left"),
                (RightHanded, RightHandField, "Right"),
                (Ambidextrous, AmbiField, "Both")
              ],
          label ""
            @@= checkboxField ridesBike BikeField "Do you ride a bicycle?"
        ]

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (E.editAttr, V.white `on` V.black),
      (E.editFocusedAttr, V.black `on` V.yellow),
      (invalidFormInputAttr, V.white `on` V.red),
      (focusedFormInputAttr, V.black `on` V.yellow)
    ]

draw :: Form UserInfo e Name -> [Widget Name]
draw f = [C.vCenter $ C.hCenter form <=> C.hCenter help]
  where
    form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
    help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
    body =
      str $
        "- Name is free-form text\n"
          <> "- Age must be an integer (try entering an\n"
          <> "  invalid age!)\n"
          <> "- Handedness selects from a list of options\n"
          <> "- The last option is a checkbox\n"
          <> "- Enter/Esc quit, mouse interacts with fields"

app :: App (Form UserInfo e Name) e Name
app =
  App
    { appDraw = draw,
      appHandleEvent = \s ev -> do
        case ev of
          VtyEvent (V.EvResize {}) -> continue s
          VtyEvent (V.EvKey V.KEsc []) -> halt s
          VtyEvent (V.EvKey V.KEnter [])
            | focusGetCurrent (formFocus s) /= Just AddressField -> halt s
          _ -> do
            s' <- handleFormEvent ev s
            continue $ setFieldValid (formState s' ^. age >= 18) AgeField s',
      appChooseCursor = focusRingCursor formFocus,
      appStartEvent = return,
      appAttrMap = const theMap
    }

main :: IO ()
main = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v

      initialUserInfo =
        UserInfo
          { _name = "",
            _address = "",
            _age = 0,
            _handed = RightHanded,
            _ridesBike = False,
            _password = ""
          }
      f =
        setFieldValid False AgeField $
          mkForm initialUserInfo

  initialVty <- buildVty
  f' <- customMain initialVty buildVty Nothing app f

  putStrLn "The starting form state was:"
  print initialUserInfo

  putStrLn "The final form state was:"
  print $ formState f'

  if allFieldsValid f'
    then putStrLn "The final form inputs were valid."
    else putStrLn $ "The final form had invalid inputs: " <> show (invalidFields f')
