module Database.SQL.SQLConverter.Gui (
	
 

) where
import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core

startGui :: IO ()
startGui = do
    startGUI defaultConfig
        { jsPort       = 8023
        , jsStatic     = Just "../js"
        } setup
        
setup :: Window -> UI ()
setup window = do
    return window # set UI.title "Hello World!"
    button <- UI.button # set UI.text "Click me!"
    getBody window #+ [element button]
    on UI.click button $ const $ do
        element button # set UI.text "I have been clicked!"