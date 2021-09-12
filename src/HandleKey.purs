module HandleKey(mainHandleKey) where
  
import Prelude

import Data.Maybe (Maybe(..),fromMaybe)
import Data.String as String
import Data.Array
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Query.Event (eventListener)
import Halogen.VDom.Driver (runUI)
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

mainHandleKey :: Effect Unit
mainHandleKey = HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

type State = {chars :: String,strarr :: Array String}

data Action 
    = Initialize 
    | HandleKey H.SubscriptionId KE.KeyboardEvent

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
    H.mkComponent
        {
            initialState
        ,   render
        ,   eval: H.mkEval $ H.defaultEval
                {
                    handleAction = handleAction
                ,   initialize   = Just Initialize
                }
        }

initialState :: forall input . input -> State
initialState _ = {chars: "",strarr: []}

render :: forall m . State -> H.ComponentHTML Action () m
render state = 
    let arr = state.strarr 
        f xs = HH.p_ [HH.text xs]
    in
    HH.div_
        ([
            HH.p_ [HH.text "Hold down the shift key and type some characters!"]
        ,   HH.p_ [HH.text "Press ENTER or RETURN to clear and remove the event listener."]
        ] <> map f arr)
        


handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
    Initialize -> do
        document <- H.liftEffect $ document =<< window
        H.subscribe' \sid ->
            eventListener
                KET.keydown
                    (HTMLDocument.toEventTarget document)
                    (map (HandleKey sid) <<< KE.fromEvent)
    
    HandleKey sid ev
        | true -> do
            H.liftEffect $ E.preventDefault $ KE.toEvent ev
            let char = KE.key ev
            when (let x = String.length char in foldr (||) false $ zipWith (==) (replicate 10 x) [0,1,2,3,4,5]  ) do
                H.modify_ \st -> if char == "Enter" then st{chars = st.chars <> "\n",strarr = st.strarr <> [st.chars]} else if char == "Tab" then st {chars = st.chars <> "    "} else st{chars = st.chars <> char}
        
        | KE.key ev == "Enter" -> do
            H.liftEffect $ E.preventDefault (KE.toEvent ev)
            let char = KE.key ev
            H.modify_ \st -> st{chars =  st.chars <> char}
        
        | otherwise -> pure unit