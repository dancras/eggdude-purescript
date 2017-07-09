module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Prelude (Unit, append, const, pure, show, unit, ($), (+), (-), (>>>))
import Sigment (Component, PureEval, Render, defConfig, init, newComponent, pureEval)
import Sigment.Dom as D
import Sigment.Dom (Node)
import Sigment.Dom.Props as P
import Sigment.Dom.Props (Prop)
import Sigment.Dom.Tweens as T

data Action = Increase | Decrease

type Model = Int

initState :: Int
initState = 0

component :: Component Unit Action Model _
component = newComponent (const initState >>> pure) (pureEval eval) render

eval :: PureEval Action Model
eval Increase = (_ + 1)
eval Decrease = (_ - 1)

defaultStyle :: Prop
defaultStyle = P.style {font: "30px Verdana", fill : "white"}

text :: String -> Array Prop -> Node
text str props = D.text $ append [P.txt str, defaultStyle, P.anchorCenter] props

countUpdate :: Prop
countUpdate = T.updating $ append [T.from [P.scale1 1.0], T.to [P.scale1 2.0], T.duration 300, T.easingCubic.out] T.yoyoOnce

render :: Render Action Model _
render _ state dispatch = D.group' [P.x 120, P.y 200] [
  text "+" [P.onClick (dispatch Increase), P.x 30],
  text "-" [P.onClick (dispatch Decrease), P.x 60],
  text (show state) [P.key "count", P.x 120, countUpdate]
]

main :: forall t45.
  Eff
    ( ref :: REF
    | t45
    )
    (Action
     -> Eff
          ( ref :: REF
          | t45
          )
          Unit
    )
main = do
   init (defConfig {width = 400, height = 400, containerId = "container"}) unit component