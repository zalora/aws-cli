{-# LANGUAGE StandaloneDeriving #-}

module Common where

import qualified Options.Applicative as O
import qualified Network.AWS.CloudWatch as C

import Control.Monad (liftM)
import Data.Text (Text, pack, split)
import Options.Applicative ((<>))


deriving instance Read C.StandardUnit

instance Read C.Dimension where
    readsPrec _ v = case split (== '=') $ pack v of
        [x, y] -> [(C.dimension x y, "")]
        _ -> []


makeOption :: String -> O.Mod O.OptionFields a
makeOption name = O.long name <> O.metavar ("<" ++ name ++ ">")


text :: O.ReadM Text
text = liftM pack O.str
