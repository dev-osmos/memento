module Control.Lens.Local where

import Control.Lens ((.~))
import Control.Lens.TH (DefName (TopName), LensRules, lensField)
import Control.Lens.TH qualified as Global (lensRules, makeLensesWith, makePrisms)
import Language.Haskell.TH (DecsQ, Name, mkName, nameBase)

lensRules :: LensRules
lensRules =
  Global.lensRules
    & lensField
    .~ \_ _ n -> [TopName $! mkName $! nameBase n <> "L"]

makeLenses :: Name -> DecsQ
makeLenses = Global.makeLensesWith lensRules <> Global.makePrisms
