module Lens.Micro.Local where

import Language.Haskell.TH (DecsQ, Name, mkName, nameBase)
import Lens.Micro ((.~))
import Lens.Micro.TH (DefName (TopName), LensRules, lensField)
import Lens.Micro.TH qualified as Global (lensRules, makeLensesWith)

lensRules :: LensRules
lensRules = Global.lensRules & lensField .~ \_ _ n -> [TopName $! mkName $! nameBase n <> "L"]

makeLenses :: Name -> DecsQ
makeLenses = Global.makeLensesWith lensRules
