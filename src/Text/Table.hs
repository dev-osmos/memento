module Text.Table where

mkTable :: [[Text]] -> [Text]
mkTable = fmap unwords . padColumns

padColumns :: [[Text]] -> [[Text]]
padColumns rows =
  let widths :: [Int] = transpose rows <&> maximum . fmap Text.length
   in rows <&> \row -> zipWith padRight widths row

padRight :: Int -> Text -> Text
padRight width x
  | Text.length x > width = error "padRight: Text.length x > width"
  | otherwise = x <> Text.replicate (width - Text.length x) " "
