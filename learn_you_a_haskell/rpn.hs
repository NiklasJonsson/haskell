
calculateRPN :: (Num a) => String -> a
calculateRPN xs = foldr (\acc c -> handleChar c acc) [] . word xs


handleChar
