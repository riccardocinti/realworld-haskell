-- 1. Write your own “safe” definitions of the standard partial list functions, but make sure they never fail.
-- As a hint, you might want to consider using the following types:

safeHead :: [a] -> Maybe a
safeHead list = case list of
                    (x:_) -> Just x
                    _ -> Nothing

safeTail :: [a] -> Maybe [a]
safeTail list = case list of
                    (_:xs) -> Just xs
                    _ -> Nothing

safeLast :: [a] -> Maybe a
safeLast list = case list of
                    (_:x:_) -> Just (last list)
                    (x:[]) -> Just x
                    _ -> Nothing

safeInit :: [a] -> Maybe [a]
safeInit list = case list of
                    (_:x:_) -> Just (init list)
                    _ -> Nothing
