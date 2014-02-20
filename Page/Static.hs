module Page.Static (
    render
) where

import Data.List

import URL

render :: ResourcePath -> IO String
render path = do
    let filePath = "static/" ++ intercalate "/" path
    contents <- readFile filePath
    return contents
