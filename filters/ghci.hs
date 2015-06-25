import Text.Pandoc.Definition
import Text.Pandoc.JSON
import Text.Pandoc.Walk

import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe

main = toJSONFilter doCodeBlocks

extractCodeBlock :: String -> Block -> [Block]
extractCodeBlock c b@(CodeBlock (_,cls,_) _)
  | c `elem` cls
  , "skip" `notElem` cls
  = [b]
extractCodeBlock _ _ = []

codeFromBlock :: Block -> String
codeFromBlock (CodeBlock _ code) = code

processCodeBlock :: (MonadInterpreter m) => Block -> m Block
processCodeBlock (CodeBlock (lbl,cls,kvs) code) | "ghci" `elem` cls = do
  res <- eval code
  return $ CodeBlock (lbl,cls,kvs) res
processCodeBlock (CodeBlock attr@(_,cls,_) _) | "hide" `elem` cls = return $ Div attr [Null]
processCodeBlock b = return b

doCodeBlocks :: Pandoc -> IO Pandoc
doCodeBlocks doc = do
  let code = query (extractCodeBlock "haskell") doc
  let args = [ "-i src" ]
  res <- unsafeRunInterpreterWithArgs args $ do
    let lines = unlines $ map codeFromBlock code
    liftIO $ writeFile "code.hs" lines
    loadModules ["code.hs"]
    setTopLevelModules ["Main"]
    -- setImportsQ [ ("Feldspar", Just "F")
    --             , ("Feldspar.Vector", Just "F")
    --             , ("Feldspar.Algorithm.CRC", Just "F")
    --             , ("Feldspar.Compiler.Signature", Nothing)
    --             ]
    -- eval $ unlines $ map codeFromBlock code
    walkM processCodeBlock doc
  case res of
    Left (WontCompile es) -> error $ unlines $ map errMsg es
    Left err              -> error $ show err
    Right r  -> return r
