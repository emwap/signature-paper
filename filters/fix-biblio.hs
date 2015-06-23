-- | Fix bibliography for numbered citation styles


import Text.Pandoc.Definition
import Text.Pandoc.JSON

main = toJSONFilter fixBiblio

fixBiblio :: Block -> Block
fixBiblio (Div attr@(_,cls,_) (h:bs))
  | "references" `elem` cls
  = let bs' = [ [Div a [Para $ drop 3 is]] | Div a [Para is] <- bs]
    in  Div attr [h,OrderedList (1,Decimal,Period) bs']
fixBiblio b = b
