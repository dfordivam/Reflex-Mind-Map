module Controller where

createNode :: 
  -> Dynamic t NodeID
  -> Event t OpenToggleEv
  -> Event t NodeEditEv
  -> Dynamic t NodePos
  -> NodeID
  -> Text
  -> Node 

createNode selNodeDyn openEv editEv nodePos i t = do
  let
    s = ffor selNodeDyn ((==) i)

    f1 (OpenToggleEv i) b = not b
    f1 _ b = b

    f2 (NodeEditEv i newText) _ = newText
    f2 _ oldtext = oldText
    
    p = ffor nodePos (\m -> join $ M.lookup i m)

  o <- foldDyn f1 True openEv
  t' <- foldDyn f2 t editEv
  
  return $ Node i t' s o p
