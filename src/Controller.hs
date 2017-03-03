module Controller where

  
main = do

  rec
    ctrlEv <- renderControlPanel ()

    -- Create the view
    nodeEvent <- renderMindMap origNodeList mapDiffEv

    dynState <- foldDynMaybe mainEventHandler initState allEvents

    let 
      allEvents = (Left <$> ctrlEv) <*> (Right <$> nodeEvent)

      openEv = fmap OpenToggleEv $
        getNodeForEvent (Left OpenToggle)

      editEv = fmap NodeEditEv $
        (getNodeForEvent (Left Edit)) ""

      getNodeForEvent ::
           AllEvents -- Filter this event
        -> Event t NodeId -- Get the current selected node
      getNodeForEvent ev =
        tagPromptlyDyn selNodeDyn
        (fmapMaybe filterEv allEvents)
          where filterEv ev = Just ()
                filterEv _ = Nothing

      nodePos = getPos
                  (uniqDyn (fmap nodeTree.mindMap dynState))
                  -- (uniqDyn (fmap nodeList.mindMap dynState))
                
      selNodeDyn = fmap selectedNode dynState
      
      mapDiffEv = tagPromptlyDyn 
        (fmap nodeListDiff dynState) reRenderEvent

      reRenderEvent = fmapMaybe f allEvents
        where
         f (Left InsertChild) = Just ()
         f (Left Delete) = Just ()
         f (Left Cut) = Just ()
         f (Left Paste) = Just ()
         f _ = Nothing

    return ()

-- Try not to change Pos with selection events
getPos ::
     Dynamic t NodeTree
  -- -> Dynamic t NodeList
  -> Dynamic t NodePos
getPos = undefined

mainEventHandler ::
     AllEvent
  -> AppState
  -> Maybe AppState
mainEventHandler = undefined

      -- Diff the map when Add, delete, cut, paste event
      mapDiffEv :: Event t (Map NodeID (Maybe Node))
      mapDiffEv = -- diffMapNoEq m1 m2

createNode :: 
     Dynamic t NodeID
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
