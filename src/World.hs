module World where

import Data.List

data Direction = North | South | East | West | None deriving (Eq, Show)
data Alley = Alley {
   srcRoom :: Room,
   srcDoor :: Maybe Door, 
   targetRoom :: Room, 
   targetDoor :: Maybe Door
   
} deriving (Eq)

instance Show Alley where
  show (Alley sr (Just sd) tr (Just td)) = (name sr) ++ "->" ++ (name tr)
  show _ = "no alley here"

data Door = Door deriving (Show, Eq)
data Room = Room {
    name :: String, 
    westDoor :: (Maybe Door),
    eastDoor :: (Maybe Door)
} deriving (Show, Eq)


containsRoom :: Alley -> Room -> Bool
containsRoom (Alley sr _ tr _) r = r == sr || r == tr

containsAsSourceRoom :: Room -> Alley -> Bool
containsAsSourceRoom r (Alley sr _ _ _) = r == sr

containsAsTargetRoom :: Room -> Alley -> Bool
containsAsTargetRoom r (Alley _ _ tr _) = r == tr

containsAsSourceRoomWithDoor :: Room -> Door -> Alley -> Bool
containsAsSourceRoomWithDoor r d(Alley sr (Just sd) _ _) = r == sr && d == sd

-- This function checks if the alley can be created 
-- so all doors needed are existing.
createAlley :: Room -> Maybe Door -> Room -> Maybe Door -> Maybe Alley
createAlley r1 (Just d1) r2 (Just d2) = Just (Alley r1 (Just d1) r2 (Just d2))
createAlley r1 Nothing _ _ = Nothing
createAlley _ _ r2 Nothing = Nothing


-- This function takes two rooms and a list 
-- of existing alleys and checks if the rooms
-- are connected. If they are, the connecting alley is 
-- returned, otherwise "Nothing"
isConnectedSourceToTarget :: Room -> Room -> [Alley] -> Maybe Alley
isConnectedSourceToTarget sr tr as = checkStuff (intersect ((filter (containsAsSourceRoom sr)) as) ((filter (containsAsTargetRoom tr)) as)) 
     where checkStuff [] = Nothing
           checkStuff (a:as) = Just a

isConnectedAnyDirection :: Room -> Room -> [Alley] -> Maybe Alley
isConnectedAnyDirection sr tr as = checkStuff (isConnectedSourceToTarget sr tr as) (isConnectedSourceToTarget tr sr as)
     where checkStuff (Just a) Nothing = Just a;
           checkStuff Nothing (Just b) = Just b
           checkStuff _ _ = Nothing

-- Checks if the room and the given door have an alley 
-- that leads to another room.		   
hasAlley :: Room -> Maybe Door -> [Alley] -> Maybe Alley
hasAlley r Nothing _ = Nothing
hasAlley r (Just d) as = checkStuff (filter (containsAsSourceRoomWithDoor r d) as)
  where checkStuff [] = Nothing;
        checkStuff (a : _) = Just a

