---------------------------------------------------------
{-
	Module		: MeetingRoom
	Name		: Azeem Mumtaz	
	Script		: MeetingRoom.hs -> The Gaming Strategy
				  inside the meeting room
-}
---------------------------------------------------------

module VirtualHms.MeetingRoom where

import Data.Map
import Data.Maybe (fromJust)

menuMap				:: Map Integer String
menuMap 			= fromList [(1, "Start a session"), 
					(2, "Participate for a session"), 
					(3, "Request a projector"), 
					(4, "Request to join a session") , 
					(5, "Leave Room")]

functionMap		:: Map Integer (Integer -> Bool)
functionMap 		= fromList [(1, even),
                        	    (2, odd),
                                    (3, even),
                                    (4, odd),
                                    (5, even)]
								

printMenu		:: [(Integer, String)] -> IO ()
printMenu []		= putStr ""
printMenu (x:xs)	= 
	do 
		putStrLn $ (++) "\t" $ (++) "(" $ (++) (show (fst x)) $ (++) ") " (snd x)
		printMenu xs


meetingRoom1		:: Map k a -> Map k a
meetingRoom1 x		= x

meetingRoom			:: IO ()
meetingRoom			= 
	do
		header
		printMenu $ toList menuMap
		putStr "\n\t\tYour Selection: "
		option <- getOption
		subMenuRouter option
		putStrLn "hehe"

-- Function: subMenuRouter
{-
	This menu routes through the submenus
-}
subMenuRouter		:: Int -> IO ()
subMenuRouter x		=
	do
		if (member (toInteger x) menuMap)
			then
                              	putStrLn $ toString $ (fromJust $ Data.Map.lookup (toInteger x) functionMap) (toInteger x)
			else     
			        putStrLn "Invalid Selection"
		
-- Function: header
{-
	This fancy function displays the header 
-}		
header		:: IO ()
header		=
	do
		putStrLn "\n\t---------------------------------"
		putStrLn "\t You are now in the Meeting Room"
		putStrLn "\t---------------------------------"
		
-- Function: getOption
{-
	This function converts an IO String to Int
-}
getOption 			:: IO Int
getOption 			= 
	do 
		s <- getLine
		return (read s :: Int)

class Visible a where
      toString  :: a -> String

instance Visible Char where
         toString a   = [a]

instance Visible Bool where
         toString True  = "True"
         toString False = "False"

instance Visible a => Visible [a] where
         toString  = concat . Prelude.map toString