-----------------------------------------------------------------------------
--
-- Module      :  NSpace.Event
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpace.Event (
	Event(..),
	Time
) where

import NSpace.Vector

-- Represents a time within a frame of reference

type Time	=	Double

-- An event is a point in four-dimensional space time, specified with an 
-- actual position and the time the point is at that position.

data Event	=	Event {
						getPosition	::	Vector,
						getTime		::	Time } deriving (Eq, Show)