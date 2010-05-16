-----------------------------------------------------------------------------
--
-- Module      :  NSpace.Event
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpace.Event (
	Event(..)
) where

import NSpace.Vector

-- An event is a point in four-dimensional space time, specified with an 
-- actual position and the time the point is at that position.

data Event	=	Event {
						getPosition	::	Vector,
						getTime		::	Double } deriving (Eq, Show)