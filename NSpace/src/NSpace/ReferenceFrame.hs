-----------------------------------------------------------------------------
--
-- Module      :  NSpace.ReferenceFrame
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpace.ReferenceFrame (
	FrameRelation,
	FrameDefinition(..),
	ReferenceFrame,
	absoluteFrame,
	createChildFrame
) where

import NSpace.Event

-- Frames of reference can hold local coordinate systems that are related
-- to other frames of reference. A frame of reference is defined in terms
-- of another called the parent with a relation to it. Relations between
-- frames specify how to transform time and space coordinates to switch
-- between frames.

-- Frame relations show relation between different frames. They can transform
-- events in the coordinate system of one frame of reference to another.

class (Eq a) => FrameRelation a where
	transformEvent		::	a -> Event -> Event
	getInverse			::	a -> a
	identity				::	a
	composition			::	a -> a -> a
	
-- Frame definitions define a frame of reference in terms of another. The
-- parent frame of reference is the one used for reference while the parent
-- relation is a relation that transforms the coordinate space of the defined
-- frame to that of the parent.

data FrameDefinition a	=	FrameDefinition {
										getParent				:: ReferenceFrame a,
										getParentRelation		:: a } deriving (Eq)
									
data ReferenceFrame a	=	ReferenceFrame {
										getDefinition		::	Maybe (FrameDefinition a),
										getLevel				::	Int } deriving (Eq)

-- Gets a frame of reference that can act as an absolute frame of reference with
-- no definition needed. Every absolute frame is equal to every other.
										
absoluteFrame		::	ReferenceFrame a
absoluteFrame		=	ReferenceFrame Nothing 0

-- Creates a child frame with the specified frame definition.

createChildFrame			::	FrameDefinition a -> ReferenceFrame a
createChildFrame d		=	ReferenceFrame (Just d) ((getLevel $ getParent d) + 1)

-- Gets the relation from one frame of reference to another.

getRelation			:: (FrameRelation a) => ReferenceFrame a -> ReferenceFrame a -> a
getRelation x y	=	if 		x == y
							then		identity
							else		undefined