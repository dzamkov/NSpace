-----------------------------------------------------------------------------
--
-- Module      :  NSpace.ReferenceFrame
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpace.ReferenceFrame (
	FrameRelation(..),
	Composite(..),
	SpatialFrameRelation(..),
	StaticFrameRelation(..),
	TimeDependentFrameRelation(..),
	SimpleFrameRelation(..),
	FrameDefinition(..),
	ReferenceFrame,
	absoluteFrame,
	createChildFrame,
	createIChildFrame,
	getRelation
) where

import NSpace.Event
import NSpace.Vector

-- Frames of reference can hold local coordinate systems that are related
-- to other frames of reference. A frame of reference is defined in terms
-- of another called the parent with a relation to it. Relations between
-- frames specify how to transform time and space coordinates to switch
-- between frames.

-- Frame relations show relation between different frames. They can transform
-- events in the coordinate system of one frame of reference to another.

class (Eq a, Composite a a a) => FrameRelation a where
	transformEvent		::	a -> Event -> Event
	getInverse			::	a -> a
	identity				::	a

class Composite a b c | a b -> c where
	composition			::	a -> b -> c

-- A frame relation where input position does not affect output time.
	
class (FrameRelation a) => SpatialFrameRelation a where
	transformTime		::	a -> Time -> Time
	
-- A spatial frame relation where input time does not affect output position. Position
-- and time are seperate in these frame relations.

class (SpatialFrameRelation a) => StaticFrameRelation a where
	transformPosition		::	a -> Vector -> Vector
	
-- Frame relation where a "slice" can be taken at a particular time. The resulting frame relation
-- will at all times represent a single time in the original frame relation.
	
class (SpatialFrameRelation a, StaticFrameRelation b) => TimeDependentFrameRelation a b | a -> b where
	sliceFrameRelation	::	a -> Time -> b
	
instance (StaticFrameRelation a) => TimeDependentFrameRelation a a where
	sliceFrameRelation x y		=	x
	
-- A type of frame relation where every instance is an identity frame relation.	

data SimpleFrameRelation 	=	SimpleFrameRelation

instance Eq SimpleFrameRelation where
	a == b		=	True
	
instance FrameRelation SimpleFrameRelation where
	transformEvent _ y	=	y
	getInverse _			=	SimpleFrameRelation
	identity					=	SimpleFrameRelation

instance Composite SimpleFrameRelation SimpleFrameRelation SimpleFrameRelation where
	composition a b		=	SimpleFrameRelation
	
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

createChildFrame			::	(FrameRelation a) => FrameDefinition a -> ReferenceFrame a
createChildFrame d		=	ReferenceFrame (Just d) ((getLevel $ getParent d) + 1)

createIChildFrame									::	(FrameRelation a) => FrameDefinition a -> ReferenceFrame a
createIChildFrame (FrameDefinition a b)	=	createChildFrame (FrameDefinition a (getInverse b))

-- Gets the relation from one frame of reference to another.

getRelation			:: (FrameRelation a) => ReferenceFrame a -> ReferenceFrame a -> a
getRelation x y	=	if 		getLevel x == getLevel y
							then		if		x	==	y
										then	identity
										else	arcRelation x y
							else		if		getLevel x > getLevel y
										then	lowerRelation x y
										else	raiseRelation x y
							
						where
							arcRelation																				::	(FrameRelation a) => ReferenceFrame a -> ReferenceFrame a -> a
							arcRelation (ReferenceFrame (Just a) _) (ReferenceFrame (Just b) _)	=	composition r1 $ composition r2 r3
																														where
																															r1		=	(getParentRelation a)
																															r2		=	(getRelation (getParent a) (getParent b))
																															r3		=	(getInverse $ getParentRelation b)
																					
							lowerRelation											::	(FrameRelation a) => ReferenceFrame a -> ReferenceFrame a -> a
							lowerRelation a (ReferenceFrame (Just b) _)	=	composition r1 r2
																						where
																							r1		=	(getRelation a (getParent b))
																							r2		=	(getInverse $ getParentRelation b)
							
							raiseRelation											::	(FrameRelation a) => ReferenceFrame a -> ReferenceFrame a -> a
							raiseRelation (ReferenceFrame (Just a) _) b	=	composition r1 r2
																						where
																							r1		=	(getParentRelation a)
																							r2		=	(getRelation (getParent a) b)