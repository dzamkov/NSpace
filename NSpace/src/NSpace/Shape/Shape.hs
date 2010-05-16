-----------------------------------------------------------------------------
--
-- Module      :  NSpace.Shape.Shape
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpace.Shape.Shape (
	Shape,
	StaticShape
) where 

import NSpace.ReferenceFrame

-- Represents a four dimensional area in spacetime that is made of some kind
-- of substance. The coordinates and measurements of shapes are given in terms
-- of a frame of reference.

class (FrameRelation b) => Shape a b where


-- A shape that may be static in terms of a frame of reference. That is, to say,
-- the shape is the same in relation to the frame of reference at every time.

class (Shape a b) => StaticShape a b where 
	getStaticFrame			::	Maybe (ReferenceFrame b)