-----------------------------------------------------------------------------
--
-- Module      :  NSpace.Shape.Volume
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpace.Shape.Volume (
	Volume,
	BoundedVolume,
	VolumeMaterial
) where 

import NSpace.Event
import NSpace.ReferenceFrame
import NSpace.Shape.Surface
import NSpace.Shape.Shape

-- Represents a volume, which is a solid, four dimensional object that can contain
-- points and store information about what is on a specific point (Material-wise)

class (Shape a fr, VolumeMaterial mat) => Volume a fr mat | a -> mat  where
	getMaterialAt			::	a -> ReferenceFrame fr -> Event -> Maybe mat

-- A volume that can be bounded (with a surface) between occupied and unoccupied materials. 
-- The volume is not infinitely large or complex. The resulting surface will face outward
-- towards the unoccupied areas of the volume.
	
class (Volume a fr mat, Surface surf fr smat) => BoundedVolume a fr mat surf smat | a -> surf where
	getSurface				::	a -> Maybe surf

-- A possible substance that can occupy a point in a volume.

class VolumeMaterial a where 
