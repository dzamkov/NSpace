//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// A region, or area in space, occupied by a certain material.
    /// </summary>
    public interface IVolume
    {

    }

    /// <summary>
    /// Describes what substance, or material is in a volume at a particular
    /// point.
    /// </summary>
    public interface IVolumeMaterial
    {

    }

    /// <summary>
    /// A shaped, or curved, two dimensional region in space.
    /// </summary>
    public interface ISurface
    {

    }

    /// <summary>
    /// Describes what substance, or material is on a surface at a particular
    /// point.
    /// </summary>
    public interface ISurfaceMaterial
    {

    }
}