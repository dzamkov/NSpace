//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// A region, or area in space that is occupied by a certain material and may change over time.
    /// </summary>
    public interface IVolume : IImmutable, IConvertible<IVolume>
    {
        
    }

    /// <summary>
    /// Describes what substance, or material is in a volume at a particular
    /// point, it may change over time.
    /// </summary>
    public interface IVolumeMaterial : IImmutable, IConvertible<IVolumeMaterial>
    {

    }

    /// <summary>
    /// A shaped, or curved, two dimensional region in space that may change over time.
    /// </summary>
    public interface ISurface : IImmutable, IConvertible<ISurface>
    {

    }

    /// Describes what substance, or material is on a surface at a particular
    /// point, it may change over time.
    /// </summary>
    public interface ISurfaceMaterial : IImmutable, IConvertible<ISurfaceMaterial>
    {

    }
}