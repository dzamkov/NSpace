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
    public interface IVolume
    {
        
    }

    /// <summary>
    /// A volume that has a very clear bound.
    /// </summary>
    public interface IDefiniteVolume : IVolume
    {
        /// <summary>
        /// Gets a surface which bounds the volume in such a way that the inside
        /// of the surface touches the volume.
        /// </summary>
        ISurface Surface { get; }
    }

    /// <summary>
    /// A volume that is only made with one kind of material.
    /// </summary>
    public interface IUniformVolume : IVolume
    {
        /// <summary>
        /// Gets the material that makes up the volume.
        /// </summary>
        IVolumeMaterial Material { get; }
    }

    /// <summary>
    /// Describes what substance, or material is in a volume at a particular
    /// point, it may change over time.
    /// </summary>
    public interface IVolumeMaterial
    {

    }

    /// <summary>
    /// A shaped, or curved, two dimensional region in space that may change over time.
    /// </summary>
    public interface ISurface
    {

    }

    /// <summary>
    /// A surface that is only made with one kind of material.
    /// </summary>
    public interface IUniformSurface : ISurface
    {
        /// <summary>
        /// Gets the material that makes the surface.
        /// </summary>
        ISurfaceMaterial Material { get; }
    }

    /// <summary>
    /// Describes what substance, or material is on a surface at a particular
    /// point, it may change over time.
    /// </summary>
    public interface ISurfaceMaterial
    {

    }
}