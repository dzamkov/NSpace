//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// A shape that occupies an area, or region in space and may change over time.
    /// </summary>
    public interface IShape : IImmutable
    {

    }

    /// <summary>
    /// A shape that occupies a three dimensional volume in space.
    /// </summary>
    public interface IVolume : IShape
    {
        /// <summary>
        /// Gets the surface that defines the shape of the volume.
        /// </summary>
        ISurface Surface { get; }

        /// <summary>
        /// Checks if an event in terms of the specified frame of reference are in the
        /// volume, if so, material is set to the material at the point.
        /// </summary>
        bool InVolume(Event Event, ReferenceFrame Frame, ref IVolumeMaterial Material);
    }

    /// <summary>
    /// A volume made up of only one kind of material.
    /// </summary>
    public interface IUniformVolume : IVolume
    {
        /// <summary>
        /// Gets the material that the volume is made from.
        /// </summary>
        IVolumeMaterial Material { get; }
    }

    /// <summary>
    /// A volume that does not change over time in relation to a frame of
    /// reference.
    /// </summary>
    public interface IStaticVolume : IVolume
    {
        /// <summary>
        /// Gets the frame of reference that the volume does not change in relation to.
        /// </summary>
        ReferenceFrame StaticFrame { get; }
    }

    /// <summary>
    /// A shaped, or curved, two dimensional region in space that may change over time.
    /// </summary>
    public interface ISurface : IShape
    {

    }

    /// <summary>
    /// A surface which is made up of only one kind of material.
    /// </summary>
    public interface IUniformSurface : ISurface
    {
        /// <summary>
        /// Gets the material the surface is made from.
        /// </summary>
        ISurfaceMaterial Material { get; }
    }

    /// <summary>
    /// A surface that does not change over time in relation to a frame
    /// of reference.
    /// </summary>
    public interface IStaticSurface : ISurface
    {
        /// <summary>
        /// Gets the frame of reference that the surface does not change in relation to.
        /// </summary>
        ReferenceFrame StaticFrame { get; }
    }
}