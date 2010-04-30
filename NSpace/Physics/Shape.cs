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
    /// <typeparam name="T">The specific type of shape this is.</typeparam>
    public interface IShape : IImmutable, IConvertible<IShape>
    {

    }

    /// <summary>
    /// A shape made up of only one kind of material.
    /// </summary>
    public interface IUniformShape : IShape
    {
        /// <summary>
        /// Gets the material that makes up this shape.
        /// </summary>
        IMaterial Material { get; }
    }

    /// <summary>
    /// A shape that does not change over time.
    /// </summary>
    public interface IStaticShape : IShape
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
        /// Checks if a point and time pair in terms of the specified section are in the
        /// volume, if so, material is set to the material at the point.
        /// </summary>
        bool InVolume(Vector Point, Time Time, Section Section, ref IMaterial Material);
    }

    /// <summary>
    /// A substance that has its own set of properties. It may describe what
    /// type of matter makes up a shape. Note that when null is given as a material,
    /// it signifies that the matter the makes a shape is undefined, it can be anything.
    /// </summary>
    public interface IMaterial : IConvertible<IMaterial>
    {

    }

    /// <summary>
    /// A shaped, or curved, two dimensional region in space that may change over time.
    /// </summary>
    public interface ISurface : IShape
    {

    }
}