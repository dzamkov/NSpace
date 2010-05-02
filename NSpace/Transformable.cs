//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;
using OpenTK;
using OpenTK.Graphics.OpenGL;

namespace NSpace
{
    /// <summary>
    /// An object capable of transforming(applying a change that results in
    /// an object of the same type) another object.
    /// </summary>
    /// <typeparam name="T">The transformable type this may interact with.</typeparam>
    /// <typeparam name="P">The type of object this may be transformed.</typeparam>
    public interface ITransformable<T, P>
        where T : ITransformable<T, P>, new()
    {
        /// <summary>
        /// Transforms an object.
        /// </summary>
        P Transform(P Value);

        /// <summary>
        /// Combines two transformables such that x.Combine(y).Transform(z) == x.Transform(y.Transform(z))
        /// </summary>
        T Combine(T Other);

        /// <summary>
        /// Gets the identity transform of this type. The identity transform is one such that
        /// x.Transform(y) == y
        /// </summary>
        T Identity { get; }

        /// <summary>
        /// Gets the inverse transform such that x.Transform(x.Inverse.Transform(y)) == y
        /// </summary>
        T Inverse { get; }
    }
}
