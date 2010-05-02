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
    /// Interface used to mark an object as immutable. All nonprivate functions in an immutable
    /// object will always return the same results for the same parameters unless explicitly
    /// stated otherwise. The functions in an immutable object must therfore have their results
    /// based on constant variables within the object and parameters.
    /// </summary>
    /// <remarks>
    /// Every concrete implementation(class) that uses IImmutable must fully represent a logical object.
    /// This means, there can be no placeholder types which dont have all the information needed
    /// to represent an object.
    /// </remarks>
    public interface IImmutable
    {

    }

    /// <summary>
    /// An immutable object that can be exactly converted into another form that still represents the
    /// object(data may be omitted, but all data in the converted form of the object must agree with
    /// the original object).
    /// </summary>
    /// <typeparam name="T">The general types of objects this may be converted to.</typeparam>
    public interface IConvertible<T> : IImmutable
        where T : class, IImmutable
    {
        /// <summary>
        /// Creates a converted form of the object as the specified type. If the conversion is unavailable, the
        /// resulting object will be null. Note that it is valid for O to be any type of T excluding T. If O happens
        /// to be a base of the concrete implementation of this class, Convert should return the object itself.
        /// </summary>
        /// <typeparam name="O">The specific type of object to convert to.</typeparam>
        void Convert<O>(out O Object) where O : class, T;
    }
}
