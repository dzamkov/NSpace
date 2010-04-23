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
    public interface IImmutable
    {

    }
}
