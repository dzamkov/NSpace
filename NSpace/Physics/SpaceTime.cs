//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// An compound entity which stores all the entities that have effect in
    /// a region of spacetime.
    /// </summary>
    public interface ISpaceTime : ISpaceTimeEntity
    {

    }

    /// <summary>
    /// An entity that has a different effect troughout out time.
    /// </summary>
    public interface ITemporalEntity : IEntity
    {

    }

    /// <summary>
    /// An entity that has effect in space.
    /// </summary>
    public interface ISpatialEntity : IEntity
    {

    }

    /// <summary>
    /// An entity that has an effect through time and space. The combination
    /// of a temporal and spatial entity.
    /// </summary>
    public interface ISpaceTimeEntity : ITemporalEntity, ISpatialEntity
    {

    }
}