//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// A static substance that describes what makes up a volume.
    /// </summary>
    public interface IVolumeMaterial : IImmutable
    {
        /// <summary>
        /// Gets the surface material this would produce if a volume of the
        /// material was cut with a straight plane.
        /// </summary>
        ISurfaceMaterial SurfaceMaterial { get; }
    }

    /// <summary>
    /// A static material that describes what makes up a surface.
    /// </summary>
    public interface ISurfaceMaterial : IImmutable
    {

    }

    /// <summary>
    /// A volume material defined from the surface material it has.
    /// </summary>
    public class SimpleVolumeMaterial : IVolumeMaterial
    {
        public SimpleVolumeMaterial(ISurfaceMaterial SurfaceMaterial)
        {
            this._SurfaceMaterial = SurfaceMaterial;
        }

        ISurfaceMaterial IVolumeMaterial.SurfaceMaterial
        {
            get
            {
                return this._SurfaceMaterial;
            }
        }

        private ISurfaceMaterial _SurfaceMaterial;
    }
}