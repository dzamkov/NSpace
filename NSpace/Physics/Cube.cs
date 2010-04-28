//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// A very clear and well defined cube.
    /// </summary>
    public class Cube : IVolume
    {
        public Cube(Section Section)
        {
            this._Section = Section;
        }

        /// <summary>
        /// Gets the section this cube is oriented in. The center of the box is
        /// at the origin of the section. The edge length of the cube is 1 in section
        /// units.
        /// </summary>
        public Section Section
        {
            get
            {
                return this._Section;
            }
        }

        void IConvertible<IVolume>.Convert<O>(out O Object)
        {
            if (typeof(O) == typeof(Cube))
            {
                Object = (O)(object)this;
                return;
            }
            Object = null;
        }

        private Section _Section;
    }
}