﻿//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// A very clear and well defined solid cube.
    /// </summary>
    public class Cube : IUniformShape, IVolume
    {
        public Cube(Section Section, IMaterial Material)
        {
            this._Section = Section;
            this._Material = Material;
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

        void IConvertible<IShape>.Convert<O>(out O Object)
        {
            Object = this as O;
        }

        IMaterial IUniformShape.Material
        {
            get 
            {
                return null;
            }
        }

        ISurface IVolume.Surface
        {
            get 
            { 
                // Create a surface mesh for the cube.
                Vector[] vertices = new Vector[8];
                int[] indices = new int[36];
                for (int t = 0; t < 8; t++)
                {
                    vertices[t] = new Vector(
                        (t % 2) < 1 ? -0.5 : 0.5,
                        (t % 4) < 2 ? -0.5 : 0.5,
                        (t % 8) < 4 ? -0.5 : 0.5);
                }
                for (int c = 0; c < 6; c++)
                {
                    int[] mindices = new int[4];
                    int m = (2 << (c / 2)) / 2;
                    int l = c % 2;
                    for (int t = 0; t < 4; t++)
                    {
                        mindices[t] = (t / m) * m * 2 + (t % m) + m * l;
                    }
                    indices[(c * 6) + 0] = mindices[0];
                    indices[(c * 6) + 1] = mindices[1];
                    indices[(c * 6) + 2] = mindices[2];
                    indices[(c * 6) + 3] = mindices[2];
                    indices[(c * 6) + 4] = mindices[1];
                    indices[(c * 6) + 5] = mindices[3];
                }

                return new SimpleMesh(this._Section, this._Material, vertices, indices);
            }
        }

        bool IVolume.InVolume(Vector Point, Section Section, ref IMaterial Material)
        {
            Vector thispoint = Section.GetRelation(this._Section).SpaceTransform * Point;
            if (thispoint.X >= -0.5 &&
                thispoint.Y >= -0.5 &&
                thispoint.Z >= -0.5 &&
                thispoint.X <= 0.5 &&
                thispoint.Y <= 0.5 &&
                thispoint.Z <= 0.5)
            {
                Material = null;
                return true;
            }
            else
            {
                return false;
            }
        }

        private IMaterial _Material;
        private Section _Section;
    }
}