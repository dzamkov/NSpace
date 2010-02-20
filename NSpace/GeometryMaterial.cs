﻿//----------------------------------------
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
    /// Represents a material which modifies the geometry of the supplied mesh.
    /// </summary>
    public abstract class GeometryMaterial : Material
    {
        /// <summary>
        /// Gets the altered geometry created by this material.
        /// </summary>
        public abstract Mesh Output { get; }

        /// <summary>
        /// Gets or sets the material that is applied to the output of
        /// this material when rendering.
        /// </summary>
        public Material BaseMaterial
        {
            get
            {
                return this._BaseMaterial;
            }
            set
            {
                this._BaseMaterial = value;
            }
        }

        public override void Render()
        {
            if (this._BaseMaterial != null)
            {
                Mesh output = this.Output;
                if (this._BaseMaterial.Mesh != output)
                {
                    this._BaseMaterial.Mesh = output;
                }
                this._BaseMaterial.Render();
            }
        }

        private Material _BaseMaterial;
    }

    /// <summary>
    /// Geometry material that puts spikes on the base mesh.
    /// </summary>
    public class SpikyMaterial : GeometryMaterial
    {
        public SpikyMaterial(double Length)
        {
            this._Length = Length;
        }

        public override Mesh Output
        {
            get
            {
                if (this._Output == null && this.Mesh != null)
                {
                    Mesh output = new SimpleMesh();
                    this._Output = output;
                    Mesh.IEditContext ec = output.GetEditContext();
                    foreach (Triangle tri in this.Mesh.Triangles)
                    {
                        Vector norm = tri.Normal;
                        Point center = tri.Points[0].Copy();
                        center.Mix(tri.Points[1], 0.5);
                        center.Mix(tri.Points[2], 1.0 / 3.0);
                        center.Position += tri.Normal * this._Length;

                        for (int t = 0; t < 3; t++)
                        {
                            Triangle ntri = new Triangle();
                            ntri.Points[0] = tri.Points[t];
                            ntri.Points[1] = tri.Points[(t + 1) % 3];
                            ntri.Points[2] = center;
                            ec.AddTriangle(ntri);
                        }
                    }
                    ec.Commit();
                }
                return this._Output;
            }
        }

        private double _Length;
        private Mesh _Output;
    }
}
