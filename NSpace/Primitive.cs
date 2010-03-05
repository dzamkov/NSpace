//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;
using OpenTK.Graphics.OpenGL;

namespace NSpace
{
    /// <summary>
    /// Contains methods for creating geometric primitives.
    /// </summary>
    public static class Primitive
    {
        /// <summary>
        /// Represents a textured cube.
        /// </summary>
        public class Cube : ICollisionShape, IVisualShape
        {
            public Cube(double EdgeLength, Texture Texture)
            {
                this._Texture = Texture;

                // Create mesh for cube
                double hl = EdgeLength / 2.0;

                this._VBO = new VBO(InterleavedArrayFormat.T2fN3fV3f, 8, 4 * 6, 2 * 3 * 6);
                VBO.EditContext ec = this._VBO.Lock();
                int[] indices = new int[2 * 3 * 6];
                Vector[] vertexs = new Vector[8];

                for (int t = 0; t < 8; t++)
                {
                    vertexs[t] = new Vector(
                        (t % 8) < 4 ? hl : -hl,
                        (t % 4) < 2 ? hl : -hl,
                        (t % 2) < 1 ? hl : -hl);
                }
                for (int t = 0; t < 6; t++)
                {
                    int[] faceinds = new int[4];
                    int m = (t / 2); m = (2 << m) / 2;
                    int r = (t % 2);
                    for (int l = 0; l < 4; l++)
                    {
                        faceinds[l] = (m * r) + (l / m) * m + (l % m);
                        Vector pos = vertexs[faceinds[l]];
                        Vector norm = pos; norm.Normalize();
                        double u = (l % 4) < 2 ? 0.0 : 1.0; double v = (l % 2) < 1 ? 0.0 : 1.0;
                        ec.SetVertexData((uint)(l + t * 4), new float[]
                        {
                            (float)u,
                            (float)v,
                            (float)norm.X,
                            (float)norm.Y,
                            (float)norm.Z,
                            (float)pos.X,
                            (float)pos.Y,
                            (float)pos.Z
                        });
                    }
                    indices[t * 6 + 0] = faceinds[0]; ec.SetIndiceData((uint)(t * 6 + 0), (uint)(t * 4 + 0));
                    indices[t * 6 + 1] = faceinds[1]; ec.SetIndiceData((uint)(t * 6 + 1), (uint)(t * 4 + 1));
                    indices[t * 6 + 2] = faceinds[2]; ec.SetIndiceData((uint)(t * 6 + 2), (uint)(t * 4 + 2));
                    indices[t * 6 + 3] = faceinds[1]; ec.SetIndiceData((uint)(t * 6 + 3), (uint)(t * 4 + 1));
                    indices[t * 6 + 4] = faceinds[3]; ec.SetIndiceData((uint)(t * 6 + 4), (uint)(t * 4 + 3));
                    indices[t * 6 + 5] = faceinds[2]; ec.SetIndiceData((uint)(t * 6 + 5), (uint)(t * 4 + 2));
                }
                this._Mesh = new SimpleMesh(indices, vertexs);
                this._VBO.Unlock();
            }

            public IEnumerable<TraceHit> Trace(Vector Start, Vector Stop)
            {
                return this._Mesh.Trace(Start, Stop);
            }


            public Bound Bound
            {
                get
                {
                    return this._Mesh.Bound;
                }
            }

            public IRenderable Renderable
            {
                get
                {
                    return 
                        new CapabilityRenderable(new EnableCap[] {
                            EnableCap.Lighting,
                            EnableCap.Texture2D
                        }, new TextureRenderable(this._Texture,
                            this._VBO.GetRenderable(0, 12)));
                }
            }

            private SimpleMesh _Mesh;
            private VBO _VBO;
            private Texture _Texture;
        }
    }
}
