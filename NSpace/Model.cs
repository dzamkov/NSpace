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
    /// A representation of an object made of triangles that can be rendered.
    /// </summary>
    public class Model
    {
        public Model()
        {
            this._VBO = null;
        }

        /// <summary>
        /// Gets or sets the source of triangles this model displays.
        /// </summary>
        public ISource<Triangle> Source
        {
            get
            {
                return this._Source;
            }
            set
            {
                this._Source = value;
            }
        }

        /// <summary>
        /// Creates a new buffer for the model to store graphics data.
        /// </summary>
        public void RefreshBuffer()
        {
            if (this._VBO != null)
            {
                this.RemoveBuffer();
            }
            this._VBO = new uint[2];
            GL.GenBuffers(2, this._VBO);

            // Being organizing and counting data.
            int numpoint = 0;
            int numtriangle = 0;
            Dictionary<Point, _PointInfo> pointinfo = new Dictionary<Point, _PointInfo>();
            foreach (Triangle tri in this._Source.Items)
            {
                foreach (Point p in tri.Points)
                {
                    _PointInfo info = null;
                    if (!pointinfo.TryGetValue(p, out info))
                    {
                        info = new _PointInfo();
                        info.Index = (ushort)numpoint;
                        info.Usages = new List<Triangle>();
                        pointinfo[p] = info;
                        numpoint++;
                    }
                    info.Usages.Add(tri);
                }
                numtriangle++;
            }

            int pointsize = 10;

            int cur = 0;
            ushort[] indices = new ushort[numtriangle * 3];
            this._Indices = indices.Length;
            foreach(Triangle tri in this._Source.Items)
            {
                foreach(Point p in tri.Points)
                {
                    indices[cur] = pointinfo[p].Index;
                    cur++;
                }
            }

            float[] vertices = new float[numpoint * pointsize];
            foreach(KeyValuePair<Point, _PointInfo> kvp in pointinfo)
            {
                Point point = kvp.Key;
                _PointInfo info = kvp.Value;
                Color col = Color.RGB(0.5, 0.5, 0.5);
                ColoredPoint cp = point as ColoredPoint;
                if (cp != null) 
                {
                    col = cp.Color;
                }
                Vector pos = point.Position;
                Vector norm = new Vector();
                foreach(Triangle tri in info.Usages) 
                {
                    norm = norm + tri.Normal;
                }
                norm.Normalize();
                
                int istart = info.Index * pointsize;
                vertices[istart + 0] = (float)col.R;
                vertices[istart + 1] = (float)col.G;
                vertices[istart + 2] = (float)col.B;
                vertices[istart + 3] = (float)col.A;
                vertices[istart + 4] = (float)norm.X;
                vertices[istart + 5] = (float)norm.Y;
                vertices[istart + 6] = (float)norm.Z;
                vertices[istart + 7] = (float)pos.X;
                vertices[istart + 8] = (float)pos.Y;
                vertices[istart + 9] = (float)pos.Z;
            }

            // Allocate and copy data to buffers
            GL.BindBuffer(BufferTarget.ArrayBuffer, this._VBO[0]);
            GL.BindBuffer(BufferTarget.ElementArrayBuffer, this._VBO[1]);
            GL.BufferData(BufferTarget.ArrayBuffer, (IntPtr)(numpoint * pointsize * sizeof(float)), vertices, BufferUsageHint.StaticDraw);
            GL.BufferData(BufferTarget.ElementArrayBuffer, (IntPtr)(numtriangle * 3 * sizeof(ushort)), indices, BufferUsageHint.StaticDraw);
            
        }

        /// <summary>
        /// Information about a point in the graphics context.
        /// </summary>
        private class _PointInfo
        {
            public ushort Index;
            public List<Triangle> Usages;
        }

        /// <summary>
        /// Deletes and removes graphics data buffers from this model.
        /// </summary>
        public void RemoveBuffer()
        {
            GL.DeleteBuffers(2, this._VBO);
            this._VBO = null;
        }

        /// <summary>
        /// Renders this model to the current graphics context.
        /// </summary>
        public void Render()
        {
            if (this._Source != null)
            {
                if (this._VBO == null)
                {
                    this.RefreshBuffer();
                }
                GL.BindBuffer(BufferTarget.ArrayBuffer, this._VBO[0]);
                GL.BindBuffer(BufferTarget.ElementArrayBuffer, this._VBO[1]);
                GL.InterleavedArrays(InterleavedArrayFormat.C4fN3fV3f, 0, IntPtr.Zero);
                GL.DrawElements(BeginMode.Triangles, this._Indices, DrawElementsType.UnsignedShort, 0);
            }
        }

        private ISource<Triangle> _Source;
        private uint[] _VBO;
        private int _Indices;
    }
}
