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
    /// Represents a material, which gives a model its surface and visual properties. An instance
    /// of material is related to a mesh that will give geomerty information.
    /// </summary>
    public abstract class Material
    {
        /// <summary>
        /// Gets the mesh this material is for.
        /// </summary>
        public Mesh Mesh
        {
            get
            {
                return this._Mesh;
            }
            set
            {
                this._Mesh = value;
                this.OnMeshChange();
            }
        }

        /// <summary>
        /// Called when the mesh this material is associated to changes.
        /// </summary>
        protected virtual void OnMeshChange()
        {

        }

        /// <summary>
        /// Renders the material applied to its associated mesh.
        /// </summary>
        public abstract void Render();

        private Mesh _Mesh;
    }

    /// <summary>
    /// Represents a material that stores vertex or indice data in a set of buffers. On these materials, there is
    /// assumed to be a direct relation between vertices and indices in the buffer and the supplied mesh data.
    /// </summary>
    public abstract class BufferedMaterial : Material
    {
        /// <summary>
        /// Gets the stride of a single vertex in this material, that is the size of a vertex in the buffer in bytes.
        /// </summary>
        public abstract int VertexStride { get; }

        /// <summary>
        /// Fills in the data for a single vertex
        /// </summary>
        /// <param name="Vertex">A pointer to the start of the data for the vertex. The data written here
        /// should not be longer then vertex stride.</param>
        /// <param name="Point">The point this vertex is meant to represent.</param>
        /// <param name="Usages">The triangles in this material's mesh that have this vertex as
        /// one of their three points.</param>
        public unsafe virtual void FillVertexData(void* Vertex, Geometry Point, List<Geometry> Usages)
        {

        }

        /// <summary>
        /// Sets the vertex format of the graphics context to match that of vertex data.
        /// </summary>
        public virtual void SetVertexFormat()
        {

        }

        /// <summary>
        /// Called before rendering of the buffer.
        /// </summary>
        public virtual void PreRender()
        {

        }

        public override void Render()
        {
            if (this._VBO == null)
            {
                this._BuildPointData();
                this._AllocateBuffers();
                this._FillIndiceBuffer();
                this._FillVertexBuffer();
            }
            GL.BindBuffer(BufferTarget.ArrayBuffer, this._VBO[0]);
            GL.BindBuffer(BufferTarget.ElementArrayBuffer, this._VBO[1]);
            this.SetVertexFormat();
            this.PreRender();
            GL.DrawElements(BeginMode.Triangles, this.Mesh.TriangleCount * 3, DrawElementsType.UnsignedShort, 0);
        }

        protected override void OnMeshChange()
        {
            this.ClearBuffers();
        }

        /// <summary>
        /// Clears and deletes vertice and indice buffers, freeing memory.
        /// </summary>
        public void ClearBuffers()
        {
            if (this._VBO != null)
            {
                GL.DeleteBuffers(2, this._VBO);
                this._VBO = null;
            }
        }

        /// <summary>
        /// Additional information about a point representing its relations within a mesh.
        /// </summary>
        private class _PointInfo
        {
            /// <summary>
            /// Index of the point in the vertex buffer.
            /// </summary>
            public ushort Index;

            /// <summary>
            /// Triangles that use this point.
            /// </summary>
            public List<Geometry> Usages;
        }

        /// <summary>
        /// Creates additional point info for all points within this mesh.
        /// </summary>
        private void _BuildPointData()
        {
            ushort curindex = 0;
            this._PointData = new Dictionary<Geometry, _PointInfo>();
            foreach (Geometry tri in this.Mesh.Triangles)
            {
                foreach (Geometry p in tri.GetData<Triangle.Data>().Points)
                {
                    _PointInfo info = null;
                    if (!this._PointData.TryGetValue(p, out info))
                    {
                        info = new _PointInfo();
                        info.Index = curindex;
                        info.Usages = new List<Geometry>();
                        this._PointData[p] = info;
                        curindex++;
                    }
                    info.Usages.Add(tri);
                }
            }
        }

        /// <summary>
        /// Allocates the vertex and index buffers for this material.
        /// </summary>
        private void _AllocateBuffers()
        {
            if (this._VBO == null)
            {
                this._VBO = new uint[2];
                GL.GenBuffers(2, this._VBO);
                GL.BindBuffer(BufferTarget.ArrayBuffer, this._VBO[0]);
                GL.BindBuffer(BufferTarget.ElementArrayBuffer, this._VBO[1]);
                GL.BufferData(BufferTarget.ArrayBuffer, (IntPtr)(this.VertexStride * this._PointData.Count), IntPtr.Zero, BufferUsageHint.StaticDraw);
                GL.BufferData(BufferTarget.ElementArrayBuffer, (IntPtr)(sizeof(ushort) * this.Mesh.TriangleCount * 3), IntPtr.Zero, BufferUsageHint.StaticDraw);
            }
        }

        /// <summary>
        /// Completely fills the indice buffer with all triangle data.
        /// </summary>
        private unsafe void _FillIndiceBuffer()
        {
            GL.BindBuffer(BufferTarget.ElementArrayBuffer, this._VBO[1]);
            ushort* indicedata = (ushort*)(GL.MapBuffer(BufferTarget.ElementArrayBuffer, BufferAccess.WriteOnly).ToPointer());
            foreach (Geometry tri in this.Mesh.Triangles)
            {
                foreach (Geometry p in tri.GetData<Triangle.Data>().Points)
                {
                    *indicedata = this._PointData[p].Index;
                    indicedata++;
                }
            }
            GL.UnmapBuffer(BufferTarget.ElementArrayBuffer);
        }

        /// <summary>
        /// Completely fills the vertex buffer with all point data.
        /// </summary>
        private unsafe void _FillVertexBuffer()
        {
            GL.BindBuffer(BufferTarget.ArrayBuffer, this._VBO[0]);
            byte* vertexdata = (byte*)GL.MapBuffer(BufferTarget.ArrayBuffer, BufferAccess.WriteOnly).ToPointer();
            foreach (KeyValuePair<Geometry, _PointInfo> kvp in this._PointData)
            {
                int index = kvp.Value.Index;
                this.FillVertexData(vertexdata + (index * this.VertexStride), kvp.Key, kvp.Value.Usages);
            }
            GL.UnmapBuffer(BufferTarget.ArrayBuffer);
        }

        private uint[] _VBO;
        private Dictionary<Geometry, _PointInfo> _PointData;
    }

    /// <summary>
    /// A material that uses colors and shading for visual properties.
    /// </summary>
    public class ColorNormalMaterial : BufferedMaterial
    {
        public ColorNormalMaterial(Color DefaultColor)
        {
            this._DefaultColor = DefaultColor;
        }

        public override int VertexStride
        {
            get
            {
                return 10 * sizeof(float);
            }
        }

        public override void SetVertexFormat()
        {
            GL.InterleavedArrays(InterleavedArrayFormat.C4fN3fV3f, 0, IntPtr.Zero);
        }

        public override unsafe void FillVertexData(void* Vertex, Geometry Point, List<Geometry> Usages)
        {
            float* vert = (float*)Vertex;
            Color col = this._DefaultColor;

            Point.ColorData coldata = Point.GetData<Point.ColorData>();
            col = coldata == null ? col : coldata.Color;

            Vector pos = Point.GetData<Point.Data>().Position;
            Vector norm = new Vector();
            foreach (Geometry tri in Usages)
            {
                norm = norm + tri.GetData<Triangle.Data>().Normal;
            }
            norm.Normalize();

            vert[0] = (float)col.R;
            vert[1] = (float)col.G;
            vert[2] = (float)col.B;
            vert[3] = (float)col.A;
            vert[4] = (float)norm.X;
            vert[5] = (float)norm.Y;
            vert[6] = (float)norm.Z;
            vert[7] = (float)pos.X;
            vert[8] = (float)pos.Y;
            vert[9] = (float)pos.Z;
        }

        private Color _DefaultColor;
    }
}
