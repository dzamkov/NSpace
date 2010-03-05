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
    /// Representation of a vertex buffer object. This includes both the indice buffer
    /// and the vertex buffer.
    /// </summary>
    public class VBO
    {
        /// <summary>
        /// Creates a vertex buffer object with the specified format, vertex stride, vertex amount and triangle amount.
        /// </summary>
        public VBO(InterleavedArrayFormat Format, uint VertexStride, uint VertexAmount, uint IndiceAmount)
        {
            this._Format = Format;
            this._VertexAmount = VertexAmount;
            this._VertexStride = VertexStride;
            this._IndiceAmount = IndiceAmount;

            this._VBO = new uint[2];
            GL.GenBuffers(2, this._VBO);
            GL.BindBuffer(BufferTarget.ArrayBuffer, this._VBO[0]);
            GL.BindBuffer(BufferTarget.ElementArrayBuffer, this._VBO[1]);
            GL.BufferData(BufferTarget.ArrayBuffer, (IntPtr)(this._VertexStride * VertexAmount), IntPtr.Zero, BufferUsageHint.StaticDraw);
            GL.BufferData(BufferTarget.ElementArrayBuffer, (IntPtr)(sizeof(ushort) * IndiceAmount), IntPtr.Zero, BufferUsageHint.StaticDraw);
        }

        /// <summary>
        /// Gets the amount of bytes in a single vertex.
        /// </summary>
        public uint VertexStride
        {
            get
            {
                return this._VertexStride;
            }
        }

        /// <summary>
        /// Gets the format for the vertexs within this VBO.
        /// </summary>
        public InterleavedArrayFormat Format
        {
            get
            {
                return this._Format;
            }
        }

        /// <summary>
        /// Gets the amount of vertexs in this VBO.
        /// </summary>
        public uint VertexAmount
        {
            get
            {
                return this._VertexAmount;
            }
        }

        /// <summary>
        /// Gets the amount of indices in this VBO.
        /// </summary>
        public uint IndiceAmount
        {
            get
            {
                return this._IndiceAmount;
            }
        }

        /// <summary>
        /// Edit context used for editing the data inside a VBO.
        /// </summary>
        public class EditContext
        {
            internal EditContext(VBO VBO)
            {
                this._VBO = VBO;
            }

            /// <summary>
            /// Sets the data for the specified vertex.
            /// </summary>
            public unsafe void SetVertexData(uint VertexIndex, float[] Data)
            {
                if (_CurrentEditContext == this)
                {
                    float* cur = ((float*)_VertexBuffer) + VertexIndex;
                    for (int l = 0; l < Data.Length; l++)
                    {
                        *cur = Data[l];
                        cur++;
                    }
                }
            }

            /// <summary>
            /// Sets the data for the specified indice. The data should point
            /// to a vertex.
            /// </summary>
            public unsafe void SetIndiceData(uint IndiceIndex, uint Data)
            {
                if (_CurrentEditContext == this)
                {
                    *(_IndiceBuffer + IndiceIndex) = (ushort)Data;
                }
            }

            internal VBO _VBO;
        }

        /// <summary>
        /// Locks the VBO for editing.
        /// </summary>
        public unsafe EditContext Lock()
        {
            if (_CurrentEditContext != null)
            {
                return null;
            }
            else
            {
                GL.BindBuffer(BufferTarget.ArrayBuffer, this._VBO[0]);
                GL.BindBuffer(BufferTarget.ElementArrayBuffer, this._VBO[1]);
                _VertexBuffer = (byte*)GL.MapBuffer(BufferTarget.ArrayBuffer, BufferAccess.WriteOnly).ToPointer();
                _IndiceBuffer = (ushort*)(GL.MapBuffer(BufferTarget.ElementArrayBuffer, BufferAccess.WriteOnly).ToPointer());
                return _CurrentEditContext = new EditContext(this);
            }
        }

        /// <summary>
        /// Unlocks the VBO and dissallows editing on the last edit context.
        /// </summary>
        public void Unlock()
        {
            _CurrentEditContext = null;
            GL.UnmapBuffer(BufferTarget.ArrayBuffer);
            GL.UnmapBuffer(BufferTarget.ElementArrayBuffer);
        }

        /// <summary>
        /// Gets a renderable that can draw this visual mesh. The renderable will draw the
        /// specified amount of triangles from this VBO starting at the specified index.
        /// </summary>
        public IRenderable GetRenderable(uint IndiceStart, uint TriangleAmount)
        {
            return new _VBORenderable() { _IndiceStart = IndiceStart, _TriangleAmount = TriangleAmount, _VBO = this };
        }

        /// <summary>
        /// Renderable for a vbo.
        /// </summary>
        private class _VBORenderable : IRenderable
        {
            public void Render()
            {
                GL.BindBuffer(BufferTarget.ArrayBuffer, this._VBO._VBO[0]);
                GL.BindBuffer(BufferTarget.ElementArrayBuffer, this._VBO._VBO[1]);
                GL.InterleavedArrays(this._VBO.Format, 0, IntPtr.Zero);
                GL.DrawElements(BeginMode.Triangles, (int)this._TriangleAmount * 3, DrawElementsType.UnsignedShort, (int)this._IndiceStart);
            }

            internal VBO _VBO;
            internal uint _IndiceStart;
            internal uint _TriangleAmount;
        }

        private static EditContext _CurrentEditContext;
        internal static unsafe ushort* _IndiceBuffer;
        internal static unsafe byte* _VertexBuffer;

        internal uint[] _VBO;
        private InterleavedArrayFormat _Format;
        private uint _VertexStride;
        private uint _VertexAmount;
        private uint _IndiceAmount;
    }
}
