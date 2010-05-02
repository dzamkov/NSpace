//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// A static surface that can be described using triangles.
    /// </summary>
    public interface IMeshSurface : ISurface, IStaticShape
    {
        /// <summary>
        /// Gets all the triangles that make up this surface.
        /// </summary>
        IEnumerable<IMeshTriangle> Triangles { get; }
    }

    /// <summary>
    /// A mesh with measurementes in only terms of only one different frame of reference.
    /// </summary>
    public interface ISingleSectionMeshSurface : IMeshSurface
    {
        /// <summary>
        /// Gets the frame of reference that all vertices are in terms of in this mesh.
        /// </summary>
        ReferenceFrame Frame { get; }
    }

    /// <summary>
    /// An object within a mesh.
    /// </summary>
    public interface IMeshObject : IImmutable
    {
        /// <summary>
        /// Gets the mesh the object is in.
        /// </summary>
        IMeshSurface Mesh { get; }
    }

    /// <summary>
    /// A triangle within a mesh.
    /// </summary>
    public interface IMeshTriangle : IMeshObject
    {
        /// <summary>
        /// Gets the (hopefully) three vertices that define the endpoints
        /// of the triangle.
        /// </summary>
        IMeshVertex[] Vertices { get; }
    }

    /// <summary>
    /// A vertex within a mesh that acts as an endpoint for triangles.
    /// </summary>
    public interface IMeshVertex : IMeshObject
    {
        /// <summary>
        /// Gets the frame of reference this vertex is in terms of.
        /// </summary>
        ReferenceFrame Frame { get; }

        /// <summary>
        /// Gets the position of the vertex.
        /// </summary>
        Vector Position { get; }
    }

    /// <summary>
    /// A mesh surface that is specified at its creation. All triangles and vertices in the mesh are in the
    /// same frame of reference.
    /// </summary>
    public class SimpleMesh : ISingleSectionMeshSurface, IUniformShape
    {
        /// <summary>
        /// Creates a simple mesh in the specified frame of reference.
        /// </summary>
        /// <param name="Vertices">An array of points to use for the mesh.</param>
        /// <param name="Indices">An array of indices to the vertices. Every sequential group
        /// of three indices makes up a triangle.</param>
        public SimpleMesh(ReferenceFrame Frame, IMaterial Material, Vector[] Vertices, int[] Indices)
        {
            this._Section = Frame;
            this._Material = Material;
            this._Vertices = Vertices;
            this._Indices = Indices;
        }

        /// <summary>
        /// Gets the frame of reference this mesh is in.
        /// </summary>
        public ReferenceFrame Frame
        {
            get
            {
                return this._Section;
            }
        }

        /// <summary>
        /// Gets the material that makes up all the triangles in the mesh.
        /// </summary>
        public IMaterial Material
        {
            get
            {
                return this._Material;
            }
        }

        /// <summary>
        /// A triangle within a simple mesh.
        /// </summary>
        internal class Triangle : IMeshTriangle
        {
            /// <summary>
            /// The mesh the triangle belongs to.
            /// </summary>
            public SimpleMesh Mesh;

            /// <summary>
            /// The position in the indice array this triangle
            /// starts at.
            /// </summary>
            public int IndiceStart;

            IMeshVertex[] IMeshTriangle.Vertices
            {
                get 
                {
                    IMeshVertex[] mv = new IMeshVertex[3];
                    for (int t = 0; t < 3; t++)
                    {
                        mv[t] = new Vertex
                        {
                            Mesh = this.Mesh,
                            Index = this.Mesh._Indices[this.IndiceStart + t]
                        };
                    }
                    return mv;
                }
            }

            IMeshSurface IMeshObject.Mesh
            {
                get 
                {
                    return this.Mesh;
                }
            }
        }

        /// <summary>
        /// A vertex within a simple mesh.
        /// </summary>
        internal class Vertex : IMeshVertex
        {
            /// <summary>
            /// The mesh the vertex belongs to.
            /// </summary>
            public SimpleMesh Mesh;

            /// <summary>
            /// The position in the vertex array this vertex is at.
            /// </summary>
            public int Index;

            ReferenceFrame IMeshVertex.Frame
            {
                get 
                {
                    return this.Mesh.Frame;
                }
            }

            Vector IMeshVertex.Position
            {
                get 
                {
                    return this.Mesh._Vertices[this.Index];
                }
            }

            IMeshSurface IMeshObject.Mesh
            {
                get 
                {
                    return this.Mesh;
                }
            }
        }

        IEnumerable<IMeshTriangle> IMeshSurface.Triangles
        {
            get 
            {
                for (int t = 0; t < this._Indices.Length; t += 3)
                {
                    yield return new Triangle
                    {
                        Mesh = this,
                        IndiceStart = t
                    };
                }
            }
        }

        void IConvertible<IShape>.Convert<O>(out O Object)
        {
            Object = this as O;
        }

        ReferenceFrame ISingleSectionMeshSurface.Frame
        {
            get 
            {
                return this._Section;
            }
        }

        IMaterial IUniformShape.Material
        {
            get 
            {
                return this._Material;
            }
        }

        private IMaterial _Material;
        private ReferenceFrame _Section;
        internal Vector[] _Vertices;
        internal int[] _Indices;
    }
}