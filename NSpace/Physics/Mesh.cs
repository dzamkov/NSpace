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
    public interface IStaticMesh : IStaticSurface
    {
        /// <summary>
        /// Gets all the triangles that make up this surface.
        /// </summary>
        IEnumerable<IStaticMeshTriangle> Triangles { get; }
    }

    /// <summary>
    /// A mesh with measurements in only terms of only one different frame of reference.
    /// </summary>
    public interface ISingleFrameStaticMesh : IStaticMesh
    {
        /// <summary>
        /// Gets the frame of reference that all vertices are in terms of in this mesh.
        /// </summary>
        ReferenceFrame Frame { get; }
    }

    /// <summary>
    /// A triangle within a mesh.
    /// </summary>
    public interface IStaticMeshTriangle
    {
        /// <summary>
        /// Gets the (hopefully) three vertices that define the endpoints
        /// of the triangle.
        /// </summary>
        IStaticMeshVertex[] Vertices { get; }
    }

    /// <summary>
    /// A vertex within a mesh that acts as an endpoint for triangles.
    /// </summary>
    public interface IStaticMeshVertex
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
    public class SimpleMesh : ISingleFrameStaticMesh, IUniformSurface
    {
        /// <summary>
        /// Creates a simple mesh in the specified frame of reference.
        /// </summary>
        /// <param name="Vertices">An array of points to use for the mesh.</param>
        /// <param name="Indices">An array of indices to the vertices. Every sequential group
        /// of three indices makes up a triangle.</param>
        public SimpleMesh(ReferenceFrame Frame, ISurfaceMaterial Material, Vector[] Vertices, int[] Indices)
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
        public ISurfaceMaterial Material
        {
            get
            {
                return this._Material;
            }
        }

        /// <summary>
        /// A triangle within a simple mesh.
        /// </summary>
        internal class Triangle : IStaticMeshTriangle
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

            IStaticMeshVertex[] IStaticMeshTriangle.Vertices
            {
                get 
                {
                    IStaticMeshVertex[] mv = new IStaticMeshVertex[3];
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
        }

        /// <summary>
        /// A vertex within a simple mesh.
        /// </summary>
        internal class Vertex : IStaticMeshVertex
        {
            /// <summary>
            /// The mesh the vertex belongs to.
            /// </summary>
            public SimpleMesh Mesh;

            /// <summary>
            /// The position in the vertex array this vertex is at.
            /// </summary>
            public int Index;

            ReferenceFrame IStaticMeshVertex.Frame
            {
                get 
                {
                    return this.Mesh.Frame;
                }
            }

            Vector IStaticMeshVertex.Position
            {
                get 
                {
                    return this.Mesh._Vertices[this.Index];
                }
            }
        }

        IEnumerable<IStaticMeshTriangle> IStaticMesh.Triangles
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

        ReferenceFrame ISingleFrameStaticMesh.Frame
        {
            get 
            {
                return this._Section;
            }
        }

        ReferenceFrame IStaticSurface.StaticFrame
        {
            get
            {
                return this._Section;
            }
        }

        ISurfaceMaterial IUniformSurface.Material
        {
            get 
            {
                return this._Material;
            }
        }

        void IConvertible<ISurface>.Convert<D>(out D Result)
        {
            Result = this as D;
        }

        private ISurfaceMaterial _Material;
        private ReferenceFrame _Section;
        internal Vector[] _Vertices;
        internal int[] _Indices;
    }
}