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
    /// A collection of geometry to which operations can be performed. The triangles and points
    /// within a mesh are only valid for that mesh and may not be used elsewhere.
    /// </summary>
    public abstract class Mesh
    {
        public Mesh()
        {

        }

        /// <summary>
        /// Gets the collection of all triangles within this mesh.
        /// </summary>
        public abstract IEnumerable<Geometry> Triangles { get; }

        /// <summary>
        /// Gets the amount of triangles in this mesh.
        /// </summary>
        public virtual int TriangleCount
        {
            get
            {
                int a = 0;
                foreach (Geometry tri in this.Triangles)
                {
                    a++;
                }
                return a;
            }
        }

        /// <summary>
        /// Creates a mesh of the simplest kind and fills it with the specified triangles.
        /// </summary>
        public static Mesh Create(IEnumerable<Geometry> Triangles)
        {
            SimpleMesh sm = new SimpleMesh();
            Mesh.IEditContext ec = sm.GetEditContext();
            foreach (Geometry tri in Triangles)
            {
                ec.AddTriangle(tri);
            }
            ec.Commit();
            return sm;
        }

        /// <summary>
        /// Gets an edit context to use to modify this mesh.
        /// </summary>
        public abstract IEditContext GetEditContext();

        /// <summary>
        /// Context used for editing the triangles in a mesh. Changes made to this context do
        /// not nessecarily appear in the mesh immediately. Commit may force the changes
        /// to appear.
        /// </summary>
        public interface IEditContext
        {
            /// <summary>
            /// Commits the changes made in this context to the mesh. 
            /// This command may be blocking depending on if the mesh is in use. On some
            /// meshes this is not required but it is never not allowed.
            /// </summary>
            void Commit();

            /// <summary>
            /// Adds the specified triangle. Note that this may make a copy of the triangle
            /// and add that instead. Once added, the triangle may not be modified.
            /// </summary>
            void AddTriangle(Geometry Tri);

            /// <summary>
            /// Modifies a triangle. The supplied triangle is the one to which modification is intended on. This function
            /// will return a triangle with the same data as the specified triangle. All changes to the returned triangle
            /// will be reflected in the mesh.
            /// </summary>
            Geometry ModifyTriangle(Geometry Tri);

            /// <summary>
            /// Removes the specified triangle from the mesh.
            /// </summary>
            void RemoveTriangle(Geometry Tri);

            /// <summary>
            /// This works in a similar way to modifiy triangle and can be used to modify points used by the mesh
            /// and triangles within it.
            /// </summary>
            Geometry ModifyPoint(Geometry Point);
        }
    }
}
