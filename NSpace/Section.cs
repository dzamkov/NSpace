//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace
{
    /// <summary>
    /// An orientation in space that is related to other Sections. Sections
    /// can be parented to other sections to establish a relation between the parents
    /// and sibblings in coordinate space.
    /// </summary>
    public class Section
    {
        public Section()
        {

        }

        /// <summary>
        /// Gets or sets a transform that, when applied to local space, will transform to the coordinate
        /// space of the parent.
        /// </summary>
        public Matrix ParentTransform
        {
            get
            {
                return this._ParentTransform;
            }
            set
            {
                this._ParentTransform = value;
                this._IParentTransform = value.Inverse();
                if (this._Parent != null)
                {
                    this._Parent._UpdateRelation(this);
                }
            }
        }

        /// <summary>
        /// Gets or sets a transform that, when applied to the coordinate space of the parent, will transform to local space.
        /// </summary>
        public Matrix InverseParentTransform
        {
            get
            {
                return this._IParentTransform;
            }
            set
            {
                this._ParentTransform = value.Inverse();
                this._IParentTransform = value;
                if (this._Parent != null)
                {
                    this._Parent._UpdateRelation(this);
                }
            }
        }

        /// <summary>
        /// Gets the matrix that, when multiplied by local space, will get the space of
        /// the specified section.
        /// </summary>
        public Matrix GetRelation(Section To)
        {
            if (this == To)
            {
                return Matrix.Identity;
            }
            if (this.Level == To.Level)
            {
                if (this._Parent == To._Parent)
                {
                    return Matrix.Transform(this.ParentTransform, To.InverseParentTransform);
                }
                else
                {
                    return Matrix.Transform(Matrix.Transform(this.ParentTransform, this.Parent.GetRelation(To.Parent)), To.InverseParentTransform);
                }
            }
            if (this.Level > To.Level)
            {
                return Matrix.Transform(this.ParentTransform, this.Parent.GetRelation(To));
            }
            else
            {
                return Matrix.Transform(this.GetRelation(To.Parent), To.InverseParentTransform);
            }
        }

        /// <summary>
        /// Creates a section related by the specified matrix such that the result of GetRelation
        /// with the section returned by this function will be the specified matrix.
        /// </summary>
        public Section CreateRelation(Matrix Transform)
        {
            Section res = new Section();
            res.Parent = this._Parent;
            res.InverseParentTransform = Matrix.Transform(this.InverseParentTransform, Transform);
            return res;
        }

        /// <summary>
        /// Gets how many levels deep into the section tree this section is. This will be
        /// 0 at the root section, 1 at a child of the root section, 2 if its the child of
        /// a child of the root section and so on.
        /// </summary>
        public int Level
        {
            get
            {
                if (this._Parent == null)
                {
                    return 0;
                }
                else
                {
                    return this._Parent.Level + 1;
                }
            }
        }

        /// <summary>
        /// Gets the root parent of this section. The is the lowest-level indirect or direct parent
        /// of this section.
        /// </summary>
        public Section RootParent
        {
            get
            {
                if (this._Parent == null)
                {
                    return this;
                }
                else
                {
                    return this._Parent.RootParent;
                }
            }
        }

        /// <summary>
        /// Gets or sets the parent section for this section. The parent
        /// should be overall larger then the child and have siblings of
        /// about the same size. This system is used to organize objects
        /// in space.
        /// </summary>
        public Section Parent
        {
            get
            {
                return this._Parent;
            }
            set
            {
                this._Parent = value;
            }
        }

        /// <summary>
        /// Gets the visual of this section, which renders the contents of the
        /// section. If null is returned, this section cannot be rendered.
        /// </summary>
        public virtual IVisual Visual
        {
            get
            {
                return null;
            }
        }

        /// <summary>
        /// Adds a section as a child to this section. The child must
        /// not yet have a parent section. The child is set to have the
        /// specified parent transform offset to this section.
        /// </summary>
        public void AddChild(Section Child, Matrix Transform)
        {
            if (Child.Parent == null)
            {
                Child._Parent = this;
                Child._ParentTransform = Transform;
                Child._IParentTransform = Transform.Inverse();
            }
        }

        /// <summary>
        /// Creates and adds a child section with the specified transform.
        /// </summary>
        public Section AddChild(Matrix Transform)
        {
            Section sec = new Section();
            this.AddChild(sec, Transform);
            return sec;
        }

        /// <summary>
        /// Updates the relation of a child section.
        /// </summary>
        private void _UpdateRelation(Section Child)
        {
        }

        private Section _Parent;
        private Matrix _ParentTransform;
        private Matrix _IParentTransform;
    }
}
