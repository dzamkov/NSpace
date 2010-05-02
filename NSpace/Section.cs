//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace
{
    /// <summary>
    /// A coordinate space, or system in which all objects representing points
    /// are related. Note that points are loosely defined, and may be represent
    /// a vector in the range of space or a single point of time over the range
    /// of all times. Sections may have a parent-child relation with another section, from
    /// which the relations to the siblings and ancestor relations can be caluclated.
    /// </summary>
    /// <typeparam name="P">The type representing a point in this section.</typeparam>
    /// <typeparam name="T">The type that represents a relation between sections, that
    /// can transform points.</typeparam>
    public class Section<P, T> : IImmutable
        where T : ITransformable<T, P>, new()
    {
        public Section()
        {
            
        }

        public Section(Section<P, T> Parent, T ParentRelation)
        {
            this._Parent = Parent;
            this._ParentRelation = ParentRelation;
        }

        /// <summary>
        /// Gets the relation this section has to its parent.
        /// </summary>
        public T ParentRelation
        {
            get
            {
                return this._ParentRelation;
            }
        }

        /// <summary>
        /// Gets the relation this section has with another section.
        /// </summary>
        public T GetRelation(Section<P, T> Other)
        {
            if (this.Level == Other.Level)
            {
                if (this == Other)
                {
                    return new T().Identity;
                }
                else
                {
                    if (this.Level > 0)
                    {
                        return this.ParentRelation.Combine(this.Parent.GetRelation(Other.Parent).Combine(Other.ParentRelation.Inverse));
                    }
                    else
                    {
                        throw new NotRelatedException();
                    }
                }
            }
            if (this.Level > Other.Level)
            {
                return this.GetRelation(Other.Parent).Combine(Other.ParentRelation.Inverse);
            }
            else
            {
                return this.ParentRelation.Combine(this.Parent.GetRelation(Other));
            }
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
        public Section<P, T> RootParent
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
        /// Gets the parent section for this section.
        /// </summary>
        public Section<P, T> Parent
        {
            get
            {
                return this._Parent;
            }
        }

        /// <summary>
        /// Creates a child section with the relation this should have to the child.
        /// </summary>
        public Section<P, T> CreateChild(T Relation)
        {
            return new Section<P, T>(this, Relation.Inverse);
        }

        private Section<P, T> _Parent;
        private T _ParentRelation;
    }

    /// <summary>
    /// Exception thrown when trying to get a relation between two unrelated sections.
    /// </summary>
    public class NotRelatedException : Exception
    {
        public NotRelatedException()
        {

        }
    }
}
