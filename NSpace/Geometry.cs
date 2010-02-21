//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace
{
    /// <summary>
    /// A geometric object or primitive that can be stored in a mesh.
    /// </summary>
    public abstract class Geometry : IDataContainer
    {
        /// <summary>
        /// Gets the mesh this geometry belongs to or null if this geometry is
        /// not assigned to a mesh.
        /// </summary>
        public abstract Mesh Mesh { get; }

        public virtual IData GetData(Type Type)
        {
            return null;
        }

        public virtual bool SetData(Type Type, IData Data)
        {
            return false;
        }

        public virtual IEnumerable<KeyValuePair<Type, IData>> FullData
        {
            get 
            {
                return new KeyValuePair<Type, IData>[0];
            }
        }
       
        /// <summary>
        /// Creates an independant copy of this geometry such that the data is the same
        /// on both copies and the copy has a mesh of null.
        /// </summary>
        public virtual Geometry Copy()
        {
            return new ElasticGeometry(null, this);
        }
    }

    /// <summary>
    /// Geometry that retains all data it is given.
    /// </summary>
    public class ElasticGeometry : Geometry
    {
        public ElasticGeometry(Mesh Mesh)
        {
            this._Data = new Dictionary<Type, IData>();
            this._Mesh = Mesh;
        }

        public ElasticGeometry(Mesh Mesh, Geometry From)
        {
            this._Data = new Dictionary<Type, IData>();
            foreach (KeyValuePair<Type, IData> kvp in From.FullData)
            {
                this._Data[kvp.Key] = kvp.Value;
            }
            this._Mesh = Mesh;
        }

        public override IData GetData(Type Type)
        {
            IData dat = null;
            this._Data.TryGetValue(Type, out dat);
            return dat;
        }

        public override bool SetData(Type Type, IData Data)
        {
            this._Data[Type] = Data;
            return true;
        }

        public override IEnumerable<KeyValuePair<Type, IData>> FullData
        {
            get
            {
                return this._Data;
            }
        }

        public override Mesh Mesh
        {
            get 
            {
                return this._Mesh;
            }
        }


        public Mesh _Mesh;
        public Dictionary<Type, IData> _Data;
    }

    /// <summary>
    /// A higher-level interface to some other geometry.
    /// </summary>
    public class DerivedGeometry : Geometry
    {
        public DerivedGeometry(Geometry Base)
        {
            this._Base = Base;
        }

        public override IData GetData(Type Type)
        {
            return this._Base.GetData(Type);
        }

        public override bool SetData(Type Type, IData Data)
        {
            return this._Base.SetData(Type, Data);
        }

        public override Mesh Mesh
        {
            get 
            {
                return this._Base.Mesh;
            }
        }

        public override IEnumerable<KeyValuePair<Type, IData>> FullData
        {
            get
            {
                return this._Base.FullData;
            }
        }

        /// <summary>
        /// Gets the base of this geometry. The base is the geometry this
        /// geometry gets and sets data from.
        /// </summary>
        public Geometry Base
        {
            get
            {
                return this._Base;
            }
        }

        private Geometry _Base;
    }
}
