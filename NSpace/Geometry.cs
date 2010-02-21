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
    /// Geometry which takes data from one source if it exists, else the data will
    /// be taken from another.
    /// </summary>
    public class SuperGeometry : Geometry
    {
        public SuperGeometry(Geometry Super, Geometry Base)
        {
            this._Super = Super;
            this._Base = Base;
        }

        public override IData GetData(Type Type)
        {
            IData dat = this._Super.GetData(Type);
            if (dat != null)
            {
                return dat;
            }
            else
            {
                return this._Base.GetData(Type);
            }
        }

        public override bool SetData(Type Type, IData Data)
        {
            if (this._Super.SetData(Type, Data))
            {
                return true;
            }
            else
            {
                return this._Base.SetData(Type, Data);
            }
        }

        public override IEnumerable<KeyValuePair<Type, IData>> FullData
        {
            get
            {
                Dictionary<Type, IData> dat = new Dictionary<Type, IData>();
                foreach (KeyValuePair<Type, IData> kvp in this._Base.FullData)
                {
                    dat[kvp.Key] = kvp.Value;
                }
                foreach (KeyValuePair<Type, IData> kvp in this._Super.FullData)
                {
                    dat[kvp.Key] = kvp.Value;
                }
                return dat;
            }
        }

        public override Mesh Mesh
        {
            get 
            {
                Mesh m = this._Super.Mesh;
                if (m != null)
                {
                    return m;
                }
                else
                {
                    return this._Base.Mesh;
                }
            }
        }

        /// <summary>
        /// Gets the super geometry, which is the first one that is looked at for data.
        /// </summary>
        public Geometry Super
        {
            get
            {
                return this._Super;
            }
        }

        /// <summary>
        /// Gets the base geometry, which is looked at if the data is not found in super.
        /// </summary>
        public Geometry Base
        {
            get
            {
                return this._Base;
            }
        }

        private Geometry _Super;
        private Geometry _Base;
    }
}
