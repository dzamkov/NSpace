//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System.Collections.Generic;

namespace NSpace
{
    /// <summary>
    /// An interface to a collection of objects that can be pulled for
    /// data.
    /// </summary>
    /// <typeparam name="T">The type of object to store.</typeparam>
    public interface ISource<T>
    {
        /// <summary>
        /// Gets the full collection of items in the source. It is possible for
        /// items to be dynamically generated and for there to be an infinite amount
        /// of items.
        /// </summary>
        IEnumerable<T> Items { get; }
    }

    /// <summary>
    /// An interface to a collection of objects that can take more object.
    /// </summary>
    /// <typeparam name="T">The type of objects to store.</typeparam>
    public interface ISink<T>
    {
        /// <summary>
        /// Adds an item to this sink.
        /// </summary>
        void Add(T Item);

        /// <summary>
        /// Adds a collection of items to this sink.
        /// </summary>
        void Add(IEnumerable<T> Items);
    }

    /// <summary>
    /// An interface to a collection of objects where more objects can be
    /// added and the objects can be retrieved.
    /// </summary>
    /// <typeparam name="T">The type of objects to store.</typeparam>
    public interface ISinkSource<T> : ISink<T>, ISource<T>
    {

    }

    /// <summary>
    /// Implementation of a sinksource for the specified type.
    /// </summary>
    /// <typeparam name="T">The type of objects stored.</typeparam>
    public class SinkSource<T> : ISinkSource<T>
    {
        public SinkSource()
        {
            this._Items = new List<T>();
        }

        public void Add(T Item)
        {
            this._Items.Add(Item);
        }

        public void Add(IEnumerable<T> Items)
        {
            foreach (T t in Items)
            {
                this._Items.Add(t);
            }
        }

        public IEnumerable<T> Items
        {
            get 
            {
                return this._Items;
            }
        }

        private List<T> _Items;
    }
}
