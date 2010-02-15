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

        /// <summary>
        /// Gets the amount of items in the source.
        /// </summary>
        int Count { get; }

        /// <summary>
        /// Attachs a listener so that its callbacks are called when the source
        /// changes.
        /// </summary>
        void Attach(ISourceListener<T> Listener);

        /// <summary>
        /// Detaches a listener so that callbacks on it from this source are
        /// no longer received.
        /// </summary>
        void Detach(ISourceListener<T> Listener);
    }

    /// <summary>
    /// An interface to an object that will be notified if the items in a source are
    /// changed.
    /// </summary>
    /// <typeparam name="T">The type of objects in the source.</typeparam>
    public interface ISourceListener<T>
    {
        /// <summary>
        /// Called when an item is added to attached sources.
        /// </summary>
        void OnItemAdded(T Item);

        /// <summary>
        /// Called when an item is removed from attached sources.
        /// </summary>
        /// <param name="Item"></param>
        void OnItemRemoved(T Item);
    }

    /// <summary>
    /// An interface to a collection of objects that can take or remove more
    /// objects.
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

        /// <summary>
        /// Removes an item from this sink.
        /// </summary>
        void Remove(T Item);

        /// <summary>
        /// Removes a collection of items from this sink.
        /// </summary>
        void Remove(IEnumerable<T> Items);
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
            this._Items = new Dictionary<T, object>();
            this._Listeners = new List<ISourceListener<T>>();
        }

        public virtual void Add(T Item)
        {
            object o;
            if (!this._Items.TryGetValue(Item, out o))
            {
                foreach (ISourceListener<T> l in this._Listeners)
                {
                    l.OnItemAdded(Item);
                }
            }
            this._Items[Item] = null;
        }

        public virtual void Add(IEnumerable<T> Items)
        {
            foreach (T t in Items)
            {
                this.Add(t);
            }
        }

        public virtual void Remove(T Item)
        {
            object o;
            if (this._Items.TryGetValue(Item, out o))
            {
                foreach (ISourceListener<T> l in this._Listeners)
                {
                    l.OnItemRemoved(Item);
                }
            }
            this._Items.Remove(Item);
        }

        public virtual void Remove(IEnumerable<T> Items)
        {
            foreach (T t in Items)
            {
                this.Remove(t);
            }
        }

        public virtual IEnumerable<T> Items
        {
            get 
            {
                return this._Items.Keys;
            }
        }

        public virtual int Count
        {
            get
            {
                return this._Items.Count;
            }
        }

        public virtual void Attach(ISourceListener<T> Listener)
        {
            this._Listeners.Add(Listener);
        }

        public virtual void Detach(ISourceListener<T> Listener)
        {
            this._Listeners.Remove(Listener);
        }

        // Dictionary has set-like qualities.
        private Dictionary<T, object> _Items;
        private List<ISourceListener<T>> _Listeners;
    }
}
