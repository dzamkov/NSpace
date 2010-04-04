//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Scoping
{
    /// <summary>
    /// A resource that can be loaded into a scope.
    /// </summary>
    public interface IResource
    {

    }

    /// <summary>
    /// A collection of mutable resources that can be loaded and indexed by a string.
    /// </summary>
    public interface IScope : IResource
    {
        /// <summary>
        /// Gets or sets a resource in this scope, indexed by a string. Null is the default
        /// and will be returned if no other resource is specified.
        /// </summary>
        IResource this[string Key] { get; }

        /// <summary>
        /// Attaches a scope event handler to the scope.
        /// </summary>
        void Attach(IScopeEventHandler EventHandler);

        /// <summary>
        /// Detaches a previously added event handler from the scope.
        /// </summary>
        void Detach(IScopeEventHandler EventHandler);
    }

    /// <summary>
    /// Event handler for scopes.
    /// </summary>
    public interface IScopeEventHandler
    {
        /// <summary>
        /// Called when a resource at a key is changed to another. If Old is null, a resource
        /// is added. If New is null, a resource is removed.
        /// </summary>
        void OnChangeResource(IScope Scope, string Key, IResource Old, IResource New);
    }

    /// <summary>
    /// A simple unoptimized implementation of a scope, along with helper functions to access them.
    /// </summary>
    public class Scope : IScope
    {
        public Scope()
        {
            this._Resources = new Dictionary<string, IResource>();
            this._EventHandlers = new List<IScopeEventHandler>();
        }

        public IResource this[string Key]
        {
            get
            {
                IResource res = null;
                this._Resources.TryGetValue(Key, out res);
                return res;
            }
            set
            {
                if (this._EventHandlers.Count > 0)
                {
                    IResource old = this._Resources[Key];
                    if (value == null)
                    {
                        this._Resources.Remove(Key);
                    }
                    else
                    {
                        this._Resources[Key] = value;
                    }
                    if (value != old)
                    {
                        foreach (IScopeEventHandler seh in this._EventHandlers)
                        {
                            seh.OnChangeResource(this, Key, old, value);
                        }
                    }
                }
                else
                {
                    if (value == null)
                    {
                        this._Resources.Remove(Key);
                    }
                    else
                    {
                        this._Resources[Key] = value;
                    }
                }
            }
        }

        public void Attach(IScopeEventHandler EventHandler)
        {
            this._EventHandlers.Add(EventHandler);
        }

        public void Detach(IScopeEventHandler EventHandler)
        {
            this._EventHandlers.Remove(EventHandler);
        }

        private Dictionary<string, IResource> _Resources;
        private List<IScopeEventHandler> _EventHandlers;
    }
}
