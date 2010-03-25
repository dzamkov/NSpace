//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;
namespace NSpace.Physics
{
    /// <summary>
    /// A group of bodies that act as a compound body.
    /// </summary>
    public class BodyGroup : IContentBody, IBodyEventHandler
    {
        public BodyGroup()
        {
            this._Bodies = new List<IBody>();
            this._EventHandlers = new List<IBodyEventHandler>();
            this._CompoundBodyEventHandlers = new List<ICompoundBodyEventHandler>();
            this._TimeBound = TimeBound.None;
        }

        public IEnumerable<IBody> Bodies
        {
            get
            {
                return this._Bodies;
            }
        }
        public TimeBound TimeBound
        {
            get
            {
                return this._TimeBound;
            }
        }

        public void Interact(IBody Other)
        {
            foreach (IBody Body in this._Bodies)
            {
                Body.Interact(Other);
            }
        }

        public void Attach(IBodyEventHandler EventHandler)
        {
            this._EventHandlers.Add(EventHandler);
        }

        public void Detach(IBodyEventHandler EventHandler)
        {
            this._EventHandlers.Remove(EventHandler);
        }

        public void Attach(ICompoundBodyEventHandler EventHandler)
        {
            this._CompoundBodyEventHandlers.Add(EventHandler);
        }

        public void Detach(ICompoundBodyEventHandler EventHandler)
        {
            this._CompoundBodyEventHandlers.Remove(EventHandler);
        }

        public void Add(IBody Body)
        {
            this._Bodies.Add(Body);
            Body.Attach(this);
            this._TimeBound = this._TimeBound.Union(Body.TimeBound);
            foreach (IBodyEventHandler beh in this._EventHandlers)
            {
                beh.OnModified(this);
            }
            foreach (ICompoundBodyEventHandler cbeh in this._CompoundBodyEventHandlers)
            {
                cbeh.OnAdd(this, Body);
            }
        }

        public void Remove(IBody Body)
        {
            this._Bodies.Remove(Body);
            Body.Detach(this);
            if (this._TimeBound.TimeStart == Body.TimeBound.TimeStart ||
                this._TimeBound.TimeEnd == Body.TimeBound.TimeEnd)
            {
                //Recalculate time bound
                this._TimeBound = TimeBound.None;
                foreach (IBody body in this._Bodies)
                {
                    this._TimeBound = this._TimeBound.Union(body.TimeBound);
                }
            }
            foreach (IBodyEventHandler beh in this._EventHandlers)
            {
                beh.OnModified(this);
            }
            foreach (ICompoundBodyEventHandler cbeh in this._CompoundBodyEventHandlers)
            {
                cbeh.OnRemove(this, Body);
            }
        }

        public void OnReassign(IBody Old, IBody New)
        {

        }

        public void OnModified(IBody Body)
        {
            foreach (IBodyEventHandler beh in this._EventHandlers)
            {
                beh.OnModified(this);
            }
        }

        public void OnRemove(IBody Body)
        {
            this.Remove(Body);
        }

        private List<IBody> _Bodies;
        private List<IBodyEventHandler> _EventHandlers;
        private List<ICompoundBodyEventHandler> _CompoundBodyEventHandlers;
        private TimeBound _TimeBound;
    }
}