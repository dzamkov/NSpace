//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// A manager for a set a of bodies that can interact. While the bodies are all
    /// placed in space-time, the world is able to manage a single slice of time to insure
    /// all bodies are evaluated when needed.
    /// </summary>
    public class World
    {
        public World(IContentBody Contents)
        {
            this._PlaceHolders = new LinkedList<IPlaceHolder>();
            this._Contents = Contents;
        }

        /// <summary>
        /// Gets the content body that contains all the bodies that can
        /// interact within this world.
        /// </summary>
        public IContentBody Contents
        {
            get
            {
                return this._Contents;
            }
        }

        /// <summary>
        /// Updates the world and objects within by the specified time in seconds.
        /// </summary>
        public void Update(TimeSpan Time)
        {
            this._CurTime += Time;

            // Handle placeholders
            LinkedListNode<IPlaceHolder> cur = this._PlaceHolders.First;
            while (cur != null)
            {
                IPlaceHolder placeholder = cur.Value;
                if (this._CurTime > placeholder.Time)
                {
                    placeholder.Evaluate(this);
                    LinkedListNode<IPlaceHolder> next = cur.Next;
                    this._PlaceHolders.Remove(cur);
                    cur = next;
                }
                else
                {
                    cur = cur.Next;
                }
            }
        }

        /// <summary>
        /// Gets the current time in the world.
        /// </summary>
        public Time CurrentTime
        {
            get
            {
                return this._CurTime;
            }
        }

        /// <summary>
        /// Adds a placeholder to the world, enabling it to be evaluated by the world
        /// when needed.
        /// </summary>
        public void AddPlaceHolder(IPlaceHolder PlaceHolder)
        {
            this._PlaceHolders.AddLast(PlaceHolder);
        }

        private IContentBody _Contents;
        private Time _CurTime;
        private LinkedList<IPlaceHolder> _PlaceHolders;
    }
}
