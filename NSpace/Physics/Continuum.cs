//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace.Physics
{
    /// <summary>
    /// A body over a period of time that is composed of several bodies
    /// such that there is one and exactly one of these bodies occupying
    /// a given point in time in the continuum.
    /// </summary>
    public class Continuum : ICompoundBody
    {
        public Continuum()
        {

        }

        public TimeBound TimeBound
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        public IEnumerable<IBody> Bodies
        {
            get 
            {
                throw new NotImplementedException();
            }
        }

        public void Interact(IBody Other)
        {
            throw new NotImplementedException();
        }

        public void Attach(IBodyEventHandler EventHandler)
        {
            throw new NotImplementedException();
        }

        public void Detach(IBodyEventHandler EventHandler)
        {
            throw new NotImplementedException();
        }
    }
}