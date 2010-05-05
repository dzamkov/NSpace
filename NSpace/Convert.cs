//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;
using System.Reflection;

namespace NSpace
{
    /// <summary>
    /// An object that can be converted to other object of the specified type. The converted object must represent
    /// exactly what the original object did.
    /// </summary>
    /// <typeparam name="T">The base type of the types the object may be converted to.</typeparam>
    public interface IConvertible<T> : IImmutable
        where T : class, IImmutable
    {
        /// <summary>
        /// Converts the object to the specified destination type and returns the object if the conversion
        /// was sucsessful or null if there is no conversion available.
        /// </summary>
        void Convert<D>(out D Result) 
            where D : class, T;

    }
}
