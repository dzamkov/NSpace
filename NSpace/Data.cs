//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace
{
    /// <summary>
    /// An object that can hold various forms of data organized by type.
    /// </summary>
    public interface IDataContainer
    {
        /// <summary>
        /// Gets data of the specified type from the data container. If the data exists in the
        /// container, this function will return it. If the data doesn't exist, this will return
        /// null. Note that even if this data is modified, the changes are guarenteed to be reflected
        /// until SetData is called.
        /// </summary>
        IData GetData(Type Type);

        /// <summary>
        /// Attempts to assign data to the container. Will return true if the data was sucsessfully assigned
        /// or false otherwise.
        /// </summary>
        bool SetData(Type Type, IData Data);

        /// <summary>
        /// Gets all data stored in this container.
        /// </summary>
        IEnumerable<KeyValuePair<Type, IData>> FullData { get; }
    }

    /// <summary>
    /// Extension methods for IDataContainer.
    /// </summary>
    public static class DataContainer
    {
        /// <summary>
        /// Tries getting the data of the specified type and returns it if its found or null otherwise.
        /// </summary>
        public static T GetData<T>(this IDataContainer DataContainer) where T : IData
        {
            return (T)(object)DataContainer.GetData(typeof(T));
        }

        /// <summary>
        /// Tries setting the data of the specified type and returns true on sucsess or false otherwise.
        /// </summary>
        public static bool SetData<T>(this IDataContainer DataContainer, T Data) where T : IData
        {
            return DataContainer.SetData(typeof(T), Data);
        }
    }

    /// <summary>
    /// An object that can be used as data in a data container.
    /// </summary>
    public interface IData
    {
        /// <summary>
        /// Creates and returns a copy of this data.
        /// </summary>
        IData Copy();
    }
}
