//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;
using OpenTK;
using OpenTK.Graphics.OpenGL;
using System.Reflection;

namespace NSpace
{
    // This may or may not be useful later on...

    /*
    /// <summary>
    /// A method of conversion of objects between different immutable types that preserves them exactly.
    /// </summary>
    /// <typeparam name="S">The required subtype an object to be converted must have.</typeparam>
    /// <typeparam name="D">The type of object to convert to.</typeparam>
    public interface IConvertMethod<S, D> : IImmutable
        where S : class, IImmutable
        where D : class, IImmutable
    {
        /// <summary>
        /// Converts an object of the source type to the destination type. Returns null if the particular
        /// object can not be converted.
        /// </summary>
        D Convert(S Object);

        /// <summary>
        /// Filters the method to have a more specific destination type. Result will be null
        /// if there is no possibility for the method to return the specified type.
        /// </summary>
        void Filter<T>(out IConvertMethod<S, T> Result) where T : class, D;

        /// <summary>
        /// Gets a more specialized method of conversion given a more specific source type.
        /// </summary>
        void Specialize<T>(out IConvertMethod<T, D> Result) where T : class, S;
    }

    /// <summary>
    /// Conversion helper methods.
    /// </summary>
    public static class Convert
    {
        /// <summary>
        /// Loads a convert method of the specified source and destination types from the defined
        /// convert methods in an assembly.
        /// </summary>
        public static void Load<S, D>(Assembly Assembly, out IConvertMethod<S, D> Method)
            where S : class, IImmutable
            where D : class, IImmutable
        {
            // Get all possible convert method types.
            IEnumerable<Type> types = Assembly.GetTypes();
            List<object> methods = new List<object>();
            foreach (Type t in types)
            {
                foreach (Type i in t.GetInterfaces())
                {
                    if (i.GetGenericTypeDefinition() == typeof(IConvertMethod<IImmutable, IImmutable>).GetGenericTypeDefinition())
                    {
                        ConstructorInfo defaultconstructor = t.GetConstructor(new Type[0]);
                        if (defaultconstructor != null)
                        {
                            methods.Add(defaultconstructor.Invoke(new object[0]));
                        }
                        break;
                    }
                }
            }

            Method = null;
        }

        /// <summary>
        /// Combines a set of convert methods to get a result method which will eventually use all the specified input methods.
        /// </summary>
        public static void Combine<S, D>(IEnumerable<object> Methods, out IConvertMethod<S, D> Result)
            where S : class, IImmutable
            where D : class, IImmutable
        {
            // Find the most generalized destination type for the methods
            Type dest = null;
            foreach (object method in Methods)
            {
                foreach (Type i in method.GetType().GetInterfaces())
                {

                }
            }
            Result = null;
        }
    }

    /// <summary>
    /// A conversion made from two other conversions that will try the first, but fall back
    /// to the second if the first fails.
    /// </summary>
    public class FallBackConvert<S, D> : IConvertMethod<S, D>
        where S : class, IImmutable
        where D : class, IImmutable
    {
        public FallBackConvert(IConvertMethod<S, D> Primary, IConvertMethod<S, D> Secondary)
        {
            this._Primary = Primary;
            this._Secondary = Secondary;
        }

        D IConvertMethod<S, D>.Convert(S Object)
        {
            D first = this._Primary.Convert(Object);
            if (first != null)
            {
                return first;
            }
            else
            {
                return this._Secondary.Convert(Object);
            }
        }

        void IConvertMethod<S, D>.Filter<T>(out IConvertMethod<S, T> Result)
        {
            IConvertMethod<S, T> a; this._Primary.Filter<T>(out a);
            IConvertMethod<S, T> b; this._Secondary.Filter<T>(out b);
            if (a == null)
            {
                Result = b;
            }
            else
            {
                if (b == null)
                {
                    Result = a;
                }
                else
                {
                    Result = new FallBackConvert<S, T>(a, b);
                }
            }
        }

        void IConvertMethod<S, D>.Specialize<T>(out IConvertMethod<T, D> Result)
        {
            IConvertMethod<T, D> a; this._Primary.Specialize<T>(out a);
            IConvertMethod<T, D> b; this._Secondary.Specialize<T>(out b);
            Result = new FallBackConvert<T, D>(a, b);
        }

        private IConvertMethod<S, D> _Primary;
        private IConvertMethod<S, D> _Secondary;
    }

    /// <summary>
    /// A convert method that generalizes(by unfiltering) another.
    /// </summary>
    /// <typeparam name="M">The destination type of the conversion that was generalized.</typeparam>
    public class UnFilterConvert<S, D, M> : IConvertMethod<S, D>
        where S : class, IImmutable
        where D : class, IImmutable
        where M : class, D, IImmutable
    {
        public UnFilterConvert(IConvertMethod<S, M> From)
        {
            this._Source = From;
        }

        D IConvertMethod<S, D>.Convert(S Object)
        {
            return this._Source.Convert(Object);
        }

        void IConvertMethod<S, D>.Filter<T>(out IConvertMethod<S, T> Result)
        {
            if (typeof(T).IsAssignableFrom(typeof(M)))
            {
                // Result = new UnFilterConvert<S, T, M>(this._Source);
                Result = typeof(UnFilterConvert<S, D, M>).MakeGenericType(
                    new Type[] { typeof(S), typeof(D), typeof(M) }).GetConstructors()[0].Invoke(
                        new object[] { this._Source }) as IConvertMethod<S, T>;
            }
            else
            {
                if (typeof(M).IsAssignableFrom(typeof(T)))
                {
                    // Preform this._Source.Filter<T> to get a new convert method
                    IConvertMethod<S, T> newsource = null;
                    typeof(IConvertMethod<S, M>).GetMethod("Filter").MakeGenericMethod(new Type[] { typeof(T) }).Invoke(this._Source, new object[] { newsource });
                    if (newsource != null)
                    {
                        Result = newsource;
                    }
                    else
                    {
                        Result = null;
                    }
                }
                else
                {
                    // T is not an M and M is not a T.
                    Result = null;
                }
            }
        }

        void IConvertMethod<S, D>.Specialize<T>(out IConvertMethod<T, D> Result)
        {
            IConvertMethod<T, M> specialsource; this._Source.Specialize<T>(out specialsource);
            Result = new UnFilterConvert<T, D, M>(specialsource);
        }

        private IConvertMethod<S, M> _Source;
    }

    /// <summary>
    /// A convert method that generalizes(by unspecializing) another.
    /// </summary>
    /// <typeparam name="N">The source type of the conversion that was generalized.</typeparam>
    public class UnSpecializedConvert<S, D, N> : IConvertMethod<S, D>
        where S : class, IImmutable
        where D : class, IImmutable
        where N : class, S, IImmutable
    {
        public UnSpecializedConvert(IConvertMethod<N, D> Source)
        {
            this._Source = Source;
        }

        D IConvertMethod<S, D>.Convert(S Object)
        {
            N s = Object as N;
            if (s != null)
            {
                return this._Source.Convert(s);
            }
            else
            {
                return null;
            }
        }

        void IConvertMethod<S, D>.Filter<T>(out IConvertMethod<S, T> Result)
        {
            IConvertMethod<N, T> newsource; this._Source.Filter<T>(out newsource);
            if (newsource != null)
            {
                Result = new UnSpecializedConvert<S, T, N>(newsource);
            }
            else
            {
                Result = null;
            }
        }

        void IConvertMethod<S, D>.Specialize<T>(out IConvertMethod<T, D> Result)
        {
            if (typeof(T).IsAssignableFrom(typeof(N)))
            {
                // Result = new UnSpecializedConvert<T, D, N>(this._Source);
                Result = typeof(UnSpecializedConvert<S, D, N>).MakeGenericType(
                    new Type[] { typeof(T), typeof(D), typeof(N) }).GetConstructors()[0].Invoke(
                        new object[] { this._Source }) as IConvertMethod<T, D>;
            }
            else
            {
                if (typeof(N).IsAssignableFrom(typeof(T)))
                {
                    IConvertMethod<T, D> newsource = null;
                    typeof(IConvertMethod<T, D>).GetMethod("Specialize").MakeGenericMethod(new Type[] { typeof(T) }).Invoke(this._Source, new object[] { newsource });
                    Result = newsource;
                }
                else
                {
                    Result = null;
                }
            }
        }

        private IConvertMethod<N, D> _Source;  
    }

    /// <summary>
    /// The default conversion using the "as" keyword. This handles all "is-a" relationships.
    /// </summary>
    public class DefaultConvertMethod<S, D> : IConvertMethod<IImmutable, IImmutable>
    {
        IImmutable IConvertMethod<IImmutable, IImmutable>.Convert(IImmutable Object)
        {
            throw new NotImplementedException();
        }

        void IConvertMethod<IImmutable, IImmutable>.Filter<T>(out IConvertMethod<IImmutable, T> Result)
        {
            throw new NotImplementedException();
        }

        void IConvertMethod<IImmutable, IImmutable>.Specialize<T>(out IConvertMethod<T, IImmutable> Result)
        {
            throw new NotImplementedException();
        }
    }
    */
}
