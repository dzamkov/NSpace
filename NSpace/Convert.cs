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
    /// A method of converting an object to a specified type. Conversion must result in objects
    /// that represent the exact same thing, but may have different interfaces for it.
    /// </summary>
    /// <typeparam name="D">The destination type this method can convert to.</typeparam>
    public interface IConvertMethod<D> : IImmutable
        where D : class, IImmutable
    {
        /// <summary>
        /// Converts an object of the specified type to the destination type.
        /// </summary>
        /// <typeparam name="S">The most specific known type of the object to convert.</typeparam>
        D Convert<S>(S Object);

        /// <summary>
        /// Specializes the method to return a more specific destination type.
        /// </summary>
        void Specialize<T>(out IConvertMethod<T> Method) 
            where T : class, D;
    }

    /// <summary>
    /// Functions and methods for conversion.
    /// </summary>
    public static class Convert
    {
        /// <summary>
        /// Loads a single convert method from an assembly which represents the combination of
        /// all convert methods defined in the assembly.
        /// </summary>
        public static IConvertMethod<IImmutable> Load(Assembly Assembly)
        {
            // Find and create convert methods from the assembly.
            List<object> convertmethods = new List<object>();
            foreach (Type t in Assembly.GetTypes())
            {
                object convertmethod = ConvertMethodFromType(t);
                if (convertmethod != null)
                {
                    convertmethods.Add(convertmethod);
                }
            }

            if (convertmethods.Count == 1)
            {
                return convertmethods[0] as IConvertMethod<IImmutable>;
            }
            else
            {
                IConvertMethod<IImmutable> result;
                Combine<IImmutable>(convertmethods, out result);
                return result;
            }
        }

        /// <summary>
        /// Tries initializing a type as a convert method. Returns the convert method if
        /// it can be created or null otherwise.
        /// </summary>
        public static object ConvertMethodFromType(Type Type)
        {
            // Only classes can be initailized
            if (Type.IsClass)
            {
                // Check for an IConvertMethod interface.
                foreach (Type i in Type.GetInterfaces())
                {
                    if (i.IsGenericType && i.GetGenericTypeDefinition() == typeof(IConvertMethod<IImmutable>).GetGenericTypeDefinition())
                    {
                        Type l = Type;

                        // Declared class that is a convert method is also generic (like DefaultConvert)
                        if (Type.IsGenericType)
                        {
                            l = Type.GetGenericTypeDefinition();
                            Type[] genargs = l.GetGenericArguments();
                            for (int x = 0; x < genargs.Length; x++)
                            {
                                // Replace with most general arguments that fit.
                                Type[] cons = genargs[x].GetGenericParameterConstraints();
                                if (cons.Length > 1)
                                {
                                    return null;
                                }
                                else
                                {
                                    genargs[x] = cons[0];
                                }
                            }
                            l = l.MakeGenericType(genargs);
                        }

                        // Try to initialize
                        ConstructorInfo defaultconstructor = l.GetConstructor(new Type[0]);
                        if (defaultconstructor != null)
                        {
                            return defaultconstructor.Invoke(new object[0]);
                        }

                        break;
                    }
                }
            }

            return null;
        }

        /// <summary>
        /// Combines several convert methods into one which effectively uses the others when a
        /// conversion is needed.
        /// </summary>
        public static void Combine<D>(IEnumerable<object> Methods, out IConvertMethod<D> Result)
            where D : class, IImmutable
        {
            throw new NotImplementedException();
        }
    }

    /// <summary>
    /// Convert method that represents the "is-a" relationship. This method works
    /// when the "as" keyword can correctly convert between types.
    /// </summary>
    public class DefaultConvert<D> : IConvertMethod<D>
        where D : class, IImmutable
    {
        D IConvertMethod<D>.Convert<S>(S Object)
        {
            return Object as D;
        }

        void IConvertMethod<D>.Specialize<T>(out IConvertMethod<T> Method)
        {
            Method = new DefaultConvert<T>();
        }
    }

    /// <summary>
    /// A convert method which combines a set of others, by calling them in
    /// order until one of them returns the converted object sucsessfully.
    /// </summary>
    public class CombineConvert<D> : IConvertMethod<D>
        where D : class, IImmutable
    {
        public CombineConvert(IEnumerable<IConvertMethod<D>> Methods)
        {
            this._Methods = Methods;
        }

        D IConvertMethod<D>.Convert<S>(S Object)
        {
            foreach(IConvertMethod<D> method in this._Methods)
            {
                D conobj = method.Convert<S>(Object);
                if (conobj != null)
                {
                    return conobj;
                }
            }
            return null;
        }

        void IConvertMethod<D>.Specialize<T>(out IConvertMethod<T> Method)
        {
            List<IConvertMethod<T>> newmethods = new List<IConvertMethod<T>>();
            foreach (IConvertMethod<D> method in this._Methods)
            {
                IConvertMethod<T> newmethod; method.Specialize<T>(out newmethod);
                if (newmethod != null)
                {
                    newmethods.Add(newmethod);
                }
            }
            if (newmethods.Count > 0)
            {
                Method = new CombineConvert<T>(newmethods);
            }
            else
            {
                Method = null;
            }
        }

        private IEnumerable<IConvertMethod<D>> _Methods;
    }

    /// <summary>
    /// A convert method which allows another to convert to a more generalized type. This effectively
    /// undues Specialize.
    /// </summary>
    public class GeneralConvert<D, M> : IConvertMethod<D>
        where D : class, IImmutable
        where M : class, D
    {
        public GeneralConvert(IConvertMethod<M> Source)
        {
            this._Source = Source;
        }

        D IConvertMethod<D>.Convert<S>(S Object)
        {
            return this._Source.Convert<S>(Object);
        }

        void IConvertMethod<D>.Specialize<T>(out IConvertMethod<T> Method)
        {
            if (typeof(T).IsAssignableFrom(typeof(M)))
            {
                // Method = new GeneralConvert<T, M>(this._Source)
                Method = typeof(GeneralConvert<D, M>).GetGenericTypeDefinition().MakeGenericType(new Type[] {
                    typeof(T), typeof(M)}).GetConstructors()[0].Invoke(new object[] { this._Source }) as IConvertMethod<T>;
            }
            else
            {
                if (typeof(M).IsAssignableFrom(typeof(T)))
                {
                    // this._Source.Specialize<T>(out Method)
                    MemberInfo[] mi = typeof(IConvertMethod<M>).GetMembers();
                    throw new NotImplementedException();
                }
                else
                {
                    Method = null;
                }
            }
        }

        private IConvertMethod<M> _Source;
    }
}
