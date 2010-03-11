//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace
{
    /// <summary>
    ///  A map for organizing bounded objects by their bounds.
    /// </summary>
    /// <typeparam name="T">The type of bounds used in this bound tree.</typeparam>
    /// <typeparam name="O">The type of object stored along with bounds.</typeparam>
    public class BoundMap<T, O>
        where T : IBound<T>
        where O : struct
    {
        public BoundMap(IBoundScorer<T> Scorer)
        {
            this._Scorer = Scorer;
        }

        /// <summary>
        /// A node within a bound map used to store a bound/object pair.
        /// </summary>
        public class Node
        {
            /// <summary>
            /// Gets the bound of this node.
            /// </summary>
            public T Bound
            {
                get
                {
                    return this._Bound;
                }
            }

            /// <summary>
            /// Gets the object represented by this node.
            /// </summary>
            public O Object
            {
                get
                {
                    return this._Object.Value;
                }
            }

            /// <summary>
            /// Removes this node from the bound map, which must be specified.
            /// </summary>
            public void Remove(BoundMap<T, O> Map)
            {
                Map._RemoveNode(this);
            }

            /// <summary>
            /// Adds all the leaf nodes that are a child of this node. Leaf nodes have an
            /// object and no child nodes.
            /// </summary>
            internal void _LeafNodes(List<Node> List)
            {
                if (this._Object == null)
                {
                    this._A._LeafNodes(List);
                    this._B._LeafNodes(List);
                }
                else
                {
                    List.Add(this);
                }
            }

            /// <summary>
            /// The bound of this node.
            /// </summary>
            internal T _Bound;

            /// <summary>
            /// The object this node represents or null if this node
            /// is a combination of other nodes.
            /// </summary>
            internal O? _Object;

            /// <summary>
            /// One of the nodes that makes up this node or null if this
            /// node has no children.
            /// </summary>
            internal Node _A;

            /// <summary>
            /// See A.
            /// </summary>
            internal Node _B;

            /// <summary>
            /// The parent node of this node.
            /// </summary>
            internal Node _Parent;
        }

        /// <summary>
        /// Adds a bounded object to this bound map.
        /// </summary>
        public Node Add(T Bound, O Object)
        {
            return this._AddNode(Object, Bound);
        }

        /// <summary>
        /// Balances the node tree to acheive minimum bound scores. This must be called
        /// for any performance improvement.
        /// </summary>
        public void Balanace()
        {
            LinkedList<KeyValuePair<Node, double>> scores
                = new LinkedList<KeyValuePair<Node, double>>(); // sorted scores, lowest to highest.


            // Initial nodes
            List<Node> leafnodes = new List<BoundMap<T, O>.Node>();
            this._Root._LeafNodes(leafnodes);
            foreach (Node n in leafnodes)
            {
                scores.AddFirst(new KeyValuePair<Node, double>(n, this._Scorer.GetScore(n._Bound)));
            }
            this._QSort(scores);


            // Begin testing possible bounds
            while (scores.Count > 1)
            {
                LinkedListNode<KeyValuePair<Node, double>> cur = scores.First;
                LinkedListNode<KeyValuePair<Node, double>> canidatenode = null;
                Node a = cur.Value.Key;
                Node canidate = null;
                double canidatescore = double.PositiveInfinity;

                while (true)
                {
                    LinkedListNode<KeyValuePair<Node, double>> next = cur.Next;
                    if (next != null && next.Value.Value < canidatescore)
                    {
                        Node b = next.Value.Key;
                        T abbound = a._Bound.Union(b._Bound);
                        double score = this._Scorer.GetScore(abbound);
                        if (score < canidatescore)
                        {
                            canidatenode = next;
                            canidatescore = score;
                            canidate = new Node();
                            canidate._Bound = abbound;
                            canidate._A = a;
                            canidate._B = b;
                            a._Parent = canidate;
                            b._Parent = canidate;
                        }
                        cur = next;
                    }
                    else
                    {
                        break;
                    }
                }

                scores.AddAfter(cur, new KeyValuePair<Node, double>(canidate, canidatescore));
                scores.RemoveFirst();
                scores.Remove(canidatenode);
            }
            this._Root = scores.First.Value.Key;
        }

        /// <summary>
        /// Quick sorts a list of scores.
        /// </summary>
        private void _QSort(LinkedList<KeyValuePair<Node, double>> List)
        {
            if (List.Count > 1)
            {
                LinkedListNode<KeyValuePair<Node, double>> cur = List.First;
                KeyValuePair<Node, double> piv = cur.Value;

                LinkedList<KeyValuePair<Node, double>> smaller = new LinkedList<KeyValuePair<Node, double>>();
                LinkedList<KeyValuePair<Node, double>> larger = new LinkedList<KeyValuePair<Node, double>>();

                while (cur.Next != null)
                {
                    cur = cur.Next;
                    KeyValuePair<Node, double> val = cur.Value;
                    if (val.Value < piv.Value)
                    {
                        smaller.AddFirst(cur.Value);
                    }
                    else
                    {
                        larger.AddFirst(cur.Value);
                    }
                }

                List.Clear();
                if (smaller.Count > 0)
                {
                    this._QSort(smaller);
                    foreach (KeyValuePair<Node, double> val in smaller)
                    {
                        List.AddLast(val);
                    }
                }
                List.AddLast(piv);
                if (larger.Count > 0)
                {
                    this._QSort(larger);
                    foreach (KeyValuePair<Node, double> val in larger)
                    {
                        List.AddLast(val);
                    }
                }
            }
        }

        /// <summary>
        /// Gets the objects intersected by the intersect test. Note that this will return objects
        /// even if their bounds are not intersected. This should only be used to narrow a list of objects down
        /// and the objects should still be tested individually.
        /// </summary>
        public IEnumerable<O> Intersect(IIntersectTest<T> Test)
        {
            List<O> res = new List<O>();
            this._Intersect(res, Test, this._Root);
            return res;
        }

        /// <summary>
        /// Tests a single node for intersections.
        /// </summary>
        private void _Intersect(List<O> Res, IIntersectTest<T> Test, Node Root)
        {
            if (Test.Intersects(Root._Bound))
            {
                if (Root._Object != null)
                {
                    Res.Add(Root._Object.Value);
                }
                else
                {
                    this._Intersect(Res, Test, Root._A);
                    this._Intersect(Res, Test, Root._B);
                }
            }
        }

        /// <summary>
        /// Removes the specified node from the node tree.
        /// </summary>
        internal void _RemoveNode(Node Node)
        {
            Node par = Node._Parent;
            if (par != null)
            {
                // Remove references of node from parent.
                Node grampa = par._Parent;
                Node other;
                if (par._A == Node)
                {
                    other = par._B;
                }
                else
                {
                    other = par._A;
                }
                if (grampa._A == par)
                {
                    grampa._A = other;
                }
                else
                {
                    grampa._B = other;
                }
            }
            else
            {
                // Node is root, just remove root
                this._Root = null;
            }
        }

        /// <summary>
        /// Adds a node to the node tree without rebalancing.
        /// </summary>
        internal Node _AddNode(O Object, T Bound)
        {
            // Make node for the object
            Node objnode = new Node();
            objnode._Bound = Bound;
            objnode._Object = Object;

            // Has root?
            if (this._Root != null)
            {
                // Combine root with new node
                Node parnode = new Node();
                parnode._A = objnode;
                parnode._B = this._Root;
                parnode._Bound = objnode._Bound.Union(this._Root._Bound);

                objnode._Parent = parnode;
                this._Root._Parent = parnode;
                this._Root = parnode;
            }
            else
            {
                // New node becomes root
                this._Root = objnode;
            }
            return objnode;
        }

        private Node _Root;
        private IBoundScorer<T> _Scorer;
    }

    /// <summary>
    /// A representation of a bound that can intersect and combine with other bounds of its
    /// own type.
    /// </summary>
    /// <typeparam name="T">The type of the bound.</typeparam>
    public interface IBound<T>
        where T : IBound<T>
    {
        /// <summary>
        /// Gets the bound that represents the union of this bound
        /// with another.
        /// </summary>
        T Union(T Other);
    }

    /// <summary>
    /// A test to see if a bound of the specified type intersects something.
    /// </summary>
    /// <typeparam name="T">The type of bounds this test for.</typeparam>
    public interface IIntersectTest<T>
        where T : IBound<T>
    {
        /// <summary>
        /// Gets if the specified bound was intersect.
        /// </summary>
        bool Intersects(T Bound);
    }

    /// <summary>
    /// An score system that can score a bound based on its effectiveness. An example
    /// of this would be a scorer that gives bounds with a low area a lower score.
    /// </summary>
    /// <typeparam name="T">The type of bounds this can score.</typeparam>
    public interface IBoundScorer<T>
        where T : IBound<T>
    {
        /// <summary>
        /// Gets the "score" of a bound. The bound map will try to minimize the score
        /// of bounds when storing bounded objects.
        /// </summary>
        double GetScore(T Bound);
    }

    /// <summary>
    /// A bound map for reference types that can change over time. The bounds for objects
    /// can be changed without the removal of them.
    /// </summary>
    /// <typeparam name="T">The type of bounds objects have.</typeparam>
    /// <typeparam name="O">The type of objects to store.</typeparam>
    public class DynamicBoundMap<T, O>
        where T : IBound<T>
        where O : class
    {
        public DynamicBoundMap(IBoundScorer<T> Scorer)
        {
            this._BoundMap = new BoundMap<T, DynamicBoundMap<T, O>.Object>(Scorer);
            this._ObjectNodes = new Dictionary<O, BoundMap<T, DynamicBoundMap<T, O>.Object>.Node>();
        }

        /// <summary>
        /// The type of object stored in the base bound map. This really shouldnt be touched.
        /// </summary>
        private struct Object
        {
            public O Ref;
        }

        /// <summary>
        /// Returns all objects whose bounds are intersected by the specified test.
        /// </summary>
        public IEnumerable<O> Intersect(IIntersectTest<T> Test)
        {
            IEnumerable<Object> objs = this._BoundMap.Intersect(Test);
            foreach (Object o in objs)
            {
                yield return o.Ref;
            }
        }

        /// <summary>
        /// Gets or sets the bounds for a specified object in this bound map.
        /// </summary>
        public T this[O Obj]
        {
            get
            {
                return this._ObjectNodes[Obj].Bound;
            }
            set
            {
                BoundMap<T, Object>.Node node;
                if(this._ObjectNodes.TryGetValue(Obj, out node))
                {
                    node.Remove(this._BoundMap);
                }
                this._ObjectNodes[Obj] = node = this._BoundMap.Add(value, new Object() { Ref = Obj });
            }
        }

        private BoundMap<T, Object> _BoundMap;
        private Dictionary<O, BoundMap<T, Object>.Node> _ObjectNodes;
    }
}
