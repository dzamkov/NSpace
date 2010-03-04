//----------------------------------------
// Copyright (c) 2010, Dmitry Zamkov 
// Open source under the new BSD License
//----------------------------------------
using System;
using System.Collections.Generic;

namespace NSpace
{
    /// <summary>
    ///  A tree for organizing bounded objects.
    /// </summary>
    /// <typeparam name="T">The type of bounds used in this bound tree.</typeparam>
    /// <typeparam name="O">The type of object stored along with bounds.</typeparam>
    public abstract class BoundTree<T, O>
        where T : IBound<T>
        where O : class
    {
        public BoundTree()
        {
            this._ObjectNodes = new Dictionary<O, Node>();
        }

        /// <summary>
        /// Gets the "score" of a bound. The bound tree will try to minimize the score
        /// of bounds when storing bounded objects.
        /// </summary>
        public abstract double GetScore(T Bound);

        /// <summary>
        /// A node within a bound tree.
        /// </summary>
        private class Node
        {
            /// <summary>
            /// The bound of this node.
            /// </summary>
            public T Bound;

            /// <summary>
            /// The object this node represents or null if this node
            /// is a combination of other nodes.
            /// </summary>
            public O Object;

            /// <summary>
            /// One of the nodes that makes up this node or null if this
            /// node has no children.
            /// </summary>
            public Node A;

            /// <summary>
            /// See A.
            /// </summary>
            public Node B;

            /// <summary>
            /// The parent node of this node.
            /// </summary>
            public Node Parent;
        }

        /// <summary>
        /// Gets or sets the bound for the specified object.
        /// </summary>
        public T this[O Key]
        {
            get
            {
                return this._ObjectNodes[Key].Bound;
            }
            set
            {
                Node n = null;
                if(this._ObjectNodes.TryGetValue(Key, out n))
                {
                    this._RemoveNode(n);
                }
                this._ObjectNodes[Key] = this._AddNode(Key, value);
            }
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
            foreach (Node n in this._ObjectNodes.Values)
            {
                scores.AddFirst(new KeyValuePair<Node, double>(n, this.GetScore(n.Bound)));
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
                        T abbound = a.Bound.Union(b.Bound);
                        double score = this.GetScore(abbound);
                        if (score < canidatescore)
                        {
                            canidatenode = next;
                            canidatescore = score;
                            canidate = new Node();
                            canidate.Bound = abbound;
                            canidate.A = a;
                            canidate.B = b;
                            a.Parent = canidate;
                            b.Parent = canidate;
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
            if (Test.Intersects(Root.Bound))
            {
                if (Root.Object != null)
                {
                    Res.Add(Root.Object);
                }
                else
                {
                    this._Intersect(Res, Test, Root.A);
                    this._Intersect(Res, Test, Root.B);
                }
            }
        }

        /// <summary>
        /// Removes the specified node from the node tree.
        /// </summary>
        private void _RemoveNode(Node Node)
        {
            Node par = Node.Parent;
            if (par != null)
            {
                // Remove references of node from parent.
                Node grampa = par.Parent;
                Node other;
                if (par.A == Node)
                {
                    other = par.B;
                }
                else
                {
                    other = par.A;
                }
                if (grampa.A == par)
                {
                    grampa.A = other;
                }
                else
                {
                    grampa.B = other;
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
        private Node _AddNode(O Object, T Bound)
        {
            // Make node for the object
            Node objnode = new Node();
            objnode.Bound = Bound;
            objnode.Object = Object;

            // Has root?
            if (this._Root != null)
            {
                // Combine root with new node
                Node parnode = new Node();
                parnode.A = objnode;
                parnode.B = this._Root;
                parnode.Bound = objnode.Bound.Union(this._Root.Bound);

                objnode.Parent = parnode;
                this._Root.Parent = parnode;
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
        private Dictionary<O, Node> _ObjectNodes;
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
}
