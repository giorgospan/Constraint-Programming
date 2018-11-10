# Constraint Programming

### About

These assignments are part of my *Logic Programming* course during Spring '17 semester.

Prolog's libraries, [ic](http://eclipseclp.org/doc/bips/lib/ic/index.html) and [branch and bound](http://eclipseclp.org/doc/bips/lib/branch_and_bound/index.html), are used in order to deal with the following constraint satisfaction problems .

### Problems

  * **Maximum Clique Problem**

    * **create_graph(N, D, G)** : creates a list of edges \[N1-N2,...\], where N1,N2 are two adjacent vertices. 
    N is the number of nodes and D is graph's density \(i.e: number of edges to number of all possible edges ratio\)

    * **maxclq(N, D, Clique, Size)** : calls create_graph(N, D, G) and searches for the maximum clique of the graph, matching the clique edge-list to "Clique" and its size to "Size".

    * **Key idea** : for every pair of non-adjacent vertices , they cannot be both of them part of the max clique, either one of them or none of them should be part of the max clique.



  * **Liars**

    * 1 stands for "Liar" , 0 stands for "Not Liar"
    
    * **genrand(N, List)** : Generates random input with N length \(i.e: N statements\)


  * **Tents Placement**

    * Our **goal** is to place some tents on a field with trees according to the following rules:

        1. Every tree must have at least one neighbouring tent, horizontally, vertically or diagonally.

    	2. There cannot be any neighbouring tents.

    	3. There cannot be a tent on a tree's place.

    	4. Some rows or columns might have a maximum number of tents that can be placed along them.

    	5. Number of tents to be placed should be bare minimum.

    * **tents/4** : Matches the 4th variable with a list containing the coordinates of tents' placements. 

    * If the given problem has more the one solutions with the same minimum number of tents, it should return every one of the solutions via backtracking.

  * **Heterogeneous Capacitated Vehicle Routing Problem**

    * **Description**  

    A company wants to deliver a specific quantity of product to every one of its customers using a number of vehicles , each one having different capacity.  

    Initially, all of the product amount is stored in the warehouse, which will be the starting point \(0,0\) of every vehicle used during the shipping process.

    It is important that every vehicle makes only one route, starting from the warehouse, shipping products to a number of customers and going back to the warehouse.

    Our **goal** is to find the optimal solution to the routing problem, i.e: a solution with the minimum overall travelled distance by the company's vehicles. 

    Note that customers' places are on a straight line, thus we make use Euclidean Distance. 

    * **hcvrp/6** : Matches on the 6th variable a list containing a list of customer-IDs for every vehicle. Those are the customers that the vehicle will visit during the shipping process.




