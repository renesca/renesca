# renesca
[![Build Status](https://travis-ci.org/renesca/renesca.svg?branch=master)](https://travis-ci.org/renesca/renesca)
[![Coverage Status](https://coveralls.io/repos/renesca/renesca/badge.svg?branch=master)](https://coveralls.io/r/renesca/renesca?branch=master)

Query and modify subgraphs from a Neo4j REST database with Scala.

Also have a look at [renesca-magic](https://github.com/renesca/renesca-magic), which generates code for typesafe graph database schemas based on renesca.

## Academic Article
There is also an academic article about renesca and renesca-magic: [An Open-Source Object-Graph-Mapping Framework for Neo4j and Scala: Renesca](https://link.springer.com/chapter/10.1007/978-3-319-45507-5_14). Feel free to use these libraries in any of your projects. If you use them in an academic setting, we appreciate if you cite this article.

> Dietze, F., Karoff, J., Calero Valdez, A. , Ziefle, M., Greven, C., & Schroeder, U. (2016, August).
> An Open-Source Object-Graph-Mapping Framework for Neo4j and Scala: Renesca.
> *In International Conference on Availability, Reliability, and Security* (pp. 204-218). Springer International Publishing.

## Concepts
### Work with graphs instead of lists
Earlier we tried to interact with graph databases as if they were relational databases. This meant working with lists of query results. As Neo4J gives us full blown graphs as query results, we now take the whole thing and provide a graph to interact with. In the rare cases where the query result is not a graph, it can also be interpreted as a table.

### Track changes, persist later
When modifying, creating and deleting nodes and connecting them with relationships it would be very expensive to submit a REST request for each change. In renesca we track changes and apply all of them at once when persisting the whole graph. This takes fewer REST requests and leaves room for optimization.

### No lazy loading
There is no further database traversing from a subgraph because there is no need to do so. When working with subgraphs retrieved from a query, you know which data you need in the future and can fetch that with the query before traversing. This approach saves a lot of unnecessary requests.

## Feature summary
* Interpret query results as graphs or tables
* Modify result graphs and persist the changes back to the database
* Do everything with transactions

## Installation

To use renesca in your sbt project, add this dependency to your ```build.sbt```:

```scala
libraryDependencies += "com.github.renesca" %% "renesca" % "0.3.2-9"
```

## Feedback
Please don't hesitate to create issues about anything. Ideas, questions, bugs, feature requests, criticism, missing documentation, confusing examples, ... . Are you stuck with renesca or renesca-magic for some time? Is there something in this README that is unclear? Anything else? This means something does not work as intended or the API is not intuitive. Contact us and let's fix this together.

## Usage Example
This example is also available as a sbt project: [renesca/renesca-example](https://github.com/renesca/renesca-example)

```scala
package renesca.example

import renesca.graph.{Node, Relation}
import renesca.parameter._
import renesca.parameter.implicits._
import renesca.{DbService, RestService, Transaction, Query}
import spray.http.BasicHttpCredentials

object Main extends App {
  // set up database connection
  val credentials = BasicHttpCredentials("neo4j", "neo4j")
  // RestService contains an ActorSystem to handle HTTP communication via spray-client
  val restService = new RestService("http://localhost:7474", Some(credentials))

  // query interface for submitting single requests
  val db = new DbService
  // dependency injection
  db.restService = restService


  // only proceed if database is available and empty
  val wholeGraph = db.queryWholeGraph
  if(wholeGraph.nonEmpty) {
    restService.actorSystem.shutdown()
    sys.error("Database is not empty.")
  }


  // create example graph:  snake -eats-> dog
  db.query("CREATE (:ANIMAL {name:'snake'})-[:EATS]->(:ANIMAL {name:'dog'})")

  val tx = db.newTransaction

  // query a subgraph from the database
  implicit val graph = tx.queryGraph("MATCH (n:ANIMAL)-[r]->() RETURN n,r")

  // access the graph like scala collections
  val snake = graph.nodes.find(_.properties("name").
    asInstanceOf[StringPropertyValue] == "snake").get

  // useful methods to access the graph (requires implicit val graph in scope)
  // e.g.: neighbours,  successors,  predecessors,  inDegree,  outDegree,  degree, ...
  val name = snake.neighbours.head.properties("name").
    asInstanceOf[StringPropertyValue].value
  println("Name of one snake neighbour: " + name) // prints "dog"

  // changes to the graph are tracked
  snake.labels += "REPTILE"
  snake.properties("hungry") = true

  // creating a local Node (a Node the database does not know about yet)
  val hippo = Node.create

  // changes to locally created Nodes are also tracked
  hippo.labels += "ANIMAL"
  hippo.properties("name") = "hippo"

  // add the created node to the Node Set
  graph.nodes += hippo

  // create a new local relation from a locally created Node to an existing Node
  graph.relations += Relation.create(snake, "EATS", hippo)

  // persist all tracked changes to the database and commit the transaction
  tx.commit.persistChanges(graph)

  // different transaction syntax
  db.transaction { tx =>
    val hippo = tx.queryGraph(
      Query( """MATCH (n:ANIMAL {name: {name}}) return n""",
        Map("name" -> "hippo")) // Cypher query parameters
    ).nodes.head
    hippo.properties("nose") = true
    tx.persistChanges(hippo)
  }

  db.transaction { tx =>
    // delete hippo
    tx.query( """MATCH (n:ANIMAL {name: "hippo"}) OPTIONAL MATCH (n)-[r]-() DELETE n,r""")

    // roll back deletion
    tx.rollback()
  }

  // interpret query result as a table
  val animals = db.queryTable("""MATCH (n:ANIMAL) OPTIONAL MATCH (n)-[r:EATS]->()
    RETURN n.name as name, COUNT(r) as eatcount""")

  println("\n" + animals.columns.mkString("\t")) // prints "name eatcount"
  for(row <- animals.rows) {
    print(row.cells(0).asInstanceOf[StringPropertyValue].value)
    print("\t")
    println(row.cells(1).asInstanceOf[LongPropertyValue].value)
  }
  println()
  // loop prints:
  //  dog	  0
  //  snake	2
  //  hippo	0

  val hungriest = animals.rows.maxBy(_.apply("eatcount").
    asInstanceOf[LongPropertyValue].value).
    apply("name").asInstanceOf[StringPropertyValue].value
  println("hungriest: " + hungriest) // prints "snake"


  // clear database
  db.query("MATCH (n) OPTIONAL MATCH (n)-[r]-() DELETE n,r")

  // shut down actor system
  restService.actorSystem.shutdown()
}
```

## License
renesca is free software released under the [Apache License, Version 2.0][Apache]

[Apache]: http://www.apache.org/licenses/LICENSE-2.0
