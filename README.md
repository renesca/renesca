# renesca

Query and modify subgraphs from a Neo4j REST Database with Scala.

[![Build Status](https://travis-ci.org/renesca/renesca.svg?branch=master)](https://travis-ci.org/renesca/renesca)
[![Coverage Status](https://coveralls.io/repos/renesca/renesca/badge.svg?branch=master)](https://coveralls.io/r/renesca/renesca?branch=master)

## Features
- Interpret query results as graphs or tables
- Modify result graphs and persist the changes back to the database
- Transactions

## Installation

To use renesca in your sbt project, add this dependency to your ```build.sbt```:

```scala
libraryDependencies += "com.github.renesca" %% "renesca" % "0.2.1"
```

## Usage Example
This example is also available as sbt project: [renesca/renesca-example](https://github.com/renesca/renesca-example)

```scala
package renesca.example

import renesca.graph.{Node, Relation}
import renesca.parameter._
import renesca.parameter.implicits._
import renesca.{DbService, RestService, Transaction}
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
  val wholeGraph = db.queryGraph("MATCH (n) RETURN n LIMIT 1")
  if(wholeGraph.nonEmpty) {
    restService.system.shutdown()
    sys.error("Database is not empty.")
  }


  // create example graph:  snake -eats-> dog
  db.query("CREATE (:ANIMAL {name:'snake'})-[:EATS]->(:ANIMAL {name:'dog'})")

  val tx = new Transaction
  // dependency injection
  tx.restService = restService


  // query a subgraph from the database
  implicit val graph = tx.queryGraph("MATCH (n:ANIMAL)-[r]->() RETURN n,r")

  // access the graph like scala collections
  val snake = graph.nodes.find(_.properties("name").asInstanceOf[StringPropertyValue] == "snake").get

  // useful methods to access the graph (requires implicit val graph in scope)
  // e.g.: neighbours,  successors,  predecessors,  inDegree,  outDegree,  degree, ...
  val name = snake.neighbours.head.properties("name").asInstanceOf[StringPropertyValue].value
  println("Name of one snake neighbour: " + name) // prints "dog"

  // changes to the graph are tracked
  snake.labels += "REPTILE"
  snake.properties("hungry") = true

  // creating a local Node (a Node the database does not know about yet)
  val hippo = Node.local

  // changes to local Nodes are also tracked
  hippo.labels += "ANIMAL"
  hippo.properties("name") = "hippo"

  // add the local node to the Node Set
  graph.nodes += hippo

  // create a new local relation between a local and existing Node
  graph.relations += Relation.local(snake, "EATS", hippo)

  // persist all tracked changes to the database and commit the transaction
  tx.commit.persistChanges(graph)


  // interpret query result as a table
  val animals = db.queryTable("MATCH (n:ANIMAL) OPTIONAL MATCH (n)-[r:EATS]->() RETURN n.name as name, COUNT(r) as eatcount")

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

  val hungriest = animals.rows.maxBy(_.apply("eatcount").asInstanceOf[LongPropertyValue].value).
    apply("name").asInstanceOf[StringPropertyValue].value
  println("hungriest: " + hungriest) // prints "snake"


  // clear database
  db.query("MATCH (n) OPTIONAL MATCH (n)-[r]-() DELETE n,r")

  // shut down actor system
  restService.system.shutdown()
}
```
