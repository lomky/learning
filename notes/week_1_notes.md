# Coursera Ruby on Rails Web Services and Integration with MongoDB - Week 1 Notes

## Introduction to NoSQL and MongoDB
### Introduction to NoSQL

#### Overview
 - Rational behind NoSQL
 - scalling issues in RDBMS
 - NoSQL what is it?


#### Why RDBMS

  - Relational Databases - _popular_ and _commonly used_
  - Initially defined for non distributed.
  - Los Cost RDBMS alternatives (PostfreSQL, MySQL, SOLite)
  - Very _Transactional_ - across tables and commands, and can even be transactional across distributed resources (XA) -- at a cost
  - Supports _joins_ -- across multiple tables allowing for _normalized_ forms of data to be stored _once_.

#### Why NoSQL

Situation:

  - Explosion in data
    - Unstructured data. Open ended format; does not have a predefined data structure.
  - Object/Relational Impedance mismatch.
    - Objects are _constantly being moved_ in/out of table/rows
  - RDBMS normalization and joins are _powerful_, but add up in _cost_
    - Complex objects stored across many tables and rows can be _expensive_ to handle.

NoSQL:

  - "Big" data handling with _better performance_
  - Supports _unstructured data_
    - Unique data type extensions can be _easily integrated_ into _existing collections_
  - Operation issues (scale, performance, and availability)

#### Scaling Out

Vertical Vs Horizontal Scaling

  - Vertical
    - 2 CPUs, 1GM Ram, 20GB disk
    - Upgrade: 4CPU, 2GB Ram, 40GB disk
  - Horizontal
    - 2CPUs, 1GB Ram, 20GB Disk
    - Upgrade, 2x (2CPUs, 1GB Ram, 20GB Disk)

  - Horizontal scaling has the advantage of redundancy, cheaper, and has a higher upper limit

#### What is NoSQL

  - Stands for "Not Only SQL"
  - No Fixed Schema
    - can add different info
  - Non-relational data storage systems
    - a books authors live inside the same object
  - Examples:
    - mongoDB
    - redis
    - amazon DynamoDB
    - Couchbase
    - Apache HBASE
    - CouchDB
    - cassandra
    - memcached

#### Summary

  - NoSQL - very popular and major companies, especially social networking sites (twitter, facebook, etc) use NoSQL db
  - Excellent performance and stability, fast and scalable and fairly simple model
  - Supports unstructured format, which makes it very agile
  - NoSQL is mostly gained when access patterns to complex objects are understood and modeled correctly up front
    - Don't use it for no reason, use it for the right reason

### Categories of NoSQL

#### Topics

  - Categories of NoSQL
  - NoSQL vs. RDBMS

#### Categories of NoSQL - Key/Value

  - Value can be String or JSON
  - Key-value hash
  - Solutions
    - Dynamo
    - Redis
    - Memcached

| ID | Attributes |
|----|----|
| 1234 | John Doe |
| 1235 | { "Name" : "Godfather", "Genre" : "Drama", "Actor" : "Robert DeNiro", "Director" : "Francis Ford Coppola" } |

#### Categories of NoSQL - Document

  - Stores docuemnts based up of tagged elements
  - Persistent and query-able
  - Solutions
    - MongoDB
    - CouchDB

|  |
|----|
| { "id": 1234, "name": "Departed", "actors": [ { "actor": "Leo" }, { "actor": "Jack" } ], "director": "Scorsese", "genre": "drama" } |

#### Categories of NoSQL - Column

  - Uses flat structure, but wiht keys stored in _columns_ rather than rows
  - Solutions
    - Cassandra
    - Hbase

| ID | 101 | 102 | 103 |
|---|---|---|---|
| Name | The Godfather | The Departed | Titanic |
| Actor | Leo | Al Pacino | Leo |
| Director | Coppola | Scorsese | Cameron |

  - personal note: this makes my head hurt. Neat!

#### Categories of NoSQL - Graph

  - A network database that uses _edges and notes_ to _represent and store_ data
  - Solutions
    - Neo4J

```
                       ____________
                       | Departed  |           ___________
___________  --------> |___________| --------> | Scorsese |
| DiCaprio |           __________              |__________|
|__________| --------> | Titanic |
                       |_________|
```

#### NoSQL - What's Not Supported

  - Joins are _not supported_
    - Instead, embedded document or in middle tier code
    - See the movie object earlier, _contains_ the actor objects
  - ACID Transactions
    - Supported at a document level only

#### NoSQL vs RDBMS - How to pick?

  - By the _Nature of the data_
    - Row/column (structured) - use RDBMS
    - Unstructured, complex (geo-spatial or engineering data), requires nexted - use NoSQL
  - Schema
    - Static - RDBMS
    - Dynamic - NoSQL

  - Are items?
    - Self Contained? - NoSQL
    - Joins? - RDBMS

  - Flexibility of query
    - RDBMS - Joins allow for flexibility
    - NoSQL - Duplication of data, implement joins in middleware


#### Summary

  - 4 different categories of NoSQL, offering different categories
    - document based mongo, for us
  - Pick what is best for _your application_, Relational or NoSQL
    - neither is the _best_, it depends

### Introduction to MongoDB

#### Topics

  - What MongoDB is
  - Reasons to use MongoDB

#### What is MongoDB

  - Created by 10gen
    - term coined from humongous
  - Definition
    - MongoDB is an _open source, document-oriented_ databased designed with both _scalability and developer agility_ in mind
  - Storage: JSON-like documents and _"schemaless"_
  - Well suited for _Object Oriented programming_

  - Stores data in _BSON_ format
    - Binary JSON
  - Birnary form for representing _simple data structures_ and _associative arrays_.

Example:

```
{
  "_id": 101,
  "title": "The Departed",
  "type": "Movie",
  "director": "Martin Scorsese",
  "actors": [
    {
      "actorName": "Leo",
      "character": "Billy",
      "main": true,
      "urlCharacter": "http://imdb.com/...",
      "urlProfile": "http://imdb.com/..."
    },
    {
      "actorName": "Matt Damon",
      "character": "COlin",
      "main": true,
      "urlCharacter": "http://imdb.com/...",
      "urlProfile": "http://imdb.com/..."
    },
  ],
}
```

#### Document Store (Mapping)

| RDBMS | MongoDB |
|---|---|
| Database | Database |
| Table,View | Collection |
| Row | JSON Document |
| Column | Field |
| Index | Index |
| Join | Embedded Document / Linking across Document |
| Foreign Key | Reference |
| Partition Key | Shard |

#### Sample Query - SQL vs Mongo

| SQL | Mongo |
|---|---|
| `CREATE TABLE movies( movieid int NOT NULL AUTHOINCREMENT, name VARCAR(30), rating VARCHAR(6), PRIMARY KEY (movieid) ` | `db.movies.insert({ "id": 10, "name": "Titantic". "rating": "R" } )` |
| `SELECT * FROM movies` | `db.movies.find()` |
| `UPDATE movies SET rating = "NR" where movieid = 101` | `db.movies.update( {"id": 101 }, { $set: { rating: "NR"}})` |
| `DELETE FROM movies WHERE rating = "R"` | `db.movies.remove({ "rating": "R" })` |

#### Why MongoDB?

  - _"Queryable"_ documents
  - _No impedance mismatch_ between object and DB form
    - ideal for web applications (fast retrieval)
  - Quick and easy _integration_ of new data variations
  - _Rich API support_ (multiple languages)

#### Ruby On Rails & Mongo

  - [Ruby Driver](http://docs.mongodb.org/ecosystem/tutorial/ruby-driver-tutorial/)
  - [Mongoid](http://docs.mongodb.org/ecosystem/tutorial/ruby-mongoid-tutorial/)

#### MongoDB Users

  - MetLife
  - Expedia
  - Disney
  - ADP
  - Craigslist
  - more

#### MongoDB Core Topics with Ruby/Rails

  - MongoDB Ruby Driver
  - Aggregation Framework
  - GridFS - breaking large files into smaller chunks (performance)
  - Geospatial - index & query geospatial data
  - Mongoid

#### Summary

  - MongoDB
    - Open Source DB
    - auto scaling
    - high performance
    - schemaless & doc oriented

### Mongo Installation

#### Topics

  - install MongoDB
  - configure MongoDB
  - start MongoDB (`mongod`)
  - launch MongoDB shell (`mongo`)

#### MongoDB Installation Steps

  - Download MongoDB
  - Mongo needs a default data folder

Just follow this internet's steps.

#### Helpful Configuration

  - Journalling in MongoDB - allocates 3GB upfront
    - write-ahead _logging_ to _guarantee write operations_
  - For _casual_ development, may turn off
  - Setting _"nojournal=true"_ in mongod.conf will keep mongo from _claiming_ this space for write-ahead journalling: `mongod --config /etc/mongod.conf`
    - Never turn this off in production!

#### Starting MongoDB

  - Open a terminal and start mongoDB: `mongod`
    - this is the server running
  - Open another terminal and access the shell `mongo`

### MongoDB Basics
#### Topics

  - importing sample data
  - basics of MongoDB shell
  - MongoDB collections
  - IRB shell and MongoDB
  - Masic MongoDB commands in IRB

#### Importing datasets

  - download sample zips.json file from mongoDB
    - `media.mongodb.org/zips.json`
  - save it (curl'd)
  - run the import command:
    `$ mongoimport --db test --collection zips -drop --file zips.json`

#### Database, Documents and Collections

  - Mongo can create database _on the fly_
    - no need to create db beforehand
    - just `use` it
  - Documents
    - Unit of _storing data_ in a MongoDB db
    - JSON document
  - Collection (similar to tables in Rel DB)
    - Unit of _storing data_ in a MongoDB db
    - Collection of documents

#### Collection Types

  - Capped Collection
    - _fixed-size_ collections that support _high-throughput_ operations
    - insert and retrieve docuemnts based on _insertion order_
    - once a collection fills its allocated space, it _makes room_ for new docs by _overwriting_ the older documents in the collection
    - `db.createCollection("log", { capped : true, size : 5242880, max : 5000 } )`
    - Fifo, queue
    - Logs are a good example of this

#### Mongo Basics

  - Start the mongo shell: `mongo`
  - Switch to test db: `use test`
  - Test the data with a simple file command: `db.zips.findOne()`

```
> db.zips.findOne()
{
 "_id" : "01001",
 "city" : "AGAWAM",
 "loc" : [
  -72.622739,
  42.070206
 ],
 "pop" : 15338,
 "state" : "MA"
}
```

#### MongoDB Ruby Driver Setup

  - mongo-ruby driver
    - `gem update -system`
    - `gem install mongo`
    - `gem install bson_ext`
  - using the gem
    - `require mongo`

#### MongoDB Basics (irb shell)

  - start irb shell
  - commands:
    - `require 'mongo'`
    - `Mongo::Logger.logger.level = ::Logger::INFO`
    - `db = Mongo::Client.new('mongodb://localhost:27017')`
    - `db=db.use('test')`
    - `db.database.name`
    - `db.database.collection_names`
    - `db[:zips].find.first`

```
$ irb
irb(main):001:0> require 'mongo'
=> true
irb(main):002:0> Mongo::Logger.logger.level = ::Logger::INFO
=> 1
irb(main):003:0> db = Mongo::Client.new('mongodb://localhost:27017')
=> #<Mongo::Client:0x70356678855840 cluster=localhost:27017>
irb(main):004:0> db=db.use('test')
=> #<Mongo::Client:0x70356678791760 cluster=localhost:27017>
irb(main):005:0> db.database.name
=> "test"
irb(main):006:0> db.database.collection_names
=> ["zips"]
irb(main):007:0> db[:zips].find.first
=> {"_id"=>"01001", "city"=>"AGAWAM", "loc"=>[-72.622739, 42.070206], "pop"=>15338, "state"=>"MA"}
irb(main):008:0>
```

#### Summary

  - covered the basics of mongodb
  - DB, document, collection
  - mongoDB ruby driver & irb

### CRUD

### Inserting Documents

### Practice Programming Assignment - MongoDB Ruby Driver CRUD

### Find

### Paging

### Advanced Find

### Replace, Update, and Delete


## Integrating MongoDB and Rails
### Introduction: Integrating MongoDB with Ruby Driver

### Rails Setup

### DAO Class Infrastructure

### CRUD

### Scaffolding

### MVC Application

### MongoLab Setup

### Heroku Setup


