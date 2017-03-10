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
| 1235 | {
  "Name" : "Godfather",
  "Genre" : "Drama",
  "Actor" : "Robert DeNiro",
  "Director" : "Francis Ford Coppola"
} |

#### Categories of NoSQL - Document

  - Stores docuemnts based up of tagged elements
  - Persistent and query-able
  - Solutions
    - MongoDB
    - CouchDB

|  |
|----|
| {
  "id": 1234,
  "name": "Departed",
  "actors": [
    {
      "actor": "Leo"
    },
    {
      "actor": "Jack"
    }
  ],
  "director": "Scorsese",
  "genre": "drama"
} |

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

### Mongo Installation

### MongoDB Basics

## CRUD
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


