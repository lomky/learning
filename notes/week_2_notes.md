# Coursera Ruby on Rails Web Services and Integration with MongoDB - Week 2 Notes

## Aggregation Framework
### Introduction to Aggregation
#### Topics

  - SQL to Mongo mapping
  - Aggregation framework
  - Aggregation pipelines
  - Aggregation example

#### SQL to Mongo (aggregation)

| SQL | Mongo |
| --- | ----- |
|WHERE|$match|
|GROUP BY|$group|
|SELECT|$project|
|ORDER BY|$sort|
|LIMIT|$limit|
|SUM()|$sum|
|COUNT()|$count|

| SQL | Mongo |
| --- | ----- |
| `SELECT COUNT(*) AS count FROM zips` | `db[:zips].find.aggregate([{:$group => {:_id => 0, count:{:$sum => }}}])`|
|`SELECT SUM(pop) AS total FROM zips`|`db[:zips].find.aggregate([{:$group => {:_id =>0, total: {:$sum => "$pop" }}}])`|

#### Aggregation Framework

  - Aggregations - operations that _process_ data records and _return_ computed results.
  - MongoDB provides a _rich_ set of aggregation operations like:
    - $project, $group, $match, $unwind, $sum, $limit
  - Running data aggregation on the mongo istance _simplifies_ application code and _limits_ resource requirements

#### Aggregation "pipeline"

  - Data processing pipeline
  - Filters that operate like queries
  - Grouping & sorting
  - Use of operators to return calculated documents
  - Ex: $limit, $sort, $skip, etc

#### Aggregation Example

See video timestamp: 04:10

  - start with all users
  - select where order is status A
  - group by the customer id

### `$project`
#### `$project`
  - _Reshapes_ a document stream by renaming, adding, or removing fields
  - Commonly used to _create_ computed values or sub-documents

#### $project - include specific fields

  - `db[:zips].find.aggregate([{:$project=>{:_id=>1, :city=>1, :state=>1, :pop=>1}},{:limit=>5}]).each {|r| pp r}`
    - displays all four fields (_id, city, state, pop)
    - :<field> => 1 or :<field> => true
      - specifies the inclusion of the field

#### $project - Alter Data

  - ```
       db[:zips].find.aggregate(
       [{:$project=>{:_id=>0,
       :state=>{:$toLower=>'$state'},
       :pop=>1 }}]).each {|r| pp r}
    ```
      - State in lower case

### `$group`
#### $group
  - similar to "GROUP BY" in RDBMS
  - Groups documents by some _specified_ expression
  - the output docuemtns contain an `_id` field which contains the _distinct group_ by key
  - the output documents can also contain _computed fields_ that hold the _values_ of some accumulator expression grouped by the $group's `_id` field

| Command | Description |
| ------- | ----------- |
| `$sum`  | Returns a sum for each group. |
| `$avg`  | Returns an average for each group. |
| `$max`  | Returns the highest expression value for each group. |
| `$min`  | Returns the lowest expression value for each group. |
| `$push` | Returns an array of expression values for each group. |
| `$addToSet` | Returns an array of unique expression values for each group. Order of the array elements is undefined |

#### $sum

  - Returns a _sum_ for each group
  - _Ignores_ non-numeric values
  - ```
      db[:zips].find.aggregate(
                          [{ 
                              :$group=>{
                                  :_id=>'$state', 
                                  :population=>{:$sum=>'$pop'}
                              }
                           }, :limit=>5]
                     ).each {|r| pp r}
    ```

#### $avg

  - ```
      db[:zips].find
               .aggregate(
                   [{
                     :$group=>{
                       :_id=>'$state',
                       :avg_population=>{:$avg=>'$pop'}}
                   }, :limit=>5])
               .each {|r| pp r}
    ```

#### $max and $min

  - ```
      db[:zips].find
               .aggregate(
                   [{
                     :$group=>{
                       :_id=>'$state',
                       :max_pop=>{:$max=>'$pop'}}
                   }, :limit=>5])
               .each {|r| pp r}
    ```


  - ```
      db[:zips].find
               .aggregate(
                   [{
                     :$group=>{
                       :_id=>'$state',
                       :min_pop=>{:$min=>'$pop'}}
                   }, :limit=>5])
               .each {|r| pp r}
    ```

#### $push

  - Returns an _array_ of _all values_ that result from applying _an expression_ to each document in a group of documents that _share_ the same group by key
  - ```
      db[:zips].find()
               .aggregate(
                   [{
                     :$group=>{
                       :_id=>{:city=>'$city', :state=>'$state'},
                       :zips=>{:$push=>'$_id'}}},
                   {:limit=>15}])
               .each {|r| pp r}
    ```
  - For each City in a State, collect the Zip Codes

#### $addToSet

  - Returns an _array_ of all _unique values_ that result from _applying an expression_ to each docuent in a group of docuemnts that _share_ teh same group by key. Order of hte elements in the output array is _unspecified_
  - ```
      db[:zips].find()
               .aggregate(
                   [{
                     :$group=>{
                       :_id=>0,
                       :zips=>{:$addToSet=>'$state'}}},
                   {:limit=>15}])
               .each {|r| pp r}
    ```
  - This will only show one entry per state it finds, whereas push would give as many of the state as matching documents
  - think 'set' as the math term

### `$match`

#### $match

  - $match _pipes_ the docs that _match its conditions_ to the next operator in the pipeline.
  - The $match qery syntax is _identical_ to the read operation query syntax (`find`).
  - $match - placed _early_ in the query - $match operations _minimize_ the amount of processig down the pipe

#### Example

  - Find and match result in the same info:
    - `db[:zips].find({:state=>'DE'}).first`
    - `db[:zips].find().aggregate([ {:$match => {:state=>'DE'}}]).first`

  - Find me all the states that have New York
    - `db[:zips].find().aggregate([{:$match=>{:state=>'NY'}}, {:$group=>{ "_id=>'$city', :population=>{:$sum=>'$pop'}}}, {:$project=>{ :_id=>0, :city=>'$_id', :population=>1}}, {:$sort=>{ :population=>-1 }}, {:$limit=>5}]).each {|r| pp r}`

### `$unwind`
#### $unwind

  - _Peels off_ the elements of an array _individually_, and returns a _stream_ of docs
  - $unwind returns _one doc_ for every member of hte unwould array within _every_ source document.

#### Examples

  - without unwind:
    - `db[:zips].find().aggregate([{:$match=>{:city=>'ELMIRA'}}, {:group=>{:_id=>{:city=>'$city',:state=>'$state'}, :zips=>{:addToSet=>'$_id}}}]).each {|r| pp r}`
    - Note: Observe "ELIMIRA,NY", we have all three zips gathered into one
  - with unwind:
    - `db[:zips].find().aggregate([{:$match=>{:city=>'ELMIRA'}}, {:group=>{:_id=>{:city=>'$city',:state=>'$state'}, :zips=>{:addToSet=>'$_id}}}, {:$unwind=>'$zips'}]).each {|r| pp r}`
    - Note: Observe "ELIMIRA,NY", we have all each of hte three zips entry for ELIMIRA as a distinct entry

## Schema Design
### Schema Design
#### Topics

  - MongoDB - schema design
  - Document store - mapping
  - Application specific vs independent approach
  - schemaless and content richness

#### Document Store (Mapping)

  - Row    -> JSON Doc
  - Column -> Field

#### MongoDB - BSON Types

  - BSON - buinary serializaiton formet used to _store documents_ and _make remote procedure calls_ in mongoDB
  - BSON support data types:
    - string
    - integrer
    - boolean
    - double
    - min/mac
    - arrays
    - timestamp
    - object
    - nill
    - symbol
    - date
    - object id
    - binary data
    - code
    - regex

#### MongoDB - Schema Design

  - Support _Rich_ Document
    - Embedded/Linked data (joins)
    - No contraints (no Foreign Key) - makes it very _flexible_)
  - Schema-Less
    - Conceptually yes, but there is still _structure_
      - but not strict

#### MongoDB - Schema Design Example

  - example contains `runtime: 151 min`
  - combined a size and the units.
    - size: 151
    - unit: "min"
  - making this distinction allows better manipulation within the db

### Normalization
#### Topics

  - Normalization and Mongo
  - 3rd normal form
  - Single collection
  - Multiple collection

#### Normalization - 3rd Normal Form (Problem)

If you have multiple entries for a single person across your database, it is hard to update their name in all places.
You can create inconsistent data.

To avoid this, you utilize spitting data across multiple tables. The person has their own table, and the movie table references them.

#### Do-normalized/Embedded approach

Mongo has no foreign key, so instead we have the embedded objects. They can also contain links to another collection that contains info about the entity.

How is this not foreign keys and linking?

### Relationships
#### Topics
  - Relationships
    - One to One
    - One to Many
    - Many to Many

#### One to One Relationships

  - Employee -> Address

#### One to Many Relationships

  - Customer -> Address
    - Same as embedded, but now in an array

#### Many to Many Relationships

  - Movie -> Actor
    - Movie can have many actors
    - Actors can be in many movies
  - Two design approaches
    - Embedding
    - Linking

  - Can put relation in _either one_ of hte documents
  - Application centric approach - focus will be on _how the data is accessed_ from the app

#### Linking vs Embedding

  - Embedding - similar to pre-joins
  - Embedded docs are _easy to handle_ for clients
  - Linking - more flexible but _extra work_ at application level

## GridFS and Geospatial
### GridFS

### GridFS Demo 1

### GridFS Demo 2

### GridFS Demo 3

### Geospatial

### Geospatial Demo

## Indexes
### Intro to Indexes

### Created Indexes

### Listing & Deleting Indexes

### Unique, Sparse, & TTL Indexes

