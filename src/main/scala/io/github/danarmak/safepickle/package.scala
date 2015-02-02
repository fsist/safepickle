package io.github.danarmak

/** Import safepickle._ to get all implicits defined in the library in scope. */
package object safepickle {
  import PrimitivePicklers._
  import CollectionPicklers._
}
