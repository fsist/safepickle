package io.github.danarmak.safepickle

/** A factory for Reader and Writer implementations, with an underlying representation of type Repr. */
trait PicklingBackend[Repr] {
  def reader(repr: Repr): Reader
  def writer(): Writer[Repr]
}
