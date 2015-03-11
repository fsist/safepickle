package com.fsist.safepickle

import scala.annotation.StaticAnnotation

/** Place this annotation on a class parameter to make the Autogen pickler use this name in the pickled version
  * instead of the parameter's name.
  */
case class Name(value: String) extends StaticAnnotation

/** Place this annotation on a class parameter to make the Autogen pickler write it out even if its value equals
  * the default value. */
case class WriteDefault() extends StaticAnnotation
