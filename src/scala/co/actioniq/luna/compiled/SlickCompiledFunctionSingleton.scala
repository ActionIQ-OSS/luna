package co.actioniq.luna.compiled

import slick.lifted.CompiledFunction

/**
  * Object level singleton to store compiled queries to be reused
  */
trait SlickCompiledFunctionSingleton {
  private val compiledQueryMap: SingletonMap[String, CompiledFunction[_, _, _, _, _]] = new SingletonMap()

  /**
    * Try to retrieve a compiled query by function name or init using the specified function
    * @param functionName lookup key for compiled query
    * @param query compiled query to init if it does not exist
    * @tparam A input type for compiled function
    * @tparam B output type for compiled function
    * @return compiled fuynction
    */
  def getOrInitCompiledQuery[A, B](functionName: String)
    (query: => CompiledFunction[_, _, A, _, B]): CompiledFunction[_, _, A, _, B] = {
    compiledQueryMap
      .getOrInit(functionName, {
        query
      })
      .asInstanceOf[CompiledFunction[_, _, A, _, B]]
  }

  def getOptionCompiledQuery[A, B](functionName: String): Option[CompiledFunction[_, _, A, _, B]] = {
    compiledQueryMap
      .getOption(functionName)
      .map(_.asInstanceOf[CompiledFunction[_, _, A, _, B]])
  }
}

/**
  * Simple singleton map that only has one value per key
  * @tparam K key type
  * @tparam T value type
  */
class SingletonMap[K, T] {
  @volatile private var value: Map[K, T] = Map()

  def getOption(key: K): Option[T] = {
    value.get(key)
  }

  /**
    * Returns the internal value, or if internal value not set yet,
    * then initializes it with the supplied function.
    */
  def getOrInit(key: K, init: => T): T = {
    if (value.get(key).isEmpty) {
      this.synchronized {
        // you have to check again because "value" could have changed between first if and the synchronized block
        if (value.get(key).isEmpty) {
          value = value ++ Map(key -> init)
        }
      }
    }
    value(key)
  }
}
