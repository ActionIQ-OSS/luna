package co.actioniq.luna.logging

import java.util.concurrent.{LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit}

import brave.internal.HexCodec
import brave.propagation.TraceContext
import brave.sampler.Sampler
import brave.{Span, Tracer, Tracing}
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.AppenderBase
import com.google.common.cache.CacheBuilder
import slick.jdbc.JdbcBackend
import zipkin.Endpoint
import zipkin.reporter.AsyncReporter
import zipkin.reporter.okhttp3.OkHttpSender

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

/**
  * Logback logger that writes to Zipkin.  It works by keeping a cache of the log with the query info mapped by
  * spanId-ThreadId.  When it receives a matching message of the duration, it will create a new trace span with
  * the start time of the original log time + duration as in the message reported by the zipkin logger.  We use a
  * cache map here in case we never get a duration message for some reason, this will not cause memory to blow up.
  */
class ZipkinLogbackAppender extends ZipkinLogbackAppenderTrait {
  override def traceIdKey: String = "X-Trace-Id"

  override def spanIdKey: String = "X-Span-Id"

  override def shouldTraceKey: String = "X-Should-Trace"
}

trait ZipkinLogbackAppenderTrait extends AppenderBase[ILoggingEvent] {
  def traceIdKey: String
  def spanIdKey: String
  def shouldTraceKey: String

  private var serviceName = "unknown"
  private var sampleRate = 1.0f
  private var url = ""
  private var tracer: Tracer = _
  private val minThreads = 1
  private val maxThreads = 10
  private val keepAliveTime: Duration = 1.minute
  private val threadPoolExecutor = new ThreadPoolExecutor(
    minThreads,
    maxThreads,
    keepAliveTime.toMillis,
    TimeUnit.MILLISECONDS,
    new LinkedBlockingQueue[Runnable]()
  )
  private val sqlCache = CacheBuilder.newBuilder().expireAfterWrite(3, TimeUnit.MINUTES).build[String, LogMeasurement]()

  /**
    * We have a string in the mdc, unsure if it is a long already or a hex that needs to be converted
    * @param id string representing a long or hex
    * @return long value of input
    */
  private def longOrHexToLong(id: String): Long = {
    try {
      id.toLong
    } catch {
      case _: NumberFormatException =>
        HexCodec.lowerHexToUnsignedLong(id)
    }
  }

  override def append(e: ILoggingEvent): Unit = {
    if (isStarted && Option(e).isDefined) {
      val time = e.getTimeStamp * 1000
      val traceIdOpt = Option(e.getMDCPropertyMap.get(traceIdKey))
      val spanIdOpt = Option(e.getMDCPropertyMap.get(spanIdKey))
      val shouldTrace = Option(e.getMDCPropertyMap.get(shouldTraceKey)).getOrElse("false") == "true"
      (shouldTrace, traceIdOpt, spanIdOpt) match {
        case (true, Some(traceId), Some(spanId)) =>
          val cacheKey = s"$spanId-${e.getThreadName}"
          if (e.getLoggerName == classOf[JdbcBackend].getName + ".statement") {
            sqlCache.put(cacheKey, LogMeasurement(e.getMessage, time))
          } else if (e.getLoggerName == classOf[JdbcBackend].getName + ".benchmark") {
            val startInfo = Option(
              sqlCache.getIfPresent(cacheKey)
            ).getOrElse(LogMeasurement("", time))
            val endTime = startInfo.timestamp + parseDurationMessageToDuration(e.getMessage)
            val query = startInfo.message + " -- " + e.getMessage
            sqlCache.invalidate(cacheKey)
            val parentContext = TraceContext.newBuilder().traceId(longOrHexToLong(traceId)).spanId(longOrHexToLong(spanId)).build()
            val endpoint = Endpoint.builder().serviceName("mysql").build()
            tracer.newChild(parentContext)
              .kind(Span.Kind.CLIENT)
              .name("MySql")
              .tag("Message", query)
              .remoteEndpoint(endpoint)
              .start(startInfo.timestamp)
              .finish(endTime)
          }
        case _ => ()
      }
    }
  }

  override def stop(): Unit = {
    if (isStarted){
      threadPoolExecutor.shutdown()
      super.stop()
    }
  }

  override def start(): Unit = {
    implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutor(threadPoolExecutor)

    tracer = Tracing.newBuilder()
      .localServiceName(serviceName)
      .reporter(AsyncReporter
        .builder(OkHttpSender.create(
          s"http://$url/api/v1/spans"
        ))
        .build()
      )
      .sampler(
        Sampler.create(sampleRate)
      )
      .build()
      .tracer()
    super.start()
  }

  def setServiceName(name: String): Unit = {
    serviceName = name
  }

  def getServiceName: String = serviceName

  def setUrl(name: String): Unit = {
    url = name
  }

  def getUrl: String = url

  def setSamplerate(rate: Float): Unit = {
    sampleRate = rate
  }

  def getSamplerate: Float = sampleRate

  private def parseDurationMessageToDuration(message: String): Long = {
    val durationString = message.split(' ').toList.last
    if (durationString.endsWith("ns")){
      durationString.replace("ns", "").toLong / 1000
    } else if (durationString.endsWith("µs")){ //scalastyle:ignore
      durationString.replace("µs", "").toLong //scalastyle:ignore
    } else if (durationString.endsWith("ms")){
      durationString.replace("ms", "").toLong * 1000
    } else {
      durationString.replace("s", "").toLong * 1000000
    }
  }
}

case class LogMeasurement(message: String, timestamp: Long)
