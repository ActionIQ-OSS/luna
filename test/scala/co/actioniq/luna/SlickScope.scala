package co.actioniq.luna

import java.sql.Driver
import java.util.UUID

import com.typesafe.config.{Config, ConfigFactory}
import org.specs2.specification.Scope
import slick.jdbc.{JdbcBackend, JdbcDataSource}
import slick.jdbc.JdbcBackend.Database
import slick.util.{ClassLoaderUtil, DefaultSlickMDCExecutor}

trait SlickScope extends Scope {
  val uuid = UUID.randomUUID().toString.replaceAll("-", "")
  val h2ConfigString =
    s"""
      |h2mem = {
      |  url = "jdbc:h2:mem:$uuid;DB_CLOSE_DELAY=-1;DATABASE_TO_UPPER=false"
      |  driver = org.h2.Driver
      |  numThreads = 10
      |  numConnections = 10
      |  connectionPool = "HikariCP"
      |  keepAliveConnection = true
      |}
    """.stripMargin
  lazy val db = databaseFromConfig(
    "h2mem",
    ConfigFactory.parseString(h2ConfigString)
  )

  protected def databaseFromConfig(
    path: String,
    config: Config = ConfigFactory.load(),
    driver: Driver = null, //scalastyle:ignore
    classLoader: ClassLoader = ClassLoaderUtil.defaultClassLoader
  ): JdbcBackend.Database = {
    val usedConfig = if (path.isEmpty) config else config.getConfig(path)
    val source = JdbcDataSource.forConfig(usedConfig, driver, path, classLoader)
    val poolName = usedConfig.getString("connectionPool")
    val numThreads = usedConfig.getInt("numThreads")
    val maxConnections = source.maxConnections.fold(numThreads*5)(identity)
    val registerMbeans = false
    val executor = new DefaultSlickMDCExecutor().apply(
      poolName,
      numThreads,
      numThreads,
      1000,
      maxConnections,
      registerMbeans = registerMbeans
    )
    Database.forSource(source, executor)
  }
}
