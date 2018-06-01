# These dependencies will be pinned to a version
managed_jar_dependencies(
  name='management',
  artifacts=[
    '3rdparty/org/scala-lang:scala-compiler',
    '3rdparty/org/scala-lang:scala-library',
  ],
)
REV_MYSQL = '5.1.41'
jar_library(name='mysql',
  jars=[
    jar('mysql', 'mysql-connector-java', REV_MYSQL)
  ],
  scope='runtime',
)
