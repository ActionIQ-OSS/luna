scala_library(name='luna',
  dependencies=[
    '3rdparty/com/typesafe:slick',
    '3rdparty/com/typesafe:slick-hikaricp',
    '3rdparty/org/clapper:grizzled-slf4j',
    '3rdparty:mysql',
    '3rdparty/ch/qos/logback:logback-classic-build',
    '3rdparty/io/zipkin/brave:brave',
    '3rdparty/io/zipkin/reporter:zipkin-sender-okhttp3',
    '3rdparty/com/twitter:finagle-core',
  ],
  sources=rglobs('*.scala'),
  scope='default',
  strict_deps=True,
  provides = scala_artifact(
    org='co.actioniq',
    name='luna',
    repo=public,
  )
)

