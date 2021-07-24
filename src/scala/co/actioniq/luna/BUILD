scala_library(name='luna',
  dependencies=[
    '3rdparty/com/typesafe:slick',
    '3rdparty/com/typesafe:slick-hikaricp',
    '3rdparty/com/google/guava:guava',
    '3rdparty/org/clapper:grizzled-slf4j',
    '3rdparty:mysql',
    '3rdparty/ch/qos/logback:logback-classic-build',
    '3rdparty/io/zipkin/brave:brave',
    '3rdparty/io/zipkin/reporter:zipkin-sender-okhttp3',
  ],
  sources=['**/*.scala'],
  scope='default',
  strict_deps=True,
  provides = scala_artifact(
    org='co.actioniq',
    name='luna',
    repo=public,
    publication_metadata=pants_library("""
      Luna makes Slick DAOs simple
    """),
  )
)

