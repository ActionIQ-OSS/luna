ORG='com.twitter'

def make_lib(name, rev, excludes=[]):
  jar_library(name=name,
    jars=[
      scala_jar(org=ORG, name=name, rev=rev, excludes=excludes)
    ],
    scope="forced",
  )

make_lib('bijection-util', '0.9.2')

EXCLUDE_GUAVA = exclude(org='com.google.guava', name='guava')

UTIL_REV = '6.35.0'
UTIL_EXCLUDES = [
  EXCLUDE_GUAVA,
  exclude(org='junit', name='junit'),
  exclude(org='org.scala-tools.testing'),
  exclude(org='org.mockito')
]
make_lib('util-app', UTIL_REV, excludes=UTIL_EXCLUDES)
make_lib('util-core', UTIL_REV, excludes=UTIL_EXCLUDES)

FINAGLE_EXCLUDES = [
  exclude(org='org.jboss.netty', name='netty'),
  exclude(org='io.netty'),
  exclude(org='org.apache.thrift', name='libthrift'),
  exclude(org='com.fasterxml.jackson.core'),
  exclude(org='com.fasterxml.jackson.module')
]
FINAGLE_REV = '6.39.0'
make_lib('finagle-core', FINAGLE_REV, excludes=FINAGLE_EXCLUDES)
