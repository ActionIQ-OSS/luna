ORG = 'com.typesafe.play'
REV = '2.4.11'
EXCLUDES = [
  exclude(org='com.fasterxml.jackson.core'),
  exclude(org='com.fasterxml.jackson.module')
]
def make_lib(name):
  jar_library(name=name,
    jars=[
      scala_jar(org=ORG, name=name, rev=REV, excludes=EXCLUDES)
    ],
    scope="forced"
  )

make_lib('play-json')
