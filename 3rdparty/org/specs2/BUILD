ORG = 'org.specs2'
REV = '4.6.0'

def make_lib(name):
  jar_library(name=name,
    jars=[
      scala_jar(org=ORG, name=name, rev=REV, excludes=[exclude(org='org.scala-lang.modules')])
    ]
  )

make_lib('specs2-core')
make_lib('specs2-junit')
make_lib('specs2-mock')
make_lib('specs2-matcher-extra')

target(name='specs2',
  dependencies=[
    ':specs2-core',
    ':specs2-junit',
    ':specs2-mock'
  ]
)
