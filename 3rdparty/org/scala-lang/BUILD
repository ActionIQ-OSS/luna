def make_lib(name):
  jar_library(name=name,
    jars=[
      jar(org='org.scala-lang', name=name, rev='2.11.12')
    ]
  )


jar_library(name="scala-xml",
  jars=[
    scala_jar(org='org.scala-lang.modules', name='scala-xml', rev='1.0.6')
  ]
)

make_lib('scala-compiler')
make_lib('scala-library')
make_lib('scala-reflect')

target(name='scala-compiler-tool', dependencies=[':scala-compiler'])
target(name='scala-library-tool', dependencies=[':scala-library'])
target(name='scala-repl-tool', dependencies=[':scala-compiler', ':scala-library', ':scala-reflect'])
